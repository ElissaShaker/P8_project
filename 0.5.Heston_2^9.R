black_scholes_call <- function(S, K, T, r, sigma) {
  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
}


implied_volatility <- function(C_target, S, K, T, r, tol = 1e-6, max_iter = 100) {
  tryCatch({
    uniroot(function(sigma) black_scholes_call(S, K, T, r, sigma) - C_target,
            lower = 1e-4, upper = 5, tol = tol, maxiter = max_iter)$root
  }, error = function(e) NA)
}

obtainHestonMetrics_BFGS <- function(N, data_3m) {
  # Calculate time to maturity
  data_3m <- data_3m %>%
    mutate(expiry_date = as.Date(as.character(expiry), format = "%y%m%d"),
           date = as.Date(as.POSIXct(t / 1000, origin = "1970-01-01")),
           tau = as.numeric(expiry_date - date) / 365)
  
  S_0 <- data_3m$c[1] # Using closing price as underlying price
  
  # Add strike prices
  data_3m <- data_3m %>% mutate(strike = strike, S_0 = S_0)
  
  ##################################
  #### FFT assumptions and grid ####
  ##################################
  # Calculate a fitting eta_value, lambda, k, b, and v
  eta_value <- compute_eta(data_3m)
  lambda <- 2 * pi / (N * eta_value)
  b <- log(S_0)  # center the strike grid
  k <- -N / 2 * lambda + lambda * (0:(N - 1)) + b
  v <- eta_value * (0:(N - 1))
  
  # Find all different maturity times
  maturity.times <- unique(data_3m$tau)
  
  ###################################
  #### Heston Characteristic fct #### \eqref{eq:hestcharact}
  ###################################
  phi <- function(v, sigma_0_sqrd, kappa, sigma, theta, rho, M) {
    d <- sqrt((rho * theta * v * 1i - kappa)^2 - theta^2 * (-1i * v - v^2))
    g <- (kappa - rho * theta * v * 1i - d) / (kappa - rho * theta * v * 1i + d)
    
    exp(1i * v * (log(S_0) + r * M)) *
      exp(sigma * kappa / theta^2 * ((kappa - rho * theta * v * 1i - d) * M -
                                       2 * log((1 - g * exp(-d * M)) / (1 - g)))) *
      exp(sigma_0_sqrd / theta^2 * (kappa - rho * theta * 1i * v - d) *
            (1 - exp(-d * M)) / (1 - g * exp(-d * M)))
  }
  
  #############################################
  #### Fourier transform of the Call Price #### \eqref{eq:sejereje} 
  #############################################
  # psi function
  psi <- function(v, sigma_0_sqrd, kappa, sigma, theta, rho, M) {
    exp(-r * M) * phi(v - (alpha + 1) * 1i, sigma_0_sqrd, kappa, sigma, theta, rho, M) /
      (alpha^2 + alpha - v^2 + 1i * (2 * alpha + 1) * v)
  }
  
  ########################
  #### Simpson's rule #### \eqref{eq:elissaergrim}
  ########################
  # Calculate the sequence for which to perform fft on
  x <- function(sigma_0_sqrd, kappa, sigma, theta, rho, M) {
    weights <- rep(2, N)
    weights[seq(2, N - 1, by = 2)] <- 4
    weights[1] <- 1
    weights[N] <- 1
    psi_val <- psi(v, sigma_0_sqrd, kappa, sigma, theta, rho, M)
    exp(1i * b * v) * psi_val * eta_value / 3 * weights
  }
  
  # Calculates the option surface based on parameters
  optionSurface <- function(sigma_0_sqrd, kappa, sigma, theta, rho) {
    surface <- matrix(nrow = length(maturity.times), ncol = N)
    for (i in 1:length(maturity.times)) {
      surface[i, ] <- Re(exp(-alpha * k) * fft(x(sigma_0_sqrd, kappa, sigma, theta, rho, maturity.times[i]))) / pi
    }
    return(surface)
  }
  
  ##########################
  #### Calibration step ####
  ##########################
  # For all maturity times and all prices find the error
  ErrorFunction <- function(params, minimize = TRUE) {
    sigma_0_sqrd <- abs(params[1])
    kappa <- abs(params[2])
    sigma <- abs(params[3])
    theta <- abs(params[4])
    rho <- params[5]
    
    surface <- optionSurface(sigma_0_sqrd, kappa, sigma, theta, rho)
    
    # Compute new indices every time, because `k` depends on `N`
    closest.maturity.index <- sapply(data_3m$tau, function(t) which(maturity.times == t))
    closest.strike.index <- sapply(data_3m$strike, function(K) which.min(abs(exp(k) - K)))
    
    C <- surface[cbind(closest.maturity.index, closest.strike.index)]
    err <- mean(((data_3m$vw - C) / data_3m$vw)^2)
    
    if (minimize) {
      return(err)
    } else {
      err_abs <- sum(abs(data_3m$c - C))
      MEAN <- mean(data_3m$c)
      RMSE <- sqrt(err)
      MAE <- err_abs / nrow(data_3m)
      return(c(RMSE = RMSE, MAE = MAE, pRMSE = RMSE / MEAN * 100, pMAE = MAE / MEAN * 100))
    }
  }
  
  initial_params <- c(
    sigma_0_sqrd = 0.04,
    kappa = 1.5,
    sigma = 0.4,
    theta = 0.04,
    rho = -0.7
  )
  
  res <- optim(
    initial_params,
    ErrorFunction,
    method = "L-BFGS-B",
    lower = c(0.001, 0.01, 0.01, 0.01, -0.99), # lower bounds: sigma^2, kappa, sigma, theta, rho
    upper = c(0.5, 5.0, 2.0, 1.0, -0.2)        # upper bounds
  )
  
  transformed_parameters <- res$par
  transformed_parameters[1:4] <- abs(transformed_parameters[1:4])
  transformed_parameters[5] <- transformed_parameters[5] / (1 + abs(transformed_parameters[5]))
  
  # Get calibrated prices
  sigma_0_sqrd <- transformed_parameters[1]
  kappa <- transformed_parameters[2]
  sigma <- transformed_parameters[3]
  theta <- transformed_parameters[4]
  rho <- transformed_parameters[5]
  
  surface <- optionSurface(sigma_0_sqrd, kappa, sigma, theta, rho)
  
  # Find the closest k_u to the real strike price
  closest.maturity.index <- sapply(data_3m$tau, function(t) which(maturity.times == t))
  closest.strike.index <- sapply(data_3m$strike, function(K) which.min(abs(exp(k) - K)))
  model_prices <- surface[cbind(closest.maturity.index, closest.strike.index)]
  
  # Compute implied volatilities
  iv <- mapply(function(price, K, T) {
    implied_volatility(price, S_0, K, T, r)
  }, model_prices, data_3m$strike, data_3m$tau)
  
  return(list(
    metrics = ErrorFunction(res$par, minimize = F),
    parameters = transformed_parameters,
    model_prices = model_prices,
    implied_volatility = iv,
    data = cbind(data_3m, heston_price = model_prices, iv_from_heston = iv)
  ))
}

results_BFGS <- obtainHestonMetrics_BFGS(2^9, data_3m)



#####################
#### Latex table ####
#####################
# Create a summary table
summary_table_BFGS <- results_BFGS[[1]]
param_table_BFGS <- results_BFGS[[2]]

SPY <- c(summary_table_BFGS, param_table_BFGS) 
SPY <- SPY %>% 
  setNames(c("RMSE", "MAE", "pRMSE", "pMAE", 
             "\\(\\sigma^2\\)", "\\(\\kappa\\)", "\\(\\sigma\\)", "\\(\\theta\\)", "\\(\\rho\\)")) 

SPY <- t(as.data.frame(SPY))

Heston_table_BFGS <- knitr::kable(
  SPY,
  format = "latex",
  booktabs = TRUE,
  digits = 5,
  caption = "Heston Model Results for $N=2^9$",
  escape = FALSE, 
  label = "Heston_results_BFGS",
  align = "c",
  position = "h"
  
)

#writeLines(Heston_table_BFGS, "Latex/Heston_results_BFGS_iv.tex")
