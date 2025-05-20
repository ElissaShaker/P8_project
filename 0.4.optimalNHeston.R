start_time <- Sys.time()
# Parameters
alpha = 1.5 # as carr and madan
r = 0.0183  # calculated in the project

obtainHestonMetrics <- function(N, data_3m) {
  # Calculate time to maturity
  data_3m <- data_3m %>%
    mutate(expiry_date = as.Date(as.character(expiry), format = "%y%m%d"),
           date = as.Date(as.POSIXct(t/1000, origin = "1970-01-01")),
           tau = as.numeric(expiry_date - date)/365)
  
  S_0 <- data_3m$c[1]  # Using closing price as underlying price
  
  # Add strike prices
  data_3m <- data_3m %>%
    mutate(strike = strike,
           S_0 = S_0)
  
  ##################################
  #### FFT assumptions and grid ####
  ##################################
  # Calculate a fitting eta_value, lambda, k, b, and v
  eta_value <- compute_eta(data_3m)
  lambda <- 2 * pi / (N * eta_value)
  k <- -pi / eta_value + lambda * (1:N - 1)
  b <- pi / eta_value
  v <- eta_value * (0:(N - 1))
  
  # Find all different maturity times
  maturity.times <- data_3m$tau %>% unique
  
  ###################################
  #### Heston Characteristic fct #### \eqref{eq:hestcharact} 
  ###################################
  phi <- function(v, sigma_0_sqrd, kappa, sigma, theta, rho, M) {
    d <- sqrt((rho * theta * v * 1i - kappa)^2 - theta^2 * (-1i * v - v^2))
    g <- (kappa - rho * theta * v * 1i - d) / (kappa - rho * theta * v * 1i + d)
    
    return(
      exp(1i * v * (log(S_0) + r * M)) *
        exp(sigma * kappa / theta^2 * ((kappa - rho * theta * v * 1i - d) * M -
                                       2 * log((1 - g * exp(-d * M)) /
                                                 (1 - g)))) *
        exp(sigma_0_sqrd / theta^2 * (kappa - rho * theta * 1i * v - d) *
              (1 - exp(-d * M)) / (1 - g * exp(-d * M)))
    )
  }
  
  #############################################
  #### Fourier transform of the Call Price #### \eqref{eq:sejereje} 
  #############################################
  # psi function
  psi <- function(v, sigma_0_sqrd, kappa, sigma, theta, rho, M) {
    return(
      exp(-r * M) * phi(v - (alpha + 1) * 1i, sigma_0_sqrd, kappa, sigma,
                        theta, rho, M) / (alpha^2 + alpha - v^2 +
                                            (2 * alpha + 1) * v * 1i)
    )
  }
  
  ########################
  #### Simpson's rule #### \eqref{eq:elissaergrim}
  ########################
  # The kronecker delta function
  kronecker_delta <- function(n) {
    return(n == 0)
  }
  
  # Calculate the sequence for which to perform fft on
  x <- function(sigma_0_sqrd, kappa, sigma, theta, rho, M) {
    return(exp(1i * b * v) * psi(v, sigma_0_sqrd, kappa, sigma, theta, rho, M) *
             eta_value / 3 * (3 + (-1)^(1:N) - kronecker_delta(0:(N - 1))))
  }
  
  # Calculates the option surface based on parameters
  optionSurface <- function(sigma_0_sqrd, kappa, sigma, theta, rho) {
    surface <- matrix(nrow = length(maturity.times), ncol = N)
    for(i in 1:length(maturity.times)) {
      surface[i,] <- Re(exp(-alpha * k) / pi * fft(x(sigma_0_sqrd, kappa, sigma,
                                                     theta, rho, maturity.times[i])))
    }
    return(surface)
  }
  
  ##########################
  #### Calibration step ####
  ##########################
  # For all maturity times and all prices find the error
  ErrorFunction <- function(params, minimize = T) {
    sigma_0_sqrd <- abs(params[1])
    kappa <- abs(params[2])
    sigma <- abs(params[3])
    theta <- abs(params[4])
    rho <- params[5] / (1 + abs(params[5]))
    
    surface <- optionSurface(sigma_0_sqrd, kappa, sigma, theta, rho)
    C <- surface[cbind(closest.maturity.index, closest.strike.index)]
    #err <- {abs(data_3m$c - C)^2} %>% sum     # Using closing price as option price 
    err <- {abs(data_3m$vw - C)^2} %>% sum   # Use vw instead: Volume-weighted average price (VWAP), reflects actual trading prices, weighted by volume (less sensitive to outliers than o or c, more stable than h/l)
    if(minimize) {
      {err/nrow(data_3m)} %>% return
    } else {
      err_abs <- {abs(data_3m$vw - C)} %>% sum
      MEAN = mean(data_3m$c)
      RMSE = sqrt(err/nrow(data_3m))
      MAE = err_abs / nrow(data_3m)
      c(RMSE = RMSE, MAE = MAE, 
        pRMSE = RMSE / MEAN * 100,
        pMAE = MAE / MEAN * 100) %>% return
    }
  }

#deepseek anbefaling
  initial_params <- c(
    sigma_0_sqrd = 0.09,  # Adjust based on stock volatility
    kappa = 1.0,          # Increase if volatility reverts quickly
    sigma = 0.07,           # Long-term variance (close to sigma_0_sqrd)
    theta = 0.2,          # Higher = more volatile volatility
    rho = -0.7            # Negative for equities
  )
  
  # Find the closest k_u to the real strike price
  closest.maturity.index <- data_3m %>% nrow %>% numeric
  closest.strike.index <- data_3m %>% nrow %>% numeric
  for(i in 1:nrow(data_3m)) {
    closest.maturity.index[i] <- which(maturity.times == data_3m$tau[i])
    closest.strike.index[i] <- which.min(abs(exp(k) - data_3m$strike[i]))
  }
  
  res <- optim(initial_params, ErrorFunction, method = "SANN",
               control = list(
                 temp = 10,
                 tmax = 10
               ))

  transformed_parameters <- res$par
  transformed_parameters[1:4] %<>% abs
  transformed_parameters[5] %<>% {. / (1 + abs(.))}
  return(list(ErrorFunction(res$par, minimize = F), 
              transformed_parameters
  ))
}

N <- 2^(8:12)
results <- list()
for(i in 1:length(N)) {
  cat(paste("Running for N =", N[i], "\n"))
  results[[i]] <- obtainHestonMetrics(N[i], data_3m)
  cat("\nDone!\n")
}

# error metric table
summary_table <- data.frame(
  N = N,
  RMSE = sapply(results, function(x) x[[1]]["RMSE"]),
  MAE = sapply(results, function(x) x[[1]]["MAE"]),
  pRMSE = sapply(results, function(x) x[[1]]["pRMSE"]),
  pMAE = sapply(results, function(x) x[[1]]["pMAE"])
)

# Parameter estimates
param_table <- data.frame(
  N = N,
  sigma_0_sqrd = sapply(results, function(x) x[[2]][1]),
  kappa = sapply(results, function(x) x[[2]][2]),
  sigma = sapply(results, function(x) x[[2]][3]),
  theta = sapply(results, function(x) x[[2]][4]),
  rho = sapply(results, function(x) x[[2]][5])
)

summary_table
param_table

slut_time <- Sys.time()
slut_time - start_time


#####################
#### Latex table ####
#####################
conclusion <- cbind(summary_table, param_table[,-1])
conclusion

conclusion <- conclusion %>% 
  setNames(c("N", "RMSE", "MAE", "pRMSE", "pMAE", 
             "\\(\\sigma^2\\)", "\\(\\kappa\\)", "\\(\\sigma\\)", "\\(\\theta\\)", "\\(\\rho\\)")) 

conclusion$N <- paste0("\\(2^{", 8:12, "}\\)")

Heston_table <- knitr::kable(
  conclusion,
  format = "latex",
  booktabs = TRUE,
  digits = 5,
  caption = "Heston Model Results",
  escape = FALSE, 
  label = "Heston_results",
  align = "c",
  position = "h"
  
)

# Save to .tex file
#writeLines(Heston_table, "Latex/Heston_results.tex")

