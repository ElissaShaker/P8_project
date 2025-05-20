#################################
#### Black-Scholes Functions ####
#################################
# d1 for Black-Scholes formula
d1 <- function(x, K, v_sqrd, tau, r) {
  (log(x / K) + (r + 0.5 * v_sqrd) * tau) / (sqrt(v_sqrd * tau))
}

# d2 for Black-Scholes formula
d2 <- function(x, K, v_sqrd, tau, r) {
  (log(x / K) + (r - 0.5 * v_sqrd) * tau) / (sqrt(v_sqrd * tau))
}

# Black-Scholes Call Price
C <- function(x, K, v_sqrd, tau, r) {
  x * pnorm(d1(x, K, v_sqrd, tau, r)) - K * exp(-r * tau) * pnorm(d2(x, K, v_sqrd, tau, r))
}

obtainBSMetrics <- function(data, risk_free_rate = 0.0183, underlying_symbol = "SPY") {
  # Convert timestamp to POSIXct date
  data$date <- as.POSIXct(data$t / 1000, origin = "1970-01-01", tz = "UTC")
  
  # Get strike prices
  strikes <- data$strike
  
  # Get underlying prices
  underlying_data <- tq_get(underlying_symbol, get = "stock.prices") %>% 
    select(date, close) %>% 
    mutate(date = as.Date(date))
  
  # Merge with option data
  data <- data %>%
    mutate(underlying_price = underlying_data$close[match(as.Date(date), underlying_data$date)])
  
  # Calculate time to expiration (years)
  expiry_date <- as.Date(as.character(unique(data$expiry)), format = "%y%m%d")
  data$tau <- as.numeric(expiry_date - as.Date(data$date))/365
  
  # Filter invalid rows
  valid_data <- data[data$tau > 0 & !is.na(data$underlying_price), ]
  if(nrow(valid_data) == 0) stop("No valid data points after filtering")
  
  # Calculate volatility (using last 3 months of underlying data)
  recent_underlying <- underlying_data %>% 
    filter(date >= (min(valid_data$date) - 90))
  returns <- diff(log(recent_underlying$close))
  vol <- sd(returns, na.rm = TRUE) * sqrt(252)
  
  # Calculate BS prices
  valid_data$BS_price <- C(
    x = valid_data$underlying_price,
    K = valid_data$strike,
    v_sqrd = vol^2,
    tau = valid_data$tau,
    r = risk_free_rate
  )
  
  # Calculate metrics
  option_prices <- valid_data$c  # Using closing prices
  errors <- option_prices - valid_data$BS_price
  RMSE <- sqrt(mean(errors^2, na.rm = TRUE))
  MAE <- mean(abs(errors), na.rm = TRUE)
  MEAN <- mean(option_prices, na.rm = TRUE)
  
  return(c(
    MEAN = MEAN,
    RMSE = RMSE,
    MAE = MAE,
    pRMSE = RMSE / MEAN * 100,
    pMAE = MAE / MEAN * 100,
    sigma = vol
    ))
}

obtainBSMetrics(data_3m)

#####################
#### Latex table ####
#####################
SPY <- obtainBSMetrics(data_3m) %>%
  setNames(c("MEAN", "RMSE", "MAE", "pRMSE", "pMAE", "\\(\\sigma\\)"))

# Transpose to make horizontal
SPY <- t(data.frame(SPY))

# latex table
BS_table <- knitr::kable(
  SPY,
  format = "latex",
  booktabs = TRUE,
  digits = 5,
  caption = "Black-Scholes Model Results",
  escape = FALSE, 
  label = "BS_results",
  align = "c",
  position = "h"
)

#writeLines(BS_table, "Latex/BS_resultar.tex")
