compute_eta <- function(data) {
  # Ensure the strikes are numeric and compute log-strikes
  log_strikes <- log(data_3m$strike)
  
  # Compute the minimum and maximum log-strike values
  k_min <- min(log_strikes)
  k_max <- max(log_strikes)
  
  # Calculate eta based on the range of log-strikes
  eta <- pi / max(abs(k_min), abs(k_max))
  
  #return(list(eta= eta, k_min=k_min, k_max=k_max))
  return(c(eta))
}

