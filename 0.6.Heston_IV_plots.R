##################
#### IV Smile ####
##################
smile_data <- results_BFGS$data %>%
  filter(!is.na(iv_from_heston)) %>%
  group_by(strike) %>%
  reframe(
    avg_iv = mean(iv_from_heston, na.rm = TRUE),
    avg_tau = mean(tau, na.rm = TRUE),
    log_moneyness = log(S_0 / strike)
  )

# log moneyness
smile_data <- smile_data %>% arrange(log_moneyness)
log_moneyness_iv <- ggplot(smile_data, aes(x = log_moneyness, y = avg_iv)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Heston Model: Implied Volatility Smile (Grouped by Strike)",
       x = "Log-Moneyness (log(S/K))",
       y = "Average Implied Volatility") +
  theme_minimal()

log_moneyness_iv

#ggsave("Latex/heston_logmoneyness_smile.png", plot = log_moneyness_iv, width = 8, height = 6, dpi = 300)


# strike
smile_data <- smile_data %>% arrange(strike)
strike_iv <- ggplot(smile_data, aes(x = strike, y = avg_iv)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Heston Model: Implied Volatility Smile (Grouped by Strike)",
       x = "Strike",
       y = "Average Implied Volatility") +
  theme_minimal()
strike_iv

#ggsave("Latex/heston_strike_smile.png", plot = strike_iv, width = 8, height = 6, dpi = 300)



##################
#### Maturity ####
##################
# Create maturity buckets
results_BS <- results_BS %>%
  mutate(maturity_bucket = cut(tau, breaks = quantile(tau, probs = c(0, 1/3, 2/3, 1)), include.lowest = TRUE))

smile_maturity <- results_BS %>%
  filter(!is.na(iv_from_heston)) %>%
  group_by(strike, maturity_bucket) %>%
  reframe(
    avg_iv = mean(iv_from_heston),
    log_moneyness = log(S_0 / strike)
  )

# Create buckets and label with average tau
results_BS <- results_BS %>%
  filter(!is.na(iv_from_heston)) %>%
  mutate(maturity_bucket = ntile(tau, 3))  # create 3 groups

# Compute bucket labels
bucket_labels <- results_BS %>%
  group_by(maturity_bucket) %>%
  summarize(label = paste0("~", round(mean(tau) * 365), " days"))

# Join labels back to main data
results_BS <- results_BS %>%
  left_join(bucket_labels, by = "maturity_bucket")

# Aggregate by strike and label
smile_maturity <- results_BS %>%
  group_by(strike, label) %>%
  summarize(
    avg_iv = mean(iv_from_heston, na.rm = TRUE),
    log_moneyness = log(S_0 / strike),
    .groups = "drop"
  )

smiles_by_maturity <- ggplot(smile_maturity, aes(x = log_moneyness, y = avg_iv, color = label)) +
  geom_line(size = 1) +
  labs(
    title = "Implied Volatility Smiles Across Maturities",
    x = "Log-Moneyness", y = "Implied Volatility",
    color = "Avg. Maturity"
  ) +
  theme_minimal()


#ggsave("Latex/smiles_by_maturity.png", plot = smiles_by_maturity, width = 8, height = 6, dpi = 300)

