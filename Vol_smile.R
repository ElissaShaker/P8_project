library(ggplot2)

# Strike-priser
K <- seq(50, 150, by = 1)

# Simuleret volatility smile
implied_vols <- 0.2 + 0.1 * (1 - exp(-((K - 100)^2) / (2 * 200)))

df <- data.frame(Strike = K, ImpliedVolatility = implied_vols)
ggplot(df, aes(x = Strike, y = ImpliedVolatility)) +
  geom_line(color = "blue", size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(title = "Volatility Smile",
       x = "Strike Price",
       y = "Implied Volatility") +
  theme(plot.title = element_text(hjust = 0.5))

