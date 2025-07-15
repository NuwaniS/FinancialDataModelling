library(quantmod)
library(xts)

# Load data
# log_returns from the financial_model.R will be used

# Parameters
window_size <- 1000
alpha <- 0.05
n <- length(log_returns)
VaR_series <- rep(NA, n - window_size)

# Rolling historical VaR
for (i in 1:(n - window_size)) {
  window_data <- log_returns[i:(i + window_size - 1)]
  VaR_series[i] <- quantile(window_data, probs = alpha, na.rm = TRUE)
}

# Convert to xts for plotting
VaR_xts <- xts(VaR_series, order.by = index(log_returns[(window_size + 1):n]))
actual_returns <- log_returns[(window_size + 1):n]

# Plot
plot(actual_returns, main = "Rolling 1-Day Historical VaR (95%)", col = "black")
lines(VaR_xts, col = "red", lwd = 2)
legend("bottomleft", legend = c("Actual Returns", "VaR 95%"), col = c("black", "red"), lty = 1)

# Compute violations
violations <- actual_returns < VaR_xts
num_violations <- sum(violations, na.rm = TRUE)
expected_violations <- length(violations) * alpha
violation_rate <- mean(violations, na.rm = TRUE)

cat("Number of Violations:", num_violations, "\n")
cat("Expected Violations (at", 100 * (1 - alpha), "%):", expected_violations, "\n")
cat("Violation Rate:", round(violation_rate * 100, 2), "%\n")
