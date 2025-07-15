library(rugarch)
library(quantmod)
library(xts)

# Load data
# log_returns from the financial_model.R will be used

# Rolling window parameters
window_size <- 500       # length of rolling window
alpha <- 0.05            # for 95% confidence
z <- qnorm(alpha)        # quantile for normal distribution

# Specify ARIMA-GARCH model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2]), include.mean = TRUE),
  distribution.model = "std"
)

# Initialize storage
n <- nrow(log_returns)
VaR_series <- rep(NA, n - window_size)

# Rolling loop
for (i in 1:(n - window_size)) {
  window_data <- log_returns[i:(i + window_size - 1)]
  
  fit <- tryCatch({
    ugarchfit(spec, data = window_data)
  }, error = function(e) {
    message(sprintf("Iteration %d failed: %s", i, e$message))
    return(NULL)
    })
  
  if (!is.null(fit) && fit@fit$convergence == 0) {
    forecast <- ugarchforecast(fit, n.ahead = 1)
    mu <- fitted(forecast)[1]
    sigma <- sigma(forecast)[1]
    VaR_series[i] <- mu + z * sigma
  } else {
    message(sprintf("Iteration %d: convergence failed", i))
    VaR_series[i] <- NA
  }
}

# Convert to xts for plotting
VaR_xts <- xts(VaR_series, order.by = index(log_returns[(window_size + 1):n]))
actual_returns <- log_returns[(window_size + 1):n]
index(actual_returns) <- index(VaR_xts)

# Plot
plot(actual_returns, main = "Rolling 1-Day VaR (95% Confidence)", col = "black")
lines(VaR_xts, col = "red", lwd = 2)
legend("bottomleft", legend = c("Actual Returns", "VaR 95%"), col = c("black", "red"), lty = 1)

print(paste("No of failures:", sum(is.na(VaR_series))))

valid_idx <- which(!is.na(VaR_series))
VaR_clean <- VaR_series[valid_idx]
returns_clean <- actual_returns[valid_idx]

# Compute violations
violations <- actual_returns < VaR_xts
num_violations <- sum(violations, na.rm = TRUE)
expected_violations <- length(violations) * alpha
violation_rate <- mean(violations, na.rm = TRUE)

cat("Number of Violations:", num_violations, "\n")
cat("Expected Violations (at", 100 * (1 - alpha), "%):", expected_violations, "\n")
cat("Violation Rate:", round(violation_rate * 100, 2), "%\n")

#Plotting violations
plot(index(actual_returns), 
     coredata(actual_returns), type = "n",
     pch = 16, col = "blue", cex = 0.5,
     main = "VaR Backtest: Returns and VaR Line")
lines(actual_returns,  col = "black", lwd = 0.5)
lines(VaR_xts, col = "blue", lwd = 0.5)
points(index(actual_returns[violations]), coredata(actual_returns[violations]), col = "red", pch = 16, cex = 2)
legend("bottomleft", legend = c("Returns", "VaR", "Violations"), col = c("black", "blue", "red"), lty = 1, pch = c(NA, NA, 16))



