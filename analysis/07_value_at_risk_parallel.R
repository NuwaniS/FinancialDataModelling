if (!require("doParallel")) install.packages("doParallel")
if (!require("foreach")) install.packages("foreach")

library(rugarch)
library(quantmod)
library(xts)
library(parallel)
library(doParallel)
library(foreach)

# Load data
# log_returns from the financial_model.R will be used

# Rolling window parameters
window_size <- 1000       # length of rolling window
alpha <- 0.05            # for 95% confidence
z <- qnorm(alpha)        # quantile for normal distribution

# Specify ARIMA-GARCH model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2]), include.mean = TRUE),
  distribution.model = "sstd"
)

# Set up parallel backend
n_cores <- detectCores() - 5
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Parallel rolling loop
n <- nrow(log_returns)
VaR_series <- foreach(i = 1:(n - window_size), .combine = 'c', .packages = c("rugarch", "xts")) %dopar% {
  window_data <- log_returns[i:(i + window_size - 1)]
  
  fit <- tryCatch({
    ugarchfit(spec, data = window_data)
  }, error = function(e) {
    message(sprintf("Iteration %d failed: %s", i, e$message))
    return(NA)
  })
  
  if (!is.na(fit) && fit@fit$convergence == 0) {
    forecast <- ugarchforecast(fit, n.ahead = 1)
    mu <- fitted(forecast)[1]
    sigma <- sigma(forecast)[1]
    return(mu + z * sigma)
  } else {
    message(sprintf("Iteration %d: convergence failed", i))
    return(NA)
  }
}

# Stop cluster
stopCluster(cl)
registerDoSEQ()

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

