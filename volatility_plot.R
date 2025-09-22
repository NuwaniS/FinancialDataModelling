library(rugarch)
library(xts)
library(forecast)
library(quantmod)

# Read the CSV file
data <- read.csv('FTSE_raw_data.csv', stringsAsFactors = FALSE)

# Convert the first column to Date
data$Index <- as.Date(data$Index)

# Convert to xts
data_xts <- xts(data[, -1], order.by = data$Index)
symbol_prices <- Ad(data_xts)

# Drop missing values
symbol_prices <- na.omit(symbol_prices)

# Calculate log returns
log_returns <- dailyReturn(symbol_prices, type = "log")

# ARIMA model
arima_model <- auto.arima(log_returns, d = 0)

# Specify GARCH(1,1) with Student-t
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                   distribution.model = "std")

# Fit the model
fit <- ugarchfit(spec = spec, data = log_returns)

# Extract conditional sigma (volatility)
garch_vol <- sigma(fit)

# Plot raw data
par(xaxs = "i") 
plot(zoo(symbol_prices), main = "Adjusted Closing Price of FTSE 100", 
     type = 'l', col = "darkgreen", ylab = "Value", xlab = "Date", lwd = 1)
grid(col = "gray", lty = "dotted")

# Plot only Log Returns
par(xaxs = "i") 
plot(zoo(log_returns), main = "FTSE 100 Log Returns", 
     type = 'l', col = "darkgreen", ylab = "Log Returns", xlab = "Date", lwd = 1)
grid(col = "gray", lty = "dotted")

# Plot GARCH volatility
par(xaxs = "i") 
plot(zoo(log_returns), main = "FTSE 100 Log Returns with Volatility", 
     type = 'l', col = "darkgreen", ylab = "", xlab = "Date", lwd = 1)
grid(col = "gray", lty = "dotted")
lines(zoo(garch_vol), col = "red", lwd = 1.5)
legend("topleft", legend=c("Daily Returns", "GARCH(1,1) Volatility"), col=c("darkgreen", "red"), lty=1,
       cex = 0.6, xpd = TRUE)

