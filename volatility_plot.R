library(rugarch)

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

# Plot GARCH volatility
plot(log_returns, main = "FTSE 100 Daily Returns with Volatility", col = "darkgreen")
lines(garch_vol, col = "red")
legend("bottomleft", legend=c("Daily Returns", "GARCH(1,1) Volatility"), col=c("darkgreen", "red"), lty=1, cex = 0.7, xpd = TRUE)
