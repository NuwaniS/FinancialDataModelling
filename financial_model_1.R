# This is the initial model used to generate the results
if (!require("rugarch")) install.packages("rugarch")

library(quantmod)
library(rugarch)

source('helper_functions.R')

# Download the FTSE 100 Index data
# Uncomment this block only when you need new data
#getSymbols("^FTSE", src = "yahoo", from = "2000-01-01")
#write.zoo(FTSE, file = "FTSE_raw_data.csv", sep = ",")

# Read the CSV file
data <- read.csv('FTSE_raw_data.csv', stringsAsFactors = FALSE)

# Convert the first column to Date
data$Index <- as.Date(data$Index)

# Convert to xts
data_xts <- xts(data[, -1], order.by = data$Index)
symbol_prices <- Ad(data_xts)

#Analyzing the missing values
na_indices <- index(symbol_prices)[which(is.na(symbol_prices))]
# Print missing value indices
print(na_indices)

# Drop missing values
symbol_prices <- na.omit(symbol_prices)

# Initial time series plot
plot(symbol_prices, type="l", main='Adjusted Closing Price of FTSE 100', ylab="Value", xlab="Time", col='darkgreen')

# Plot the raw data
plot_time_series_data(symbol_prices, paste('FTSE 100', '2000-2025'))

# Summary statistics of the raw data
summary(symbol_prices)

# Stationarity tests
stationarity_tests(symbol_prices)

# Calculate log returns
#log_returns <- diff(log(coredata(symbol_prices)))
log_returns <- dailyReturn(symbol_prices, type = "log")

plot_time_series_data(log_returns, paste('FTSE 100 Log Returns', '2000-2025'))

plot_zoomed_hist(log_returns, paste('FTSE 100 Log Returns', '2000-2025'), 4)
# Stationary Test for log returns
stationarity_tests(log_returns)

#### Training and Testing Data Split ####

test_size <- 20

# Split the data into training and testing sets
train_data <- head(log_returns, -test_size)
test_data <- tail(log_returns, test_size)

## Find a way to plot it nicely

#### ARIMA ####

# Fit ARIMA model using automatic selection
arima_model <- auto.arima(train_data, d = 0) # log_returns was confirmed to be stationary
residuals <- residuals(arima_model)

checkresiduals(arima_model)
# Test for zero mean
mean(residuals)
# Autocorrelations in residuals test
independence_test(residuals)
# Normality test using Shapiro-Wilk test
normality_tests(residuals)

calculate_arima_metrics(arima_model)

# Compare the empirical distribution and the simulated distribution
ks_test_arima(train_data, arima_model)

# Forecasting
horizon_window <- test_size
arima_forecast <- forecast(arima_model, h = horizon_window)

# View the forecasted values
print(arima_forecast)

# Plot the forecasts
plot(arima_forecast, main = "ARIMA Forecasts for FTSE 100 Returns")
lines(test_data, col = "blue", lwd = 2)

# Add legend
legend("topleft", legend = c("Forecast", "Actual"), 
       col = c("black", "blue"), lwd = 2)

forecast_evaluation(test_data, arima_forecast$mean)

# Find a way to plot it nicely

#### GARCH ######
## Standard GARCH with student T distribution ##
arch_test <- arch_lm_test(residuals)

# Fit a GARCH(1,1) model
garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                         variance.model = list(garchOrder = c(1, 1)),
                         distribution.model = "std")  # Use Student's t for fat tails

# Fit the model
garch_model <- ugarchfit(spec = garch_spec, data = train_data)
garch_residuals <- as.numeric(residuals(garch_model))
# Print the model values
show(garch_model)
# Plot diagnostic graphs
plot(garch_model, which = 'all')

# Normality test using Shapiro-Wilk test
normality_tests(garch_residuals)

#Forecasting
garch_forecast <- ugarchforecast(garch_model, n.ahead = horizon_window)

# Extract mean forecasts
garch_mean_forecast <- as.numeric(garch_forecast@forecast$seriesFor)
forecast_evaluation(test_data, garch_mean_forecast)

## Standard GARCH with SKEWED T distribution ##

# Fit a GARCH(1,1) model
garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                         variance.model = list(garchOrder = c(1, 1)),
                         distribution.model = "sstd")  

# Fit the model
garch_model <- ugarchfit(spec = garch_spec, data = train_data)
garch_residuals <- as.numeric(residuals(garch_model))
# Print the model values
show(garch_model)
# Plot diagnostic graphs
plot(garch_model, which = 'all')

# Normality test using Shapiro-Wilk test
normality_tests(garch_residuals)

#Forecasting
garch_forecast <- ugarchforecast(garch_model, n.ahead = horizon_window)

# Extract mean forecasts
garch_mean_forecast <- as.numeric(garch_forecast@forecast$seriesFor)
forecast_evaluation(test_data, garch_mean_forecast)
