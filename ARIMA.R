# This script contains an exploration of the simulated ARIMA models

source('helper_functions.R')

###  AR(p) Model  ###
set.seed(123)
model_name <- 'AR(2)'
ar_process <- arima.sim(n = 500, model = list(ar = c(0.5, 0.4)))

plot_time_series_data(ar_process, model_name)
s_test <- stationarity_tests(ar_process)
print(s_test)

# For some parameter values AR processes are not stationary.
# They all resemble the normal distribution

###  MA(q) model ###
set.seed(123)
model_name <- 'MA(2)'
ma_process <- arima.sim(n = 500, model = list(ma = c(0.5, 0.5, 0.8)))

plot_time_series_data(ma_process, model_name)
s_test <- stationarity_tests(ma_process)
print(s_test)

### Special ARIMA Models - White Noise	ARIMA(0,0,0)
set.seed(123)
model_name <- 'White Noise'
arima_process <- arima.sim(n = 500, model = list(order = c(0, 0, 0)))

plot_time_series_data(arima_process, model_name)
s_test <- stationarity_tests(arima_process)
print(s_test)

### Special ARIMA Models - Random Walk	ARIMA(0,1,0)
set.seed(123)
model_name <- 'Random Wal'
arima_process <- arima.sim(n = 500, model = list(order = c(0, 1, 0)))

plot_time_series_data(arima_process, model_name)
s_test <- stationarity_tests(arima_process)
print(s_test)

### ARIMA Models
set.seed(123)
model_name <- 'ARIMA(1,0,1)'
arima_process <- arima.sim(n = 500, model = list(order = c(1, 1, 1), ar = c(0.4), ma = c(0.6)))

plot_time_series_data(arima_process, model_name)
s_test <- stationarity_tests(arima_process)
print(s_test)