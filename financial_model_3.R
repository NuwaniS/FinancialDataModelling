# This is the model used to generate the results after discussion on 20/06/2025

library(quantmod)
library(forecast)
library(rugarch)
library(xts)

source('helper_functions.R')

# Read the CSV
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

results <- list()
var_results <- list()

# Define look-back window lengths in trading days
window_lengths <- list(
  '3_months' = 63     # 21 trading days/month × 3
  #  '6_months' = 126
  #  '1_year'   = 250
  #  '2_year'   = 500
  #  'full'     = NA      # All available data up to the selected date
)

# GARCH parameters
alpha_95 <- 0.05            # for 95% confidence
alpha_99 <- 0.01            # for 99% confidence

# Set date range for VaR estimation
# Initial Test Dates
#forward_start_date <- as.Date("2025-01-02")
#forward_end_date   <- as.Date("2025-05-19")

# Test Dates considering the whole data set
forward_start_date <- as.Date("2001-01-02")
forward_end_date   <- as.Date("2025-05-19")

# Test Dates for the demo

test_dates <- index(log_returns)[index(log_returns) >= forward_start_date & index(log_returns) <= forward_end_date]
test_data_1d_all <- log_returns[test_dates]

N = length(test_data_1d_all)

# Consider the windows for the first loop since it is more favourable for Var Calculations
for (win_name in names(window_lengths)) {
  window_len <- window_lengths[[win_name]]
  
  test_data_cum_10d_all <- numeric(N)
  test_data_10d_list <- list()
  
  arima_forecast_1d_series <- numeric(N)
  garch_forecast_11_std_1d_series <- numeric(N)
  garch_forecast_12_std_1d_series <- numeric(N)
  garch_forecast_21_std_1d_series <- numeric(N)
  garch_forecast_22_std_1d_series <- numeric(N)
  
  egarch_forecast_11_std_1d_series <- numeric(N)
  egarch_forecast_12_std_1d_series <- numeric(N)
  egarch_forecast_21_std_1d_series <- numeric(N)
  egarch_forecast_22_std_1d_series <- numeric(N)
  
  tgarch_forecast_11_std_1d_series <- numeric(N)
  tgarch_forecast_12_std_1d_series <- numeric(N)
  tgarch_forecast_21_std_1d_series <- numeric(N)
  tgarch_forecast_22_std_1d_series <- numeric(N)
  
  arima_forecast_10d_list <- list()
  garch_forecast_11_std_10d_list <- list()
  garch_forecast_12_std_10d_list <- list()
  garch_forecast_21_std_10d_list <- list()
  garch_forecast_22_std_10d_list <- list()
  
  egarch_forecast_11_std_10d_list <- list()
  egarch_forecast_12_std_10d_list <- list()
  egarch_forecast_21_std_10d_list <- list()
  egarch_forecast_22_std_10d_list <- list()
  
  tgarch_forecast_11_std_10d_list <- list()
  tgarch_forecast_12_std_10d_list <- list()
  tgarch_forecast_21_std_10d_list <- list()
  tgarch_forecast_22_std_10d_list <- list()
  
  var_series_1d_95 <- list(
    his_var_series_95 = numeric(N),
    garch_11std_var_series_95 = numeric(N),
    garch_21std_var_series_95 = numeric(N),
    garch_12std_var_series_95 = numeric(N),
    garch_22std_var_series_95 = numeric(N),
  
    egarch_11std_var_series_95 = numeric(N),
    egarch_21std_var_series_95 = numeric(N),
    egarch_12std_var_series_95 = numeric(N),
    egarch_22std_var_series_95 = numeric(N),
  
    tgarch_11std_var_series_95 = numeric(N),
    tgarch_21std_var_series_95 = numeric(N),
    tgarch_12std_var_series_95 = numeric(N),
    tgarch_22std_var_series_95 = numeric(N)
  )
  
  var_series_10d_95 <- list(
    his_var_series_10d_95 = numeric(N),
    garch_11std_10d_var_series_95 = numeric(N),
    garch_21std_10d_var_series_95 = numeric(N),
    garch_12std_10d_var_series_95 = numeric(N),
    garch_22std_10d_var_series_95 = numeric(N),
  
    egarch_11std_10d_var_series_95 = numeric(N),
    egarch_21std_10d_var_series_95 = numeric(N),
    egarch_12std_10d_var_series_95 = numeric(N),
    egarch_22std_10d_var_series_95 = numeric(N),
  
    tgarch_11std_10d_var_series_95 = numeric(N),
    tgarch_21std_10d_var_series_95 = numeric(N),
    tgarch_12std_10d_var_series_95 = numeric(N),
    tgarch_22std_10d_var_series_95 = numeric(N)
  )
  
  var_series_1d_99 <- list(
    his_var_series_99 = numeric(N),
    garch_11std_var_series_99 = numeric(N),
    garch_21std_var_series_99 = numeric(N),
    garch_12std_var_series_99 = numeric(N),
    garch_22std_var_series_99 = numeric(N),
    
    egarch_11std_var_series_99 = numeric(N),
    egarch_21std_var_series_99 = numeric(N),
    egarch_12std_var_series_99 = numeric(N),
    egarch_22std_var_series_99 = numeric(N),
    
    tgarch_11std_var_series_99 = numeric(N),
    tgarch_21std_var_series_99 = numeric(N),
    tgarch_12std_var_series_99 = numeric(N),
    tgarch_22std_var_series_99 = numeric(N)
  )
  
  var_series_10d_99 <- list(
    his_var_series_10d_99 = numeric(N),
    garch_11std_10d_var_series_99 = numeric(N),
    garch_21std_10d_var_series_99 = numeric(N),
    garch_12std_10d_var_series_99 = numeric(N),
    garch_22std_10d_var_series_99 = numeric(N),
    
    egarch_11std_10d_var_series_99 = numeric(N),
    egarch_21std_10d_var_series_99 = numeric(N),
    egarch_12std_10d_var_series_99 = numeric(N),
    egarch_22std_10d_var_series_99 = numeric(N),
    
    tgarch_11std_10d_var_series_99 = numeric(N),
    tgarch_21std_10d_var_series_99 = numeric(N),
    tgarch_12std_10d_var_series_99 = numeric(N),
    tgarch_22std_10d_var_series_99 = numeric(N)
  )

  
  for (current_date in test_dates) {
    i = 1 # counter for the iterations
    current_date_string <- index(log_returns[index(log_returns) == current_date])
    
    test_data_1d <- log_returns[index(log_returns) == current_date]
    test_data_10d <- log_returns[index(log_returns) >= current_date]
    test_data_10d <- head(test_data_10d, 10)
    test_data_cum_10d_all[i] <- sum(test_data_10d)
    test_data_10d_list[[i]] <- test_data_10d
    
    # Subset the look-back window
    if (is.na(window_len)) {
      training_data <- log_returns[index(log_returns) < current_date]
    } else {
      training_data <- log_returns[index(log_returns) < current_date]
      training_data <- tail(training_data, window_len)
    }
    
    # Stationary Test for log returns
    log_returns_stest <- stationarity_tests(training_data)
    
    # Fit ARIMA model using automatic selection
    arima_model <- auto.arima(training_data, d = 0) # log_returns was confirmed to be stationary
    residuals <- residuals(arima_model)
    
    # Test for zero mean
    mean_residuals = mean(residuals)
    # Autocorrelations in residuals test
    i_test <- independence_test(residuals)
    # Normality test using Shapiro-Wilk test
    n_test <- normality_tests(residuals)
    
    # ARIMA forecast
    arima_forecast_1d <- forecast(arima_model, h = 1)
    arima_1d <- forecast_evaluation(test_data_1d, arima_forecast_1d$mean)
    arima_forecast_1d_series[i] <- arima_forecast_1d$mean
    
    arima_forecast_10d <- forecast(arima_model, h = 10)
    arima_10d <- forecast_evaluation(test_data_10d, arima_forecast_10d$mean)
    arima_forecast_10d_list[[i]] <- arima_forecast_10d$mean
    
    #### GARCH ######
    arch_test <- arch_lm_test(residuals)
    
    ## GARCH with student T distribution ##
    # Fit a GARCH(1,1) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(garchOrder = c(1, 1)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    garch_model_11_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for GARCH(1,1) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    garch_11_std_metrics <- calculate_garch_metrics(garch_model_11_std, 1)
    
    if (!is.null(garch_model_11_std) && garch_model_11_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(garch_model_11_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(garch_model_11_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      garch_11_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      garch_forecast_11_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(garch_model_11_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      garch_11_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      garch_forecast_11_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(garch_model_11_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$garch_11std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$garch_11std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$garch_11std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$garch_11std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for GARCH(1,1) std", win_name, current_date_string))
      garch_11_std_1d <- dummy_forecast_evaluation()
      garch_11_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$garch_11std_var_series_95[i] <- NA
      var_series_1d_99$garch_11std_var_series_99[i] <- NA
      var_series_10d_95$garch_11std_10d_var_series_95[i] <- NA
      var_series_10d_99$garch_11std_10d_var_series_99[i] <- NA
      
      garch_forecast_11_std_1d_series[i] <- NA
      garch_forecast_11_std_10d_list[[i]] <- NA
    }
    
    # Fit a GARCH(2,1) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(garchOrder = c(2, 1)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    garch_model_21_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for GARCH(2,1) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    garch_21_std_metrics <- calculate_garch_metrics(garch_model_21_std, 3)
    
    if (!is.null(garch_model_21_std) && garch_model_21_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(garch_model_21_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(garch_model_21_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      garch_21_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      garch_forecast_21_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(garch_model_21_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      garch_21_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      garch_forecast_21_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(garch_model_21_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$garch_21std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$garch_21std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$garch_21std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$garch_21std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for GARCH(2,1) std", win_name, current_date_string))
      garch_21_std_1d <- dummy_forecast_evaluation()
      garch_21_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$garch_21std_var_series_95[i] <- NA
      var_series_1d_99$garch_21std_var_series_99[i] <- NA
      var_series_10d_95$garch_21std_10d_var_series_95[i] <- NA
      var_series_10d_99$garch_21std_10d_var_series_99[i] <- NA
      
      garch_forecast_21_std_1d_series[i] <- NA
      garch_forecast_21_std_10d_list[[i]] <- NA
      
    }
    
    # Fit a GARCH(1,2) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(garchOrder = c(1, 2)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    garch_model_12_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for GARCH(1,2) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    garch_12_std_metrics <- calculate_garch_metrics(garch_model_12_std, 2)
    
    if (!is.null(garch_model_12_std) && garch_model_12_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(garch_model_12_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(garch_model_12_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      garch_12_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      garch_forecast_12_std_1d_series[i] <-garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(garch_model_12_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      garch_12_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      garch_forecast_12_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(garch_model_12_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$garch_12std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$garch_12std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$garch_12std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$garch_12std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for GARCH(1,2) std", win_name, current_date_string))
      garch_12_std_1d <- dummy_forecast_evaluation()
      garch_12_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$garch_12std_var_series_95[i] <- NA
      var_series_1d_99$garch_12std_var_series_99[i] <- NA
      var_series_10d_95$garch_12std_10d_var_series_95[i] <- NA
      var_series_10d_99$garch_12std_10d_var_series_99[i] <- NA
      
      garch_forecast_12_std_1d_series[i] <- NA
      garch_forecast_12_std_10d_list[[i]] <- NA
      
    }
    
    # Fit a GARCH(2,2) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(garchOrder = c(2, 2)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    garch_model_22_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for GARCH(2,2) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    garch_22_std_metrics <- calculate_garch_metrics(garch_model_22_std, 4)
    
    if (!is.null(garch_model_22_std) && garch_model_22_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(garch_model_22_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(garch_model_22_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      garch_22_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      garch_forecast_22_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(garch_model_22_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      garch_22_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      garch_forecast_22_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(garch_model_22_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$garch_22std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$garch_22std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$garch_22std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$garch_22std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for GARCH(2,2) std", win_name, current_date_string))
      garch_22_std_1d <- dummy_forecast_evaluation()
      garch_22_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$garch_22std_var_series_95[i] <- NA
      var_series_1d_99$garch_22std_var_series_99[i] <- NA
      var_series_10d_95$garch_22std_10d_var_series_95[i] <- NA
      var_series_10d_99$garch_22std_10d_var_series_99[i] <- NA
      
      garch_forecast_22_std_1d_series[i] <- NA
      garch_forecast_22_std_10d_list[[i]] <- NA
    }
    
    ## eGARCH with student T distribution ##
    # Fit a eGARCH(1,1) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    egarch_model_11_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for eGARCH(1,1) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    egarch_11_std_metrics <- calculate_garch_metrics(egarch_model_11_std, 1)
    
    if (!is.null(egarch_model_11_std) && egarch_model_11_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(egarch_model_11_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(egarch_model_11_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      egarch_11_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      egarch_forecast_11_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(egarch_model_11_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      egarch_11_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      egarch_forecast_11_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(egarch_model_11_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$egarch_11std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$egarch_11std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$egarch_11std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$egarch_11std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for eGARCH(1,1) std", win_name, current_date_string))
      egarch_11_std_1d <- dummy_forecast_evaluation()
      egarch_11_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$egarch_11std_var_series_95[i] <- NA
      var_series_1d_99$egarch_11std_var_series_99[i] <- NA
      var_series_10d_95$egarch_11std_10d_var_series_95[i] <- NA
      var_series_10d_99$egarch_11std_10d_var_series_99[i] <- NA
      
      egarch_forecast_11_std_1d_series[i] <- NA
      egarch_forecast_11_std_10d_list[[i]] <- NA
    }
    
    # Fit a eGARCH(2,1) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'eGARCH', garchOrder = c(2, 1)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    egarch_model_21_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for eGARCH(2,1) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    egarch_21_std_metrics <- calculate_garch_metrics(egarch_model_21_std, 3)
    
    if (!is.null(egarch_model_21_std) && egarch_model_21_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(egarch_model_21_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(egarch_model_21_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      egarch_21_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      egarch_forecast_21_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(egarch_model_21_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      egarch_21_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      egarch_forecast_21_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(egarch_model_21_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$egarch_21std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$egarch_21std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$egarch_21std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$egarch_21std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for GARCH(2,1) std", win_name, current_date_string))
      egarch_21_std_1d <- dummy_forecast_evaluation()
      egarch_21_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$egarch_21std_var_series_95[i] <- NA
      var_series_1d_99$egarch_21std_var_series_99[i] <- NA
      var_series_10d_95$egarch_21std_10d_var_series_95[i] <- NA
      var_series_10d_99$egarch_21std_10d_var_series_99[i] <- NA
      
      egarch_forecast_21_std_1d_series[i] <- NA
      egarch_forecast_21_std_10d_list[[i]] <- NA
    }
    
    # Fit a eGARCH(1,2) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'eGARCH', garchOrder = c(1, 2)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    egarch_model_12_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for eGARCH(1,2) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    egarch_12_std_metrics <- calculate_garch_metrics(egarch_model_12_std, 2)
    
    if (!is.null(egarch_model_12_std) && egarch_model_12_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(egarch_model_12_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(egarch_model_12_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      egarch_12_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      egarch_forecast_12_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(egarch_model_12_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      egarch_12_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      egarch_forecast_12_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(egarch_model_12_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$egarch_12std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$egarch_12std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$egarch_12std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$egarch_12std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for eGARCH(1,2) std", win_name, current_date_string))
      egarch_12_std_1d <- dummy_forecast_evaluation()
      egarch_12_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$egarch_12std_var_series_95[i] <- NA
      var_series_1d_99$egarch_12std_var_series_99[i] <- NA
      var_series_10d_95$egarch_12std_10d_var_series_95[i] <- NA
      var_series_10d_99$egarch_12std_10d_var_series_99[i] <- NA
      
      egarch_forecast_12_std_1d_series[i] <- NA
      egarch_forecast_12_std_10d_list[[i]] <- NA
    }
    
    # Fit a eGARCH(2,2) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'eGARCH', garchOrder = c(2, 2)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    egarch_model_22_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for eGARCH(2,2) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    egarch_22_std_metrics <- calculate_garch_metrics(egarch_model_22_std, 4)
    
    if (!is.null(egarch_model_22_std) && egarch_model_22_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(egarch_model_22_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(egarch_model_22_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      egarch_22_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      egarch_forecast_22_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(egarch_model_22_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      egarch_22_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      egarch_forecast_22_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(egarch_model_22_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$egarch_22std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$egarch_22std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$egarch_22std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$egarch_22std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for eGARCH(2,2) std", win_name, current_date_string))
      egarch_22_std_1d <- dummy_forecast_evaluation()
      egarch_22_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$egarch_22std_var_series_95[i] <- NA
      var_series_1d_99$egarch_22std_var_series_99[i] <- NA
      var_series_10d_95$egarch_22std_10d_var_series_95[i] <- NA
      var_series_10d_99$egarch_22std_10d_var_series_99[i] <- NA
      
      egarch_forecast_22_std_1d_series[i] <- NA
      egarch_forecast_22_std_10d_list[[i]] <- NA
    }
    
    # Fit a tGARCH(1,1) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'fGARCH', submodel = 'TGARCH', garchOrder = c(1, 1)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    tgarch_model_11_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for tGARCH(1,1) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    tgarch_11_std_metrics <- calculate_garch_metrics(tgarch_model_11_std, 1)
    
    if (!is.null(tgarch_model_11_std) && tgarch_model_11_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(tgarch_model_11_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(tgarch_model_11_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      tgarch_11_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      tgarch_forecast_11_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(tgarch_model_11_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      tgarch_11_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      tgarch_forecast_11_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(tgarch_model_11_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$tgarch_11std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$tgarch_11std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$tgarch_11std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$tgarch_11std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for tGARCH(1,1) std", win_name, current_date_string))
      tgarch_11_std_1d <- dummy_forecast_evaluation()
      tgarch_11_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$tgarch_11std_var_series_95[i] <- NA
      var_series_1d_99$tgarch_11std_var_series_99[i] <- NA
      var_series_10d_95$tgarch_11std_10d_var_series_95[i] <- NA
      var_series_10d_99$tgarch_11std_10d_var_series_99[i] <- NA
      
      tgarch_forecast_11_std_1d_series[i] <- NA
      tgarch_forecast_11_std_10d_list[[i]] <- NA
    }
    
    # Fit a tGARCH(2,1) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'fGARCH', submodel = 'TGARCH', garchOrder = c(2, 1)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    tgarch_model_21_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for tGARCH(2,1) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    tgarch_21_std_metrics <- calculate_garch_metrics(tgarch_model_21_std, 3)
    
    if (!is.null(tgarch_model_21_std) && tgarch_model_21_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(tgarch_model_21_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(tgarch_model_21_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      tgarch_21_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      tgarch_forecast_21_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(tgarch_model_21_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      tgarch_21_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      tgarch_forecast_21_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(tgarch_model_21_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$tgarch_21std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$tgarch_21std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$tgarch_21std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$tgarch_21std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for tGARCH(2,1) std", win_name, current_date_string))
      tgarch_21_std_1d <- dummy_forecast_evaluation()
      tgarch_21_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$tgarch_21std_var_series_95[i] <- NA
      var_series_1d_99$tgarch_21std_var_series_99[i] <- NA
      var_series_10d_95$tgarch_21std_10d_var_series_95[i] <- NA
      var_series_10d_99$tgarch_21std_10d_var_series_99[i] <- NA
      
      tgarch_forecast_21_std_1d_series[i] <- NA
      tgarch_forecast_21_std_10d_list[[i]] <- NA
    }
    
    # Fit a tGARCH(1,2) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'fGARCH', submodel = 'TGARCH', garchOrder = c(1, 2)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    tgarch_model_12_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for tGARCH(1,2) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    tgarch_12_std_metrics <- calculate_garch_metrics(tgarch_model_12_std, 2)
    
    if (!is.null(tgarch_model_12_std) && tgarch_model_12_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(tgarch_model_12_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(tgarch_model_12_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      tgarch_12_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      tgarch_forecast_12_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(tgarch_model_12_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      tgarch_12_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      tgarch_forecast_12_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(tgarch_model_12_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$tgarch_12std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$tgarch_12std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$tgarch_12std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$tgarch_12std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for tGARCH(1,2) std", win_name, current_date_string))
      tgarch_12_std_1d <- dummy_forecast_evaluation()
      tgarch_12_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$tgarch_12std_var_series_95[i] <- NA
      var_series_1d_99$tgarch_12std_var_series_99[i] <- NA
      var_series_10d_95$tgarch_12std_10d_var_series_95[i] <- NA
      var_series_10d_99$tgarch_12std_10d_var_series_99[i] <- NA
      
      tgarch_forecast_12_std_1d_series[i] <- NA
      tgarch_forecast_12_std_10d_list[[i]] <- NA
    }
    
    # Fit a tGARCH(2,2) model
    garch_spec <- ugarchspec(mean.model = list(armaOrder = c(arima_model$arma[1], arima_model$arma[2])),
                             variance.model = list(model = 'fGARCH', submodel = 'TGARCH', garchOrder = c(2, 2)),
                             distribution.model = "std")  # Use Student's t for fat tails
    
    # Fit the model
    tgarch_model_22_std <- tryCatch({
      ugarchfit(spec = garch_spec, data = training_data)
    }, error = function(e) {
      message(sprintf("Iteration %s %s failed for tGARCH(2,2) std: %s", win_name, current_date_string, e$message))
      return(NULL)
    })
    
    tgarch_22_std_metrics <- calculate_garch_metrics(tgarch_model_22_std, 4)
    
    if (!is.null(tgarch_model_22_std) && tgarch_model_22_std@fit$convergence == 0) {
      
      # Normality test using Shapiro-Wilk test
      garch_residuals <- as.numeric(residuals(tgarch_model_22_std))
      garch_n_test <- normality_tests(garch_residuals)
      
      garch_forecast_1d <- ugarchforecast(tgarch_model_22_std, n.ahead = 1)
      garch_mean_forecast_1d <- as.numeric(garch_forecast_1d@forecast$seriesFor)
      tgarch_22_std_1d <- forecast_evaluation(test_data_1d, garch_mean_forecast_1d)
      tgarch_forecast_22_std_1d_series[i] <- garch_mean_forecast_1d
      
      garch_forecast_10d <- ugarchforecast(tgarch_model_22_std, n.ahead = 10)
      garch_mean_forecast_10d <- as.numeric(garch_forecast_10d@forecast$seriesFor)
      tgarch_22_std_10d <- forecast_evaluation(test_data_10d, garch_mean_forecast_10d)
      tgarch_forecast_22_std_10d_list[[i]] <- garch_mean_forecast_10d
      
      garch_var <- garch_1d_10d_var(tgarch_model_22_std, garch_forecast_1d, garch_forecast_10d)
      var_series_1d_95$tgarch_22std_var_series_95[i] <- garch_var$garch_var_95
      var_series_1d_99$tgarch_22std_var_series_99[i] <- garch_var$garch_var_99
      
      var_series_10d_95$tgarch_22std_10d_var_series_95[i] <- garch_var$garch_var_10d_95
      var_series_10d_99$tgarch_22std_10d_var_series_99[i] <- garch_var$garch_var_10d_99
    } else {
      message(sprintf("Iteration %s %s: convergence failed for tGARCH(2,2) std", win_name, current_date_string))
      tgarch_22_std_1d <- dummy_forecast_evaluation()
      tgarch_22_std_10d <- dummy_forecast_evaluation()
      var_series_1d_95$tgarch_22std_var_series_95[i] <- NA
      var_series_1d_99$tgarch_22std_var_series_99[i] <- NA
      var_series_10d_95$tgarch_22std_10d_var_series_95[i] <- NA
      var_series_10d_99$tgarch_22std_10d_var_series_99[i] <- NA
      
      tgarch_forecast_22_std_1d_series[i] <- NA
      tgarch_forecast_22_std_10d_list[[i]] <- NA
    }
    
    i <- i + 1 # Increment the counter
    
    #### Historical VaR ######
    historical_var = quantile(training_data, probs = 0.05, na.rm = TRUE)  #95% confidence
    var_series_1d_95$his_var_series_95 <- c(var_series_1d_95$his_var_series_95, historical_var)
    
    historical_var = quantile(training_data, probs = 0.01, na.rm = TRUE)  #99% confidence
    var_series_1d_99$his_var_series_99 <- c(var_series_1d_99$his_var_series_99, historical_var)
    
    #### Historical 10d VaR ####
    
    historical_var_10d <- quantile(rollapply(training_data, 10, sum), probs = 0.05, na.rm = TRUE)  #95% confidence
    var_series_10d_95$his_var_series_10d_95 <- c(var_series_10d_95$his_var_series_10d_95, historical_var_10d)
    
    historical_var_10d <- quantile(rollapply(training_data, 10, sum), probs = 0.01, na.rm = TRUE)  #99% confidence
    var_series_10d_99$his_var_series_10d_99 <- c(var_series_10d_99$his_var_series_10d_99, historical_var_10d)
    
    # Store results
    results[[length(results) + 1]] <- data.frame(
      Date = current_date_string,
      LookBackWin = win_name,
      LR_ADF_PValue = log_returns_stest$adf_pvalue,
      LR_ADF = log_returns_stest$adf_decision,
      LR_KPSS_Trend_PValue = log_returns_stest$kpss_trend_pvalue,
      LR_KPSS_Trend = log_returns_stest$kpss_trend_decision,
      LR_KPSS_Level_PValue = log_returns_stest$kpss_level_pvalue,
      LR_KPSS_Level = log_returns_stest$kpss_level_decision,
      LR_PP_PValue = log_returns_stest$pp_pvalue,
      LR_PP = log_returns_stest$pp_decision,
      LR_PPT_PValue = log_returns_stest$ppt_pvalue,
      LR_PPT = log_returns_stest$ppt_decision,
      Residual_mean = mean_residuals,
      Ljung_PValue = i_test$p_value,
      Ljung = i_test$decision,
      Shapiro_PValue = n_test$shapiro_p_value,
      Shapiro = n_test$shapiro_decision,
      Jarque_PValue = n_test$jarque_p_value,
      Jarque = n_test$jarque_decision,
      AIC = arima_model$aic,
      BIC = arima_model$bic,
      ARIMA_1d_MAE = arima_1d$mae,
      ARIMA_1d_RMSE = arima_1d$rmse,
      ARIMA_1d_MAPE = arima_1d$mape,
      ARIMA_10d_MAE = arima_10d$mae,
      ARIMA_10d_RMSE = arima_10d$rmse,
      ARIMA_10d_MAPE = arima_10d$mape,
      ARCH_1_p = arch_test$arch_1_p,
      ARCH_1_decision = arch_test$arch_1_decision,
      ARCH_5_p = arch_test$arch_5_p,
      ARCH_5_decision = arch_test$arch_5_decision,
      ARCH_10_p = arch_test$arch_10_p,
      ARCH_10_decision = arch_test$arch_10_decision,
      
      GARCH_11_STD_AIC = garch_11_std_metrics$aic,
      GARCH_11_STD_BIC = garch_11_std_metrics$bic,
      GARCH_11_STD_LIKE = garch_11_std_metrics$like,
      GARCH_11_STD_PARA = garch_11_std_metrics$a_b,
      GARCH_11_STD_1d_MAE = garch_11_std_1d$mae,
      GARCH_11_STD_1d_RMSE = garch_11_std_1d$rmse,
      GARCH_11_STD_1d_MAPE = garch_11_std_1d$mape,
      GARCH_11_STD_10d_MAE = garch_11_std_10d$mae,
      GARCH_11_STD_10d_RMSE = garch_11_std_10d$rmse,
      GARCH_11_STD_10d_MAPE = garch_11_std_10d$mape,
      
      GARCH_21_STD_AIC = garch_21_std_metrics$aic,
      GARCH_21_STD_BIC = garch_21_std_metrics$bic,
      GARCH_21_STD_LIKE = garch_21_std_metrics$like,
      GARCH_21_STD_PARA = garch_21_std_metrics$a_b,
      GARCH_21_STD_1d_MAE = garch_21_std_1d$mae,
      GARCH_21_STD_1d_RMSE = garch_21_std_1d$rmse,
      GARCH_21_STD_1d_MAPE = garch_21_std_1d$mape,
      GARCH_21_STD_10d_MAE = garch_21_std_10d$mae,
      GARCH_21_STD_10d_RMSE = garch_21_std_10d$rmse,
      GARCH_21_STD_10d_MAPE = garch_21_std_10d$mape,
      
      GARCH_12_STD_AIC = garch_12_std_metrics$aic,
      GARCH_12_STD_BIC = garch_12_std_metrics$bic,
      GARCH_12_STD_LIKE = garch_12_std_metrics$like,
      GARCH_12_STD_PARA = garch_12_std_metrics$a_b,
      GARCH_12_STD_1d_MAE = garch_12_std_1d$mae,
      GARCH_12_STD_1d_RMSE = garch_12_std_1d$rmse,
      GARCH_12_STD_1d_MAPE = garch_12_std_1d$mape,
      GARCH_12_STD_10d_MAE = garch_12_std_10d$mae,
      GARCH_12_STD_10d_RMSE = garch_12_std_10d$rmse,
      GARCH_12_STD_10d_MAPE = garch_12_std_10d$mape,
      
      GARCH_22_STD_AIC = garch_22_std_metrics$aic,
      GARCH_22_STD_BIC = garch_22_std_metrics$bic,
      GARCH_22_STD_LIKE = garch_22_std_metrics$like,
      GARCH_2_STD_PARA = garch_22_std_metrics$a_b,
      GARCH_22_STD_1d_MAE = garch_22_std_1d$mae,
      GARCH_22_STD_1d_RMSE = garch_22_std_1d$rmse,
      GARCH_22_STD_1d_MAPE = garch_22_std_1d$mape,
      GARCH_22_STD_10d_MAE = garch_22_std_10d$mae,
      GARCH_22_STD_10d_RMSE = garch_22_std_10d$rmse,
      GARCH_22_STD_10d_MAPE = garch_22_std_10d$mape,
      
      eGARCH_11_STD_AIC = egarch_11_std_metrics$aic,
      eGARCH_11_STD_BIC = egarch_11_std_metrics$bic,
      eGARCH_11_STD_LIKE = egarch_11_std_metrics$like,
      eGARCH_11_STD_PARA = egarch_11_std_metrics$a_b,
      eGARCH_11_STD_1d_MAE = egarch_11_std_1d$mae,
      eGARCH_11_STD_1d_RMSE = egarch_11_std_1d$rmse,
      eGARCH_11_STD_1d_MAPE = egarch_11_std_1d$mape,
      eGARCH_11_STD_10d_MAE = egarch_11_std_10d$mae,
      eGARCH_11_STD_10d_RMSE = egarch_11_std_10d$rmse,
      eGARCH_11_STD_10d_MAPE = egarch_11_std_10d$mape,
      
      eGARCH_21_STD_AIC = egarch_21_std_metrics$aic,
      eGARCH_21_STD_BIC = egarch_21_std_metrics$bic,
      eGARCH_21_STD_LIKE = egarch_21_std_metrics$like,
      eGARCH_21_STD_PARA = egarch_21_std_metrics$a_b,
      eGARCH_21_STD_1d_MAE = egarch_21_std_1d$mae,
      eGARCH_21_STD_1d_RMSE = egarch_21_std_1d$rmse,
      eGARCH_21_STD_1d_MAPE = egarch_21_std_1d$mape,
      eGARCH_21_STD_10d_MAE = egarch_21_std_10d$mae,
      eGARCH_21_STD_10d_RMSE = egarch_21_std_10d$rmse,
      eGARCH_21_STD_10d_MAPE = egarch_21_std_10d$mape,
      
      eGARCH_12_STD_AIC = egarch_12_std_metrics$aic,
      eGARCH_12_STD_BIC = egarch_12_std_metrics$bic,
      eGARCH_12_STD_LIKE = egarch_12_std_metrics$like,
      eGARCH_12_STD_PARA = egarch_12_std_metrics$a_b,
      eGARCH_12_STD_1d_MAE = egarch_12_std_1d$mae,
      eGARCH_12_STD_1d_RMSE = egarch_12_std_1d$rmse,
      eGARCH_12_STD_1d_MAPE = egarch_12_std_1d$mape,
      eGARCH_12_STD_10d_MAE = egarch_12_std_10d$mae,
      eGARCH_12_STD_10d_RMSE = egarch_12_std_10d$rmse,
      eGARCH_12_STD_10d_MAPE = egarch_12_std_10d$mape,
      
      eGARCH_22_STD_AIC = egarch_22_std_metrics$aic,
      eGARCH_22_STD_BIC = egarch_22_std_metrics$bic,
      eGARCH_22_STD_LIKE = egarch_22_std_metrics$like,
      eGARCH_22_STD_PARA = egarch_22_std_metrics$a_b,
      eGARCH_22_STD_1d_MAE = egarch_22_std_1d$mae,
      eGARCH_22_STD_1d_RMSE = egarch_22_std_1d$rmse,
      eGARCH_22_STD_1d_MAPE = egarch_22_std_1d$mape,
      eGARCH_22_STD_10d_MAE = egarch_22_std_10d$mae,
      eGARCH_22_STD_10d_RMSE = egarch_22_std_10d$rmse,
      eGARCH_22_STD_10d_MAPE = egarch_22_std_10d$mape,
      
      tGARCH_11_STD_AIC = tgarch_11_std_metrics$aic,
      tGARCH_11_STD_BIC = tgarch_11_std_metrics$bic,
      tGARCH_11_STD_LIKE = tgarch_11_std_metrics$like,
      tGARCH_11_STD_PARA = tgarch_11_std_metrics$a_b,
      tGARCH_11_STD_1d_MAE = tgarch_11_std_1d$mae,
      tGARCH_11_STD_1d_RMSE = tgarch_11_std_1d$rmse,
      tGARCH_11_STD_1d_MAPE = tgarch_11_std_1d$mape,
      tGARCH_11_STD_10d_MAE = tgarch_11_std_10d$mae,
      tGARCH_11_STD_10d_RMSE = tgarch_11_std_10d$rmse,
      tGARCH_11_STD_10d_MAPE = tgarch_11_std_10d$mape,
      
      tGARCH_21_STD_AIC = tgarch_21_std_metrics$aic,
      tGARCH_21_STD_BIC = tgarch_21_std_metrics$bic,
      tGARCH_21_STD_LIKE = tgarch_21_std_metrics$like,
      tGARCH_21_STD_PARA = tgarch_21_std_metrics$a_b,
      tGARCH_21_STD_1d_MAE = tgarch_21_std_1d$mae,
      tGARCH_21_STD_1d_RMSE = tgarch_21_std_1d$rmse,
      tGARCH_21_STD_1d_MAPE = tgarch_21_std_1d$mape,
      tGARCH_21_STD_10d_MAE = tgarch_21_std_10d$mae,
      tGARCH_21_STD_10d_RMSE = tgarch_21_std_10d$rmse,
      tGARCH_21_STD_10d_MAPE = tgarch_21_std_10d$mape,
      
      tGARCH_12_STD_AIC = tgarch_12_std_metrics$aic,
      tGARCH_12_STD_BIC = tgarch_12_std_metrics$bic,
      tGARCH_12_STD_LIKE = tgarch_12_std_metrics$like,
      tGARCH_12_STD_PARA = tgarch_12_std_metrics$a_b,
      tGARCH_12_STD_1d_MAE = tgarch_12_std_1d$mae,
      tGARCH_12_STD_1d_RMSE = tgarch_12_std_1d$rmse,
      tGARCH_12_STD_1d_MAPE = tgarch_12_std_1d$mape,
      tGARCH_12_STD_10d_MAE = tgarch_12_std_10d$mae,
      tGARCH_12_STD_10d_RMSE = tgarch_12_std_10d$rmse,
      tGARCH_12_STD_10d_MAPE = tgarch_12_std_10d$mape,
      
      tGARCH_22_STD_AIC = tgarch_22_std_metrics$aic,
      tGARCH_22_STD_BIC = tgarch_22_std_metrics$bic,
      tGARCH_22_STD_LIKE = tgarch_22_std_metrics$like,
      tGARCH_22_STD_PARA = tgarch_22_std_metrics$a_b,
      tGARCH_22_STD_1d_MAE = tgarch_22_std_1d$mae,
      tGARCH_22_STD_1d_RMSE = tgarch_22_std_1d$rmse,
      tGARCH_22_STD_1d_MAPE = tgarch_22_std_1d$mape,
      tGARCH_22_STD_10d_MAE = tgarch_22_std_10d$mae,
      tGARCH_22_STD_10d_RMSE = tgarch_22_std_10d$rmse,
      tGARCH_22_STD_10d_MAPE = tgarch_22_std_10d$mape
    )
  }
  # Print overall forecast evaluation for each window
  print(paste(win_name, 'arima_1d ', forecast_evaluation(test_data_1d_all, arima_forecast_1d_series)))
  print(paste(win_name, 'garch_11_std_1d ', forecast_evaluation(test_data_1d_all, garch_forecast_11_std_1d_series)))
  print(paste(win_name, 'garch_12_std_1d ', forecast_evaluation(test_data_1d_all, garch_forecast_12_std_1d_series)))
  print(paste(win_name, 'garch_21_std_1d ', forecast_evaluation(test_data_1d_all, garch_forecast_12_std_1d_series)))
  print(paste(win_name, 'garch_22_std_1d ', forecast_evaluation(test_data_1d_all, garch_forecast_22_std_1d_series)))
  
  print(paste(win_name, 'egarch_11_std_1d ', forecast_evaluation(test_data_1d_all, egarch_forecast_11_std_1d_series)))
  print(paste(win_name, 'egarch_12_std_1d ', forecast_evaluation(test_data_1d_all, egarch_forecast_12_std_1d_series)))
  print(paste(win_name, 'egarch_21_std_1d ', forecast_evaluation(test_data_1d_all, egarch_forecast_21_std_1d_series)))
  print(paste(win_name, 'egarch_22_std_1d ', forecast_evaluation(test_data_1d_all, egarch_forecast_22_std_1d_series)))
  
  print(paste(win_name, 'tgarch_11_std_1d ', forecast_evaluation(test_data_1d_all, tgarch_forecast_11_std_1d_series)))
  print(paste(win_name, 'tgarch_12_std_1d ', forecast_evaluation(test_data_1d_all, tgarch_forecast_12_std_1d_series)))
  print(paste(win_name, 'tgarch_21_std_1d ', forecast_evaluation(test_data_1d_all, tgarch_forecast_21_std_1d_series)))
  print(paste(win_name, 'tgarch_22_std_1d ', forecast_evaluation(test_data_1d_all, tgarch_forecast_22_std_1d_series)))
  
  print(paste(win_name, 'arima_10d', forecast_evaluation_10d(test_data_10d_list, arima_forecast_10d_list)))
  print(paste(win_name, 'garch_11_std_10d', forecast_evaluation_10d(test_data_10d_list, garch_forecast_11_std_10d_list)))
  print(paste(win_name, 'garch_12_std_10d ', forecast_evaluation_10d(test_data_10d_list, garch_forecast_12_std_10d_list)))
  print(paste(win_name, 'garch_21_std_10d ', forecast_evaluation_10d(test_data_10d_list, garch_forecast_12_std_10d_list)))
  print(paste(win_name, 'garch_22_std_10d ', forecast_evaluation_10d(test_data_10d_list, garch_forecast_22_std_10d_list)))
  
  print(paste(win_name, 'egarch_11_std_10d ', forecast_evaluation_10d(test_data_10d_list, egarch_forecast_11_std_10d_list)))
  print(paste(win_name, 'egarch_12_std_10d ', forecast_evaluation_10d(test_data_10d_list, egarch_forecast_12_std_10d_list)))
  print(paste(win_name, 'egarch_21_std_10d ', forecast_evaluation_10d(test_data_10d_list, egarch_forecast_21_std_10d_list)))
  print(paste(win_name, 'egarch_22_std_10d ', forecast_evaluation_10d(test_data_10d_list, egarch_forecast_22_std_10d_list)))
  
  print(paste(win_name, 'tgarch_11_std_10d ', forecast_evaluation_10d(test_data_10d_list, tgarch_forecast_11_std_10d_list)))
  print(paste(win_name, 'tgarch_12_std_10d ', forecast_evaluation_10d(test_data_10d_list, tgarch_forecast_12_std_10d_list)))
  print(paste(win_name, 'tgarch_21_std_10d ', forecast_evaluation_10d(test_data_10d_list, tgarch_forecast_21_std_10d_list)))
  print(paste(win_name, 'tgarch_22_std_10d ', forecast_evaluation_10d(test_data_10d_list, tgarch_forecast_22_std_10d_list)))
  
  test_data_cum_10d_xts <- xts(test_data_cum_10d_all, order.by = index(test_data_1d_all))
  
  # VaR Evaluation
  var_1d_95_metrics <- garch_var_test_all(var_series_1d_95, test_data_1d_all, 0.95)
  var_1d_99_metrics <- garch_var_test_all(var_series_1d_99, test_data_1d_all, 0.99)
  
  var_10d_95_metrics <- garch_var_test_all(var_series_10d_95, test_data_cum_10d_xts, 0.95)
  var_10d_99_metrics <- garch_var_test_all(var_series_10d_99, test_data_cum_10d_xts, 0.99)
  
  # Comparative Backtesting
  qs_1d_95 <- mean_quantile_loss_all(var_series_1d_95, test_data_1d_all, alpha_95)
  qs_1d_99 <- mean_quantile_loss_all(var_series_1d_99, test_data_1d_all, alpha_99)
  
  qs_10d_95 <- mean_quantile_loss_all(var_series_10d_95, test_data_cum_10d_all, alpha_95)
  qs_10d_99 <- mean_quantile_loss_all(var_series_10d_99, test_data_cum_10d_all, alpha_99)
  
  # Store Var Results
  var_results[[length(var_results) + 1]] <- data.frame(
    LookBackWin = win_name,
    var_95_exp_violations = var_1d_95_metrics$his_var_series_95$expected.exceed,
    var_95_violations = var_1d_95_metrics$his_var_series_95$actual.exceed,
    var_95_kupic_p = var_1d_95_metrics$his_var_series_95$uc.LRp,
    var_95_kupic_decision = var_1d_95_metrics$his_var_series_95$uc.Decision,
    var_95_chris_p = var_1d_95_metrics$his_var_series_95$cc.LRp,
    var_95_chris_decision = var_1d_95_metrics$his_var_series_95$cc.Decision,
    var_99_exp_violations = var_1d_99_metrics$his_var_series_99$expected.exceed,
    var_99_violations = var_1d_99_metrics$his_var_series_99$actual.exceed,
    var_99_kupic_p = var_1d_99_metrics$his_var_series_99$uc.LRp,
    var_99_kupic_decision = var_1d_99_metrics$his_var_series_99$uc.Decision,
    var_99_chris_p = var_1d_99_metrics$his_var_series_99$cc.LRp,
    var_99_chris_decision = var_1d_99_metrics$his_var_series_99$cc.Decision,
    
    garch_11std_95_failures = sum(is.na(var_series_1d_95$garch_11std_var_series_95)),
    garch_11std_99_failures = sum(is.na(var_series_1d_99$garch_11std_var_series_99)),
    
    garch_11std_95_violations = var_1d_95_metrics$garch_11std_var_series_95$actual.exceed,
    garch_11std_95_kupic_p = var_1d_95_metrics$garch_11std_var_series_95$uc.LRp,
    garch_11std_95_kupic_decision = var_1d_95_metrics$garch_11std_var_series_95$uc.Decision,
    garch_11std_95_chris_p = var_1d_95_metrics$garch_11std_var_series_95$cc.LRp,
    garch_11std_95_chris_decision = var_1d_95_metrics$garch_11std_var_series_95$cc.Decision,
    garch_11std_99_violations = var_1d_99_metrics$garch_11std_var_series_99$actual.exceed,
    garch_11std_99_kupic_p = var_1d_99_metrics$garch_11std_var_series_99$uc.LRp,
    garch_11std_99_kupic_decision = var_1d_99_metrics$garch_11std_var_series_99$uc.Decision,
    garch_11std_99_chris_p = var_1d_99_metrics$garch_11std_var_series_99$cc.LRp,
    garch_11std_99_chris_decision = var_1d_99_metrics$garch_11std_var_series_99$cc.Decision,
    
    garch_21std_95_failures = sum(is.na(var_series_1d_95$garch_21std_var_series_95)),
    garch_21std_99_failures = sum(is.na(var_series_1d_99$garch_21std_var_series_99)),
    
    garch_21std_95_violations = var_1d_95_metrics$garch_21std_var_series_95$actual.exceed,
    garch_21std_95_kupic_p = var_1d_95_metrics$garch_21std_var_series_95$uc.LRp,
    garch_21std_95_kupic_decision = var_1d_95_metrics$garch_21std_var_series_95$uc.Decision,
    garch_21std_95_chris_p = var_1d_95_metrics$garch_21std_var_series_95$cc.LRp,
    garch_21std_95_chris_decision = var_1d_95_metrics$garch_21std_var_series_95$cc.Decision,
    garch_21std_99_violations = var_1d_99_metrics$garch_21std_var_series_99$actual.exceed,
    garch_21std_99_kupic_p = var_1d_99_metrics$garch_21std_var_series_99$uc.LRp,
    garch_21std_99_kupic_decision = var_1d_99_metrics$garch_21std_var_series_99$uc.Decision,
    garch_21std_99_chris_p = var_1d_99_metrics$garch_21std_var_series_99$cc.LRp,
    garch_21std_99_chris_decision = var_1d_99_metrics$garch_21std_var_series_99$cc.Decision,
    
    garch_12std_95_failures = sum(is.na(var_series_1d_95$garch_12std_var_series_95)),
    garch_12std_99_failures = sum(is.na(var_series_1d_99$garch_12std_var_series_99)),
    
    garch_12std_95_violations = var_1d_95_metrics$garch_12std_var_series_95$actual.exceed,
    garch_12std_95_kupic_p = var_1d_95_metrics$garch_12std_var_series_95$uc.LRp,
    garch_12std_95_kupic_decision = var_1d_95_metrics$garch_12std_var_series_95$uc.Decision,
    garch_12std_95_chris_p = var_1d_95_metrics$garch_12std_var_series_95$cc.LRp,
    garch_12std_95_chris_decision = var_1d_95_metrics$garch_12std_var_series_95$cc.Decision,
    garch_12std_99_violations = var_1d_99_metrics$garch_12std_var_series_99$actual.exceed,
    garch_12std_99_kupic_p = var_1d_99_metrics$garch_12std_var_series_99$uc.LRp,
    garch_12std_99_kupic_decision = var_1d_99_metrics$garch_12std_var_series_99$uc.Decision,
    garch_12std_99_chris_p = var_1d_99_metrics$garch_12std_var_series_99$cc.LRp,
    garch_12std_99_chris_decision = var_1d_99_metrics$garch_12std_var_series_99$cc.Decision,
    
    garch_22std_95_failures = sum(is.na(var_series_1d_95$garch_22std_var_series_95)),
    garch_22std_99_failures = sum(is.na(var_series_1d_99$garch_22std_var_series_99)),
    
    garch_22std_95_violations = var_1d_95_metrics$garch_22std_var_series_95$actual.exceed,
    garch_22std_95_kupic_p = var_1d_95_metrics$garch_22std_var_series_95$uc.LRp,
    garch_22std_95_kupic_decision = var_1d_95_metrics$garch_22std_var_series_95$uc.Decision,
    garch_22std_95_chris_p = var_1d_95_metrics$garch_22std_var_series_95$cc.LRp,
    garch_22std_95_chris_decision = var_1d_95_metrics$garch_22std_var_series_95$cc.Decision,
    garch_22std_99_violations = var_1d_99_metrics$garch_22std_var_series_99$actual.exceed,
    garch_22std_99_kupic_p = var_1d_99_metrics$garch_22std_var_series_99$uc.LRp,
    garch_22std_99_kupic_decision = var_1d_99_metrics$garch_22std_var_series_99$uc.Decision,
    garch_22std_99_chris_p = var_1d_99_metrics$garch_22std_var_series_99$cc.LRp,
    garch_22std_99_chris_decision = var_1d_99_metrics$garch_22std_var_series_99$cc.Decision,
    
    egarch_11std_95_failures = sum(is.na(var_series_1d_95$egarch_11std_var_series_95)),
    egarch_11std_99_failures = sum(is.na(var_series_1d_99$egarch_11std_var_series_99)),
    
    egarch_11std_95_violations = var_1d_95_metrics$egarch_11std_var_series_95$actual.exceed,
    egarch_11std_95_kupic_p = var_1d_95_metrics$egarch_11std_var_series_95$uc.LRp,
    egarch_11std_95_kupic_decision = var_1d_95_metrics$egarch_11std_var_series_95$uc.Decision,
    egarch_11std_95_chris_p = var_1d_95_metrics$egarch_11std_var_series_95$cc.LRp,
    egarch_11std_95_chris_decision = var_1d_95_metrics$egarch_11std_var_series_95$cc.Decision,
    egarch_11std_99_violations = var_1d_99_metrics$egarch_11std_var_series_99$actual.exceed,
    egarch_11std_99_kupic_p = var_1d_99_metrics$egarch_11std_var_series_99$uc.LRp,
    egarch_11std_99_kupic_decision = var_1d_99_metrics$egarch_11std_var_series_99$uc.Decision,
    egarch_11std_99_chris_p = var_1d_99_metrics$egarch_11std_var_series_99$cc.LRp,
    egarch_11std_99_chris_decision = var_1d_99_metrics$egarch_11std_var_series_99$cc.Decision,
    
    egarch_21std_95_failures = sum(is.na(var_series_1d_95$egarch_21std_var_series_95)),
    egarch_21std_99_failures = sum(is.na(var_series_1d_99$egarch_21std_var_series_99)),
    
    egarch_21std_95_violations = var_1d_95_metrics$egarch_21std_var_series_95$actual.exceed,
    egarch_21std_95_kupic_p = var_1d_95_metrics$egarch_21std_var_series_95$uc.LRp,
    egarch_21std_95_kupic_decision = var_1d_95_metrics$egarch_21std_var_series_95$uc.Decision,
    egarch_21std_95_chris_p = var_1d_95_metrics$egarch_21std_var_series_95$cc.LRp,
    egarch_21std_95_chris_decision = var_1d_95_metrics$egarch_21std_var_series_95$cc.Decision,
    egarch_21std_99_violations = var_1d_99_metrics$egarch_21std_var_series_99$actual.exceed,
    egarch_21std_99_kupic_p = var_1d_99_metrics$egarch_21std_var_series_99$uc.LRp,
    egarch_21std_99_kupic_decision = var_1d_99_metrics$egarch_21std_var_series_99$uc.Decision,
    egarch_21std_99_chris_p = var_1d_99_metrics$egarch_21std_var_series_99$cc.LRp,
    egarch_21std_99_chris_decision = var_1d_99_metrics$egarch_21std_var_series_99$cc.Decision,
    
    egarch_12std_95_failures = sum(is.na(var_series_1d_95$egarch_12std_var_series_95)),
    egarch_12std_99_failures = sum(is.na(var_series_1d_99$egarch_12std_var_series_99)),
    
    egarch_12std_95_violations = var_1d_95_metrics$egarch_12std_var_series_95$actual.exceed,
    egarch_12std_95_kupic_p = var_1d_95_metrics$egarch_12std_var_series_95$uc.LRp,
    egarch_12std_95_kupic_decision = var_1d_95_metrics$egarch_12std_var_series_95$uc.Decision,
    egarch_12std_95_chris_p = var_1d_95_metrics$egarch_12std_var_series_95$cc.LRp,
    egarch_12std_95_chris_decision = var_1d_95_metrics$egarch_12std_var_series_95$cc.Decision,
    egarch_12std_99_violations = var_1d_99_metrics$egarch_12std_var_series_99$actual.exceed,
    egarch_12std_99_kupic_p = var_1d_99_metrics$egarch_12std_var_series_99$uc.LRp,
    egarch_12std_99_kupic_decision = var_1d_99_metrics$egarch_12std_var_series_99$uc.Decision,
    egarch_12std_99_chris_p = var_1d_99_metrics$egarch_12std_var_series_99$cc.LRp,
    egarch_12std_99_chris_decision = var_1d_99_metrics$egarch_12std_var_series_99$cc.Decision,
    
    egarch_22std_95_failures = sum(is.na(var_series_1d_95$egarch_22std_var_series_95)),
    egarch_22std_99_failures = sum(is.na(var_series_1d_99$egarch_22std_var_series_99)),
    
    egarch_22std_95_violations = var_1d_95_metrics$egarch_22std_var_series_95$actual.exceed,
    egarch_22std_95_kupic_p = var_1d_95_metrics$egarch_22std_var_series_95$uc.LRp,
    egarch_22std_95_kupic_decision = var_1d_95_metrics$egarch_22std_var_series_95$uc.Decision,
    egarch_22std_95_chris_p = var_1d_95_metrics$egarch_22std_var_series_95$cc.LRp,
    egarch_22std_95_chris_decision = var_1d_95_metrics$egarch_22std_var_series_95$cc.Decision,
    egarch_22std_99_violations = var_1d_99_metrics$egarch_22std_var_series_99$actual.exceed,
    egarch_22std_99_kupic_p = var_1d_99_metrics$egarch_22std_var_series_99$uc.LRp,
    egarch_22std_99_kupic_decision = var_1d_99_metrics$egarch_22std_var_series_99$uc.Decision,
    egarch_22std_99_chris_p = var_1d_99_metrics$egarch_22std_var_series_99$cc.LRp,
    egarch_22std_99_chris_decision = var_1d_99_metrics$egarch_22std_var_series_99$cc.Decision,
    
    tgarch_11std_95_failures = sum(is.na(var_series_1d_95$tgarch_11std_var_series_95)),
    tgarch_11std_99_failures = sum(is.na(var_series_1d_99$tgarch_11std_var_series_99)),
    
    tgarch_11std_95_violations = var_1d_95_metrics$tgarch_11std_var_series_95$actual.exceed,
    tgarch_11std_95_kupic_p = var_1d_95_metrics$tgarch_11std_var_series_95$uc.LRp,
    tgarch_11std_95_kupic_decision = var_1d_95_metrics$tgarch_11std_var_series_95$uc.Decision,
    tgarch_11std_95_chris_p = var_1d_95_metrics$tgarch_11std_var_series_95$cc.LRp,
    tgarch_11std_95_chris_decision = var_1d_95_metrics$tgarch_11std_var_series_95$cc.Decision,
    tgarch_11std_99_violations = var_1d_99_metrics$tgarch_11std_var_series_99$actual.exceed,
    tgarch_11std_99_kupic_p = var_1d_99_metrics$tgarch_11std_var_series_99$uc.LRp,
    tgarch_11std_99_kupic_decision = var_1d_99_metrics$tgarch_11std_var_series_99$uc.Decision,
    tgarch_11std_99_chris_p = var_1d_99_metrics$tgarch_11std_var_series_99$cc.LRp,
    tgarch_11std_99_chris_decision = var_1d_99_metrics$tgarch_11std_var_series_99$cc.Decision,
    
    tgarch_21std_95_failures = sum(is.na(var_series_1d_95$tgarch_21std_var_series_95)),
    tgarch_21std_99_failures = sum(is.na(var_series_1d_99$tgarch_21std_var_series_99)),
    
    tgarch_21std_95_violations = var_1d_95_metrics$tgarch_21std_var_series_95$actual.exceed,
    tgarch_21std_95_kupic_p = var_1d_95_metrics$tgarch_21std_var_series_95$uc.LRp,
    tgarch_21std_95_kupic_decision = var_1d_95_metrics$tgarch_21std_var_series_95$uc.Decision,
    tgarch_21std_95_chris_p = var_1d_95_metrics$tgarch_21std_var_series_95$cc.LRp,
    tgarch_21std_95_chris_decision = var_1d_95_metrics$tgarch_21std_var_series_95$cc.Decision,
    tgarch_21std_99_violations = var_1d_99_metrics$tgarch_21std_var_series_99$actual.exceed,
    tgarch_21std_99_kupic_p = var_1d_99_metrics$tgarch_21std_var_series_99$uc.LRp,
    tgarch_21std_99_kupic_decision = var_1d_99_metrics$tgarch_21std_var_series_99$uc.Decision,
    tgarch_21std_99_chris_p = var_1d_99_metrics$tgarch_21std_var_series_99$cc.LRp,
    tgarch_21std_99_chris_decision = var_1d_99_metrics$tgarch_21std_var_series_99$cc.Decision,
    
    tgarch_12std_95_failures = sum(is.na(var_series_1d_95$tgarch_12std_var_series_95)),
    tgarch_12std_99_failures = sum(is.na(var_series_1d_99$tgarch_12std_var_series_99)),
    
    tgarch_12std_95_violations = var_1d_95_metrics$tgarch_12std_var_series_95$actual.exceed,
    tgarch_12std_95_kupic_p = var_1d_95_metrics$tgarch_12std_var_series_95$uc.LRp,
    tgarch_12std_95_kupic_decision = var_1d_95_metrics$tgarch_12std_var_series_95$uc.Decision,
    tgarch_12std_95_chris_p = var_1d_95_metrics$tgarch_12std_var_series_95$cc.LRp,
    tgarch_12std_95_chris_decision = var_1d_95_metrics$tgarch_12std_var_series_95$cc.Decision,
    tgarch_12std_99_violations = var_1d_99_metrics$tgarch_12std_var_series_99$actual.exceed,
    tgarch_12std_99_kupic_p = var_1d_99_metrics$tgarch_12std_var_series_99$uc.LRp,
    tgarch_12std_99_kupic_decision = var_1d_99_metrics$tgarch_12std_var_series_99$uc.Decision,
    tgarch_12std_99_chris_p = var_1d_99_metrics$tgarch_12std_var_series_99$cc.LRp,
    tgarch_12std_99_chris_decision = var_1d_99_metrics$tgarch_12std_var_series_99$cc.Decision,
    
    tgarch_22std_95_failures = sum(is.na(var_series_1d_95$tgarch_22std_var_series_95)),
    tgarch_22std_99_failures = sum(is.na(var_series_1d_99$tgarch_22std_var_series_99)),
    
    tgarch_22std_95_violations = var_1d_95_metrics$tgarch_22std_var_series_95$actual.exceed,
    tgarch_22std_95_kupic_p = var_1d_95_metrics$tgarch_22std_var_series_95$uc.LRp,
    tgarch_22std_95_kupic_decision = var_1d_95_metrics$tgarch_22std_var_series_95$uc.Decision,
    tgarch_22std_95_chris_p = var_1d_95_metrics$tgarch_22std_var_series_95$cc.LRp,
    tgarch_22std_95_chris_decision = var_1d_95_metrics$tgarch_22std_var_series_95$cc.Decision,
    tgarch_22std_99_violations = var_1d_99_metrics$tgarch_22std_var_series_99$actual.exceed,
    tgarch_22std_99_kupic_p = var_1d_99_metrics$tgarch_22std_var_series_99$uc.LRp,
    tgarch_22std_99_kupic_decision = var_1d_99_metrics$tgarch_22std_var_series_99$uc.Decision,
    tgarch_22std_99_chris_p = var_1d_99_metrics$tgarch_22std_var_series_99$cc.LRp,
    tgarch_22std_99_chris_decision = var_1d_99_metrics$tgarch_22std_var_series_99$cc.Decision,
    # 10d VaR metrics
    var_95_10d_exp_violations = var_10d_95_metrics$his_var_series_10d_95$expected.exceed,
    var_95_10d_violations = var_10d_95_metrics$his_var_series_10d_95$actual.exceed,
    var_95_10d_kupic_p = var_10d_95_metrics$his_var_series_10d_95$uc.LRp,
    var_95_10d_kupic_decision = var_10d_95_metrics$his_var_series_10d_95$uc.Decision,
    var_95_10d_chris_p = var_10d_95_metrics$his_var_series_10d_95$cc.LRp,
    var_95_10d_chris_decision = var_10d_95_metrics$his_var_series_10d_95$cc.Decision,
    var_99_10d_exp_violations = var_10d_99_metrics$his_var_series_10d_99$expected.exceed,
    var_99_10d_violations = var_10d_99_metrics$his_var_series_10d_99$actual.exceed,
    var_99_10d_kupic_p = var_10d_99_metrics$his_var_series_10d_99$uc.LRp,
    var_99_10d_kupic_decision = var_10d_99_metrics$his_var_series_10d_99$uc.Decision,
    var_99_10d_chris_p = var_10d_99_metrics$his_var_series_10d_99$cc.LRp,
    var_99_10d_chris_decision = var_10d_99_metrics$his_var_series_10d_99$cc.Decision,
    
    garch_11std_10d_95_violations = var_10d_95_metrics$garch_11std_10d_var_series_95$actual.exceed,
    garch_11std_10d_95_kupic_p = var_10d_95_metrics$garch_11std_10d_var_series_95$uc.LRp,
    garch_11std_10d_95_kupic_decision = var_10d_95_metrics$garch_11std_10d_var_series_95$uc.Decision,
    garch_11std_10d_95_chris_p = var_10d_95_metrics$garch_11std_10d_var_series_95$cc.LRp,
    garch_11std_10d_95_chris_decision = var_10d_95_metrics$garch_11std_10d_var_series_95$cc.Decision,
    garch_11std_10d_99_violations = var_10d_99_metrics$garch_11std_10d_var_series_99$actual.exceed,
    garch_11std_10d_99_kupic_p = var_10d_99_metrics$garch_11std_10d_var_series_99$uc.LRp,
    garch_11std_10d_99_kupic_decision = var_10d_99_metrics$garch_11std_10d_var_series_99$uc.Decision,
    garch_11std_10d_99_chris_p = var_10d_99_metrics$garch_11std_10d_var_series_99$cc.LRp,
    garch_11std_10d_99_chris_decision = var_10d_99_metrics$garch_11std_10d_var_series_99$cc.Decision,
    
    garch_21std_10d_95_violations = var_10d_95_metrics$garch_21std_10d_var_series_95$actual.exceed,
    garch_21std_10d_95_kupic_p = var_10d_95_metrics$garch_21std_10d_var_series_95$uc.LRp,
    garch_21std_10d_95_kupic_decision = var_10d_95_metrics$garch_21std_10d_var_series_95$uc.Decision,
    garch_21std_10d_95_chris_p = var_10d_95_metrics$garch_21std_10d_var_series_95$cc.LRp,
    garch_21std_10d_95_chris_decision = var_10d_95_metrics$garch_21std_10d_var_series_95$cc.Decision,
    garch_21std_10d_99_violations = var_10d_99_metrics$garch_21std_10d_var_series_99$actual.exceed,
    garch_21std_10d_99_kupic_p = var_10d_99_metrics$garch_21std_10d_var_series_99$uc.LRp,
    garch_21std_10d_99_kupic_decision = var_10d_99_metrics$garch_21std_10d_var_series_99$uc.Decision,
    garch_21std_10d_99_chris_p = var_10d_99_metrics$garch_21std_10d_var_series_99$cc.LRp,
    garch_21std_10d_99_chris_decision = var_10d_99_metrics$garch_21std_10d_var_series_99$cc.Decision,
    
    garch_12std_10d_95_violations = var_10d_95_metrics$garch_12std_10d_var_series_95$actual.exceed,
    garch_12std_10d_95_kupic_p = var_10d_95_metrics$garch_12std_10d_var_series_95$uc.LRp,
    garch_12std_10d_95_kupic_decision = var_10d_95_metrics$garch_12std_10d_var_series_95$uc.Decision,
    garch_12std_10d_95_chris_p = var_10d_95_metrics$garch_12std_10d_var_series_95$cc.LRp,
    garch_12std_10d_95_chris_decision = var_10d_95_metrics$garch_12std_10d_var_series_95$cc.Decision,
    garch_12std_10d_99_violations = var_10d_99_metrics$garch_12std_10d_var_series_99$actual.exceed,
    garch_12std_10d_99_kupic_p = var_10d_99_metrics$garch_12std_10d_var_series_99$uc.LRp,
    garch_12std_10d_99_kupic_decision = var_10d_99_metrics$garch_12std_10d_var_series_99$uc.Decision,
    garch_12std_10d_99_chris_p = var_10d_99_metrics$garch_12std_10d_var_series_99$cc.LRp,
    garch_12std_10d_99_chris_decision = var_10d_99_metrics$garch_12std_10d_var_series_99$cc.Decision,
    
    garch_22std_10d_95_violations = var_10d_95_metrics$garch_22std_10d_var_series_95$actual.exceed,
    garch_22std_10d_95_kupic_p = var_10d_95_metrics$garch_22std_10d_var_series_95$uc.LRp,
    garch_22std_10d_95_kupic_decision = var_10d_95_metrics$garch_22std_10d_var_series_95$uc.Decision,
    garch_22std_10d_95_chris_p = var_10d_95_metrics$garch_22std_10d_var_series_95$cc.LRp,
    garch_22std_10d_95_chris_decision = var_10d_95_metrics$garch_22std_10d_var_series_95$cc.Decision,
    garch_22std_10d_99_violations = var_10d_99_metrics$garch_22std_10d_var_series_99$actual.exceed,
    garch_22std_10d_99_kupic_p = var_10d_99_metrics$garch_22std_10d_var_series_99$uc.LRp,
    garch_22std_10d_99_kupic_decision = var_10d_99_metrics$garch_22std_10d_var_series_99$uc.Decision,
    garch_22std_10d_99_chris_p = var_10d_99_metrics$garch_22std_10d_var_series_99$cc.LRp,
    garch_22std_10d_99_chris_decision = var_10d_99_metrics$garch_22std_10d_var_series_99$cc.Decision,
    
    egarch_11std_10d_95_violations = var_10d_95_metrics$egarch_11std_10d_var_series_95$actual.exceed,
    egarch_11std_10d_95_kupic_p = var_10d_95_metrics$egarch_11std_10d_var_series_95$uc.LRp,
    egarch_11std_10d_95_kupic_decision = var_10d_95_metrics$egarch_11std_10d_var_series_95$uc.Decision,
    egarch_11std_10d_95_chris_p = var_10d_95_metrics$egarch_11std_10d_var_series_95$cc.LRp,
    egarch_11std_10d_95_chris_decision = var_10d_95_metrics$egarch_11std_10d_var_series_95$cc.Decision,
    egarch_11std_10d_99_violations = var_10d_99_metrics$egarch_11std_10d_var_series_99$actual.exceed,
    egarch_11std_10d_99_kupic_p = var_10d_99_metrics$egarch_11std_10d_var_series_99$uc.LRp,
    egarch_11std_10d_99_kupic_decision = var_10d_99_metrics$egarch_11std_10d_var_series_99$uc.Decision,
    egarch_11std_10d_99_chris_p = var_10d_99_metrics$egarch_11std_10d_var_series_99$cc.LRp,
    egarch_11std_10d_99_chris_decision = var_10d_99_metrics$egarch_11std_10d_var_series_99$cc.Decision,
    
    egarch_21std_10d_95_violations = var_10d_95_metrics$egarch_21std_10d_var_series_95$actual.exceed,
    egarch_21std_10d_95_kupic_p = var_10d_95_metrics$egarch_21std_10d_var_series_95$uc.LRp,
    egarch_21std_10d_95_kupic_decision = var_10d_95_metrics$egarch_21std_10d_var_series_95$uc.Decision,
    egarch_21std_10d_95_chris_p = var_10d_95_metrics$egarch_21std_10d_var_series_95$cc.LRp,
    egarch_21std_10d_95_chris_decision = var_10d_95_metrics$egarch_21std_10d_var_series_95$cc.Decision,
    egarch_21std_10d_99_violations = var_10d_99_metrics$egarch_21std_10d_var_series_99$actual.exceed,
    egarch_21std_10d_99_kupic_p = var_10d_99_metrics$egarch_21std_10d_var_series_99$uc.LRp,
    egarch_21std_10d_99_kupic_decision = var_10d_99_metrics$egarch_21std_10d_var_series_99$uc.Decision,
    egarch_21std_10d_99_chris_p = var_10d_99_metrics$egarch_21std_10d_var_series_99$cc.LRp,
    egarch_21std_10d_99_chris_decision = var_10d_99_metrics$egarch_21std_10d_var_series_99$cc.Decision,
    
    egarch_12std_10d_95_violations = var_10d_95_metrics$egarch_12std_10d_var_series_95$actual.exceed,
    egarch_12std_10d_95_kupic_p = var_10d_95_metrics$egarch_12std_10d_var_series_95$uc.LRp,
    egarch_12std_10d_95_kupic_decision = var_10d_95_metrics$egarch_12std_10d_var_series_95$uc.Decision,
    egarch_12std_10d_95_chris_p = var_10d_95_metrics$egarch_12std_10d_var_series_95$cc.LRp,
    egarch_12std_10d_95_chris_decision = var_10d_95_metrics$egarch_12std_10d_var_series_95$cc.Decision,
    egarch_12std_10d_99_violations = var_10d_99_metrics$egarch_12std_10d_var_series_99$actual.exceed,
    egarch_12std_10d_99_kupic_p = var_10d_99_metrics$egarch_12std_10d_var_series_99$uc.LRp,
    egarch_12std_10d_99_kupic_decision = var_10d_99_metrics$egarch_12std_10d_var_series_99$uc.Decision,
    egarch_12std_10d_99_chris_p = var_10d_99_metrics$egarch_12std_10d_var_series_99$cc.LRp,
    egarch_12std_10d_99_chris_decision = var_10d_99_metrics$egarch_12std_10d_var_series_99$cc.Decision,
    
    egarch_22std_10d_95_violations = var_10d_95_metrics$egarch_22std_10d_var_series_95$actual.exceed,
    egarch_22std_10d_95_kupic_p = var_10d_95_metrics$egarch_22std_10d_var_series_95$uc.LRp,
    egarch_22std_10d_95_kupic_decision = var_10d_95_metrics$egarch_22std_10d_var_series_95$uc.Decision,
    egarch_22std_10d_95_chris_p = var_10d_95_metrics$egarch_22std_10d_var_series_95$cc.LRp,
    egarch_22std_10d_95_chris_decision = var_10d_95_metrics$egarch_22std_10d_var_series_95$cc.Decision,
    egarch_22std_10d_99_violations = var_10d_99_metrics$egarch_22std_10d_var_series_99$actual.exceed,
    egarch_22std_10d_99_kupic_p = var_10d_99_metrics$egarch_22std_10d_var_series_99$uc.LRp,
    egarch_22std_10d_99_kupic_decision = var_10d_99_metrics$egarch_22std_10d_var_series_99$uc.Decision,
    egarch_22std_10d_99_chris_p = var_10d_99_metrics$egarch_22std_10d_var_series_99$cc.LRp,
    egarch_22std_10d_99_chris_decision = var_10d_99_metrics$egarch_22std_10d_var_series_99$cc.Decision,
    
    tgarch_11std_10d_95_violations = var_10d_95_metrics$tgarch_11std_10d_var_series_95$actual.exceed,
    tgarch_11std_10d_95_kupic_p = var_10d_95_metrics$tgarch_11std_10d_var_series_95$uc.LRp,
    tgarch_11std_10d_95_kupic_decision = var_10d_95_metrics$tgarch_11std_10d_var_series_95$uc.Decision,
    tgarch_11std_10d_95_chris_p = var_10d_95_metrics$tgarch_11std_10d_var_series_95$cc.LRp,
    tgarch_11std_10d_95_chris_decision = var_10d_95_metrics$tgarch_11std_10d_var_series_95$cc.Decision,
    tgarch_11std_10d_99_violations = var_10d_99_metrics$tgarch_11std_10d_var_series_99$actual.exceed,
    tgarch_11std_10d_99_kupic_p = var_10d_99_metrics$tgarch_11std_10d_var_series_99$uc.LRp,
    tgarch_11std_10d_99_kupic_decision = var_10d_99_metrics$tgarch_11std_10d_var_series_99$uc.Decision,
    tgarch_11std_10d_99_chris_p = var_10d_99_metrics$tgarch_11std_10d_var_series_99$cc.LRp,
    tgarch_11std_10d_99_chris_decision = var_10d_99_metrics$tgarch_11std_10d_var_series_99$cc.Decision,
    
    tgarch_21std_10d_95_violations = var_10d_95_metrics$tgarch_21std_10d_var_series_95$actual.exceed,
    tgarch_21std_10d_95_kupic_p = var_10d_95_metrics$tgarch_21std_10d_var_series_95$uc.LRp,
    tgarch_21std_10d_95_kupic_decision = var_10d_95_metrics$tgarch_21std_10d_var_series_95$uc.Decision,
    tgarch_21std_10d_95_chris_p = var_10d_95_metrics$tgarch_21std_10d_var_series_95$cc.LRp,
    tgarch_21std_10d_95_chris_decision = var_10d_95_metrics$tgarch_21std_10d_var_series_95$cc.Decision,
    tgarch_21std_10d_99_violations = var_10d_99_metrics$tgarch_21std_10d_var_series_99$actual.exceed,
    tgarch_21std_10d_99_kupic_p = var_10d_99_metrics$tgarch_21std_10d_var_series_99$uc.LRp,
    tgarch_21std_10d_99_kupic_decision = var_10d_99_metrics$tgarch_21std_10d_var_series_99$uc.Decision,
    tgarch_21std_10d_99_chris_p = var_10d_99_metrics$tgarch_21std_10d_var_series_99$cc.LRp,
    tgarch_21std_10d_99_chris_decision = var_10d_99_metrics$tgarch_21std_10d_var_series_99$cc.Decision,
    
    tgarch_12std_10d_95_violations = var_10d_95_metrics$tgarch_12std_10d_var_series_95$actual.exceed,
    tgarch_12std_10d_95_kupic_p = var_10d_95_metrics$tgarch_12std_10d_var_series_95$uc.LRp,
    tgarch_12std_10d_95_kupic_decision = var_10d_95_metrics$tgarch_12std_10d_var_series_95$uc.Decision,
    tgarch_12std_10d_95_chris_p = var_10d_95_metrics$tgarch_12std_10d_var_series_95$cc.LRp,
    tgarch_12std_10d_95_chris_decision = var_10d_95_metrics$tgarch_12std_10d_var_series_95$cc.Decision,
    tgarch_12std_10d_99_violations = var_10d_99_metrics$tgarch_12std_10d_var_series_99$actual.exceed,
    tgarch_12std_10d_99_kupic_p = var_10d_99_metrics$tgarch_12std_10d_var_series_99$uc.LRp,
    tgarch_12std_10d_99_kupic_decision = var_10d_99_metrics$tgarch_12std_10d_var_series_99$uc.Decision,
    tgarch_12std_10d_99_chris_p = var_10d_99_metrics$tgarch_12std_10d_var_series_99$cc.LRp,
    tgarch_12std_10d_99_chris_decision = var_10d_99_metrics$tgarch_12std_10d_var_series_99$cc.Decision,
    
    tgarch_22std_10d_95_violations = var_10d_95_metrics$tgarch_22std_10d_var_series_95$actual.exceed,
    tgarch_22std_10d_95_kupic_p = var_10d_95_metrics$tgarch_22std_10d_var_series_95$uc.LRp,
    tgarch_22std_10d_95_kupic_decision = var_10d_95_metrics$tgarch_22std_10d_var_series_95$uc.Decision,
    tgarch_22std_10d_95_chris_p = var_10d_95_metrics$tgarch_22std_10d_var_series_95$cc.LRp,
    tgarch_22std_10d_95_chris_decision = var_10d_95_metrics$tgarch_22std_10d_var_series_95$cc.Decision,
    tgarch_22std_10d_99_violations = var_10d_99_metrics$tgarch_22std_10d_var_series_99$actual.exceed,
    tgarch_22std_10d_99_kupic_p = var_10d_99_metrics$tgarch_22std_10d_var_series_99$uc.LRp,
    tgarch_22std_10d_99_kupic_decision = var_10d_99_metrics$tgarch_22std_10d_var_series_99$uc.Decision,
    tgarch_22std_10d_99_chris_p = var_10d_99_metrics$tgarch_22std_10d_var_series_99$cc.LRp,
    tgarch_22std_10d_99_chris_decision = var_10d_99_metrics$tgarch_22std_10d_var_series_99$cc.Decision,
    # Backtesting results
    his_95_qs = qs_1d_95[['his_var_series_95']],
    garch_11std_95_qs = qs_1d_95[['garch_11std_var_series_95']],
    garch_12std_95_qs = qs_1d_95[['garch_12std_var_series_95']],
    garch_21std_95_qs = qs_1d_95[['garch_21std_var_series_95']],
    garch_22std_95_qs = qs_1d_95[['garch_22std_var_series_95']],
    egarch_11std_95_qs = qs_1d_95[['egarch_11std_var_series_95']],
    egarch_12std_95_qs = qs_1d_95[['egarch_12std_var_series_95']],
    egarch_21std_95_qs = qs_1d_95[['egarch_21std_var_series_95']],
    egarch_22std_95_qs = qs_1d_95[['egarch_22std_var_series_95']],
    tgarch_11std_95_qs = qs_1d_95[['tgarch_11std_var_series_95']],
    tgarch_12std_95_qs = qs_1d_95[['tgarch_12std_var_series_95']],
    tgarch_21std_95_qs = qs_1d_95[['tgarch_21std_var_series_95']],
    tgarch_22std_95_qs = qs_1d_95[['tgarch_22std_var_series_95']],
    
    his_99_qs = qs_1d_99[['his_var_series_99']],
    garch_11std_99_qs = qs_1d_99[['garch_11std_var_series_99']],
    garch_12std_99_qs = qs_1d_99[['garch_12std_var_series_99']],
    garch_21std_99_qs = qs_1d_99[['garch_21std_var_series_99']],
    garch_22std_99_qs = qs_1d_99[['garch_22std_var_series_99']],
    egarch_11std_99_qs = qs_1d_99[['egarch_11std_var_series_99']],
    egarch_12std_99_qs = qs_1d_99[['egarch_12std_var_series_99']],
    egarch_21std_99_qs = qs_1d_99[['egarch_21std_var_series_99']],
    egarch_22std_99_qs = qs_1d_99[['egarch_22std_var_series_99']],
    tgarch_11std_99_qs = qs_1d_99[['tgarch_11std_var_series_99']],
    tgarch_12std_99_qs = qs_1d_99[['tgarch_12std_var_series_99']],
    tgarch_21std_99_qs = qs_1d_99[['tgarch_21std_var_series_99']],
    tgarch_22std_99_qs = qs_1d_99[['tgarch_22std_var_series_99']],
    
    garch_11std_10d_95_qs = qs_10d_95[['garch_11std_10d_var_series_95']],
    garch_12std_10d_95_qs = qs_10d_95[['garch_12std_10d_var_series_95']],
    garch_21std_10d_95_qs = qs_10d_95[['garch_21std_10d_var_series_95']],
    garch_22std_10d_95_qs = qs_10d_95[['garch_22std_10d_var_series_95']],
    egarch_11std_10d_95_qs = qs_10d_95[['egarch_11std_10d_var_series_95']],
    egarch_12std_10d_95_qs = qs_10d_95[['egarch_12std_10d_var_series_95']],
    egarch_21std_10d_95_qs = qs_10d_95[['egarch_21std_10d_var_series_95']],
    egarch_22std_10d_95_qs = qs_10d_95[['egarch_22std_10d_var_series_95']],
    tgarch_11std_10d_95_qs = qs_10d_95[['tgarch_11std_10d_var_series_95']],
    tgarch_12std_10d_95_qs = qs_10d_95[['tgarch_12std_10d_var_series_95']],
    tgarch_21std_10d_95_qs = qs_10d_95[['tgarch_21std_10d_var_series_95']],
    tgarch_22std_10d_95_qs = qs_10d_95[['tgarch_22std_10d_var_series_95']],
    
    garch_11std_10d_99_qs = qs_10d_99[['garch_11std_10d_var_series_99']],
    garch_12std_10d_99_qs = qs_10d_99[['garch_12std_10d_var_series_99']],
    garch_21std_10d_99_qs = qs_10d_99[['garch_21std_10d_var_series_99']],
    garch_22std_10d_99_qs = qs_10d_99[['garch_22std_10d_var_series_99']],
    egarch_11std_10d_99_qs = qs_10d_99[['egarch_11std_10d_var_series_99']],
    egarch_12std_10d_99_qs = qs_10d_99[['egarch_12std_10d_var_series_99']],
    egarch_21std_10d_99_qs = qs_10d_99[['egarch_21std_10d_var_series_99']],
    egarch_22std_10d_99_qs = qs_10d_99[['egarch_22std_10d_var_series_99']],
    tgarch_11std_10d_99_qs = qs_10d_99[['tgarch_11std_10d_var_series_99']],
    tgarch_12std_10d_99_qs = qs_10d_99[['tgarch_12std_10d_var_series_99']],
    tgarch_21std_10d_99_qs = qs_10d_99[['tgarch_21std_10d_var_series_99']],
    tgarch_22std_10d_99_qs = qs_10d_99[['tgarch_22std_10d_var_series_99']]
  )
}

# Combine results into a single data frame
final_results <- do.call(rbind, results)
final_var_results <- do.call(rbind, var_results)

# Save results to CSV
write.csv(final_results, "results_20250816_3m.csv", row.names = FALSE)
write.csv(final_var_results, "var_results_20250816_3m.csv", row.names = FALSE)
