# This is the model used to generate the results after discussion on 20/06/2025

library(quantmod)
library(forecast)
library(rugarch)

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
  '3_months' = 63,     # 21 trading days/month × 3
  '6_months' = 126,
  '1_year'   = 250,
  '2_year'   = 500,
  'full'     = NA      # All available data up to date
)

# Set date range for VaR estimation
forward_start_date <- as.Date("2025-01-02")
forward_end_date   <- as.Date("2025-05-19")

test_dates <- index(log_returns)[index(log_returns) >= forward_start_date & index(log_returns) <= forward_end_date]
test_data_1d_all <- log_returns[test_dates]

# Consider the windows for the first loop since it is more favourable for Var Calculations
for (win_name in names(window_lengths)) {
  window_len <- window_lengths[[win_name]]
  # Series to store 1d VaR values
  vaR_series_95 <- numeric(0)
  vaR_series_99 <- numeric(0)
  for (current_date in test_dates) {
    current_date_string <- index(log_returns[index(log_returns) == current_date])
    
    test_data_1d <- log_returns[index(log_returns) == current_date]
    test_data_10d <- log_returns[index(log_returns) >= current_date]
    test_data_10d <- head(test_data_10d, 10)
    
    # Subset the look-back window
    if (is.na(window_len)) {
      training_data <- log_returns[index(log_returns) < current_date]
    } else {
      training_data <- log_returns[index(log_returns) < current_date]
      training_data <- tail(training_data, window_len)
    }
    
    # EDA for the selected data range
    
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
    
    arima_forecast_10d <- forecast(arima_model, h = 10)
    arima_10d <- forecast_evaluation(test_data_10d, arima_forecast_10d$mean)
    
    # Historical VaR
    historical_var = quantile(training_data, probs = 0.05, na.rm = TRUE)  #95% confidence
    vaR_series_95 <- c(vaR_series_95, historical_var)
    
    historical_var = quantile(training_data, probs = 0.01, na.rm = TRUE)  #99% confidence
    vaR_series_99 <- c(vaR_series_99, historical_var)
    
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
      ARIMA_10d_MAPE = arima_10d$mape
    )
  }
  
  # VaR Metrics
  vaR_xts_95 <- xts(vaR_series_95, order.by = index(test_data_1d_all))
  vaR_xts_99 <- xts(vaR_series_99, order.by = index(test_data_1d_all))
  
  var_95_metrics <- VaRTest(0.05, test_data_1d_all, vaR_xts_95, 0.95)
  var_99_metrics <- VaRTest(0.01, test_data_1d_all, vaR_xts_99, 0.99)
  
  # Store the plots in folders
  
  # Store Var Results
  var_results[[length(var_results) + 1]] <- data.frame(
    LookBackWin = win_name,
    var_95_exp_violations = var_95_metrics$expected.exceed,
    var_95_violations = var_95_metrics$actual.exceed,
    var_95_kupic_p = var_95_metrics$uc.LRp,
    var_95_kupic_decision = var_95_metrics$uc.Decision,
    var_95_chris_p = var_95_metrics$cc.LRp,
    var_95_chris_decision = var_95_metrics$cc.Decision,
    var_99_exp_violations = var_99_metrics$expected.exceed,
    var_99_violations = var_99_metrics$actual.exceed,
    var_99_kupic_p = var_99_metrics$uc.LRp,
    var_99_kupic_decision = var_99_metrics$uc.Decision,
    var_99_chris_p = var_99_metrics$cc.LRp,
    var_99_chris_decision = var_99_metrics$cc.Decision
  )
}

# Combine results into a single data frame
final_results <- do.call(rbind, results)
final_var_results <- do.call(rbind, var_results)

# Save results to CSV
write.csv(final_results, "results_20250715.csv", row.names = FALSE)
write.csv(final_var_results, "var_results_20250715.csv", row.names = FALSE)
