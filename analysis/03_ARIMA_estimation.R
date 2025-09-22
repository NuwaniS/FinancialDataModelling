# This script contains ARIMA model estimations for different periods on FTSE 100.
if (!require("forecast")) install.packages("forecast")

library(quantmod)
library(forecast)

source('R/helper_functions.R')

# Evaluate dataset over specified date ranges
evaluate_dataset <- function(symbol_name, csv_file_name, date_ranges) {
  results <- list()
  
  # Read the CSV
  data <- read.csv(csv_file_name, stringsAsFactors = FALSE)
  
  # Convert the first column to Date
  data$Index <- as.Date(data$Index)
  
  # Convert to xts
  data_xts <- xts(data[, -1], order.by = data$Index)
  symbol_prices <- Ad(data_xts)
  
  # Drop missing values
  symbol_prices <- na.omit(symbol_prices)
  
  for (range in date_ranges) {
    date_range <- paste0(range[1],'/',range[2])
    price_data <- symbol_prices[date_range]
    
    print(paste('Evaluating', symbol_name, date_range, nrow(price_data)))
    
    # Plot the raw data
    plot_time_series_data(price_data, paste(symbol_name, date_range))
    
    #Stationary test for raw data
    stest <- stationarity_tests(price_data)
    
    # Calculate log returns
    log_returns <- diff(log(coredata(price_data)))
    
    plot_time_series_data(log_returns, paste(symbol_name, "Log Returns", date_range))
    # Stationary Test for log returns
    log_returns_stest <- stationarity_tests(log_returns)
    
    # Fit ARIMA model using automatic selection
    arima_model <- auto.arima(log_returns, d = 0) # log_returns was confirmed to be stationary
    residuals <- residuals(arima_model)
    
    # Test for zero mean
    mean_residuals = mean(residuals)
    # Autocorrelations in residuals test
    i_test <- independence_test(residuals)
    # Normality test using Shapiro-Wilk test
    n_test <- normality_tests(residuals)
    
    # Store results
    results[[length(results) + 1]] <- data.frame(
      Dataset = symbol_name,
      DateRange = date_range,
      Raw_ADF_PValue = stest$adf_pvalue,
      Raw_ADF = stest$adf_decision,
      Raw_KPSS_Trend_PValue = stest$kpss_trend_pvalue,
      Raw_KPSS_Trend = stest$kpss_trend_decision,
      Raw_KPSS_Level_PValue = stest$kpss_level_pvalue,
      Raw_KPSS_Level = stest$kpss_level_decision,
      Raw_PP_PValue = stest$pp_pvalue,
      Raw_PP = stest$pp_decision,
      Raw_PPT_PValue = stest$ppt_pvalue,
      Raw_PPT = stest$ppt_decision,
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
      BIC = arima_model$bic
    )
  }
  return(do.call(rbind, results))
}

# Load the data sets
datasets <- list(
  FTSE_100 = 'data/FTSE_raw_data.csv'
)

date_ranges <- list(
  c('2007-01-02', '2025-04-25'),
  c('2007-01-02', '2018-12-31'),
  c('2019-01-02', '2022-12-30'),
  c('2015-01-02', '2022-12-30'),
  c('2022-01-04', '2025-04-25')
)

# Evaluate each dataset
all_results <- lapply(names(datasets), function(name) {
  evaluate_dataset(name, datasets[[name]], date_ranges)
})

# Combine results into a single data frame
final_results <- do.call(rbind, all_results)

# Save results to CSV
write.csv(final_results, "arima_evaluation_results.csv", row.names = FALSE)