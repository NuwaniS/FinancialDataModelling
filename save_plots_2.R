# Use this file to save plots because the plots can't be generated inside loops in R.

library(xts)

# Create folder for plots 
plot_dir <- "plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Change the window name after running the script financial_model_4.R - 3 Months, 6 Months, 1 Year, 2 Year, Full, 
win_tag = 'Full'

removeNA <- function(test_data, var_series) {
  var_xts <- xts(var_series, order.by = index(test_data))
  valid_idx <- which(!is.na(var_xts))
  var_xts_clean <- var_xts[valid_idx]
  test_data_clean <- test_data[valid_idx]
  
  exceptions <- which(test_data_clean < var_xts_clean)
  exceptions_index <- index(test_data_clean)[exceptions]
  exceptions_data <- coredata(test_data_clean)[exceptions]
  exceptions_xts <- xts(exceptions_data, order.by = exceptions_index)
  
  y_min <- min(min(test_data_clean, na.rm = TRUE), min(var_xts_clean, na.rm = TRUE))
  y_max <- max(max(test_data_clean, na.rm = TRUE), max(var_xts_clean, na.rm = TRUE))
  
  x_limits <- range(time(test_data_clean))
  
  return (list(test_data_clean = test_data_clean, var_xts_clean = var_xts_clean, x_limits = x_limits,
               y_limits = c(y_min,y_max), exceptions = exceptions_xts))
}

plot_one_series <- function(actual_data, var_series, file_name, title, legend_names) {
  clean_data <- removeNA(actual_data, var_series)
  png(filename = file.path(plot_dir, paste0(win_name, file_name)), width = 800, height = 600)
  par(xaxs = "i") 
  plot(zoo(actual_data), main = paste(win_tag, title), 
       type = 'l', ylim = clean_data$y_limits, xlim = clean_data$x_limits, col = "blue", ylab = "", xlab = "Date", lwd = 1)
  grid(col = "gray", lty = "dotted")
  lines(zoo(clean_data$var_xts_clean), col = "red", lwd = 2)
  points(zoo(clean_data$exceptions), col = "black", pch = 19, cex = 0.85)
  legend("topleft" , legend = legend_names, col = c("blue", "red", "black"), 
         lty = c(1, 1, NA), pch = c(NA, NA, 19), xpd = TRUE)
  dev.off()
}

plot_forecast_values <- function(actual_data, forecast_data, file_name, title, legend_names) {
  clean_data <- removeNA(actual_data, forecast_data)
  png(filename = file.path(plot_dir, paste0(win_name, file_name)), width = 800, height = 600)
  par(xaxs = "i") 
  plot(zoo(actual_data), main = paste(win_tag, title), 
       type = 'l', ylim = clean_data$y_limits, xlim = clean_data$x_limits, col = "blue", ylab = "", xlab = "Date", lwd = 1)
  grid(col = "gray", lty = "dotted")
  lines(zoo(clean_data$var_xts_clean), col = "red", lwd = 2)
  legend("topleft" , legend = legend_names, col = c("blue", "red"), lty = 1, xpd = TRUE)
  dev.off()
}

plot_two_series <- function(actual_data, var_series_95, var_series_99, file_name, title, legend_names) {
  clean_data <- removeNA(actual_data, var_series_95)
  clean_data_2 <- removeNA(actual_data, var_series_99)
  png(filename = file.path(plot_dir, paste0(win_name, file_name)), width = 800, height = 600)
  par(xaxs = "i") 
  plot(zoo(actual_data), main = paste(win_tag, title), 
       type = 'l', ylim = clean_data_2$y_limits, xlim = clean_data$x_limits, col = "blue", ylab = "", xlab = "Date", lwd = 1)
  grid(col = "gray", lty = "dotted")
  lines(zoo(clean_data$var_xts_clean), col = "red", lwd = 2)
  lines(zoo(clean_data_2$var_xts_clean), col = "orange", lwd = 2)
  points(zoo(clean_data$exceptions), col = "black", pch = 19, cex = 0.85)
  points(zoo(clean_data_2$exceptions), col = "black", pch = 19, cex = 0.85)
  legend("topleft" , legend = legend_names, 
         col = c("blue", "red", "orange", "black"), lty = c(1, 1, 1,NA), pch = c(NA, NA, NA, 19), xpd = TRUE)
  dev.off()
}

plot_four_series <- function(actual_data, his_series, garch_series, egarch_series, tgarch_series,
                             file_name, title, legend_names) {
  clean_data <- removeNA(actual_data, his_series)
  clean_data_2 <- removeNA(actual_data, garch_series)
  clean_data_3 <- removeNA(actual_data, egarch_series)
  clean_data_4 <- removeNA(actual_data, tgarch_series)
  png(filename = file.path(plot_dir, paste0(win_name, file_name)), width = 800, height = 600)
  par(xaxs = "i") 
  plot(zoo(actual_data), main = paste(win_tag, title),  type = 'p', pch = 19, cex = 0.75,
       ylim = clean_data_2$y_limits, xlim = clean_data$x_limits, col = "blue", ylab = "", xlab = "Date", lwd = 1)
  grid(col = "gray", lty = "dotted")
  lines(zoo(clean_data$var_xts_clean), col = "red", lty = "dashed", lwd = 2)
  lines(zoo(clean_data_2$var_xts_clean), col = "orange", lwd = 2)
  lines(zoo(clean_data_3$var_xts_clean), col = "green3", lwd = 2)
  lines(zoo(clean_data_4$var_xts_clean), col = "magenta", lty = "dotdash", lwd = 2)
  legend("topleft" , legend = legend_names, 
         col = c("blue", "red", "orange", "green3", "magenta"), lty = c(NA, 2, 1, 1, 4),
         pch = c(19, NA, NA, NA, NA), xpd = TRUE)
  dev.off()
}

#### 1d VaR Plots ####

plot_one_series(test_data_1d_all, 
                var_series_1d_95$his_var_series_95,
                file_name = '_1d_95_historical_var.png', 
                title = 'Period Lookback with Rolling 1-Day Historical VaR (95% Confidence)',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_one_series(test_data_1d_all, 
                var_series_1d_99$his_var_series_99,
                file_name = '_1d_99_historical_var.png', 
                title = 'Period Lookback with Rolling 1-Day Historical VaR (99% Confidence)',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_two_series(test_data_1d_all, 
                var_series_1d_95$his_var_series_95,
                var_series_1d_99$his_var_series_99,
                file_name = '_1d_historical_var.png', 
                title = 'Period Lookback with Rolling 1-Day Historical VaR',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

# GARCH(1,1)
plot_one_series(test_data_1d_all, 
                var_series_1d_95$garch_11std_var_series_95,
                file_name = '_1d_95_garch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR (95% Confidence) - GARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_one_series(test_data_1d_all, 
                var_series_1d_99$garch_11std_var_series_99,
                file_name = '_1d_99_garch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR (99% Confidence) - GARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_two_series(test_data_1d_all, 
                var_series_1d_95$garch_11std_var_series_95,
                var_series_1d_99$garch_11std_var_series_99,
                file_name = '_1d_garch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR - GARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

## Plotting eGARCH Results ##

# eGARCH(1,1)

plot_one_series(test_data_1d_all, 
                var_series_1d_95$egarch_11std_var_series_95,
                file_name = '_1d_95_egarch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR (95% Confidence) - eGARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_one_series(test_data_1d_all, 
                var_series_1d_99$egarch_11std_var_series_99,
                file_name = '_1d_99_egarch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR (99% Confidence) - eGARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_two_series(test_data_1d_all, 
                var_series_1d_95$egarch_11std_var_series_95,
                var_series_1d_99$egarch_11std_var_series_99,
                file_name = '_1d_egarch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR - eGARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

## Plotting tGARCH ###

# tGARCH(1,1)

plot_one_series(test_data_1d_all, 
                var_series_1d_95$tgarch_11std_var_series_95,
                file_name = '_1d_95_tgarch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR (95% Confidence) - tGARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_one_series(test_data_1d_all, 
                var_series_1d_99$egarch_11std_var_series_99,
                file_name = '_1d_99_egarch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR (99% Confidence) - tGARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "VaR", "Exceptions"))

plot_two_series(test_data_1d_all, 
                var_series_1d_95$tgarch_11std_var_series_95,
                var_series_1d_99$tgarch_11std_var_series_99,
                file_name = '_1d_tgarch_11std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR - tGARCH(1,1) with Student t Distribution',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

## Plotting some combinations ###

plot_four_series(test_data_1d_all,
                var_series_1d_95$his_var_series_95,
                var_series_1d_95$garch_11std_var_series_95,
                var_series_1d_95$egarch_11std_var_series_95,
                var_series_1d_95$tgarch_11std_var_series_95,
                file_name = '_1d_11_95std_var.png', 
                title = 'Period Lookback with Rolling 1-Day VaR (95% Confidence)',
                legend_names = c("Daily Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"))

plot_four_series(test_data_1d_all,
                 var_series_1d_99$his_var_series_99,
                 var_series_1d_99$garch_11std_var_series_99,
                 var_series_1d_99$egarch_11std_var_series_99,
                 var_series_1d_99$tgarch_11std_var_series_99,
                 file_name = '_1d_11_99std_var.png', 
                 title = 'Period Lookback with Rolling 1-Day VaR (99% Confidence)',
                 legend_names = c("Daily Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"))

#################################################
## 10d VaR Plots ##

plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_95$his_var_series_10d_95,
                file_name = '_10d_95_historical_var.png', 
                title = 'Period Lookback with Rolling 10-Day Historical VaR (95% Confidence)',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_99$his_var_series_10d_99,
                file_name = '_10d_99_historical_var.png', 
                title = 'Period Lookback with Rolling 10-Day Historical VaR (99% Confidence)',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_two_series(test_data_cum_10d_xts, 
                var_series_10d_95$his_var_series_10d_95,
                var_series_10d_99$his_var_series_10d_99,
                file_name = '_10d_historical_var.png', 
                title = 'Period Lookback with Rolling 10-Day Historical VaR',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

# GARCH(1,1)
plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_95$garch_11std_10d_var_series_95,
                file_name = '_10d_95_garch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR (95% Confidence) - GARCH(1,1) with Student t Distribution',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_99$garch_11std_10d_var_series_99,
                file_name = '_10d_99_garch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR (99% Confidence) - GARCH(1,1) with Student t Distribution',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_two_series(test_data_cum_10d_xts, 
                var_series_10d_95$garch_11std_10d_var_series_95,
                var_series_10d_99$garch_11std_10d_var_series_99,
                file_name = '_10d_garch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR - GARCH(1,1) with Student t Distribution ',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

## Plotting eGARCH Results ##

# eGARCH(1,1)
plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_95$egarch_11std_10d_var_series_95,
                file_name = '_10d_95_egarch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR (95% Confidence) - eGARCH(1,1) with Student t Distribution',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_99$egarch_11std_10d_var_series_99,
                file_name = '_10d_99_egarch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR (99% Confidence) - eGARCH(1,1) with Student t Distribution',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_two_series(test_data_cum_10d_xts, 
                var_series_10d_95$egarch_11std_10d_var_series_95,
                var_series_10d_99$egarch_11std_10d_var_series_99,
                file_name = '_10d_egarch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR - eGARCH(1,1) with Student t Distribution ',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

## Plotting tGARCH ###

# tGARCH(1,1)
plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_95$tgarch_11std_10d_var_series_95,
                file_name = '_10d_95_tgarch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR (95% Confidence) - tGARCH(1,1) with Student t Distribution',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_one_series(test_data_cum_10d_xts, 
                var_series_10d_99$tgarch_11std_10d_var_series_99,
                file_name = '_10d_99_tgarch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR (99% Confidence) - tGARCH(1,1) with Student t Distribution',
                legend_names = c("10-Day Returns", "VaR", "Exceptions"))

plot_two_series(test_data_cum_10d_xts, 
                var_series_10d_95$tgarch_11std_10d_var_series_95,
                var_series_10d_99$tgarch_11std_10d_var_series_99,
                file_name = '_10d_tgarch_11std_var.png', 
                title = 'Period Lookback with Rolling 10-Day VaR - tGARCH(1,1) with Student t Distribution ',
                legend_names = c("Daily Returns", "95% VaR", "99% VaR", "Exceptions"))

## Plotting some combinations ###
plot_four_series(test_data_cum_10d_xts,
                 var_series_10d_95$his_var_series_10d_95,
                 var_series_10d_95$garch_11std_10d_var_series_95,
                 var_series_10d_95$egarch_11std_10d_var_series_95,
                 var_series_10d_95$tgarch_11std_10d_var_series_95,
                 file_name = '_10d_11_95std_var.png', 
                 title = 'Period Lookback with Rolling 10-Day VaR (95% Confidence)',
                 legend_names = c("Daily Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"))

plot_four_series(test_data_cum_10d_xts,
                 var_series_10d_99$his_var_series_10d_99,
                 var_series_10d_99$garch_11std_10d_var_series_99,
                 var_series_10d_99$egarch_11std_10d_var_series_99,
                 var_series_10d_99$tgarch_11std_10d_var_series_99,
                 file_name = '_10d_11_99std_var.png', 
                 title = 'Period Lookback with Rolling 10-Day VaR (99% Confidence)',
                 legend_names = c("Daily Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"))

#################################################
### Forecast plots #####

plot_forecast_values(test_data_1d_all, 
                     arima_forecast_1d_series,
                     file_name = '_arima_1d_forecast.png', 
                     title = 'Period Lookback with Rolling 1-Day Forecast with ARIMA',
                     legend_names = c("Daily Returns", "Forecast"))

plot_forecast_values(test_data_1d_all, 
                     garch_forecast_11_std_1d_series,
                     file_name = '_1d_garch_11std_forecast.png', 
                     title = 'Period Lookback with Rolling 1-Day Forecast with GARCH(1,1) with Student t Distribution',
                     legend_names = c("Daily Returns", "Forecast"))

plot_forecast_values(test_data_1d_all, 
                     egarch_forecast_11_std_1d_series,
                     file_name = '_1d_egarch_11std_forecast.png', 
                     title = 'Period Lookback with Rolling 1-Day Forecast with eGARCH(1,1) with Student t Distribution',
                     legend_names = c("Daily Returns", "Forecast"))

plot_forecast_values(test_data_1d_all, 
                     tgarch_forecast_11_std_1d_series,
                     file_name = '_1d_tgarch_11std_forecast.png', 
                     title = 'Period Lookback with Rolling 1-Day Forecast with tGARCH(1,1) with Student t Distribution',
                     legend_names = c("Daily Returns", "Forecast"))

