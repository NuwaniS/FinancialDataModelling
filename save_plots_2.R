# Use this file to save plots because the plots can't be generated inside loops in R.

library(xts)

# Create folder for plots 
plot_dir <- "plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Change the window name after running the script financial_model_2.R - 3 Months, 6 Months, 1 Year, 2 Year, Full, 
win_tag = '3 Months'

removeNA <- function(test_data, var_series) {
  var_xts <- xts(var_series, order.by = index(test_data))
  valid_idx <- which(!is.na(var_xts))
  var_xts_clean <- var_xts[valid_idx]
  test_data_clean <- test_data[valid_idx]
  
  exceptions <- which(test_data_clean < var_xts_clean)
  exceptions_index <- index(test_data_clean)[exceptions]
  exceptions_data <- coredata(test_data_clean)[exceptions]
  exceptions_xts <- xts(exceptions_data, order.by = exceptions_index)
  
  y_min <- min(min(test_data_clean, na.rm = TRUE), min(var_xts_clean, na.rm = TRUE)) - 0.005
  y_max <- max(max(test_data_clean, na.rm = TRUE), max(var_xts_clean, na.rm = TRUE)) + 0.005
  
  return (list(test_data_clean = test_data_clean, var_xts_clean = var_xts_clean,
               y_min = y_min, y_max = y_max, exceptions = exceptions_xts))
}

#### 1d VaR Plots ####

clean_data <- removeNA(test_data_1d_all, var_series_1d_95$his_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_1d_all, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), 
     type = 'l', xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max), col = "blue", lwd = 1) 
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
points(clean_data$exceptions, col = "black", pch = 19, cex = 1)
legend("bottomright", legend = c("Daily Returns", "VaR", "Exceptions"), col = c("blue", "red", "black"), 
       lty = c(1, 1, NA), pch = c(NA, NA, 19), , xpd = TRUE)
dev.off()

clean_data <- removeNA(test_data_1d_all, var_series_1d_99$his_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_1d_all, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), 
     type = 'l', col = "blue", lwd = 1, xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
points(clean_data$exceptions, col = "black", pch = 19, cex = 1)
legend("bottomright", legend = c("Daily Returns", "VaR", "Exceptions"), col = c("blue", "red", "black"), 
       lty = c(1, 1, NA), pch = c(NA, NA, 19), xpd = TRUE)
dev.off()

clean_data <- removeNA(test_data_1d_all, var_series_1d_99$his_var_series_99)
clean_data_2 <- removeNA(test_data_1d_all, var_series_1d_95$his_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_1d_all, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Historical VaR'), 
     type = 'l', col = "blue", xlab = "Date", lwd = 1)
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "orange", lwd = 2)
points(clean_data$exceptions, col = "black", pch = 19, cex = 1)
points(clean_data_2$exceptions, col = "black", pch = 19, cex = 1)
legend("bottomright", legend = c("Daily Returns", "99% VaR", "95% VaR", "Exceptions"), 
       col = c("blue", "red", "orange", "black"), lty = c(1, 1, 1,NA), pch = c(NA, NA, NA, 19), xpd = TRUE)
dev.off()

# GARCH(1,1)
clean_data = removeNA(test_data_1d_all, var_series_1d_95$garch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_garch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, var_series_1d_99$garch_11std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_garch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, var_series_1d_99$garch_11std_var_series_99)
clean_data_2 <- removeNA(test_data_1d_all, var_series_1d_95$garch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - GARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

## Plotting eGARCH Results ##

# eGARCH(1,1)
clean_data = removeNA(test_data_1d_all, var_series_1d_95$egarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_egarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, var_series_1d_99$egarch_11std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_egarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, var_series_1d_99$egarch_11std_var_series_99)
clean_data_2 <- removeNA(test_data_1d_all, var_series_1d_95$egarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - eGARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

## Plotting tGARCH ###

# tGARCH(1,1)
clean_data = removeNA(test_data_1d_all, var_series_1d_95$tgarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_tgarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, var_series_1d_99$tgarch_11std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_tgarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, var_series_1d_99$tgarch_11std_var_series_99)
clean_data_2 <- removeNA(test_data_1d_all, var_series_1d_95$tgarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - tGARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

## Plotting some combinations ###

clean_data <- removeNA(test_data_1d_all, var_series_1d_95$garch_11std_var_series_95)
clean_data_2 <- removeNA(test_data_1d_all, var_series_1d_95$egarch_11std_var_series_95)
clean_data_3 <- removeNA(test_data_1d_all, var_series_1d_95$tgarch_11std_var_series_95)
clean_data_4 <- removeNA(test_data_1d_all, var_series_1d_95$his_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_11_95std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "orange", lwd = 2)
lines(clean_data_3$var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"), 
       col = c("black","blue", "red", "orange", "green"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, var_series_1d_99$garch_11std_var_series_99)
clean_data_2 <- removeNA(test_data_1d_all, var_series_1d_99$egarch_11std_var_series_99)
clean_data_3 <- removeNA(test_data_1d_all, var_series_1d_99$tgarch_11std_var_series_99)
clean_data_4 <- removeNA(test_data_1d_all, var_series_1d_99$his_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_11_99std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "orange", lwd = 2)
lines(clean_data_3$var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"), 
       col = c("black", "blue","red", "orange", "green"), lty = 1, xpd = TRUE)

dev.off()

#################################################
## 10d VaR Plots ##

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_95$his_var_series_10d_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_95_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_cum_10d_xts, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (95% Confidence)'), type = 'l', col = "black", xlab = "Date") 
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)
dev.off()

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_99$his_var_series_10d_99)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_99_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_cum_10d_xts, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (99% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_99$his_var_series_10d_99)
clean_data_2 <- removeNA(test_data_cum_10d_xts, var_series_10d_95$his_var_series_10d_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_cum_10d_xts, main = paste(win_tag, 'Period Lookback with Rolling 10-Day Historical VaR'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("10d Day Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(1,1)
clean_data = removeNA(test_data_cum_10d_xts, var_series_10d_95$garch_11std_10d_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_95_garch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (95% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_cum_10d_xts, var_series_10d_99$garch_11std_10d_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_99_garch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (99% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_99$garch_11std_10d_var_series_99)
clean_data_2 <- removeNA(test_data_cum_10d_xts, var_series_10d_95$garch_11std_10d_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_garch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR - GARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

## Plotting eGARCH Results ##

# eGARCH(1,1)
clean_data = removeNA(test_data_cum_10d_xts, var_series_10d_95$egarch_11std_10d_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_95_egarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (95% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_cum_10d_xts, var_series_10d_99$egarch_11std_10d_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_99_egarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (99% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_99$egarch_11std_10d_var_series_99)
clean_data_2 <- removeNA(test_data_cum_10d_xts, var_series_10d_95$egarch_11std_10d_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_egarch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR - eGARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

## Plotting tGARCH ###

# tGARCH(1,1)
clean_data = removeNA(test_data_cum_10d_xts, var_series_10d_95$tgarch_11std_10d_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_95_tgarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (95% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_cum_10d_xts, var_series_10d_99$tgarch_11std_10d_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_99_tgarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (99% Confidence)'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_99$tgarch_11std_10d_var_series_99)
clean_data_2 <- removeNA(test_data_cum_10d_xts, var_series_10d_95$tgarch_11std_10d_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_tgarch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR - tGARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

## Plotting some combinations ###

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_95$garch_11std_10d_var_series_95)
clean_data_2 <- removeNA(test_data_cum_10d_xts, var_series_10d_95$egarch_11std_10d_var_series_95)
clean_data_3 <- removeNA(test_data_cum_10d_xts, var_series_10d_95$tgarch_11std_10d_var_series_95)
clean_data_4 <- removeNA(test_data_cum_10d_xts, var_series_10d_95$his_var_series_10d_95)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_11_95std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (95% Confidence)'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "orange", lwd = 2)
lines(clean_data_3$var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"), 
       col = c("black","blue", "red", "orange", "green"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_cum_10d_xts, var_series_10d_99$garch_11std_10d_var_series_99)
clean_data_2 <- removeNA(test_data_cum_10d_xts, var_series_10d_99$egarch_11std_10d_var_series_99)
clean_data_3 <- removeNA(test_data_cum_10d_xts, var_series_10d_99$tgarch_11std_10d_var_series_99)
clean_data_4 <- removeNA(test_data_cum_10d_xts, var_series_10d_99$his_var_series_10d_99)
png(filename = file.path(plot_dir, paste0(win_name, '_10d_11_99std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 10-Day VaR (99% Confidence)'), 
     type = 'l', col = "black", xlab = "Date", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$var_xts_clean, col = "orange", lwd = 2)
lines(clean_data_3$var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("10 Day Returns", "Historical VaR", "GARCH(1,1) VaR", "eGARCH(1,1) VaR", "tGARCH(1,1) VaR"), 
       col = c("black", "blue","red", "orange", "green"), lty = 1, xpd = TRUE)

dev.off()

#################################################
### Forecast plots #####

clean_data <- removeNA(test_data_1d_all, arima_forecast_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_arima_1d_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_forecast_11_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_11std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

## EGARCH models forecast ###

clean_data <- removeNA(test_data_1d_all, egarch_forecast_11_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_11std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

## tGARCH models forecast ####

clean_data <- removeNA(test_data_1d_all, tgarch_forecast_11_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_11std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", xlab = "Date")
lines(clean_data$var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Daily Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

