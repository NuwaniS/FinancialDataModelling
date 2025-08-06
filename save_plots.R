# Use this file to save plots because the plots can't be generated inside loops in R.

library(xts)

# Create folder for plots 
plot_dir <- "plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Change the window name after running the script financial_model_2.R - 3 Months, 6 Months, 1 Year, 2 Year, Full, 
win_tag = 'Full'

# Store the plots in the folder
removeNA <- function(test_data, garch_var) {
  garch_var_xts <- xts(garch_var, order.by = index(test_data_1d_all))
  valid_idx <- which(!is.na(garch_var_xts))
  garch_var_xts_clean <- garch_var_xts[valid_idx]
  test_data_1d_clean <- test_data_1d_all[valid_idx]
  
  y_min <- min(min(test_data_1d_clean, na.rm = TRUE), min(garch_var_xts_clean, na.rm = TRUE))
  y_max <- max(max(test_data_1d_clean, na.rm = TRUE), max(garch_var_xts_clean, na.rm = TRUE))
  
  return (list(test_data_1d_clean = test_data_1d_clean, garch_var_xts_clean = garch_var_xts_clean,
               y_min = y_min, y_max = y_max))
}

png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_1d_all, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return") 
lines(his_var_xts_95, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)
dev.off()


png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_1d_all, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(his_var_xts_99, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

png(filename = file.path(plot_dir, paste0(win_name, '_1d_historical_var.png')), 
    width = 1000, height = 800)
plot(test_data_1d_all, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Historical VaR'), type = 'l', col = "black", ylab = "Daily Return")
lines(his_var_xts_99, col = "red", lwd = 2)
lines(his_var_xts_95, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(1,1)
clean_data = removeNA(test_data_1d_all, garch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_garch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, garch_11std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_garch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_11std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, garch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - GARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(1,2)
clean_data = removeNA(test_data_1d_all, garch_12std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_garch_12std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, garch_12std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_garch_12std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_12std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, garch_12std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_12std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - GARCH(1,2) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(2,1)
clean_data = removeNA(test_data_1d_all, garch_21std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_garch_21std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, garch_21std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_garch_21std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_21std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, garch_21std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_21std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - GARCH(2,1) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(2,2)
clean_data = removeNA(test_data_1d_all, garch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_garch_22std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, garch_22std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_garch_22std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_22std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, garch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_22std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - GARCH(2,2) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_11std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, garch_12std_var_series_99)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, garch_21std_var_series_99)
clean_data_4 <- removeNA(clean_data$test_data_1d_clean, garch_22std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_99std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$garch_var_xts_clean, col = "orange", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "GARCH(1,1)", "GARCH(1,2)", "GARCH(2,1)", "GARCH(2,2)"), 
       col = c("black", "red", "blue", "green", "orange"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_11std_var_series_95)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, garch_12std_var_series_95)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, garch_21std_var_series_95)
clean_data_4 <- removeNA(clean_data$test_data_1d_clean, garch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_95std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$garch_var_xts_clean, col = "orange", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "GARCH(1,1)", "GARCH(1,2)", "GARCH(2,1)", "GARCH(2,2)"), 
       col = c("black", "red", "blue", "green", "orange"), lty = 1, xpd = TRUE)

dev.off()

#######################################################
## Plotting eGARCH Results ##

# eGARCH(1,1)
clean_data = removeNA(test_data_1d_all, egarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_egarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, egarch_11std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_egarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_11std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - eGARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(1,2)
clean_data = removeNA(test_data_1d_all, egarch_12std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_egarch_12std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, egarch_12std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_egarch_12std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_12std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_12std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_12std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - eGARCH(1,2) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(2,1)
clean_data = removeNA(test_data_1d_all, egarch_21std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_egarch_21std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, egarch_21std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_egarch_21std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_21std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_21std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_21std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - eGARCH(2,1) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# GARCH(2,2)
clean_data = removeNA(test_data_1d_all, egarch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_egarch_22std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, egarch_22std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_egarch_22std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_22std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_22std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - eGARCH(2,2) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_11std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_12std_var_series_99)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, egarch_21std_var_series_99)
clean_data_4 <- removeNA(clean_data$test_data_1d_clean, egarch_22std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_99std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$garch_var_xts_clean, col = "orange", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "eGARCH(1,1)", "eGARCH(1,2)", "eGARCH(2,1)", "eGARCH(2,2)"), 
       col = c("black", "red", "blue", "green", "orange"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_11std_var_series_95)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_12std_var_series_95)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, egarch_21std_var_series_95)
clean_data_4 <- removeNA(clean_data$test_data_1d_clean, egarch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_95std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$garch_var_xts_clean, col = "orange", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "eGARCH(1,1)", "eGARCH(1,2)", "eGARCH(2,1)", "eGARCH(2,2)"), 
       col = c("black", "red", "blue", "green", "orange"), lty = 1, xpd = TRUE)

dev.off()

############################################################################
## Plotting tGARCH ###

# tGARCH(1,1)
clean_data = removeNA(test_data_1d_all, tgarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_tgarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, tgarch_11std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_tgarch_11std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_11std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, tgarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_11std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - tGARCH(1,1) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# tGARCH(1,2)
clean_data = removeNA(test_data_1d_all, tgarch_12std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_tgarch_12std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, tgarch_12std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_tgarch_12std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_12std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, tgarch_12std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_12std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - tGARCH(1,2) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# tGARCH(2,1)
clean_data = removeNA(test_data_1d_all, tgarch_21std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_tgarch_21std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, tgarch_21std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_tgarch_21std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_21std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, tgarch_21std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_21std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - tGARCH(2,1) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

# tGARCH(2,2)
clean_data = removeNA(test_data_1d_all, tgarch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_95_tgarch_22std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data = removeNA(test_data_1d_all, tgarch_22std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_99_tgarch_22std_var.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence)'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "VaR"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_22std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, tgarch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_22std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR - tGARCH(2,2) with Student t Distribution'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "99% VaR", "95% VaR"), col = c("black", "red", "blue"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_11std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, tgarch_12std_var_series_99)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, tgarch_21std_var_series_99)
clean_data_4 <- removeNA(clean_data$test_data_1d_clean, tgarch_22std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_99std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$garch_var_xts_clean, col = "orange", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "tGARCH(1,1)", "tGARCH(1,2)", "tGARCH(2,1)", "tGARCH(2,2)"), 
       col = c("black", "red", "blue", "green", "orange"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_11std_var_series_95)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, tgarch_12std_var_series_95)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, tgarch_21std_var_series_95)
clean_data_4 <- removeNA(clean_data$test_data_1d_clean, tgarch_22std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_95std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
lines(clean_data_4$garch_var_xts_clean, col = "orange", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "tGARCH(1,1)", "tGARCH(1,2)", "tGARCH(2,1)", "tGARCH(2,2)"), 
       col = c("black", "red", "blue", "green", "orange"), lty = 1, xpd = TRUE)

dev.off()

#########################################################
## Plotting some combinations ###

clean_data <- removeNA(test_data_1d_all, garch_11std_var_series_95)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_11std_var_series_95)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, tgarch_11std_var_series_95)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_11_95std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (95% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "GARCH(1,1)", "eGARCH(1,1)", "tGARCH(1,1)"), 
       col = c("black", "red", "blue", "green"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_11std_var_series_99)
clean_data_2 <- removeNA(clean_data$test_data_1d_clean, egarch_11std_var_series_99)
clean_data_3 <- removeNA(clean_data$test_data_1d_clean, tgarch_11std_var_series_99)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_11_99std_var.png')), 
    width = 1000, height = 1000)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day VaR (99% Confidence Interval'), 
     type = 'l', col = "black", ylab = "Daily Return", ylim = c(clean_data$y_min, clean_data$y_max))
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
lines(clean_data_2$garch_var_xts_clean, col = "blue", lwd = 2)
lines(clean_data_3$garch_var_xts_clean, col = "green", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "GARCH(1,1)", "eGARCH(1,1)", "tGARCH(1,1)"), 
       col = c("black", "red", "blue", "green"), lty = 1, xpd = TRUE)

dev.off()

#################################################
### Forecast plots #####

clean_data <- removeNA(test_data_1d_all, arima_forecast_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_arima_1d_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_forecast_11_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_11std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_forecast_12_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_12std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_forecast_21_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_21std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, garch_forecast_22_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_garch_22std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

## EGARCH models forecast ###

clean_data <- removeNA(test_data_1d_all, egarch_forecast_11_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_11std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_forecast_12_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_12std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_forecast_21_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_21std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, egarch_forecast_22_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_egarch_22std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

## tGARCH models forecast ####

clean_data <- removeNA(test_data_1d_all, tgarch_forecast_11_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_11std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_forecast_12_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_12std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_forecast_21_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_21std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

clean_data <- removeNA(test_data_1d_all, tgarch_forecast_22_std_1d_series)
png(filename = file.path(plot_dir, paste0(win_name, '_1d_tgarch_22std_forecast.png')), 
    width = 1000, height = 800)
plot(clean_data$test_data_1d_clean, main = paste(win_tag, 'Period Lookback with Rolling 1-Day Forecast'), type = 'l', col = "black", ylab = "Daily Return")
lines(clean_data$garch_var_xts_clean, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual Returns", "Forecast"), col = c("black", "red"), lty = 1, xpd = TRUE)

dev.off()

