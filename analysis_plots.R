library(xts)
library(readxl)
library(dplyr)

# Create folder for plots 
plot_dir <- "plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Read the Excel sheet
df <- read.csv("results_20250803.csv", stringsAsFactors = FALSE)

# View first few rows
head(df)

# Convert the first column to Date
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

plot_different_lookback_windows <- function(file_name, title, data_3m, data_6m, data_1y, data_2y, data_full, y_lab) {
  
  y_min <- min(min(data_3m[2], na.rm = TRUE), min(data_6m[2], na.rm = TRUE), min(data_1y[2], na.rm = TRUE), min(data_2y[2], na.rm = TRUE), min(data_full[2], na.rm = TRUE))
  y_max <- max(max(data_3m[2], na.rm = TRUE), max(data_6m[2], na.rm = TRUE), max(data_1y[2], na.rm = TRUE), max(data_2y[2], na.rm = TRUE), max(data_full[2], na.rm = TRUE))
  
  png(filename = file.path(plot_dir, file_name), width = 1000, height = 800)
  plot(data_3m[[1]], data_3m[[2]], main = title, type = 'l', col = "black", lwd = 2,
       ylim = c(y_min, y_max), xlab = 'Test Date', ylab = y_lab) 
  lines(data_6m[[1]], data_6m[[2]], col = "red", lwd = 2)
  lines(data_1y[[1]], data_1y[[2]], col = "green", lwd = 2)
  lines(data_2y[[1]], data_2y[[2]], col = "orange", lwd = 2)
  lines(data_full[[1]], data_full[[2]], col = "blue", lwd = 2)
  legend("bottomright", legend = c("3 months", "6 months", "1 year", "2 year", "full"), col = c("black", "red", "green", "orange", "blue"), lty = 1, xpd = TRUE)
  dev.off()
}

# Create new data frames for plotting
arima_aic_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, AIC)
arima_aic_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, AIC)
arima_aic_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, AIC)
arima_aic_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, AIC)
arima_aic_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, AIC)
plot_different_lookback_windows('arima_aic.png', 'ARIMA - AIC Values', arima_aic_3m, arima_aic_6m, arima_aic_1y,
                                arima_aic_2y, arima_aic_full, 'AIC')

arima_bic_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, BIC)
arima_bic_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, BIC)
arima_bic_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, BIC)
arima_bic_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, BIC)
arima_bic_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, BIC)
plot_different_lookback_windows('arima_bic.png', 'ARIMA - BIC Values', arima_bic_3m, arima_bic_6m, arima_bic_1y,
                                arima_bic_2y, arima_bic_full, 'BIC')

arima_1d_mae_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, ARIMA_1d_MAE)
arima_1d_mae_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, ARIMA_1d_MAE)
arima_1d_mae_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, ARIMA_1d_MAE)
arima_1d_mae_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, ARIMA_1d_MAE)
arima_1d_mae_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, ARIMA_1d_MAE)
plot_different_lookback_windows('arima_1d_mae.png', 'ARIMA - 1d Forecast MAE Values', arima_1d_mae_3m, arima_1d_mae_6m, arima_1d_mae_1y,
                                arima_1d_mae_2y, arima_1d_mae_full, 'MAE')

arima_1d_mape_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, ARIMA_1d_MAPE)
arima_1d_mape_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, ARIMA_1d_MAPE)
arima_1d_mape_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, ARIMA_1d_MAPE)
arima_1d_mape_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, ARIMA_1d_MAPE)
arima_1d_mape_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, ARIMA_1d_MAPE)
plot_different_lookback_windows('arima_1d_mape.png', 'ARIMA - 1d Forecast MAPE Values', 
                                arima_1d_mape_3m, arima_1d_mape_6m, arima_1d_mape_1y,
                                arima_1d_mape_2y, arima_1d_mape_full, 'MAPE')

arima_1d_rmse_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, ARIMA_1d_RMSE)
arima_1d_rmse_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, ARIMA_1d_RMSE)
arima_1d_rmse_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, ARIMA_1d_RMSE)
arima_1d_rmse_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, ARIMA_1d_RMSE)
arima_1d_rmse_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, ARIMA_1d_RMSE)
plot_different_lookback_windows('arima_1d_rmse.png', 'ARIMA - 1d Forecast RMSE Values', 
                                arima_1d_rmse_3m, arima_1d_rmse_6m, arima_1d_rmse_1y,
                                arima_1d_rmse_2y, arima_1d_rmse_full, 'RMSE')

arima_10d_mae_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, ARIMA_10d_MAE)
arima_10d_mae_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, ARIMA_10d_MAE)
arima_10d_mae_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, ARIMA_10d_MAE)
arima_10d_mae_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, ARIMA_10d_MAE)
arima_10d_mae_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, ARIMA_10d_MAE)
plot_different_lookback_windows('arima_10d_mae.png', 'ARIMA - 10d Forecast MAE Values', arima_10d_mae_3m, arima_10d_mae_6m, arima_10d_mae_1y,
                                arima_10d_mae_2y, arima_10d_mae_full, 'MAE')

arima_10d_mape_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, ARIMA_10d_MAPE)
arima_10d_mape_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, ARIMA_10d_MAPE)
arima_10d_mape_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, ARIMA_10d_MAPE)
arima_10d_mape_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, ARIMA_10d_MAPE)
arima_10d_mape_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, ARIMA_10d_MAPE)
plot_different_lookback_windows('arima_10d_mape.png', 'ARIMA - 10d Forecast MAPE Values', 
                                arima_10d_mape_3m, arima_10d_mape_6m, arima_10d_mape_1y,
                                arima_10d_mape_2y, arima_10d_mape_full, 'MAPE')

arima_10d_rmse_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, ARIMA_10d_RMSE)
arima_10d_rmse_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, ARIMA_10d_RMSE)
arima_10d_rmse_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, ARIMA_10d_RMSE)
arima_10d_rmse_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, ARIMA_10d_RMSE)
arima_10d_rmse_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, ARIMA_10d_RMSE)
plot_different_lookback_windows('arima_10d_rmse.png', 'ARIMA - 10d Forecast RMSE Values', 
                                arima_10d_rmse_3m, arima_10d_rmse_6m, arima_10d_rmse_1y,
                                arima_10d_rmse_2y, arima_10d_rmse_full, 'RMSE')

############GARCH(1,1) Plots ######################

garch_11_aic_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_AIC)
garch_11_aic_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_AIC)
garch_11_aic_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_AIC)
garch_11_aic_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_AIC)
garch_11_aic_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_AIC)
plot_different_lookback_windows('garch_11_aic.png', 'GARCH(1,1) - AIC Values', garch_11_aic_3m, 
                                garch_11_aic_6m, garch_11_aic_1y, garch_11_aic_2y, garch_11_aic_full, 'AIC')

garch_11_bic_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_BIC)
garch_11_bic_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_BIC)
garch_11_bic_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_BIC)
garch_11_bic_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_BIC)
garch_11_bic_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_BIC)
plot_different_lookback_windows('garch_11_bic.png', 'GARCH(1,1) - BIC Values', garch_11_bic_3m, 
                                garch_11_bic_6m, garch_11_bic_1y, garch_11_bic_2y, garch_11_bic_full, 'BIC')

garch_11_log_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_LIKE)
garch_11_log_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_LIKE)
garch_11_log_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_LIKE)
garch_11_log_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_LIKE)
garch_11_log_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_LIKE)
plot_different_lookback_windows('garch_11_log.png', 'GARCH(1,1) - Log Likelihood Values', garch_11_log_3m, 
                                garch_11_log_6m, garch_11_log_1y, garch_11_log_2y, garch_11_log_full, 'Log Likelihood')

garch_11_1d_mae_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_1d_MAE)
garch_11_1d_mae_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_1d_MAE)
garch_11_1d_mae_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_1d_MAE)
garch_11_1d_mae_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_1d_MAE)
garch_11_1d_mae_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_1d_MAE)
plot_different_lookback_windows('garch_11_1d_mae.png', 'GARCH(1,1) - 1d Forecast MAE Values', garch_11_1d_mae_3m, garch_11_1d_mae_6m, garch_11_1d_mae_1y,
                                garch_11_1d_mae_2y, garch_11_1d_mae_full, 'MAE')

garch_11_1d_mape_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_1d_MAPE)
garch_11_1d_mape_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_1d_MAPE)
garch_11_1d_mape_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_1d_MAPE)
garch_11_1d_mape_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_1d_MAPE)
garch_11_1d_mape_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_1d_MAPE)
plot_different_lookback_windows('garch_11_1d_mape.png', 'GARCH(1,1) - 1d Forecast MAPE Values', 
                                garch_11_1d_mape_3m, garch_11_1d_mape_6m, garch_11_1d_mape_1y,
                                garch_11_1d_mape_2y, garch_11_1d_mape_full, 'MAPE')

garch_11_1d_rmse_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_1d_RMSE)
garch_11_1d_rmse_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_1d_RMSE)
garch_11_1d_rmse_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_1d_RMSE)
garch_11_1d_rmse_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_1d_RMSE)
garch_11_1d_rmse_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_1d_RMSE)
plot_different_lookback_windows('garch_11_1d_rmse.png', 'GARCH(1,1) - 1d Forecast RMSE Values', 
                                garch_11_1d_rmse_3m, garch_11_1d_rmse_6m, garch_11_1d_rmse_1y,
                                garch_11_1d_rmse_2y, garch_11_1d_rmse_full, 'RMSE')

garch_11_10d_mae_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_10d_MAE)
garch_11_10d_mae_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_10d_MAE)
garch_11_10d_mae_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_10d_MAE)
garch_11_10d_mae_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_10d_MAE)
garch_11_10d_mae_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_10d_MAE)
plot_different_lookback_windows('garch_11_10d_mae.png', 'GARCH(1,1) - 10d Forecast MAE Values', 
                                garch_11_10d_mae_3m, garch_11_10d_mae_6m, garch_11_10d_mae_1y,
                                garch_11_10d_mae_2y, garch_11_10d_mae_full, 'MAE')

garch_11_10d_mape_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_10d_MAPE)
garch_11_10d_mape_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_10d_MAPE)
garch_11_10d_mape_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_10d_MAPE)
garch_11_10d_mape_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_10d_MAPE)
garch_11_10d_mape_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_10d_MAPE)
plot_different_lookback_windows('garch_11_10d_mape.png', 'GARCH(1,1) - 10d Forecast MAPE Values', 
                                garch_11_10d_mape_3m, garch_11_10d_mape_6m, garch_11_10d_mape_1y,
                                garch_11_10d_mape_2y, garch_11_10d_mape_full, 'MAPE')

garch_11_10d_rmse_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, GARCH_11_STD_10d_RMSE)
garch_11_10d_rmse_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, GARCH_11_STD_10d_RMSE)
garch_11_10d_rmse_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, GARCH_11_STD_10d_RMSE)
garch_11_10d_rmse_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, GARCH_11_STD_10d_RMSE)
garch_11_10d_rmse_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, GARCH_11_STD_10d_RMSE)
plot_different_lookback_windows('garch_11_10d_rmse.png', 'GARCH(1,1) - 10d Forecast RMSE Values', 
                                garch_11_10d_rmse_3m, garch_11_10d_rmse_6m, garch_11_10d_rmse_1y,
                                garch_11_10d_rmse_2y, garch_11_10d_rmse_full, 'RMSE')

############eGARCH(1,1) Plots ######################

egarch_11_aic_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_AIC)
egarch_11_aic_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_AIC)
egarch_11_aic_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_AIC)
egarch_11_aic_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_AIC)
egarch_11_aic_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_AIC)
plot_different_lookback_windows('egarch_11_aic.png', 'eGARCH(1,1) - AIC Values', egarch_11_aic_3m, 
                                egarch_11_aic_6m, egarch_11_aic_1y, egarch_11_aic_2y, egarch_11_aic_full, 'AIC')

egarch_11_bic_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_BIC)
egarch_11_bic_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_BIC)
egarch_11_bic_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_BIC)
egarch_11_bic_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_BIC)
egarch_11_bic_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_BIC)
plot_different_lookback_windows('egarch_11_bic.png', 'eGARCH(1,1) - BIC Values', egarch_11_bic_3m, 
                                egarch_11_bic_6m, egarch_11_bic_1y, egarch_11_bic_2y, egarch_11_bic_full, 'BIC')

egarch_11_log_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_LIKE)
egarch_11_log_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_LIKE)
egarch_11_log_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_LIKE)
egarch_11_log_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_LIKE)
egarch_11_log_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_LIKE)
plot_different_lookback_windows('egarch_11_log.png', 'eGARCH(1,1) - Log Likelihood Values', 
                                egarch_11_log_3m, egarch_11_log_6m, egarch_11_log_1y, egarch_11_log_2y, 
                                egarch_11_log_full, 'Log Likelihood')

egarch_11_1d_mae_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_1d_MAE)
egarch_11_1d_mae_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_1d_MAE)
egarch_11_1d_mae_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_1d_MAE)
egarch_11_1d_mae_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_1d_MAE)
egarch_11_1d_mae_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_1d_MAE)
plot_different_lookback_windows('egarch_11_1d_mae.png', 'eGARCH(1,1) - 1d Forecast MAE Values', 
                                egarch_11_1d_mae_3m, egarch_11_1d_mae_6m, egarch_11_1d_mae_1y,
                                egarch_11_1d_mae_2y, egarch_11_1d_mae_full, 'MAE')

egarch_11_1d_mape_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_1d_MAPE)
egarch_11_1d_mape_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_1d_MAPE)
egarch_11_1d_mape_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_1d_MAPE)
egarch_11_1d_mape_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_1d_MAPE)
egarch_11_1d_mape_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_1d_MAPE)
plot_different_lookback_windows('egarch_11_1d_mape.png', 'eGARCH(1,1) - 1d Forecast MAPE Values', 
                                egarch_11_1d_mape_3m, egarch_11_1d_mape_6m, egarch_11_1d_mape_1y,
                                egarch_11_1d_mape_2y, egarch_11_1d_mape_full, 'MAPE')

egarch_11_1d_rmse_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_1d_RMSE)
egarch_11_1d_rmse_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_1d_RMSE)
egarch_11_1d_rmse_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_1d_RMSE)
egarch_11_1d_rmse_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_1d_RMSE)
egarch_11_1d_rmse_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_1d_RMSE)
plot_different_lookback_windows('egarch_11_1d_rmse.png', 'eGARCH(1,1) - 1d Forecast RMSE Values', 
                                egarch_11_1d_rmse_3m, egarch_11_1d_rmse_6m, egarch_11_1d_rmse_1y,
                                egarch_11_1d_rmse_2y, egarch_11_1d_rmse_full, 'RMSE')

egarch_11_10d_mae_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_10d_MAE)
egarch_11_10d_mae_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_10d_MAE)
egarch_11_10d_mae_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_10d_MAE)
egarch_11_10d_mae_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_10d_MAE)
egarch_11_10d_mae_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_10d_MAE)
plot_different_lookback_windows('egarch_11_10d_mae.png', 'eGARCH(1,1) - 10d Forecast MAE Values', 
                                egarch_11_10d_mae_3m, egarch_11_10d_mae_6m, egarch_11_10d_mae_1y,
                                egarch_11_10d_mae_2y, egarch_11_10d_mae_full, 'MAE')

egarch_11_10d_mape_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_10d_MAPE)
egarch_11_10d_mape_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_10d_MAPE)
egarch_11_10d_mape_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_10d_MAPE)
egarch_11_10d_mape_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_10d_MAPE)
egarch_11_10d_mape_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_10d_MAPE)
plot_different_lookback_windows('egarch_11_10d_mape.png', 'eGARCH(1,1) - 10d Forecast MAPE Values', 
                                egarch_11_10d_mape_3m, egarch_11_10d_mape_6m, egarch_11_10d_mape_1y,
                                egarch_11_10d_mape_2y, egarch_11_10d_mape_full, 'MAPE')

egarch_11_10d_rmse_3m <- df %>% filter(LookBackWin == '3_months') %>% select(Date, eGARCH_11_STD_10d_RMSE)
egarch_11_10d_rmse_6m <- df %>% filter(LookBackWin == '6_months') %>% select(Date, eGARCH_11_STD_10d_RMSE)
egarch_11_10d_rmse_1y <- df %>% filter(LookBackWin == '1_year') %>% select(Date, eGARCH_11_STD_10d_RMSE)
egarch_11_10d_rmse_2y <- df %>% filter(LookBackWin == '2_year') %>% select(Date, eGARCH_11_STD_10d_RMSE)
egarch_11_10d_rmse_full <- df %>% filter(LookBackWin == 'full') %>% select(Date, eGARCH_11_STD_10d_RMSE)
plot_different_lookback_windows('egarch_11_10d_rmse.png', 'eGARCH(1,1) - 10d Forecast RMSE Values', 
                                egarch_11_1d_rmse_3m, egarch_11_1d_rmse_6m, egarch_11_1d_rmse_1y,
                                egarch_11_1d_rmse_2y, egarch_11_1d_rmse_full, 'RMSE')
