if (!require("tseries")) install.packages("tseries")
if (!require("nortest")) install.packages("nortest")
if (!require("FinTS")) install.packages("FinTS")
if (!require("Metrics")) install.packages("Metrics")

library(tseries)
library(nortest)
library(forecast)
library(FinTS)
library(Metrics)
library(xts)
#This script contains a set of common function required for the project.
#
# 1. plot_time_series_data
# 2. stationarity_tests
# 3. Independence test
# 4. normality tests
# 5. calculate_arima_metrics
# 6. two sided ks test for ARIMA model data
# 7. ARCH-LM Test
# 8. forecast evaluation
# 9. Zoomed in histogram
# 10. calculate_garch_metrics
# 11. garch_var_test
# 12. mean_quantile_loss
# 13. GARCH 1d VaR
# 14. GARCH 10d VaR

# Define a function for the plots
plot_time_series_data <- function(model_data, model_name) {
  
  # Initial time series plot
  plot(model_data, type="l", main=model_name, ylab="Value", xlab="Time")
  # Auto correlation plot
  acf(model_data, main = paste("ACF of ",model_name))
  # Partial Auto correlation plot
  pacf(model_data, main=paste("PACF for ",model_name))
  
  # Compute mean and variance of the MA process
  data_mean <- mean(model_data)
  data_sd <- sd(model_data)
  
  # Add histogram of the time series data
  hist(model_data, breaks = 50, probability = TRUE, main=paste("Empirical vs Normal Distribution of ",model_name),
       xlab = "Log Returns",
       ylim = c(0, max(density(model_data)$y, dnorm(seq(min(model_data), max(model_data), length.out = 100), mean = data_mean, sd = data_sd))))
  
  # Plot the density of the time series data
  lines(density(model_data), col = "blue", lwd=2)
  
  # Add a normal distribution for comparison
  curve(dnorm(x, mean = data_mean, sd = data_sd), col = "red", lty="dotdash", lwd = 2, add = TRUE)
  
  # Add legend
  legend("topright", legend=c("Empirical", "Normal"), col=c("blue", "red"), lwd=2, lty=c(1,4))
  
  #Boxplot
  boxplot(model_data, main = "Boxplot of Adjusted Closing Prices", ylab = "Price")
}

# Define a function for stationarity tests
stationarity_tests <- function(model_data) {
  tryCatch(
    {
      adf_test <- adf.test(model_data)
      adf_p <- adf_test$p.value
      if (adf_p < 0.05 ) {
        adf_decision <- "Reject H0: Series is Stationary"
      } else {
        adf_decision <- "Failed to reject H0: Series is likely not Stationary"
      }
    },
    error = function(e) {
      adf_p <- -1
      adf_decision <- 'Test Failed'
    }
  )
  
  tryCatch(
    {
      kpss_test_trend <- kpss.test(model_data, null = 'Trend')
      kpss_trend_p <- kpss_test_trend$p.value
      if (kpss_trend_p < 0.05 ) {
        kpss_decision1 <- "Reject H0: Series is non Stationary around a deterministic Trend"
      } else {
        kpss_decision1 <- "Failed to reject H0: Series is likely Stationary"
      }
    },
    error = function(e) {
      kpss_trend_p <- -1
      kpss_decision1 <- 'Test Failed'
    }
  )
  
  tryCatch(
    {
      kpss_test_level <- kpss.test(model_data, null = 'Level')
      kpss_level_p <- kpss_test_level$p.value
      if (kpss_level_p < 0.05 ) {
        kpss_decision2 <- "Reject H0: Series is non Stationary around a constant mean"
      } else {
        kpss_decision2 <- "Failed to reject H0: Series is likely Stationary"
      }
    },
    error = function(e) {
      kpss_level_p <- -1
      kpss_decision2 <- 'Test Failed'
    }
  )
  
  tryCatch(
    {
      pp_test <- pp.test(model_data, type = "Z(alpha)")
      pp_p <- pp_test$p.value
      if (pp_p < 0.05 ) {
        pp_decision <- "Reject H0: Series is Stationary"
      } else {
        pp_decision <- "Failed to reject H0: Series is likely not Stationary"
      }
    },
    error = function(e) {
      pp_p <- -1
      pp_decision <- 'Test Failed'
    }
  )
  
  tryCatch(
    {
      pp_test_t <- pp.test(model_data, type = "Z(t_alpha)")
      pp_t_p <- pp_test_t$p.value
      if (pp_t_p < 0.05 ) {
        ppt_decision <- "Reject H0: Series is Stationary"
      } else {
        ppt_decision <- "Failed to reject H0: Series is likely not Stationary"
      }
    },
    error = function(e) {
      pp_t_p <- -1
      ppt_decision <- 'Test Failed'
    }
  )
  
  return(list(adf_pvalue = adf_p, adf_decision = adf_decision,
              kpss_trend_pvalue = kpss_trend_p, kpss_trend_decision = kpss_decision1,
              kpss_level_pvalue = kpss_level_p, kpss_level_decision = kpss_decision2,
              pp_pvalue = pp_p, pp_decision = pp_decision,
              ppt_pvalue = pp_t_p, ppt_decision = ppt_decision))
}

# Define a function for independence tests
# Independence check should be done for the residuals, not the raw time-series data
independence_test <- function(model_data) {
  tryCatch(
    {
      lb_test <- Box.test(model_data, type = "Ljung-Box", lag = 10)
      pval <- lb_test$p.value
      if (pval < 0.05) {
        decision <- "Reject H0: Data are not uncorrelated."
      } else {
        decision <- "Failed to reject H0: There is no significant auto correlation"
      }
    },
    error = function(e) {
      pval <- -1
      decision <- 'Test Failed'
    }
  )
  
  return(list(p_value = pval, decision = decision))
}

# Define a function for normality tests
# Normality check should be done for the residuals, not the raw timeseries data in ARIMA modelling.
normality_tests <- function(model_data) {
  tryCatch(
    {
      s_test <- shapiro.test(model_data[0:5000])
      shapiro_p <- s_test$p.value
      if (shapiro_p < 0.05) {
        s_decision <- "Reject H0: Data is not normally distributed."
      } else {
        s_decision <- "Failed to reject H0: Data is likely normally distributed"
      }
    },
    error = function(e) {
      shapiro_p <- -1
      s_decision <- 'Test Failed'
    }
  )
  
  tryCatch(
    {
      j_test <- jarque.bera.test(model_data)
      jarque_p <- j_test$p.value
      if (jarque_p < 0.05) {
        j_decision <- "Reject H0: Data is not normally distributed."
      } else {
        j_decision <- "Failed to reject H0: Data is likely normally distributed"
      }
    },
    error = function(e) {
      jarque_p <- -1
      j_decision <- 'Test Failed'
    }
  )
  
  #Kolmogorov-Smirnov (KS) Test
  tryCatch(
    {
      ks_test <- ks.test(model_data, 'pnorm')
      ks_p <- ks_test$p.value
      ks_stat <- ks_test$statistic
      if (ks_p < 0.05) {
        ks_decision <- "Reject H0: Data does not follow a normal distribution."
      } else {
        ks_decision <- "Failed to reject H0: Data is likely normally distributed"
      }
    },
    error = function(e) {
      ks_p <- -1
      ks_stat <- -1
      ks_decision <- 'Test Failed'
    }
  )
  
  # Histogram
  #hist(model_data, breaks = 50, main = "Histogram", xlab = "Residuals")
  
  # QQ-plot
  #qqnorm(model_data, main = "QQ-Plot")
  #qqline(model_data)
  
  return(list(shapiro_p_value = shapiro_p, shapiro_decision = s_decision,
              jarque_p_value = jarque_p, jarque_decision = j_decision, 
              ks_p_value = ks_p, ks_statistic = ks_stat, ks_decision = ks_decision))
}

calculate_arima_metrics <- function(arima_model) {
  aic <- AIC(arima_model)
  
  bic <- BIC(arima_model)
  
  rmse <- sqrt(mean(residuals(arima_model)^2))

  mae <- mean(abs(residuals(arima_model)))
  
  return(list(aic = aic, bic = bic, rmse= rmse, mae = mae))
}

# Two sided ks test to compare empirical distribution with simulated distribution
# emp_model is an xts object
ks_test_arima <- function(emp_model, fit_model) {
  # Set the number of simulations to match the original series length
  n <- length(emp_model)
  
  # Simulate data based on the manually fitted ARIMA model parameters
  set.seed(123)
  sim_model <- arima.sim(model = list(order = arimaorder(fit_model),
                                       ar = fit_model$coef[grep("ar", names(fit_model$coef))],
                                       ma = fit_model$coef[grep("ma", names(fit_model$coef))]), n = n)
  
  # Rescale the simulated series to match the empirical distribution of log_returns
  sim_model <- scale(sim_model) * sd(emp_model) + mean(emp_model)
  
  # Plot empirical distribution with rescaled simulated distributions for comparison
  hist(emp_model, breaks = 50, probability = TRUE, main = "Empirical vs. Rescaled Simulated Distributions of Log Returns",
       xlab = "Log Returns", col = rgb(0, 0, 1, 0.4))
  lines(density(sim_model), col = "red", lwd = 2)
  legend("topright", legend = c("Empirical", "Simulated (Auto ARIMA)"),
         col = c("blue", "red"), lty = 1, lwd = 2)
  
  ks_test <- ks.test(as.numeric(emp_model), sim_model)
  ks_p <- ks_test$p.value
  ks_stat <- ks_test$statistic
  if (ks_p < 0.05) {
    ks_decision <- "Reject H0: Two models do not follow the same distribution."
  } else {
    ks_decision <- "Failed to reject H0: Two models likely follow the same distribution"
  }
  
  return(list(ks_p_value = ks_p, ks_statistic = ks_stat, ks_decision = ks_decision))
}


arch_lm_test <- function(residuals) {
  
  arch_test <- ArchTest(residuals, lags = 1)  # test with lag 1
  arch_1_p <- arch_test$p.value
  if (arch_1_p < 0.05) {
    arch_1_decision <- "Reject H0: There is significant evidence of ARCH effects"
  } else {
    arch_1_decision <- "Failed to reject H0: No significant evidence of ARCH effects"
  }
  
  arch_test <- ArchTest(residuals, lags = 5)  # test with lag 5
  arch_5_p <- arch_test$p.value
  if (arch_5_p < 0.05) {
    arch_5_decision <- "Reject H0: There is significant evidence of ARCH effects"
  } else {
    arch_5_decision <- "Failed to reject H0: No significant evidence of ARCH effects"
  }
  
  arch_test <- ArchTest(residuals, lags = 10)  # test with lag 10
  arch_10_p <- arch_test$p.value
  if (arch_10_p < 0.05) {
    arch_10_decision <- "Reject H0: There is significant evidence of ARCH effects"
  } else {
    arch_10_decision <- "Failed to reject H0: No significant evidence of ARCH effects"
  }
  
  return(list(arch_1_p = arch_1_p, arch_1_decision = arch_1_decision,
              arch_5_p = arch_5_p, arch_5_decision = arch_5_decision,
              arch_10_p = arch_10_p, arch_10_decision = arch_10_decision))
}

forecast_evaluation <- function(actual_values, forecast_values) {
  
  mask <- complete.cases(actual_values, forecast_values)
  
  return(list(mae = mae(actual_values[mask], forecast_values[mask]), 
              rmse = rmse(actual_values[mask], forecast_values[mask]), 
              mape = mape(actual_values[mask], forecast_values[mask])))
}

forecast_evaluation_10d <- function(actual_values, forecast_values) {
  # actual_values and forecast_values are 2D lists
  
  # Remove rows with any NA (in either matrix)
  complete_rows <- !sapply(forecast_values, function(x) is.null(x) || all(is.na(x))) &
    !sapply(actual_values, function(x) is.null(x) || all(is.na(x)))
  
  # Subset matrices
  forecast_matrix_clean <- forecast_values[complete_rows]
  actual_matrix_clean   <- actual_values[complete_rows]
  
  forecast_matrix <- do.call(rbind, forecast_matrix_clean)
  actual_matrix   <- do.call(rbind, actual_matrix_clean)
  
  # Then flatten
  forecast_vec <- as.vector(forecast_matrix)
  actual_vec   <- as.vector(actual_matrix)
  
  return(list(mae = mae(actual_vec, forecast_vec), 
              rmse = rmse(actual_vec, forecast_vec), 
              mape = mape(actual_vec, forecast_vec)))
}

dummy_forecast_evaluation <- function() {
  
  return(list(mae = NA, rmse = NA, mape = NA))
}

# Plot the histogram with zooming
plot_zoomed_hist <- function(model_data, model_name, tail_range = 3) {
  
  # Change the values of min and x_max to get the required range
  x_min <- mean(model_data) - tail_range * sd(model_data)
  x_max <- mean(model_data) + tail_range * sd(model_data)
  x_min <- 0.015
  x_max <- 0.05
  
  hist(model_data, breaks = 50, probability = TRUE, xlim = c(x_min,x_max), ylim = c(0, 5), 
       main=paste("Empirical vs Normal Distributions of Log Returns (Right Tail)"),
       xlab = "Log Returns")
  
  # Plot the density of the time series data
  lines(density(model_data), col= "blue", lwd=2)
  
  # Add a normal distribution for comparison
  curve(dnorm(x, mean = mean(model_data), sd = sd(model_data)), col = "red", lwd = 2, add = TRUE)
  
  # Add legend
  legend("topright", legend=c("Empirical", "Normal"), col=c("blue", "red"), lwd=2)
}

calculate_garch_metrics <- function(garch_model, order, win_length) {
  
  if (!is.null(garch_model) && garch_model@fit$convergence == 0) {
    aic <- infocriteria(garch_model)[1]
    bic <- infocriteria(garch_model)[2]
    like <- likelihood(garch_model)
    n_like <- like / win_length          # Normalized log likelihood
    if (order == 1) {  # (1,1) model
      a_b <- coef(garch_model)['alpha1'] + coef(garch_model)['beta1']    
    } else if (order == 2) {  # (1,2) model
      a_b <- coef(garch_model)['alpha1'] + coef(garch_model)['beta1'] + coef(garch_model)['beta2']   
    } else if (order == 3) {  # (2,1) model
      a_b <- coef(garch_model)['alpha1'] + coef(garch_model)['alpha2'] + coef(garch_model)['beta1']   
    } else if (order == 4) {  # (2,2) model
      a_b <- coef(garch_model)['alpha1'] + coef(garch_model)['alpha2'] + coef(garch_model)['beta1'] + coef(garch_model)['beta2']   
    }
  } else {
    aic <- NA
    bic <- NA
    like <- NA
    a_b <- NA
    n_like <- NA
  }
  return(list(aic = aic, bic = bic, like = like, n_like=n_like, a_b = a_b))
}

garch_var_test <- function(calculated_var_series, test_data, confidence = 0.95) {
  garch_var_xts <- xts(calculated_var_series, order.by = index(test_data))
  valid_idx <- which(!is.na(garch_var_xts))
  garch_var_xts_clean <- garch_var_xts[valid_idx]
  test_data_clean <- test_data[valid_idx]
  metric_values <- rugarch::VaRTest(1-confidence, test_data_clean, garch_var_xts_clean, confidence)
  return (metric_values)
}

garch_var_test_all <- function(var_list, test_data, confidence = 0.95) {
  # Combine actuals and forecasts
  all_data <- c(list(actual = test_data), var_list)
  
  # Find complete cases
  keep_idx <- complete.cases(do.call(cbind, all_data))
  
  # Cleaned inputs
  actual_clean <- test_data[keep_idx]
  var_clean_list <- lapply(var_list, function(f) f[keep_idx])
  
  # Run VaRTest
  var_test_results <- lapply(var_clean_list, function(f) 
    rugarch::VaRTest(alpha = 1 - confidence, actual_clean, f, conf.level = confidence))
  
  return(var_test_results)
}

mean_quantile_loss_all <- function(calculated_var_series, test_data, alpha = 0.05) {
  # Combine actuals and forecasts
  all_data <- c(list(actual = test_data), calculated_var_series)
  
  # Find complete cases
  keep_idx <- complete.cases(do.call(cbind, all_data))
  
  # Cleaned inputs
  actual_clean <- test_data[keep_idx]
  var_clean_list <- lapply(calculated_var_series, function(f) f[keep_idx])
  
  q_loss_results <- sapply(var_clean_list, function(forecast) {
    mean_quantile_loss_nomask(forecast, actual_clean, alpha)
  })
  
  return (q_loss_results)
}

mean_quantile_loss_nomask <- function(calculated_var_series, test_data, alpha = 0.05) {
  
  error <- test_data - calculated_var_series
  loss <- ifelse(test_data < calculated_var_series, (1 - alpha) * abs(error), alpha * abs(error))
  return(mean(loss))
}

mean_quantile_loss <- function(calculated_var_series, test_data, alpha = 0.05) {
  mask <- complete.cases(test_data, calculated_var_series)
  
  error <- test_data[mask] - calculated_var_series[mask]
  loss <- ifelse(test_data[mask] < calculated_var_series[mask], (1 - alpha) * abs(error), alpha * abs(error))
  return(mean(loss))
}

garch_1d_10d_var <- function(garch_model, garch_forecast_1d, garch_forecast_10d) {
  # GARCH parameters
  alpha_95 <- 0.05            # for 95% confidence
  alpha_99 <- 0.01            # for 99% confidence
  
  params <- coef(garch_model)
  shape <- params["shape"]
  q_95 <- qdist("std", p = alpha_95, shape = shape)
  q_99 <- qdist("std", p = alpha_99, shape = shape)
  
  # 1d VaR
  mu <- fitted(garch_forecast_1d)[1]
  sigma <- sigma(garch_forecast_1d)[1]
  garch_var_95 <- mu + q_95 * sigma    # 95% confidence
  garch_var_99 <- mu + q_99 * sigma    # 99% confidence
  
  # 10d VaR
  mu <- fitted(garch_forecast_10d)[1]
  sigma <- sigma(garch_forecast_10d)[1]
  # Compute cumulative 10-day VaR assuming mean and variance are additive
  # Sum of mu and sum of variances
  mu_10d <- sum(mu)
  var_10d <- sum(sigma^2)
  sd_10d <- sqrt(var_10d)
  
  garch_var_10d_95 <- mu_10d + q_95 * sd_10d # 95% confidence
  garch_var_10d_99 <- mu_10d + q_99 * sd_10d # 99% confidence
  
  return(list(garch_var_95 = garch_var_95, garch_var_99 = garch_var_99, 
              garch_var_10d_95 = garch_var_10d_95, garch_var_10d_99 = garch_var_10d_99))
}

print_summary_model_metrics <- function(model_metrics) {
  # Use sapply to calculate the mean and median for each list
  summary_stats <- sapply(model_metrics, function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE)))
  
  # Print the summary statistics
  print(summary_stats)
}