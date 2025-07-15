if (!require("tseries")) install.packages("tseries")
if (!require("nortest")) install.packages("nortest")
if (!require("FinTS")) install.packages("FinTS")
if (!require("Metrics")) install.packages("Metrics")

library(tseries)
library(nortest)
library(forecast)
library(FinTS)
library(Metrics)
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
  hist(model_data, breaks = 50, probability = TRUE, main=paste("Density of ",model_name, " vs Normal Distribution"))
  
  # Plot the density of the time series data
  lines(density(model_data), lwd=2)
  
  # Add a normal distribution for comparison
  curve(dnorm(x, mean = data_mean, sd = data_sd), col = "orange", lwd = 2, add = TRUE)
  
  # Add legend
  legend("topright", legend=c("Empirical", "Normal"), col=c("black", "orange"), lwd=2)
  
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
  hist(model_data, breaks = 50, main = "Histogram", xlab = "Residuals")
  
  # QQ-plot
  qqnorm(model_data, main = "QQ-Plot")
  qqline(model_data)
  
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
  
  return(list(mae = mae(actual_values, forecast_values), 
              rmse = rmse(actual_values, forecast_values), 
              mape = mape(actual_values, forecast_values)))
}

# Plot the histogram with zooming
plot_zoomed_hist <- function(model_data, model_name, tail_range = 3) {
  
  # Change the values of min and x_max to get the required range
  x_min <- mean(model_data) - tail_range * sd(model_data)
  x_max <- mean(model_data) + tail_range * sd(model_data)
  x_max <- -0.015
  x_min <- -0.05
  
  hist(model_data, breaks = 50, probability = TRUE, xlim = c(x_min, x_max), 
       main=paste("Density of ",model_name, " vs Normal Distribution"))
  
  # Plot the density of the time series data
  lines(density(model_data), lwd=2)
  
  # Add a normal distribution for comparison
  curve(dnorm(x, mean = mean(model_data), sd = sd(model_data)), col = "orange", lwd = 2, add = TRUE)
  
  # Add legend
  legend("topright", legend=c("Empirical", "Normal"), col=c("black", "orange"), lwd=2)
}
