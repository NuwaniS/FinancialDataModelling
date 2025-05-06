if (!require("tseries")) install.packages("tseries")

library(tseries)

#This script contains a set of common function required for the project.
#
#1. plot_time_series_data
#2. stationarity_tests
#3. Independence test
#4. normality tests


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
  
  return(list(shapiro_p_value = shapiro_p, shapiro_decision = s_decision,
              jarque_p_value = jarque_p, jarque_decision = j_decision))
}