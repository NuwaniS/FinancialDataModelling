if (!require("tseries")) install.packages("tseries")

library(tseries)

#This script contains a set of common function required for the project.
#
#1. plot_time_series_data
#2. stationarity_tests


# Define a function for the plots
plot_time_series_data <- function(model_data, model_name) {
  
  # Initial time series plot
  plot(model_data, type="l", main=paste("Simulated ",model_name, " Process"), ylab="Value", xlab="Time")
  # Auto correlation plot
  acf(model_data, main = paste("ACF of Simulated ",model_name, " Process"))
  # Partial Auto correlation plot
  pacf(model_data, main=paste("PACF for Simulated ",model_name, " Process"))
  
  # Compute mean and variance of the MA process
  data_mean <- mean(model_data)
  data_sd <- sd(model_data)
  
  # Add histogram of the time series data
  hist(model_data, breaks = 50, probability = TRUE, main=paste("Density of ",model_name, "Process vs Normal Distribution"))
  
  # Plot the density of the time series data
  lines(density(model_data), lwd=2)
  
  # Add a normal distribution for comparison
  curve(dnorm(x, mean = data_mean, sd = data_sd), col = "orange", lwd = 2, add = TRUE)
  
  # Add legend
  legend("topright", legend=c(model_name, "Normal"), col=c("black", "orange"), lwd=2)
}

# Define a function for stationarity tests
stationarity_tests <- function(model_data, model_name) {
  print(paste("Stationarity tests for ", model_name))
  
  adf_test <- adf.test(model_data)
  print(adf_test)
  if (adf_test$p.value < 0.05 ) {
    print("Reject H0: Series is Stationary")
  } else {
    print("Failed to reject H0: Series is likely not Stationary")
  }
  
  kpss_test <- kpss.test(model_data, null = 'Trend')
  print(kpss_test)
  if (kpss_test$p.value < 0.05 ) {
    print("Reject H0: Series is non Stationary around a deterministic Trend")
  } else {
    print("Failed to reject H0: Series is likely Stationary")
  }
  
  kpss_test <- kpss.test(model_data, null = 'Level')
  print(kpss_test)
  if (kpss_test$p.value < 0.05 ) {
    print("Reject H0: Series is non Stationary around a constant mean")
  } else {
    print("Failed to reject H0: Series is likely Stationary")
  }
  
  pp_test <- pp.test(model_data, type = "Z(alpha)")
  print(pp_test)
  if (pp_test$p.value < 0.05 ) {
    print("Reject H0: Series is Stationary")
  } else {
    print("Failed to reject H0: Series is likely not Stationary")
  }
  
  pp_test <- pp.test(model_data, type = "Z(t_alpha)")
  print(pp_test)
  if (pp_test$p.value < 0.05 ) {
    print("Reject H0: Series is Stationary")
  } else {
    print("Failed to reject H0: Series is likely not Stationary")
  }
}

# Define a function for independance tests
# Independence check should be done for the residuals, not the raw timeseries data

# Define a function for normality tests
# Normality check should be done for the residuals, not the raw timeseries data in ARIMA modelling.