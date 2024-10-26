# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(forecast)

# Define the file path for the earthquake data
file_path <- '/Users/WilsonWang/Desktop/Research/*Data earthquake/Japan.csv'

# Read the earthquake data
earthquake_data <- read_csv(file_path)

# Filter for catastrophic earthquakes (magnitude >= 6.0)
catastrophic_earthquakes <- earthquake_data %>% filter(mag >= 6.0)

# Convert the time column to Date class
catastrophic_earthquakes$time <- as.Date(catastrophic_earthquakes$time)

# Plot observed data
plot(as.Date(catastrophic_earthquakes$time), catastrophic_earthquakes$mag, type="l", lwd=1.5,
     col="green", main="Catastrophic Earthquakes: Observed and Predicted", 
     ylim=c(min(catastrophic_earthquakes$mag) - 1, max(catastrophic_earthquakes$mag) + 1), 
     xlab="Date", ylab="Magnitude", xaxt="n", panel.first=grid())
points(as.Date(catastrophic_earthquakes$time), catastrophic_earthquakes$mag, pch=20, col="green") 
axis(1, at=as.Date(catastrophic_earthquakes$time), labels=format(as.Date(catastrophic_earthquakes$time), "%Y-%m-%d"))

# Function to fit model and plot
fit_and_plot <- function(data, order, col, label) {
  model <- arima(data, order = order)
  fitted_values <- data - residuals(model)
  lines(as.Date(catastrophic_earthquakes$time), fitted_values, lwd=1.5, col=col)
  points(as.Date(catastrophic_earthquakes$time), fitted_values, pch=20, col=col)
  return(model)
}

# Fit models
ar1_model <- fit_and_plot(catastrophic_earthquakes$mag, c(1,0,0), "blue", "AR(1)")
ma1_model <- fit_and_plot(catastrophic_earthquakes$mag, c(0,0,1), "red", "MA(1)")
arma11_model <- fit_and_plot(catastrophic_earthquakes$mag, c(1,0,1), "purple", "ARMA(1,1)")
arima111_model <- fit_and_plot(catastrophic_earthquakes$mag, c(1,1,1), "orange", "ARIMA(1,1,1)")
arima011_model <- fit_and_plot(catastrophic_earthquakes$mag, c(0,1,1), "brown", "ARIMA(0,1,1)")
arima110_model <- fit_and_plot(catastrophic_earthquakes$mag, c(1,1,0), "pink", "ARIMA(1,1,0)")

# Add legend
legend("bottom", legend = c("Observed", "AR(1) Predicted", "MA(1) Predicted", "ARMA(1,1) Predicted",
                            "ARIMA(1,1,1) Predicted", "ARIMA(0,1,1) Predicted", "ARIMA(1,1,0) Predicted"),
       col = c("green", "blue", "red", "purple", "orange", "brown", "pink"),
       lty = 1, lwd = 1.5, cex = 0.6)  # Adjust 'cex' to a smaller value for a smaller legend

# Compare models using AIC
model_aic <- data.frame(
  Model = c("AR(1)", "MA(1)", "ARMA(1,1)", "ARIMA(1,1,1)", "ARIMA(0,1,1)", "ARIMA(1,1,0)"),
  AIC = c(AIC(ar1_model), AIC(ma1_model), AIC(arma11_model), AIC(arima111_model), AIC(arima011_model), AIC(arima110_model))
)

print(model_aic)

# Select the best model (lowest AIC)
best_model <- switch(
  which.min(model_aic$AIC),
  ar1_model,
  ma1_model,
  arma11_model,
  arima111_model,
  arima011_model,
  arima110_model
)

# Convert the earthquake magnitude data to a time series object
catastrophic_ts <- ts(catastrophic_earthquakes$mag, frequency = 12)  # Adjust frequency based on your data's periodicity

# Refit the best model to the time series data
best_model <- arima(catastrophic_ts, order = c(1,1,1))  # Example: Adjust this if necessary based on the best model

# Forecast future values with an extended horizon
forecast_horizon <- 24  # Forecasting 24 time periods into the future
forecast_values <- forecast(best_model, h = forecast_horizon)

# Generate future dates for plotting
forecast_dates <- seq.Date(from = max(as.Date(catastrophic_earthquakes$time)), 
                           by = "month", length.out = forecast_horizon + 1)[-1]

# Plot observed data and forecasted values with confidence intervals
plot(as.Date(catastrophic_earthquakes$time), catastrophic_earthquakes$mag, type="l", lwd=1.5,
     col="green", xlab="Date", ylab="Magnitude", main="Catastrophic Earthquakes Forecast",
     ylim=c(min(catastrophic_earthquakes$mag, forecast_values$lower[,2]) - 1, 
            max(catastrophic_earthquakes$mag, forecast_values$upper[,2]) + 1))
points(as.Date(catastrophic_earthquakes$time), catastrophic_earthquakes$mag, pch=20, col="green")

# Plot forecasted values and confidence intervals
lines(forecast_dates, forecast_values$mean, col="blue", lwd=2)
lines(forecast_dates, forecast_values$lower[,2], col="blue", lty=2)
lines(forecast_dates, forecast_values$upper[,2], col="blue", lty=2)

# Add legend
legend("topright", legend=c("Observed", "Forecast", "95% CI"),
       col=c("green", "blue", "blue"), lty=c(1,1,2), lwd=c(1,2,1))
