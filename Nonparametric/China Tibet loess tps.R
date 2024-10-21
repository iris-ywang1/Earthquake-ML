# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate) # For handling date-time data
library(fields)    # For Thin Plate Spline
library(caret)     # For cross-validation

# Load the earthquake data
earthquake_data <- read_csv("/Users/WilsonWang/Desktop/Research/*Data earthquake/China Tibet.csv")

# Convert 'time' to POSIXct class
earthquake_data <- earthquake_data %>%
  mutate(time = ymd_hms(time))

# Remove rows with NA, NaN, or Inf in 'time' or 'mag'
earthquake_data <- earthquake_data %>%
  filter(!is.na(time) & !is.na(mag) & !is.nan(mag) & is.finite(mag))

# Group by month and year, and get the maximum magnitude for each month
monthly_max <- earthquake_data %>%
  mutate(year_month = floor_date(time, "month")) %>%
  group_by(year_month) %>%
  summarize(max_mag = max(mag, na.rm = TRUE)) %>%
  ungroup()

# Convert 'year_month' to numeric for LOESS and TPS
monthly_max <- monthly_max %>%
  mutate(time_numeric = as.numeric(year_month))

# Define a function to calculate the cross-validation error for a given span
cv_loess <- function(span) {
  fit <- loess(max_mag ~ time_numeric, data = monthly_max, span = span)
  pred <- predict(fit, newdata = monthly_max)
  mean((monthly_max$max_mag - pred)^2, na.rm = TRUE)
}

# Print the span
span <- 0.05
print(paste("Best Span for LOESS:", span))

# Fit a LOESS model using the best span
loess_model <- loess(max_mag ~ time_numeric, data = monthly_max, span = span)

# Create a sequence for numeric time values
time_seq_numeric <- seq(min(monthly_max$time_numeric), max(monthly_max$time_numeric), length.out = 100)

# Predict values using LOESS and get confidence intervals
predicted_loess <- predict(loess_model, newdata = data.frame(time_numeric = time_seq_numeric), se = TRUE)

# Create data frame for LOESS predictions
predicted_loess_data <- data.frame(
  time = as.POSIXct(time_seq_numeric, origin = "1970-01-01"), 
  max_mag = predicted_loess$fit,
  lower = predicted_loess$fit - 1.96 * predicted_loess$se.fit,
  upper = predicted_loess$fit + 1.96 * predicted_loess$se.fit
)

# LOESS plot
loess_plot <- ggplot() +
  geom_point(data = monthly_max, aes(x = year_month, y = max_mag), alpha = 0.5) +
  geom_line(data = predicted_loess_data, aes(x = time, y = max_mag), color = "blue") +
  geom_ribbon(data = predicted_loess_data, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(title = "LOESS Fit with 95% Confidence Band",
       x = "Time",
       y = "Maximum Magnitude")

# --- Thin Plate Spline Model ---

# Find the lambda with the minimum GCV score
lambda <- 0.000005
print(paste("Best Lambda for TPS:", lambda))

# Fit a Thin Plate Spline model using the best lambda
tps_model <- Tps(monthly_max$time_numeric, monthly_max$max_mag, lambda = lambda)

# Print TPS smoothing parameter
print(paste("TPS Lambda:", tps_model$lambda))

# Predict values using TPS
predicted_tps <- predict(tps_model, time_seq_numeric)
predicted_tps_se <- predictSE(tps_model, time_seq_numeric)

# Create data frame for TPS predictions
predicted_tps_data <- data.frame(
  time = as.POSIXct(time_seq_numeric, origin = "1970-01-01"),
  max_mag = predicted_tps,
  lower = predicted_tps - 1.96 * predicted_tps_se,
  upper = predicted_tps + 1.96 * predicted_tps_se
)

# TPS plot
tps_plot <- ggplot() +
  geom_point(data = monthly_max, aes(x = year_month, y = max_mag), alpha = 0.5) +
  geom_line(data = predicted_tps_data, aes(x = time, y = max_mag), color = "red") +
  geom_ribbon(data = predicted_tps_data, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
  labs(title = "Thin Plate Spline Fit with 95% Confidence Band",
       x = "Time",
       y = "Maximum Magnitude")

# Display the plots
print(loess_plot)
print(tps_plot)
