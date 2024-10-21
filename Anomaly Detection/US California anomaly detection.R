library(tidyverse)
library(timetk)
library(anomalize)

# Read the earthquake data
earthquake_data <- read.csv(file = "/Users/WilsonWang/Desktop/Research/*Data earthquake/US California.csv", header = TRUE, sep = ",")

# Sort data
earthquake_data$Date <- as.Date(substr(earthquake_data$time, 1, 10), format = "%Y-%m-%d")
earthquake_data <- earthquake_data %>% arrange(Date)
earthquake_data$mag <- as.numeric(earthquake_data$mag)
earthquake_data <- earthquake_data %>% filter(mag >= 5.0)

# Create a time tibble with the Date column as index
earthquake_data_tbl <- earthquake_data %>%
  tk_tbl() %>%
  tk_augment_timeseries_signature() %>%
  select(Date, everything()) %>%
  mutate(Date = as.Date(Date))

# Decompose the time series using STL decomposition with a weekly period
earthquake_data_decomposed <- earthquake_data_tbl %>%
  time_decompose(mag, method = "stl", frequency = 7, trend = 14)

# Detect anomalies with adjusted alpha for outlier detection
earthquake_data_anomalized <- earthquake_data_decomposed %>%
  anomalize(remainder, alpha = 0.10)
earthquake_data_recomposed <- earthquake_data_anomalized %>%
  time_recompose()

# Plot anomalies with lines connecting the dots
earthquake_data_recomposed %>%
  plot_anomalies(time_recomposed = TRUE, color_no = 'navy', color_yes = 'red', fill_ribbon = 'gray', size_circles = 4) +
  geom_line(aes(x = Date, y = observed), color = 'blue', linewidth = 0.5) + # Connect dots with lines
  labs(title = "Anomalies in Earthquake Magnitudes (5.0 and above)",
       subtitle = "Time Period of Your Data",
       x = "Date",
       y = "Magnitude") +
  theme_minimal()
