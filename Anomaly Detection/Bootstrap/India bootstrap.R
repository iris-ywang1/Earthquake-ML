library(tidyverse)
library(timetk)
library(anomalize)
library(boot)

# Read the earthquake data
earthquake_data <- read.csv(file = "/Users/WilsonWang/Desktop/Research/*Data earthquake/India.csv", header = TRUE, sep = ",")

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
  anomalize(remainder, alpha = 0.05)
earthquake_data_recomposed <- earthquake_data_anomalized %>%
  time_recompose()

# Extract anomalies
anomalies <- earthquake_data_anomalized %>%
  filter(anomaly == 'Yes') %>%
  select(Date, observed) %>%
  arrange(Date) %>%
  drop_na(observed)

# Calculate the number of days between consecutive anomalies, merging those less than 30 days apart
adjusted_anomalies <- anomalies %>%
  mutate(days_between = c(NA, diff(Date)))
adjusted_dates <- adjusted_anomalies$Date[1]
adjusted_severities <- adjusted_anomalies$observed[1]
for (i in 2:nrow(adjusted_anomalies)) {
  days_diff <- as.numeric(difftime(adjusted_anomalies$Date[i], tail(adjusted_dates, 1), units = "days"))
  if (days_diff >= 30) {
    adjusted_dates <- c(adjusted_dates, adjusted_anomalies$Date[i])
    adjusted_severities <- c(adjusted_severities, adjusted_anomalies$observed[i])
  } else {
    adjusted_severities[length(adjusted_severities)] <- max(adjusted_severities[length(adjusted_severities)], adjusted_anomalies$observed[i])
  }
}

# Create a new dataframe with adjusted dates and severities
adjusted_anomalies <- data.frame(Date = adjusted_dates, observed = adjusted_severities)
adjusted_anomalies <- adjusted_anomalies %>%
  mutate(days_between = c(NA, diff(Date)))

# Extract the last observation and calculate the correct days between for it
last_observation <- tail(adjusted_anomalies, 1)
second_last_observation <- tail(adjusted_anomalies, 2)[1,]
last_days_between <- as.numeric(difftime(last_observation$Date, second_last_observation$Date, units = "days"))
last_severity <- last_observation$observed

# Remove the last observation
adjusted_anomalies <- head(adjusted_anomalies, -1)

# Check distribution of anomalies
print(paste("Number of anomalies detected:", nrow(adjusted_anomalies)+1))
print("Summary of anomaly magnitudes:")
print(summary(adjusted_anomalies$observed))

# Print the frequency mean and median
frequency_mean <- mean(adjusted_anomalies$days_between, na.rm = TRUE)
frequency_median <- median(adjusted_anomalies$days_between, na.rm = TRUE)
print(paste("Frequency mean:", frequency_mean))
print(paste("Frequency median:", frequency_median))

# Bootstrap Sampling Function
calc_stats <- function(data, indices) {
  sample_data <- data[indices, ]
  n <- nrow(sample_data)
  if (n < 2) {
    frequency_mean <- NA
    frequency_median <- NA
  } else {
    frequency_mean <- mean(sample_data$days_between, na.rm = TRUE)
    frequency_median <- median(sample_data$days_between, na.rm = TRUE)
  }
  severity_mean <- if (n == 0) NA else mean(sample_data$observed, na.rm = TRUE)
  severity_median <- if (n == 0) NA else median(sample_data$observed, na.rm = TRUE)
  return(c(frequency_mean, frequency_median, severity_mean, severity_median))
}

# Perform bootstrap sampling
set.seed(123)
bootstrap_results <- boot(data = adjusted_anomalies, statistic = calc_stats, R = 1000)

# Get the Efron confidence intervals for frequency mean
bootstrap_ci_freq_mean <- boot.ci(bootstrap_results, type = "perc", index = 1)
print("Efron confidence interval for frequency mean:")
print(bootstrap_ci_freq_mean)

# Get the Efron confidence intervals for frequency median
bootstrap_ci_freq_median <- boot.ci(bootstrap_results, type = "perc", index = 2)
print("Efron confidence interval for frequency median:")
print(bootstrap_ci_freq_median)

# Get the Efron confidence intervals for severity mean
bootstrap_ci_sev_mean <- boot.ci(bootstrap_results, type = "perc", index = 3)
print("Efron confidence interval for severity mean:")
print(bootstrap_ci_sev_mean)

# Get the Efron confidence intervals for severity median
bootstrap_ci_sev_median <- boot.ci(bootstrap_results, type = "perc", index = 4)
print("Efron confidence interval for severity median:")
print(bootstrap_ci_sev_median)

# Print the last observation details
print(paste("Last days_between:", last_days_between))
print(paste("Last severity:", last_severity))

# Check if the last observation falls within the confidence intervals
is_last_days_between_within_ci <- (last_days_between >= bootstrap_ci_freq_mean$percent[4] & last_days_between <= bootstrap_ci_freq_mean$percent[5]) |
  (last_days_between >= bootstrap_ci_freq_median$percent[4] & last_days_between <= bootstrap_ci_freq_median$percent[5])

is_last_severity_within_ci <- (last_severity >= bootstrap_ci_sev_mean$percent[4] & last_severity <= bootstrap_ci_sev_mean$percent[5]) |
  (last_severity >= bootstrap_ci_sev_median$percent[4] & last_severity <= bootstrap_ci_sev_median$percent[5])

print(paste("Is the last days_between within the confidence intervals?", is_last_days_between_within_ci))
print(paste("Is the last severity within the confidence intervals?", is_last_severity_within_ci))
