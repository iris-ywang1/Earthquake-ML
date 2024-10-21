library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load earthquake data
earthquake_data <- read_csv("/Users/WilsonWang/Desktop/Research/*Data earthquake/Taiwan.csv")
earthquake_data <- mutate(earthquake_data, time = as.Date(time))

# Extract year and month
earthquake_data <- mutate(earthquake_data, year_month = floor_date(time, "month"))

# Extract the maximum magnitude and its date for each month
monthly_maxima <- earthquake_data %>%
  group_by(year_month) %>%
  summarize(
    max_mag = max(mag, na.rm = TRUE),
    max_mag_date = time[which.max(mag)]
  ) %>%
  ungroup()

# Calculate time differences between consecutive monthly maximum magnitudes
monthly_maxima <- monthly_maxima %>%
  arrange(year_month) %>%
  mutate(time_diff = c(NA, as.numeric(diff(max_mag_date), units = "days")))
monthly_maxima <- monthly_maxima %>%
  filter(time_diff <= 62)

# Plot the histogram of time differences
ggplot(monthly_maxima, aes(x = time_diff)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Time Differences Between Consecutive Monthly Max Magnitudes",
    x = "Time Difference (days)",
    y = "Frequency"
  )

# Summary statistics and CIs for time differences (time_diff)
time_diff_summary <- summary(monthly_maxima$time_diff)
# Mode for time differences (time_diff)
time_diff_mode <- as.numeric(names(which.max(table(monthly_maxima$time_diff))))
# Empirical equi-tailed 95% confidence interval for time differences
time_diff_ci <- quantile(monthly_maxima$time_diff, c(0.025, 0.975))

# Summary statistics and CIs for maximum magnitudes (max_mag)
max_mag_summary <- summary(monthly_maxima$max_mag)
# Mode for maximum magnitudes (max_mag)
max_mag_mode <- as.numeric(names(which.max(table(monthly_maxima$max_mag))))
# Empirical equi-tailed 95% confidence interval for maximum magnitudes
max_mag_ci <- quantile(monthly_maxima$max_mag, c(0.025, 0.975))

cat("Summary Statistics for Time Differences:\n")
print(time_diff_summary)
cat("\nMode for Time Differences:\n")
print(time_diff_mode)
cat("\nEmpirical Equi-tailed 95% Confidence Interval for Time Differences:\n")
print(time_diff_ci)
cat("\n\nSummary Statistics for Maximum Magnitudes:\n")
print(max_mag_summary)
cat("\nMode for Maximum Magnitudes:\n")
print(max_mag_mode)
cat("\nEmpirical Equi-tailed 95% Confidence Interval for Maximum Magnitudes:\n")
print(max_mag_ci)

# Estimating the next significant earthquake
next_time_diff <- mean(monthly_maxima$time_diff, na.rm = TRUE)
last_max_mag_date <- max(monthly_maxima$max_mag_date, na.rm = TRUE)
next_earthquake_date <- last_max_mag_date + days(round(next_time_diff))

# Standard error of the mean for time differences
se_time_diff <- sd(monthly_maxima$time_diff, na.rm = TRUE) / sqrt(sum(!is.na(monthly_maxima$time_diff)))
next_time_diff_ci <- c(next_time_diff - 1.96 * se_time_diff, next_time_diff + 1.96 * se_time_diff)
next_earthquake_date_ci <- last_max_mag_date + days(round(next_time_diff_ci))

next_earthquake_mag <- mean(monthly_maxima$max_mag, na.rm = TRUE)
se_max_mag <- sd(monthly_maxima$max_mag, na.rm = TRUE) / sqrt(sum(!is.na(monthly_maxima$max_mag)))
next_earthquake_mag_ci <- c(next_earthquake_mag - 1.96 * se_max_mag, next_earthquake_mag + 1.96 * se_max_mag)

cat("\nEstimated Next Significant Earthquake Date:\n")
print(next_earthquake_date)
cat("95% CI for the date:\n")
print(next_earthquake_date_ci)

cat("\nEstimated Magnitude of Next Significant Earthquake:\n")
print(next_earthquake_mag)
cat("95% CI for the magnitude:\n")
print(next_earthquake_mag_ci)