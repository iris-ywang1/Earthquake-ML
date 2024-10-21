library(ggplot2)
library(dplyr)

csv_directory <- "/Users/WilsonWang/Desktop/Research/*Data earthquake"
csv_files <- list.files(path = csv_directory, pattern = "\\.csv$", full.names = TRUE)

# Empty list to store data
all_data <- list()

for (file in csv_files) {
  data <- read.csv(file)
  data$time <- as.POSIXct(data$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
  data$File <- basename(file)
  all_data[[basename(file)]] <- data
}
combined_data <- do.call(rbind, all_data)

# Add a year column
combined_data$year <- format(combined_data$time, "%Y")

# Calculate annual average magnitude for each site
annual_avg_data <- combined_data %>%
  group_by(year, File) %>%
  summarise(avg_mag = mean(mag)) %>%
  ungroup()

# Plotting with a line for each file
ggplot(annual_avg_data, aes(x = as.numeric(year), y = avg_mag, color = File, group = File)) + geom_line() +
  labs(title="Annual Average Earthquake Magnitudes",
       x="Year",
       y="Average Magnitude",
       color="Site")
