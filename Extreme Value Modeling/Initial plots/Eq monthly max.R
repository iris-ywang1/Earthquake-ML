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

# Add a year and month column
combined_data$year_month <- format(combined_data$time, "%Y-%m")

# Calculate monthly maximum for each site
monthly_max_data <- combined_data %>%
  group_by(year_month, File) %>%
  summarise(max_mag = max(mag, na.rm = TRUE)) %>%
  ungroup()

# Plotting with a line for each file
ggplot(monthly_max_data, aes(x = as.Date(paste0(year_month, "-01")), y = max_mag, color = File, group = File)) + 
  geom_line() +
  labs(title="Monthly Maximum Earthquake Magnitudes",
       x="Date",
       y="Maximum Magnitude",
       color="Site")
