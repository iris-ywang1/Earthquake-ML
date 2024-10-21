library(ggplot2)

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

# Combine all data
combined_data <- do.call(rbind, all_data)

# Plotting with different lines for each file
ggplot(combined_data, aes(x = time, y = mag, color = File)) +
  geom_line() +
  labs(title="Earthquake Magnitudes 1980 to 2024", x="Time", y="Magnitude", color="File")
