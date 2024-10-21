# Load necessary libraries
library(ggplot2)

# Reading the data
eq.data <- read.csv(file="/Users/WilsonWang/Desktop/Research/*Data earthquake/US California.csv", 
                    header=TRUE, sep=",")

# Filtering data for magnitude greater than 4.0
eq.data <- eq.data[eq.data$mag > 4.0, ]

# Converting 'time' column to POSIXct and sorting the data by time
eq.data$time <- as.POSIXct(eq.data$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
eq.data <- eq.data[order(eq.data$time), ]

# Creating date-time variable
datetime <- eq.data$time

# Computing lag
datetime.lag <- c(0, head(datetime, -1))

# Computing interarrival times (in days)
int.time <- (as.numeric(datetime) - as.numeric(datetime.lag)) / (3600 * 24)
int.time <- int.time[-1]

# Filtering aftershocks
int <- int.time[int.time > 30]

print("Interarrival times after filtering immediate aftershocks:")
print(summary(int))

# Calculate bin width to divide the range into 7-8 bins
num_bins <- 8
range_int <- max(int) - min(int)
bin_width <- range_int / num_bins

# Calculate bin edges
bin_edges <- seq(min(int), max(int) + bin_width, by=bin_width)

# Plotting histogram for filtered interarrival times in days
hist(int, main="Histogram of Interarrival Times", col="dark magenta", xlab="Interarrival Time (days)", 
     breaks=bin_edges, freq=FALSE, xaxt='n')

# Customize x-axis ticks
axis(1, at=bin_edges, labels=round(bin_edges, 1))

# Add exponential distribution fit starting at 30 days
curve(dexp(x - 30, rate=1/mean(int - 30)), col="blue", lwd=2, add=TRUE, from=30)

# Goodness-of-fit test for exponential distribution
obs_freq <- hist(int, breaks=bin_edges, plot=FALSE)$counts  # Observed frequencies

# Expected frequencies
lambda <- 1 / mean(int - 30)
exp_freq <- length(int) * diff(pexp(bin_edges - 30, rate=lambda))

# Chi-squared test
chi_sq_stat <- sum((obs_freq - exp_freq)^2 / exp_freq)
p_value <- 1 - pchisq(chi_sq_stat, df=length(obs_freq)-1)

cat("Chi-squared statistic:", chi_sq_stat, "\n")
cat("P-value:", p_value, "\n")
