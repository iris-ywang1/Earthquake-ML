library(readr)
library(fitdistrplus)
library(dplyr)
library(extRemes)

# Define the density, distribution, quantile, and random generation functions for the Gumbel distribution
dgumbel <- function(x, location, scale) {
  z <- (x - location) / scale
  exp(-(z + exp(-z))) / scale
}
pgumbel <- function(q, location, scale) {
  exp(-exp(-(q - location) / scale))
}
qgumbel <- function(p, location, scale) {
  location - scale * log(-log(p))
}
rgumbel <- function(n, location, scale) {
  location - scale * log(-log(runif(n)))
}

# Read earthquake data
earthquake_data <- read_csv("/Users/WilsonWang/Desktop/Research/*Data earthquake/Southern Chile.csv")

# Plot histogram of earthquake magnitudes
hist(as.numeric(earthquake_data$mag),
     main = "Histogram of Earthquake Magnitudes",
     xlab = "Magnitude",
     col = "skyblue",
     border = "white")

# Extract year from the time column
earthquake_data <- mutate(earthquake_data, year = format(time, "%Y"))

# Calculate maximum magnitudes by year
max_quakes <- earthquake_data %>%
  group_by(year) %>%
  summarise(max_magnitude = max(mag, na.rm = TRUE))

# Plot histogram of maximum magnitudes
hist(as.numeric(max_quakes$max_magnitude),
     main = "Histogram of Maximum Earthquake Magnitudes by Year",
     xlab = "Maximum Magnitude",
     col = "skyblue",
     border = "white")

# Scaling maxima according to EVT
n <- nrow(max_quakes)
# Calculate an and bn
a_n <- qnorm(1 - 1 / n)
b_n <- 1 / a_n
normalized_max <- (max_quakes$max_magnitude-a_n) / b_n
max_quakes$normalized_max <- normalized_max
summary(max_quakes$normalized_max)
qqnorm(max_quakes$normalized_max) # Q-Q plot to visualize the fit to the Gumbel distribution
qqline(max_quakes$normalized_max)

# Fit Gumbel distribution using Maximum Likelihood Estimation (MLE) with fitdist
fit_gumbel_mle <- fitdist(normalized_max, "gumbel", 
                          start = list(location = mean(normalized_max), scale = sd(normalized_max)), 
                          method = "mle")
print("Maximum Likelihood Estimates:")
print(fit_gumbel_mle)

# Numerical Optimization for MLE estimation using custom log-likelihood function
log_likelihood_gumbel <- function(params, data) {
  alpha <- params[1]
  beta <- params[2]
  if (beta <= 0) return(Inf)
  n <- length(data)
  ll <- -n * log(beta) - sum((data - alpha) / beta) - sum(exp(-(data - alpha) / beta))
  return(-ll)  # Return negative log-likelihood for minimization
}

start_params <- c(mean(normalized_max), sd(normalized_max))
fit_gumbel_numerical <- optim(start_params, log_likelihood_gumbel, data = normalized_max)
numerical_estimates <- fit_gumbel_numerical$par
names(numerical_estimates) <- c("location", "scale")
print("Numerical Optimization Estimates:")
print(numerical_estimates)

# Plotting EVA results for Gumbel distribution
gumbel_fit <- fevd(normalized_max, type = "Gumbel")
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(gumbel_fit, type = "density", main = "Density Plot")
plot(gumbel_fit, type = "qq", main = "Q-Q Plot")
plot(gumbel_fit, type = "rl", main = "Return Level Plot")

# Asymptotic confidence intervals for Gumbel parameters using fit_gumbel_mle
alpha <- 0.05
params <- coef(fit_gumbel_mle)
vcov_matrix <- fit_gumbel_mle$vcov
z_value <- qnorm(1 - alpha / 2)  # Critical value from normal distribution
se_params <- sqrt(diag(vcov_matrix))  # Standard errors of parameters
conf_int_gumbel <- data.frame(
  Parameter = names(params),
  Estimate = params,
  Lower = params - z_value * se_params,
  Upper = params + z_value * se_params
)
print("Asymptotic Confidence Intervals:")
print(conf_int_gumbel)

# 95% confidence interval for the mean
location <- fit_gumbel_mle$estimate["location"]
scale <- fit_gumbel_mle$estimate["scale"]
gumbel_mean <- location + scale * 0.57721
se_mean <- sqrt((scale^2 * pi^2) / 6) / sqrt(nrow(max_quakes))
ci_mean_lower <- gumbel_mean - z_value * se_mean
ci_mean_upper <- gumbel_mean + z_value * se_mean
ci_mean <- data.frame(
  Mean = gumbel_mean,
  Lower = ci_mean_lower,
  Upper = ci_mean_upper
)
print("95% Confidence Interval for the Mean:")
print(ci_mean)

# Plot histogram of the normalized maxima overlaid with the fitted Gumbel density
x_vals <- seq(min(normalized_max), max(normalized_max), length.out = 100)
gumbel_density <- dgumbel(x_vals, location, scale)
hist(normalized_max, breaks = 20, freq = FALSE, 
     main = "Histogram of Normalized Maximum Earthquake Magnitudes\nwith Fitted Gumbel Density",
     xlab = "Normalized Maximum Magnitude",
     col = "skyblue", border = "white")
lines(x_vals, gumbel_density, col = "red", lwd = 2)

# Chi-squared goodness-of-fit test
num_bins <- 12
bin_edges <- seq(min(normalized_max), max(normalized_max), length.out = num_bins + 1)
observed_frequencies <- hist(normalized_max, breaks = bin_edges, plot = FALSE)$counts
expected_frequencies <- numeric(length(observed_frequencies))
for (i in 1:length(observed_frequencies)) {
  expected_frequencies[i] <- length(normalized_max) * (pgumbel(bin_edges[i + 1], location, scale) - pgumbel(bin_edges[i], location, scale))
}
chi_squared_stat <- sum((observed_frequencies - expected_frequencies)^2 / expected_frequencies)
df <- num_bins - 1 - 2
p_value <- pchisq(chi_squared_stat, df, lower.tail = FALSE)
cat("Chi-Squared Statistic:", chi_squared_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n")
if (p_value > 0.05) {
  cat("Failed to reject the null hypothesis. The data follows the Gumbel distribution.\n")
} else {
  cat("Rejected the null hypothesis. The data does not follow the Gumbel distribution.\n")
}