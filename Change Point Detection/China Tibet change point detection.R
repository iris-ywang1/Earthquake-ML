# Load necessary libraries
library(changepoint)

# Load the earthquake data
earthquake_data <- read.csv("/Users/WilsonWang/Desktop/Research/*Data earthquake/China Tibet.csv")

# Convert the time column to Date type if it's not already
earthquake_data$time <- as.Date(earthquake_data$time)

# Filter the data for magnitudes above 5.0
filtered_data <- earthquake_data[earthquake_data$mag > 5.0, ]

# Perform change point detection for changes in mean
ansmean <- cpt.mean(filtered_data$mag, penalty = "AIC", method = "BinSeg", Q = 5)
plot(ansmean, cpt.col = "red", ylab = "Magnitude", main = "Change Point Detection for Change in Mean (Mag > 5.0)")
print(ansmean)

# Convert change points to dates
change_points_mean <- filtered_data$time[cpts(ansmean)]
print("Change points for mean (dates):")
print(change_points_mean)

# Perform change point detection for changes in variance
ansvar <- cpt.var(filtered_data$mag, penalty = "AIC", method = "BinSeg", Q = 3)
plot(ansvar, cpt.col = "red", ylab = "Magnitude", main = "Change Point Detection for Change in Variance (Mag > 5.0)")
print(ansvar)

# Convert change points to dates
change_points_var <- filtered_data$time[cpts(ansvar)]
print("Change points for variance (dates):")
print(change_points_var)

# Perform change point detection for changes in both mean and variance
ansmeanvar <- cpt.meanvar(filtered_data$mag, penalty = "AIC", method = "BinSeg", Q = 3)
plot(ansmeanvar, cpt.col = "red", ylab = "Magnitude", main = "Change Point Detection for Change in Mean and Variance (Mag > 5.0)")
print(ansmeanvar)

# Convert change points to dates
change_points_meanvar <- filtered_data$time[cpts(ansmeanvar)]
print("Change points for mean and variance (dates):")
print(change_points_meanvar)
