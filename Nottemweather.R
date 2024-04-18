# Load necessary libraries
library(ggplot2)
library(magrittr)

# Load dataset from GitHub
url <- "https://raw.githubusercontent.com/jamovi/r-datasets/master/data/nottem.csv"
nottem <- read.csv(url)

# Check the structure of the dataset
str(nottem)

# Create a sequence of time values
time <- seq(from = 1920, by = 1/12, length.out = nrow(nottem))

# Extract year from time
nottem$year <- as.integer(floor(nottem$time))

# Ensure each year has 12 months
months <- rep(1:12, nrow(nottem) / 12)
nottem$month <- months

# Plot the variation of temperatures for each month
temperature_variation_plot <- ggplot(nottem, aes(x = month, y = value, group = year, color = as.factor(year))) +
  geom_line(alpha = 0.5) +
  labs(title = "Variation of Temperatures in Fahrenheit (°F)",
       x = "Month",
       y = "Temperature",
       color = "Year") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12))

# Histogram of temperatures
histogram <- ggplot(data = nottem, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Monthly Average Temperatures",
       x = "Temperature",
       y = "Frequency") +
  theme_minimal()

# Density plot of temperatures
density_plot <- ggplot(data = nottem, aes(x = value)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Density Plot of Monthly Average Temperatures",
       x = "Temperature",
       y = "Density") +
  theme_minimal()

# Scatter plot of temperatures over time
scatter_plot <- ggplot(data = nottem, aes(x = time, y = value)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Monthly Average Temperatures",
       x = "Year",
       y = "Temperature") +
  theme_minimal()

# Display the plots individually
print(temperature_variation_plot)
print(histogram)
print(density_plot)
print(scatter_plot)

# Fit a linear trend
linear_model <- lm(nottem$value ~ time)

# Plot the monthly average temperatures
plot(nottem$time, nottem$value, type = "l", main = "Monthly Average Temperatures at Nottingham Castle", xlab = "Year", ylab = "Temperature")
# Add the fitted line
lines(nottem$time, fitted(linear_model), col = "green")
# Add a horizontal line representing the mean
abline(mean(nottem$value), 0, col = "blue")


# Fit a polynomial trend (2nd degree)
poly_model <- lm(nottem$value ~ poly(time, 2))

# Fit a cubic trend
cubic_model <- lm(nottem$value ~ poly(time, 3))

# Calculate AIC for linear model
linear_aic <- AIC(linear_model)

# Calculate AIC for polynomial model
poly_aic <- AIC(poly_model)

# Calculate AIC for cubic model
cubic_aic <- AIC(cubic_model)

# Print AIC values
cat("AIC for linear model:", linear_aic, "\n")
cat("AIC for polynomial model:", poly_aic, "\n")
cat("AIC for cubic model:", cubic_aic, "\n")

# Choose the trend model with the lowest AIC value
if (linear_aic <= poly_aic & linear_aic <= cubic_aic) {
  selected_trend <- linear_model
} else if (poly_aic <= cubic_aic) {
  selected_trend <- poly_model
} else {
  selected_trend <- cubic_model
}
# Calculate AIC for selected model
selected_aic <- AIC(selected_trend)

# Print AIC for selected model
cat("AIC for selected trend model:", selected_aic, "\n")

# Detrending the data for linear model
data_notrend_linear <- nottem$value - fitted(selected_trend)

# Plotting the detrended data for the linear model
plot(time, data_notrend_linear, type = "l", main = "Detrended Monthly Average Temperatures (Linear Model)", xlab = "Year", ylab = "Temperature")


# Fit seasonal component
seasonal_freq <- c(3, 6, 12)  # seasonal frequencies
aic_values <- numeric(length(seasonal_freq))
seasonal_models <- list()

for (freq in seasonal_freq) {
  seasonal_model <- lm(nottem$value ~ sin(2 * pi * time / freq) + cos(2 * pi * time / freq))
  seasonal_models[[as.character(freq)]] <- seasonal_model
  aic_values[which(seasonal_freq == freq)] <- AIC(seasonal_model)
}

best_seasonal_freq <- seasonal_freq[which.min(aic_values)]
best_seasonal_model <- seasonal_models[[as.character(best_seasonal_freq)]]

# Extracting the seasonal component
data_seasonal_component <- fitted(best_seasonal_model)

# Plotting the seasonal component
plot(time, data_seasonal_component, type = "l", main = "Seasonal Component of Monthly Average Temperatures", xlab = "Year", ylab = "Temperature")

# Calculate the residual by subtracting the seasonal component from the detrended data
residual <- data_notrend_linear - data_seasonal_component

# Plot the residual
plot(time, residual, type = "l", main = "Residual Plot", xlab = "Year", ylab = "Residuals")

stats::acf(residual)
stats::pacf(residual)


# Define the range of orders for ARIMA
max_order <- 3
orders <- expand.grid(p = 1:max_order, d = 1:max_order, q = 1:max_order)

# Fit ARIMA models for different orders
aic_values <- numeric(nrow(orders))
arima_models <- list()

for (i in 1:nrow(orders)) {
  order <- orders[i, ]
  arima_model <- arima(nottem$value, order = c(order$p, order$d, order$q))
  arima_models[[i]] <- arima_model
  aic_values[i] <- AIC(arima_model)
}

# Find the index of the model with the lowest AIC
best_model_index <- which.min(aic_values)

# Retrieve the best model and its corresponding AIC
best_arima_model <- arima_models[[best_model_index]]
best_aic <- aic_values[best_model_index]

# Print the best model and its AIC
cat("Best ARIMA model:", "\n")
print(best_arima_model)
cat("AIC for best ARIMA model:", best_aic, "\n")

# Check assumptions
# Normality assumption
shapiro.test(residuals(best_arima_model))

# Constant variance assumption
plot(residuals(best_arima_model), ylab = "Residuals", main ='Constant variance assessment')
abline(h = 0, col = "red")

# Independence assumption
acf_res <- acf(residuals(best_arima_model))
pacf_res <- pacf(residuals(best_arima_model))

# Plot ACF
plot(acf_res, main = "ACF of Residuals")

# Plot PACF
plot(pacf_res, main = "PACF of Residuals(best arima model)")




################################################
# Define the range of years to forecast
forecast_years <- c( 1938,1939, 1940, 1941, 1942)

# Create a sequence of time values for the forecast years
forecast_time <- seq(from = min(forecast_years), to = max(forecast_years), by = 1/12)

# Forecast the series manually using the ARIMA model for existing years
forecast_values <- numeric(length(forecast_time))
for (i in 1:length(forecast_time)) {
  forecast_values[i] <- predict(best_arima_model, n.ahead = i)$pred[i]
}

# Plot actual data
plot(time, nottem$value, type = "l", xlim = c(1920, max(time) + 2), ylim = c(min(nottem$value), max(nottem$value)), xlab = "Year", ylab = "Temperature")

# Manually add x-axis labels
axis(1, at = seq(1920, max(time) + 2 , by = 5), labels = seq(1920, max(time)+ 2, by = 5))

# Overlay forecast onto the plot
lines(forecast_time, forecast_values, col = "red", lwd = 2.5)




###########################################################
# Extract the actual values for the forecast years
actual_values <- nottem$value[time %in% forecast_years]

# Check for missing or NA values in actual_values and forecast_values
if (any(is.na(actual_values)) || any(is.na(forecast_values))) {
  stop("Actual or forecasted values contain missing or NA values.")
}

# Truncate or extend vectors to match their lengths
if (length(actual_values) > length(forecast_values)) {
  actual_values <- actual_values[1:length(forecast_values)]
} else if (length(actual_values) < length(forecast_values)) {
  forecast_values <- forecast_values[1:length(actual_values)]
}

# Now both vectors have the same length, so you can subtract them without encountering a warning
accuracy <- actual_values - forecast_values

# Compute the mean absolute percentage error (MAPE) or any other accuracy metric
mape <- mean(abs(accuracy / actual_values) * 100)

# Check for division by zero in MAPE calculation
if (any(actual_values == 0)) {
  warning("Division by zero encountered in MAPE calculation.")
}

# Calculate the squared differences between actual and forecasted values
squared_errors <- (actual_values - forecast_values)^2

# Calculate the mean squared error
mse <- mean(squared_errors)

# Calculate the root mean squared error
rmse <- sqrt(mse)

cat("Mean Absolute Percentage Error (MAPE):", mape, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

