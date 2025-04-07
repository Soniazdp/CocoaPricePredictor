library(dplyr)
library(forecast)
library(tseries)
library(rugarch)
library(Metrics)
library(ggplot2)
library(xts)
library(zoo)

# Load datasets
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")

# Combine data for walk-forward validation
full_data <- rbind(train, test)

# Define window sizes
train_window <- 24 # 24 months (2 years)
test_window <- 4 # 1 month for testing

# Store performance & prediction metrics
results <- data.frame(iteration = integer(), model = character(), MAE = numeric(), MSE = numeric(), RMSE = numeric())
sarima_results <- data.frame(time = numeric(), actual = numeric(), predicted = numeric())

### Loop through the dataset with rolling training and test sets ###
for (i in seq(from = train_window, to = (nrow(full_data) - test_window), by = test_window)) {
  # Define Training and Test Indices
  train_index <- (i - train_window):(i - 1) # Training data is the previous 24 months (before the current test window)
  test_index <- i:(i + test_window - 1) # Test data is the current point (the 25th, 26th, etc.)

  train_window_data <- full_data[train_index, ]
  test_window_data <- full_data[test_index, ]

  exog_train_data <- full_data[train_index, c("precipitation_sum", "max_temp_mean", "min_temp_mean", "avg_temp_mean", "Mid.Rate_mean")]

  #### Fit SARIMAX Model ####
  sarimax_model <- auto.arima(train_window_data$price_usd_per_tonne_mean, xreg = as.matrix(exog_train_data), seasonal = TRUE)

  # Fit each SARIMAX model excluding its own value from xreg
  sarimax_precipitation <- auto.arima(train_window_data$precipitation_sum, seasonal = TRUE)
  sarimax_max_temp <- auto.arima(train_window_data$max_temp_mean, seasonal = TRUE)
  sarimax_min_temp <- auto.arima(train_window_data$min_temp_mean, seasonal = TRUE)
  sarimax_avg_temp <- auto.arima(train_window_data$avg_temp_mean, seasonal = TRUE)
  sarimax_rate <- auto.arima(train_window_data$Mid.Rate_mean, seasonal = TRUE)

  # Forecasting each variable using SARIMA models (excluding the target variable from xreg)
  forecast_sarimax_precipitation <- forecast(sarimax_precipitation, h = test_window)
  forecast_sarimax_max_temp <- forecast(sarimax_max_temp, h = test_window)
  forecast_sarimax_min_temp <- forecast(sarimax_min_temp, h = test_window)
  forecast_sarimax_avg_temp <- forecast(sarimax_avg_temp, h = test_window)
  forecast_sarimax_rate <- forecast(sarimax_rate, h = test_window)

  forecasted_exog_test <- data.frame(
    precipitation_sum = as.numeric(forecast_sarimax_precipitation$mean),
    max_temp_mean = as.numeric(forecast_sarimax_max_temp$mean),
    min_temp_mean = as.numeric(forecast_sarimax_min_temp$mean),
    avg_temp_mean = as.numeric(forecast_sarimax_avg_temp$mean),
    Mid.Rate_mean = as.numeric(forecast_sarimax_rate$mean)
  )

  forecast_sarimax <- forecast(sarimax_model, xreg = as.matrix(forecasted_exog_test), h = test_window)

  # SARIMAX performance
  rmse_sarimax <- rmse(test_window_data$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mae_sarimax <- mae(test_window_data$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mse_sarimax <- mse(test_window_data$price_usd_per_tonne_mean, forecast_sarimax$mean)

  # Store results
  results <- rbind(results, data.frame(iteration = i, model = "SARIMAX", MAE = mae_sarimax, MSE = mse_sarimax, RMSE = rmse_sarimax))
  sarima_results <- rbind(sarima_results, data.frame(
    time = test_index, # Time points for test set
    actual = test_window_data$price_usd_per_tonne_mean,
    predicted = as.numeric(forecast_sarimax$mean)
  ))
}

# Aggregate and compare models
summary_results <- results %>%
  group_by(model) %>%
  summarise(avg_MAE = mean(MAE), avg_MSE = mean(MSE), avg_RMSE = mean(RMSE))

summary_results


# Create a sequence of dates starting from December 1997
start_date <- as.yearmon("1997-12")
time_labels <- seq(start_date, length.out = nrow(sarima_results), by = 1/12)

# Forecast v.s. actual Plot by SARIMA model
plot(time_labels, sarima_results$actual,
  type = "l", col = "black",
  xlab = "Time", ylab = "Price (USD per tonne)",
  main = "Walk-Forward SARIMA Forecast vs. Actual Prices"
)
lines(time_labels, sarima_results$predicted, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1, cex = 1)
