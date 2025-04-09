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
results <- data.frame(
  iteration = integer(), model = character(),
  MAE_train = numeric(), MSE_train = numeric(), RMSE_train = numeric(),
  MAE_test = numeric(), MSE_test = numeric(), RMSE_test = numeric()
)
sarima_results <- data.frame(time = numeric(), actual = numeric(), predicted = numeric())

for (i in seq(from = train_window, to = (nrow(full_data) - test_window), by = test_window)) {
  # Define Training and Test Indices
  train_index <- (i - train_window):(i - 1)
  test_index <- i:(i + test_window - 1)
  
  train_window_data <- full_data[train_index, ]
  test_window_data <- full_data[test_index, ]
  
  exog_train_data <- full_data[train_index, c("precipitation_sum", "max_temp_mean", "min_temp_mean", "avg_temp_mean", "Mid.Rate_mean")]
  
  # Fit SARIMAX model
  sarimax_model <- auto.arima(train_window_data$price_usd_per_tonne_mean, xreg = as.matrix(exog_train_data), seasonal = TRUE)
  
  # Get fitted values for training performance
  fitted_train <- fitted(sarimax_model)
  
  # Training performance
  rmse_train <- rmse(train_window_data$price_usd_per_tonne_mean, fitted_train)
  mae_train <- mae(train_window_data$price_usd_per_tonne_mean, fitted_train)
  mse_train <- mse(train_window_data$price_usd_per_tonne_mean, fitted_train)
  
  # Fit and forecast each exogenous variable
  sarimax_precipitation <- auto.arima(train_window_data$precipitation_sum, seasonal = TRUE)
  sarimax_max_temp <- auto.arima(train_window_data$max_temp_mean, seasonal = TRUE)
  sarimax_min_temp <- auto.arima(train_window_data$min_temp_mean, seasonal = TRUE)
  sarimax_avg_temp <- auto.arima(train_window_data$avg_temp_mean, seasonal = TRUE)
  sarimax_rate <- auto.arima(train_window_data$Mid.Rate_mean, seasonal = TRUE)
  
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
  
  # Test performance
  rmse_test <- rmse(test_window_data$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mae_test <- mae(test_window_data$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mse_test <- mse(test_window_data$price_usd_per_tonne_mean, forecast_sarimax$mean)
  
  # Store results
  results <- rbind(results, data.frame(
    iteration = i,
    model = "SARIMAX",
    MAE_train = mae_train, MSE_train = mse_train, RMSE_train = rmse_train,
    MAE_test = mae_test, MSE_test = mse_test, RMSE_test = rmse_test
  ))
  
  sarima_results <- rbind(sarima_results, data.frame(
    time = test_index,
    actual = test_window_data$price_usd_per_tonne_mean,
    predicted = as.numeric(forecast_sarimax$mean)
  ))
}

# Summarise average performance across all iterations
summary_results <- results %>%
  group_by(model) %>%
  summarise(
    avg_MAE_train = mean(MAE_train), avg_MSE_train = mean(MSE_train), avg_RMSE_train = mean(RMSE_train),
    avg_MAE_test = mean(MAE_test), avg_MSE_test = mean(MSE_test), avg_RMSE_test = mean(RMSE_test)
  )

summary_results

sarimax_model$aic


# Create a sequence of dates starting from December 1997
start_date <- as.yearmon("1997-12")
time_labels <- seq(start_date, length.out = nrow(sarima_results), by = 1/12)

# Forecast v.s. actual Plot by SARIMA model
plot(time_labels, sarima_results$actual,
     type = "l", col = "black",
     xlab = "Time", ylab = "Price (USD per tonne)",
     main = "SARIMAX Forecast vs. Actual Prices"
)
lines(time_labels, sarima_results$predicted, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1, cex = 1)
