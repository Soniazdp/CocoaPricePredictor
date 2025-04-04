library(dplyr)
library(forecast)
library(tseries) 
library(rugarch)
library(Metrics)
library(ggplot2)
library(xts)

# Load datasets
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")

# Combine data for walk-forward validation
full_data <- rbind(train, test)

# Define window sizes
train_window <- 24  # 24 months (2 years)
test_window <- 1    # 1 month for testing

# Store performance & prediction metrics
results <- data.frame(iteration = integer(), model = character(), MAE = numeric(), MSE = numeric(), RMSE = numeric())
sarima_results <- data.frame(time = numeric(), actual = numeric(), predicted = numeric())

# Initialize exogenous variables for prediction tracking
exog_train <- train %>% select(precipitation_sum, max_temp_mean, min_temp_mean, avg_temp_mean, Mid.Rate_mean)
exog_test <- test %>% select(precipitation_sum, max_temp_mean, min_temp_mean, avg_temp_mean, Mid.Rate_mean)

### Loop through the dataset with rolling training and test sets ###
for (i in seq(from = 1, to = (nrow(full_data) - test_window), by = test_window)) {
  
  # Define Training and Test Indices
  train_index <- (i - train_window):(i - 1)   # Training data is the previous 24 months (before the current test window)
  test_index <- i:(i + test_window - 1)       # Test data is the current point (the 25th, 26th, etc.)
  
  train <- full_data[train_index, ]
  test <- full_data[test_index, ]
  
  # Exogenous variables for training (observed values for training period)
  exog_train_subset <- exog_train[(i - train_window):(i - 1), ]
  # Exogenous variables for testing (true values for test period)
  exog_test_subset <- exog_test[i:(i + test_window - 1), ]
  
  #### Fit SARIMAX Model ####
  sarimax_model <- auto.arima(train$price_usd_per_tonne_mean, xreg = as.matrix(exog_train_subset), seasonal = TRUE)
  
  # Fit each SARIMAX model excluding its own value from xreg
  sarimax_precipitation <- auto.arima(train$precipitation_sum, seasonal = TRUE)
  sarimax_max_temp <- auto.arima(train$max_temp_mean, seasonal = TRUE)
  sarimax_min_temp <- auto.arima(train$min_temp_mean, seasonal = TRUE)
  sarimax_avg_temp <- auto.arima(train$avg_temp_mean, seasonal = TRUE)
  sarimax_rate <- auto.arima(train$Mid.Rate_mean, seasonal = TRUE)
  
  forecast_sarimax <- forecast(sarimax_model, xreg = as.matrix(exog_test_subset), h = test_window)
  
  # Forecasting each variable using SARIMA models (excluding the target variable from xreg)
  forecast_sarimax_precipitation <- forecast(sarimax_precipitation, h = test_window)
  forecast_sarimax_max_temp <- forecast(sarimax_max_temp, h = test_window)
  forecast_sarimax_min_temp <- forecast(sarimax_min_temp, h = test_window)
  forecast_sarimax_avg_temp <- forecast(sarimax_avg_temp, h = test_window)
  forecast_sarimax_rate <- forecast(sarimax_rate, h = test_window)
  
  # Update exog_train and exog_test with forecasts
  exog_train <- rbind(exog_train, data.frame(
    precipitation_sum = as.numeric(forecast_sarimax_precipitation$mean),
    max_temp_mean = as.numeric(forecast_sarimax_max_temp$mean),
    min_temp_mean = as.numeric(forecast_sarimax_min_temp$mean),
    avg_temp_mean = as.numeric(forecast_sarimax_avg_temp$mean),
    Mid.Rate_mean = as.numeric(forecast_sarimax_rate$mean)
  ))  
  
  exog_test <- rbind(exog_test, data.frame(
    precipitation_sum = as.numeric(forecast_sarimax_precipitation$mean),
    max_temp_mean = as.numeric(forecast_sarimax_max_temp$mean),
    min_temp_mean = as.numeric(forecast_sarimax_min_temp$mean),
    avg_temp_mean = as.numeric(forecast_sarimax_avg_temp$mean),
    Mid.Rate_mean = as.numeric(forecast_sarimax_rate$mean)
  ))
  
  # SARIMAX performance
  rmse_sarimax <- rmse(test$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mae_sarimax <- mae(test$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mse_sarimax <- mse(test$price_usd_per_tonne_mean, forecast_sarimax$mean)
  
  # Store results
  results <- rbind(results, data.frame(iteration = i, model = "SARIMAX", MAE = mae_sarimax, MSE = mse_sarimax, RMSE = rmse_sarimax))
  sarima_results <- rbind(sarima_results, data.frame(
    time = test_index,  # Time points for test set
    actual = test$price_usd_per_tonne_mean,
    predicted = as.numeric(forecast_sarimax$mean)  
  ))
}



# Aggregate and compare models
summary_results <- results %>%
  group_by(model) %>%
  summarise(avg_MAE = mean(MAE), avg_MSE = mean(MSE), avg_RMSE = mean(RMSE))

summary_results


# Forecast v.s. actual Plot by SARIMA model
plot(sarima_results$time, sarima_results$actual, type = "l", col = "black", 
     xlab = "Time", ylab = "Price (USD per tonne)", 
     main = "Walk-Forward SARIMA Forecast vs. Actual Prices")
lines(sarima_results$time, sarima_results$predicted, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1)



