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
test_window <- 4    # 1 month for testing

# Store performance & prediction metrics
results <- data.frame(iteration = integer(), model = character(), MAE = numeric(), MSE = numeric(), RMSE = numeric())
ets_results <- data.frame(time = numeric(), actual = numeric(), predicted = numeric())
sarima_results <- data.frame(time = numeric(), actual = numeric(), predicted = numeric())

### Loop through the dataset with rolling training and test sets ###
# Generates a sequence that starts at 1, Steps forward by 3 months.
for (i in seq(from = 1, to = (nrow(full_data) - train_window - test_window + 1), by = test_window)) {
  # Define Training and Test Indices
  train_index <- i:(i + train_window - 1)
  test_index <- (i + train_window):(i + train_window + test_window - 1)
  
  train <- full_data[train_index, ]
  test <- full_data[test_index, ]
  
  # Exogenous variables
  exog_train <- train %>% select(precipitation_sum, max_temp_mean, min_temp_mean, avg_temp_mean)
  exog_test <- test %>% select(precipitation_sum, max_temp_mean, min_temp_mean, avg_temp_mean)
  
  #### Fit ETS Model ####
  ets_model <- ets(train$price_usd_per_tonne_mean)
  forecast_ets <- forecast(ets_model, h = test_window)
  
  # ETS performance
  rmse_ets <- rmse(test$price_usd_per_tonne_mean, forecast_ets$mean)
  mae_ets <- mae(test$price_usd_per_tonne_mean, forecast_ets$mean)
  mse_ets <- mse(test$price_usd_per_tonne_mean, forecast_ets$mean)
  
  # Store results
  results <- rbind(results, data.frame(iteration = i, model = "ETS", MAE = mae_ets, MSE = mse_ets, RMSE = rmse_ets))
  ets_results <- rbind(ets_results, data.frame(
    time = test_index,  # Time points for test set
    actual = test$price_usd_per_tonne_mean,
    predicted = as.numeric(forecast_ets$mean)  
  ))
  
  #### Fit SARIMAX Model ####
  sarimax_model <- auto.arima(train$price_usd_per_tonne_mean, xreg = as.matrix(exog_train), seasonal = TRUE)
  forecast_sarimax <- forecast(sarimax_model, xreg = as.matrix(exog_test), h = test_window)
  
  # SARIMAX performance
  rmse_sarimax <- rmse(test$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mae_sarimax <- mae(test$price_usd_per_tonne_mean, forecast_sarimax$mean)
  mse_sarimax <- mse(test$price_usd_per_tonne_mean, forecast_sarimax$mean)
  
  # Store results
  results <- rbind(results, data.frame(iteration = i, model = "SARIMAX", MAE = mae_sarimax, MSE = mse_sarimax, RMSE = rmse_sarimax))
  sarima_results <- rbind(sarima_results, data.frame(
    time = test_index,  # Time points for test set
    actual = test$price_usd_per_tonne_mean,
    predicted = as.numeric(forecast_sarimax$mean)  # Convert to numeric
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

# Forecast v.s. actual Plot by ETS model
plot(time_labels, ets_results$actual, type = "l", col = "black", 
     xlab = "Time", ylab = "Price (USD per tonne)", 
     main = "ETS Forecast vs. Actual Prices")
lines(time_labels, ets_results$predicted, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1)


# Forecast v.s. actual Plot by SARIMA model
plot(time_labels, sarima_results$actual, type = "l", col = "black", 
     xlab = "Time", ylab = "Price (USD per tonne)", 
     main = "SARIMA Forecast vs. Actual Prices")
lines(time_labels, sarima_results$predicted, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1)

