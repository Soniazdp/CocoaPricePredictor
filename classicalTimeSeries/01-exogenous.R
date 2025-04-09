#### Preamble ####
# Purpose: Fit model (ETS,SARIMAX,GARCH) with exogenous variables to forecast
# Author: Deyi Kong
# Date: March 31th, 2025
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `dplyr`, `forecast`, `tseries`, `Metrics`, and `rugarch` packages must be installed
# Any other information needed? Make sure you are in the `CocoaPricePredictor` rproj

library(dplyr)
library(forecast)
library(tseries) 
library(rugarch)
library(Metrics)

# Load datasets
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")

# Exogenous variables
exog_train <- train %>% select(precipitation_sum, max_temp_mean, min_temp_mean, avg_temp_mean)
exog_test <- test %>% select(precipitation_sum, max_temp_mean, min_temp_mean, avg_temp_mean)

#### Fit ETS Models #### 
ets_model <- ets(train$monthly_price_mean)
ets_model # ETS(A,Ad,N); with AIC: 35035.28
forecast_ets <- forecast(ets_model, h = nrow(test))
accuracy(forecast_ets, test$monthly_price_mean) # test RMSE(2100.57421) 50× higher than training RMSE(43.30166)


#### Fit SARIMAX model ####
sarimax_model <- auto.arima(train$monthly_price_mean, xreg = as.matrix(exog_train), seasonal = TRUE)
sarimax_model # ARIMA(0,1,0); with AIC: 23785.94
# Forecasting for Test Set
forecast_sarimax <- forecast(sarimax_model, xreg = as.matrix(exog_test), h = nrow(test))
accuracy(forecast_sarimax, test$monthly_price_mean) # test RMSE(2104.82515) 50× higher than training RMSE(43.27465)


#### Fit GARCH model ####
spec <- ugarchspec(
  variance.model = list(garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1), external.regressors = as.matrix(exog_train)),
  distribution.model = "norm"
)
garch_model <- ugarchfit(spec, data = train$monthly_price_mean)
garch_model # AIC: 10.299

# Forecast the next 578 months (matches test set) 
garch_forecast <- ugarchforecast(garch_model, n.ahead = 578, n.roll = 0)
# Extract mean forecast (predicted values)
garch_pred <- as.numeric(fitted(garch_forecast))
# Extract volatility forecast (standard deviation)
garch_volatility <- as.numeric(sigma(garch_forecast))
# Extract actual values from the test set
actual_values <- test$monthly_price_mean

# Compute error metrics
rmse_garch <- rmse(actual_values, garch_pred) # RMSE: 2319.09 
mae_garch <- mae(actual_values, garch_pred) # MAE: 1237.367 
mape_garch <- mape(actual_values, garch_pred) # MAPE: 0.2229223 

