#### Preamble ####
# Purpose: Fit some univariate model to forecast using Daily Prices_ICCO
# Author: Deyi Kong
# Date: March 30th, 2025
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `forecast` and `tseries` packages must be installed
# Any other information needed? Make sure you are in the `CocoaPricePredictor` rproj

library(forecast)
library(tseries) 

# Load dataset
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")

#### Fit ETS model ####
ets_model <- ets(train$price_usd_per_tonne_last)
ets_model # AIC: 37327.95

# Forecast on test set
forecast_ets <- forecast(ets_model, h = length(train$price_usd_per_tonne_last))
accuracy(forecast_ets, test$price_usd_per_tonne_last)  # test RMSE(2418.2449) 33× higher than training RMSE(70.6784)


#### Fit SARIMA model ####
sarima_model <- auto.arima(train$price_usd_per_tonne_last, seasonal = TRUE) # auto-tuning ARIMA(0,1,1) -> no strong seasonality detected
summary(sarima_model) # AIC: 26062.84

# Forecast on test set
forecast_sarima <- forecast(sarima_model, h = length(train$price_usd_per_tonne_last))
accuracy(forecast_sarima, test$price_usd_per_tonne_last)  # test RMSE(2418.24317) 33× higher than training RMSE(70.67838)


#### Fit GARCH model ####
garch_model <- ugarchfit(ugarchspec(variance.model = list(garchOrder = c(1,1))), data = train$price_usd_per_tonne_last)
print(garch_model) # GARCH is the best among three (lowest AIC(11.213)), ETS is the worst

