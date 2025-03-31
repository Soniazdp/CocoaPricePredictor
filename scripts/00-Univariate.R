#### Preamble ####
# Purpose: Fit some univariate model to forecast using Daily Prices_ICCO
# Author: Deyi Kong
# Date: March 31th, 2025
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `dplyr`, `lubridate`,`forecast`, `tseries`, and `zoo` packages must be installed
# Any other information needed? Make sure you are in the `CocoaPricePredictor` rproj

library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(tseries) 

# Load dataset
daily_price <- read.csv("data/Daily Prices_ICCO.csv")

#### Data cleaning ####
# Convert to monthly data
monthly_price <- daily_price %>%
  mutate(price = as.numeric(gsub(",", "", ICCO.daily.price..US..tonne.)), date = as.Date(Date, format = "%d/%m/%Y")) %>% 
  mutate(month = as.yearmon(date)) %>% 
  group_by(month) %>%
  summarise(
    # Monthly average
    price = mean(price, na.rm = TRUE)
  ) %>%
  arrange(month)  # Sort chronologically

# Check missing data
is.na(monthly_price)

### Save monthly data ###
write.csv(monthly_price, "data/Monthly_Prices_ICCO.csv")


#### Split training and testing sets ####
# Consist with later multivariate data spliting
split_month <- as.yearmon("Feb 2020")

# Split into train/test
uni_train <- monthly_price %>% filter(month < split_month)
uni_test <- monthly_price %>% filter(month >= split_month)

# Convert time series
uni_train_ts <- ts(uni_train$price, start = c(1994, 10), frequency = 12)
uni_test_ts <- ts(uni_test$price, start = c(2020, 2), frequency = 12)


#### Fit ETS model ####
ets_model <- ets(uni_train_ts, model ="ZZZ")
ets_model # AIC: 4599.675

# Forecast on test set
forecast_ets <- forecast(ets_model, h = length(uni_test_ts))
accuracy(forecast_ets, uni_test_ts)  # test RMSE(2676.9524) 25× higher than training RMSE(117.0653); ACF1(0.9087675) residual strong pattern 


#### Fit SARIMA model ####
sarima_model <- auto.arima(uni_train_ts, seasonal = TRUE) # auto-tuning ARIMA(0,1,1) -> no strong seasonality detected
summary(sarima_model) # AIC: 3734.32

# Forecast on test set
forecast_sarima <- forecast(sarima_model, h = length(uni_test_ts))
accuracy(forecast_sarima, uni_test_ts)  # test RMSE(2657.6344) 23× higher than training RMSE(113.8591); ACF1(0.908767545) residual strong pattern 


#### Fit GARCH model ####
garch_model <- ugarchfit(ugarchspec(variance.model = list(garchOrder = c(1,1))), data = uni_train_ts)
print(garch_model) # GARCH is the best among three (lowest AIC(12.208)), ETS is the worst

