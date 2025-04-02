#### Preamble ####
# Purpose: Fit some univariate model to forecast using Daily Prices_ICCO
# Author: Deyi Kong
# Date: March 31th, 2025
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `dplyr`, `lubridate`,`forecast`, `tseries`, and `zoo` packages must be installed
# Any other information needed? Make sure you are in the `CocoaPricePredictor` rproj

install.packages("rugarch")
library(rugarch)
library(dplyr)
library(lubridate)
library(zoo)
library(forecast)
library(tseries) 
library(ggplot2)
install.packages("fGarch")
library(fGarch)
library(xts)


# Load dataset
daily_price <- read.csv("Daily Prices_ICCO (1).csv")

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

# Plot forecast vs actual for ETS
autoplot(forecast_ets) +
  autolayer(uni_test_ts, series = "Test Data", PI = FALSE) +
  ggtitle("ETS Forecast vs Test Data") +
  xlab("Year") + ylab("Price (USD/tonne)") +
  theme_minimal()

#### Fit SARIMA model ####
sarima_model <- auto.arima(uni_train_ts, seasonal = TRUE) # auto-tuning ARIMA(0,1,1) -> no strong seasonality detected
summary(sarima_model) # AIC: 3734.32

###graph of residuals, Ljung-Box test, Histogram and time plot
checkresiduals(sarima_model)

# Forecast on test set
forecast_sarima <- forecast(sarima_model, h = length(uni_test_ts))
accuracy(forecast_sarima, uni_test_ts)  # test RMSE(2657.6344) 23× higher than training RMSE(113.8591); ACF1(0.908767545) residual strong pattern 

# Plot forecast vs actual for SARIMA
autoplot(forecast_sarima) +
  autolayer(uni_test_ts, series = "Test Data", PI = FALSE) +
  ggtitle("SARIMA Forecast vs Test Data") +
  xlab("Year") + ylab("Price (USD/tonne)") +
  theme_minimal()

#pick the price column in the monthly price df
price_series <- monthly_price$price

# Compute log returns
cocoa_returns <- diff(log(price_series))[-1]
plot(cocoa_returns, main = "Cocoa Log Returns")
library(astsa)
acf2(cocoa_returns, main = "ACF & PACF of Cocoa Returns")
acf2(cocoa_returns^2, main = "ACF & PACF of Squared Returns")
# Fit GARCH(1,1) with AR(1) mean and t-distributed errors
garch_cocoa <- garchFit(~ arma(1, 0) + garch(1, 1), data = cocoa_returns,
                        cond.dist = "std", trace = FALSE)
summary(garch_cocoa)
predict(garch_cocoa, n.ahead = 12, plot = TRUE)  # Forecast 12 periods ahead
