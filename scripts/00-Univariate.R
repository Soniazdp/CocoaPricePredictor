#### Preamble ####
# Purpose: Fit some univariate model to forecast using Daily Prices_ICCO
# Date: Apr 2, 2025
# Pre-requisites: The `dplyr`, `forecast`, `tseries`, `rugarch`, `fGarch`, and `xts` packages must be installed
# Any other information needed? Make sure you are in the `CocoaPricePredictor` rproj


library(dplyr)
library(forecast)
library(tseries) 
library(ggplot2)
library(Metrics)
library(rugarch)
library(fGarch)
library(xts)


# Load datasets
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")

# Convert time series
uni_train_ts <- ts(train$price_usd_per_tonne_mean, start = c(1996, 1), frequency = 12)
uni_test_ts <- ts(test$price_usd_per_tonne_mean, start = c(2019, 2), frequency = 12)
  

#### Fit ETS model ####
ets_model <- ets(uni_train_ts, model ="ZZZ") # ETS(M,N,N)
ets_model # AIC: 4186.622

# Forecast on test set
forecast_ets <- forecast(ets_model, h = length(uni_test_ts))
accuracy(forecast_ets, uni_test_ts)  # test RMSE(2071.7702) 17× higher than training RMSE(120.2793); ACF1(0.9072913) residual strong pattern 

#the coeffitient of the x_t-1 something like that

# Plot forecast vs actual for ETS
autoplot(forecast_ets) +
  autolayer(uni_test_ts, series = "Test Data") +
  ggtitle("ETS Forecast vs Test Data") +
  xlab("Year") + ylab("Price (USD/tonne)") +
  theme_minimal()


#### Fit SARIMA model ####
sarima_model <- auto.arima(uni_train_ts, seasonal = TRUE) # auto-tuning ARIMA(0,1,1) -> no strong seasonality detected
summary(sarima_model) # AIC: 3734.32

# graph of residuals, Ljung-Box test, Histogram and time plot
checkresiduals(sarima_model)

# Forecast on test set
forecast_sarima <- forecast(sarima_model, h = length(uni_test_ts))
accuracy(forecast_sarima, uni_test_ts)  # test RMSE(2657.6344) 23× higher than training RMSE(113.8591); ACF1(0.908767545) residual strong pattern 

# Plot forecast vs actual for SARIMA
autoplot(forecast_sarima) +
  autolayer(uni_test_ts, series = "Test Data", PI = FALSE) +
  ggtitle("SARIMA Forecast vs. Test Data") +
  xlab("Year") + ylab("Price (USD/tonne)") +
  theme_minimal()


#### Fit GARCH Model ####
# pick the price column in the monthly price df
price_series <- train$price_usd_per_tonne_mean

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



