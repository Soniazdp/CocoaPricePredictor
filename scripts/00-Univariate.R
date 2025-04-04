#### Preamble ####
# Purpose: Fit some univariate model to forecast using Daily Prices_ICCO
# Date: Apr 4, 2025
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
library(astsa)


# Load datasets
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")
data <- rbind(train, test)
# Convert time series
uni_train_ts <- ts(train$price_usd_per_tonne_mean, start = c(1996, 1), frequency = 12)
uni_test_ts <- ts(test$price_usd_per_tonne_mean, start = c(2019, 2), frequency = 12)
data_ts <- ts(data$price_usd_per_tonne_mean,  start = c(1996, 1), frequency = 12)

# Examine data
decomp <- stl(uni_train_ts, s.window = "periodic")
plot(decomp)

# Examine ACF & PACF for AR and MA terms
par(mfrow = c(1,2))
acf(uni_train_ts, main="ACF of Time Series")  # Check for MA terms
pacf(uni_train_ts, main="PACF of Time Series") # Check for AR terms

adf.test(uni_train_ts) # p-value > 0.05, failed to reject H_0 suggest may not stationary

# Consider to difference the data to make stationary
adf.test(diff(uni_train_ts)) # good
acf(diff(uni_train_ts))
pacf(diff(uni_train_ts))

## Comparing Models
# Identify Differencing (d, D)
ndiffs(uni_train_ts)  # Non-seasonal differencing (d) = 1
nsdiffs(uni_train_ts) # Seasonal differencing (D) = 0


#### Fit ETS model ####
ets_model <- ets(uni_train_ts, model ="ZZZ") # ETS(M,N,N)
ets_model # AIC: 4186.622

# Forecast on test set
forecast_ets <- forecast(ets_model, h = length(uni_test_ts))
accuracy(forecast_ets, uni_test_ts)  # test RMSE(2071.7702) 17× higher than training RMSE(120.2793); ACF1(0.9072913) residual strong pattern 

# Plot forecast vs actual for ETS
autoplot(forecast_ets) +
  autolayer(uni_test_ts, series = "Test Data") +
  ggtitle("ETS Forecast vs. Actual Prices") +
  xlab("Time") + ylab("Price (USD/tonne)") +
  theme_classic()


#### Fit SARIMA model ####
sarima_model <- auto.arima(uni_train_ts, seasonal = TRUE) # auto-tuning ARIMA(0,1,1)
summary(sarima_model) # AIC: 3413.94

# graph of residuals, Ljung-Box test, Histogram and time plot
checkresiduals(sarima_model)

# Manually Fit SARIMA models
sarima1 <- Arima(uni_train_ts, order=c(0,1,1), seasonal=c(1,0,0))
sarima2 <- Arima(uni_train_ts, order=c(0,1,1), seasonal=c(0,0,1))
sarima3 <- Arima(uni_train_ts, order=c(0,1,1), seasonal=c(1,0,1))

# Compare models using AIC and BIC
AIC(sarima1, sarima2, sarima3) # 3417.465, 3417.462, 3418.611
BIC(sarima1, sarima2, sarima3) # 3428.326, 3428.324, 3433.093

# Check residuals to ensure white noise
checkresiduals(sarima1)
checkresiduals(sarima2)
checkresiduals(sarima3)

# Forecast using the best model
best_sarima <- sarima_model  # ARIMA(0,1,1) is best with smallest AIC
forecast_values <- forecast(best_sarima, h=length(uni_test_ts))
accuracy(forecast_sarima, uni_test_ts)  # test RMSE(2657.6344) 23× higher than training RMSE(113.8591); ACF1(0.908767545) residual strong pattern 

# Plot forecast vs actual for SARIMA (ARIMA(0,1,1))
autoplot(forecast_sarima) +
  autolayer(uni_test_ts, series = "Test Data", PI = FALSE) +
  ggtitle("SARIMA Forecast vs. Actual Prices") +
  xlab("Time") + ylab("Price (USD/tonne)") +
  theme_classic()

acf(residuals(sarima_model)^2, main = "ACF of Squared Residuals")
library(FinTS)
ArchTest(residuals(sarima_model), lags = 12) # p-value < 0.05, reject null hypothesis ->> ARCH effects, meaning time-varying volatility exists.

#### Fit GARCH Model ####
# Compute log returns
log_returns <- diff(log(uni_train_ts))
plot(log_returns, main = "Log Returns")
acf2(log_returns, main = "ACF & PACF of Log Returns")
acf2(log_returns^2, main = "ACF & PACF of Squared Log Returns")
# Perform ARCH test
ArchTest(log_returns, lags = 12)

# Fit GARCH(1,1) with AR(1) mean and t-distributed errors
garch_cocoa <- garchFit(~ arma(1, 0) + garch(1, 1), data = log_returns,
                        cond.dist = "std", trace = FALSE)
summary(garch_cocoa)
predict(garch_cocoa, n.ahead = 4, plot = TRUE)  # Forecast 4 periods ahead

## GARCH Coefficient(s):
#     mu        ar1      omega     alpha1      beta1      shape  
# 0.0011882  0.2225179  0.0004446  0.1082413  0.7576350  7.6000121  

