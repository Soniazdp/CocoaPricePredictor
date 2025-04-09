#### Preamble ####
# Purpose: Fit some univariate model to forecast using Daily Prices_ICCO
# Date: Apr 4, 2025
# Pre-requisites: The `dplyr`, `forecast`, `tseries`, and `xts` packages must be installed
# Any other information needed? Make sure you are in the `CocoaPricePredictor` rproj


library(dplyr)
library(forecast)
library(tseries) 
library(ggplot2)
library(Metrics)
library(xts)
library(astsa)


# Load datasets
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")

# Convert time series
uni_train_ts <- ts(train$price_usd_per_tonne_mean, start = c(1996, 1), frequency = 12)
uni_test_ts <- ts(test$price_usd_per_tonne_mean, start = c(2019, 2), frequency = 12)

# Examine data
decomp <- stl(uni_train_ts, s.window = "periodic")
plot(decomp, main = "Decomposition of Time Series Train Data")

# Examine ACF & PACF for AR and MA terms
par(mfrow = c(1,2))
acf(uni_train_ts, main="ACF of Time Series")  # Check for MA terms
pacf(uni_train_ts, main="PACF of Time Series") # Check for AR terms

adf.test(uni_train_ts) # p-value > 0.05, failed to reject H_0 suggest may not stationary

# Consider to difference the data to make stationary
adf.test(diff(uni_train_ts)) # good
acf(diff(uni_train_ts), main = "Differenced Train Data")
pacf(diff(uni_train_ts), main = "Differenced Train Data")
# Examine data
decomp <- stl(diff(uni_train_ts), s.window = "periodic")
plot(decomp, main = "Decomposition of Differenced Time Series Train Data")

## Comparing Models
# Identify Differencing (d, D)
ndiffs(uni_train_ts)  # Non-seasonal differencing (d) = 1
nsdiffs(uni_train_ts) # Seasonal differencing (D) = 0


#### Fit ETS model ####
ets_model <- ets(uni_train_ts, model ="ZZZ") # ETS(M,N,N)
ets_model # AIC: 4186.622
summary(ets_model)
# Forecast on test set
forecast_ets <- forecast(ets_model, h = nrow(test))
forecast::accuracy(forecast_ets, test$price_usd_per_tonne_mean)  # test RMSE(2071.7702) 17× higher than training RMSE(120.2793)

# Plot forecast vs actual for ETS
plot(forecast_ets, main = "ETS Forecast vs. Actual Prices", 
     xlab = "Time", ylab = "Price (USD/tonne)", col = "black")
lines(uni_test_ts, col = "black")
legend("topleft", legend = c("Forecast", "Actual"),
       col = c("steelblue", "black"), lty = 1)


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
forecast::accuracy(forecast_values, uni_test_ts)  # test RMSE(2065.0533) 23× higher than training RMSE(116.3438); ACF1(0.9072913092) residual strong pattern 

# Plot forecast vs actual for SARIMA (ARIMA(0,1,1))
plot(forecast_values, main = "ARIMA Forecast vs. Actual Prices", 
     xlab = "Time", ylab = "Price (USD/tonne)", col = "black")
lines(uni_test_ts, col = "black")
legend("topleft", legend = c("Forecast", "Actual"),
       col = c("steelblue", "black"), lty = 1)

acf(residuals(sarima_model)^2, main = "ACF of Squared Residuals")
library(FinTS)
ArchTest(residuals(sarima_model), lags = 12) # p-value < 0.05, reject null hypothesis ->> ARCH effects, meaning time-varying volatility exists.
