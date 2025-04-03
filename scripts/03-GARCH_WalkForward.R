# Required packages
library(rugarch)
library(xts)
library(Metrics)

# Load datasets
train <- read.csv("data/train_simple.csv")
test <- read.csv("data/test_simple.csv")
# Combine data for walk-forward validation
full_data <- rbind(train, test)

# Convert time series
full_data_ts <- ts(full_data$price_usd_per_tonne_mean, start = c(1996, 1), frequency = 12)

# Convert data to returns 
returns <- diff(log(full_data_ts))[-1]  

# Walk-forward parameters
train_window <- 24  # 2 years 
h <- 3  # Forecast 3 months ahead each time
start_point <- train_window + 1
n_forecasts <- length(returns) - start_point + 1

# Storage
actuals <- numeric(n_forecasts)
forecasts <- numeric(n_forecasts)

# GARCH(1,1) specification
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

for (i in 1:n_forecasts) {
  train_data <- returns[(i):(train_window + i - 1)]
  
  fit <- tryCatch(
    ugarchfit(spec, data = train_data, solver = "hybrid"),
    error = function(e) NULL
  )
  
  if (!is.null(fit)) {
    forecast <- ugarchforecast(fit, n.ahead = h)
    forecasts[i] <- fitted(forecast)[h]  # Mean forecast
    actuals[i] <- returns[train_window + i]  # Actual value
  } else {
    forecasts[i] <- NA
    actuals[i] <- NA
  }
}


# Remove NAs (if any)
valid <- !is.na(forecasts)
actuals_clean <- actuals[valid]
forecasts_clean <- forecasts[valid]

# RMSE
rmse(actuals_clean, forecasts_clean)
mae(actuals_clean, forecasts_clean)
mse(actuals_clean, forecasts_clean)

# Plot
plot(actuals_clean, type = 'l', col = 'black', ylab = 'Return', main = 'Walk-Forward GARCH Forecast vs Actuals')
lines(forecasts_clean, col = 'red')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1)
