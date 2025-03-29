## Dataset Overview

| Dataset Type | Features | Best For | File |
|-------------|----------|----------|------|
| **Full** | 17 aggregated features | Machine Learning | `{train/test}_full.csv` |
| **Simple** | 9 key features | Classical Time Series | `{train/test}_simple.csv` |

### Temporal Coverage

| Split Type | Period | Duration | Notes |
|------------|--------|----------|-------|
| **Training** | 1994-10 to 2020-02 | 2296 months | Chronologically first 80% |
| **Test** | 2020-02 to 2024-11 | 575 months | Chronologically last 20% |


## Other Explanation

### Core Metrics
| Feature | Description |
|--------|-------------|
| `price_usd_per_tonne_last` | End-of-month cocoa price (USD/tonne)|
| `price_usd_per_tonne_std` | Price volatility (USD/tonne)|
| `precipitation_sum` | Monthly total rainfall
| `avg_temp_mean` | Monthly average temperature
| `max_temp_max` | Monthly maximum high temp

### Specialized Features
| Feature | Calculation | Use Case |
|---------|-------------|---------|
| `precipitation_count` | Rainy days per month | Drought analysis |
| `min_temp_min` | Coldest daily low | Frost impact studies |
| `avg_temp_std` | Temperature variability | Climate change research |
