# Cocoa Price & Climate Data

## Data Versions
- **Full** (17 features): For ML (`*_full.csv`)  
- **Simple** (9 features): For time series (`*_simple.csv`)

## Data Splits (80:20)
| Split      | Period          | Months |
|------------|-----------------|--------|
| Training   | 1994-10:2020-02 | 2,296  |
| Test       | 2020-02:2024-11 | 575    |

## Key Features
### Pricing
- `price_usd_per_tonne_last`: Month-end price (USD/tonne)
- `price_usd_per_tonne_std`: Price volatility

### Climate
- `precipitation_sum`: Monthly rainfall (mm)
- `avg_temp_mean`: Avg temperature (°C)  
- `max_temp_max`: Hottest day (°C)

### Specialized
- `precipitation_count`: Rainy days → Drought studies  
- `min_temp_min`: Coldest day → Frost analysis  
- `avg_temp_std`: Temp variation → Climate research