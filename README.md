****# Cocoa Price & Climate Data

## Data Versions
- **Fx rates** (USDGHS): Released by the Bank of Ghana (`28 Mar 2025` - `02 Jan 1996`)
- **Full** (17 features): For ML (`*_full.csv`)  
- **Simple** (9 features): For time series (`*_simple.csv`)

## Data Splits (80:20)
| Split      | Period          | Months |
|------------|-----------------|--------|
| Training   | 1996-01 to 2019-01 | 277 months |
| Test       | 2019-02 to 2024-11 | 70 months |

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