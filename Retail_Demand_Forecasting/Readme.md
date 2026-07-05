# 🇳🇱 Dutch Retail Demand Forecasting & Inventory Optimization

> Predict product demand for a retail chain to reduce overstock and stockouts — a critical capability for companies like Albert Heijn, Jumbo, Coolblue, and Bol.com.

![Python](https://img.shields.io/badge/Python-3.10+-blue?logo=python&logoColor=white)
![Pandas](https://img.shields.io/badge/Pandas-2.x-150458?logo=pandas)
![XGBoost](https://img.shields.io/badge/XGBoost-3.x-green)
![Prophet](https://img.shields.io/badge/Prophet-1.x-blue)
![License](https://img.shields.io/badge/License-MIT-yellow)

## 📋 Business Problem

Demand forecasting is one of the most common analytical challenges in the Dutch retail and logistics sector. Inaccurate forecasts lead to:
- **Overstock** → tied-up capital, warehouse costs, product waste
- **Stockouts** → lost sales, unhappy customers, competitive disadvantage

This project builds a forecasting pipeline that predicts daily product demand 30 days ahead and translates accuracy metrics into actionable inventory decisions.

## 📊 Dataset

| Property | Value |
|----------|-------|
| Source | [Kaggle: Store Sales - Time Series Forecasting](https://www.kaggle.com/competitions/store-sales-time-series-forecasting) |
| Records | 3,000,888 daily sales records |
| Time Range | 2013-01-01 to 2017-08-15 |
| Stores | 54 |
| Product Families | 33 |
| Additional Data | Holidays, oil prices, transactions, store metadata |

## 🔍 Analysis Pipeline

```
Data Loading → Quality Check → EDA → Feature Engineering → Train/Test Split → Modeling → Evaluation → Business Recommendations
```

### Exploratory Data Analysis
- Sales trends and year-over-year growth
- Day-of-week demand patterns (peak: Sunday, low: Thursday)
- Monthly seasonality (peak: December)
- Holiday impact analysis (+6% uplift on holidays)
- Promotion effectiveness analysis

### Feature Engineering
- **Calendar features**: day of week, month, quarter, week of year, weekend flag
- **Lag features**: 1-day, 7-day, 14-day, 28-day lags
- **Rolling statistics**: 7/14/30-day rolling mean and standard deviation
- **Expanding mean**: cumulative average up to each point

### Models Compared

| Model | RMSE | MAE | MAPE |
|-------|------|-----|------|
| **XGBoost** | **1,124** | **880** | **9.2%** |
| LightGBM | 1,164 | 844 | 8.3% |
| Prophet | 1,507 | 1,089 | 11.7% |
| Seasonal Naive (7d) | 1,574 | 1,255 | 13.3% |
| Moving Average (30d) | 2,103 | 1,837 | 19.4% |
| Naive (Last Value) | 4,968 | 4,521 | 51.3% |

## 💡 Key Business Insights

> "Increasing inventory by 12% before peak periods would reduce estimated stockouts by approximately 92%."

- **Stockout Analysis**: Without buffer, 40% of days face potential stockouts. A 12% safety buffer reduces this to just 3%.
- **Staffing**: Peak demand on Sundays → plan extra delivery capacity. Lowest demand on Thursdays → schedule restocking/maintenance.
- **Seasonal Planning**: December is peak month — increase stock levels 2 weeks before.
- **Holiday Preparation**: Plan 6% additional inventory for holiday periods.

## 🛠️ Tech Stack

- **Python** (pandas, NumPy, matplotlib, seaborn)
- **Prophet** (Facebook/Meta time series library)
- **XGBoost** / **LightGBM** (gradient boosting)
- **scikit-learn** (evaluation metrics)

## 📁 Project Structure

```
├── 01_retail_demand_forecasting.ipynb   # Main analysis notebook
├── datasets/
│   └── store_sales/
│       ├── train.csv                    # 3M daily sales records
│       ├── stores.csv                   # Store metadata
│       ├── holidays_events.csv          # Holiday calendar
│       ├── oil.csv                      # Oil prices (economic indicator)
│       └── transactions.csv             # Daily transaction counts
└── README.md
```

## 🚀 How to Run

```bash
# Clone the repository
git clone https://github.com/YOUR_USERNAME/retail-demand-forecasting.git
cd retail-demand-forecasting

# Install dependencies
pip install pandas numpy matplotlib seaborn scikit-learn xgboost lightgbm prophet

# Download dataset from Kaggle
kaggle competitions download -c store-sales-time-series-forecasting -p datasets/store_sales/

# Run the notebook
jupyter notebook 01_retail_demand_forecasting.ipynb
```

## 📈 Skills Demonstrated

- Time series analysis & decomposition
- Feature engineering for temporal data
- Multiple model comparison (statistical + ML)
- Model evaluation with business-relevant metrics
- Translating model accuracy into inventory decisions
- Data visualization & storytelling
