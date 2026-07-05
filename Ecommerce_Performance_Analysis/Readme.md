# 🛒 E-commerce Product & Marketing Performance Analysis

> Which products generate the highest profit? Which customer segments drive growth? A comprehensive commercial analytics project demonstrating the breadth of skills expected at Dutch startups and scale-ups.

![Python](https://img.shields.io/badge/Python-3.10+-blue?logo=python&logoColor=white)
![Pandas](https://img.shields.io/badge/Pandas-2.x-150458?logo=pandas)
![Statistics](https://img.shields.io/badge/Statistics-scipy-green)
![RFM](https://img.shields.io/badge/RFM-Segmentation-purple)
![License](https://img.shields.io/badge/License-MIT-yellow)

## 📋 Business Questions

1. Which products generate the highest **profit** (not just revenue)?
2. Which customer segments spend the most?
3. What is the **Customer Lifetime Value** distribution?
4. Which markets have the best growth potential?
5. What drives **repeat purchases**?

## 📊 Dataset

| Property | Value |
|----------|-------|
| Source | [UCI Machine Learning Repository: Online Retail II](https://archive.ics.uci.edu/dataset/502/online+retail+ii) |
| Raw Transactions | 1,067,371 |
| After Cleaning | 805,549 (75.5% retained) |
| Time Range | Dec 2009 – Dec 2011 |
| Customers | 5,878 |
| Products | 4,631 |
| Countries | 41 |
| Total Revenue | £17,743,429 |

### Data Fields:
- **Invoice**: Transaction ID
- **StockCode**: Product identifier
- **Description**: Product name
- **Quantity**: Units purchased
- **InvoiceDate**: Transaction timestamp
- **Price**: Unit price (£)
- **Customer ID**: Unique customer identifier
- **Country**: Customer location

## 🔍 Analysis Pipeline

```
Data Loading → Cleaning → Revenue Analysis → Pareto Analysis → Market Analysis → RFM Segmentation → CLV Estimation → Statistical Testing → Cohort Retention → Business Recommendations
```

### Key Findings

| KPI | Value |
|-----|-------|
| Total Revenue | £17,743,429 |
| Total Orders | 36,969 |
| Average Order Value (AOV) | £479.95 |
| Unique Customers | 5,878 |
| Repeat Purchase Rate | 64.9% |
| Countries Served | 41 |

### Pareto Analysis (80/20 Rule)
> **21.3% of products generate 80% of revenue.**

This means the company can focus marketing and inventory efforts on roughly 1 in 5 products to cover the vast majority of sales.

### Top International Markets (excl. UK)

| Rank | Country | Revenue | Revenue per Customer |
|------|---------|---------|---------------------|
| 1 | EIRE (Ireland) | — | High |
| 2 | Germany | — | Medium |
| **3** | **Netherlands** | **£554,232** | **£25,192** |
| 4 | France | — | Medium |
| 5 | Australia | — | High |

### RFM Customer Segmentation

| Segment | % Customers | % Revenue | Action |
|---------|-------------|-----------|--------|
| Champions | 30.9% | 55.7% | Reward & upsell |
| Loyal Customers | 13.7% | 3.3% | Increase frequency |
| New Customers | 7.6% | 2.1% | Nurture & onboard |
| At Risk | 18.8% | 9.0% | Re-engage quickly |
| Can't Lose Them | 3.8% | 7.4% | Win-back campaign |
| Lost/Hibernating | 27.2% | 3.0% | Targeted reactivation |

### Statistical Analysis

- **95% Confidence Intervals** calculated for AOV, Customer Spend, and Basket Size
- **Hypothesis test**: Weekend vs Weekday order values (business operates primarily on weekdays)
- Revenue decomposition by price tier and product category

## 💡 Business Recommendations

### 1. Product Portfolio Optimization
> 21% of products generate 80% of revenue.

Focus marketing spend on top performers. Consider discontinuing bottom 20% low-performers to reduce inventory costs and operational complexity.

### 2. Customer Retention Focus
> Champions (31% of customers) generate 56% of revenue.

Implement loyalty program. Even a 5% improvement in retention could increase revenue by £887,000 annually.

### 3. International Expansion
> Netherlands ranks #3 internationally with £25,192 revenue per customer.

Invest in localized marketing for top 5 international markets. Non-UK markets contribute 17% of revenue with significant growth potential.

### 4. Average Order Value Growth
> Current AOV: £479.95

Implement cross-selling ("frequently bought together") and minimum order thresholds for free shipping. Target: +15% AOV through bundling.

### 5. Win-Back Campaign
> 27% of customers are Lost/Hibernating.

Targeted email campaign with personalized discounts for dormant high-value customers. Potential recovery: 10% reactivation rate.

## 🛠️ Tech Stack

- **Python** (pandas, NumPy, matplotlib, seaborn)
- **scipy** (confidence intervals, hypothesis testing)
- **RFM Analysis** (customer segmentation methodology)
- **Pareto Analysis** (80/20 product optimization)
- **Cohort Analysis** (monthly retention tracking)

## 📁 Project Structure

```
├── 03_ecommerce_performance_analysis.ipynb   # Main analysis notebook
├── datasets/
│   └── online_retail_II.xlsx                 # UCI dataset (2 sheets)
└── README.md
```

## 🚀 How to Run

```bash
# Clone the repository
git clone https://github.com/YOUR_USERNAME/ecommerce-performance-analysis.git
cd ecommerce-performance-analysis

# Install dependencies
pip install pandas numpy matplotlib seaborn scikit-learn scipy openpyxl

# Download dataset from UCI
wget http://archive.ics.uci.edu/ml/machine-learning-databases/00502/online_retail_II.xlsx -P datasets/

# Run the notebook
jupyter notebook 03_ecommerce_performance_analysis.ipynb
```

## 📈 Skills Demonstrated

- Complex SQL-style operations (window functions, ranking, CTEs via pandas)
- RFM Analysis & customer segmentation
- Pareto analysis (product portfolio optimization)
- Customer Lifetime Value estimation
- Statistical testing (confidence intervals, t-tests)
- Cohort retention analysis
- Revenue decomposition by segment/market/product
- KPI dashboard design
- Business storytelling with actionable insights
