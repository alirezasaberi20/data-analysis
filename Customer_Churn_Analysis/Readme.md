# 📊 Customer Retention & Churn Analysis

> Why are customers leaving, and how can the company improve retention? A data-driven approach to reducing churn and protecting recurring revenue.

![Python](https://img.shields.io/badge/Python-3.10+-blue?logo=python&logoColor=white)
![scikit-learn](https://img.shields.io/badge/scikit--learn-1.x-orange?logo=scikit-learn)
![Pandas](https://img.shields.io/badge/Pandas-2.x-150458?logo=pandas)
![Statistics](https://img.shields.io/badge/Statistics-scipy-green)
![License](https://img.shields.io/badge/License-MIT-yellow)

## 📋 Business Problem

Customer churn is one of the most critical KPIs for subscription-based businesses. In the Dutch market, this is relevant across telecom (KPN, T-Mobile, Ziggo), SaaS, insurance, and utilities. This project:

1. Identifies **why** customers leave (root cause analysis)
2. Quantifies the **revenue impact** of churn
3. Segments customers by **risk level**
4. Provides **actionable recommendations** to reduce churn

## 📊 Dataset

| Property | Value |
|----------|-------|
| Source | [IBM Telco Customer Churn](https://www.kaggle.com/datasets/blastchar/telco-customer-churn) |
| Customers | 7,043 |
| Features | 21 (demographics, services, contract, billing) |
| Target | Churn (Yes/No) |
| Churn Rate | 26.5% |

### Features Include:
- **Demographics**: gender, senior citizen, partner, dependents
- **Services**: phone, internet, online security, backup, device protection, tech support, streaming
- **Account**: tenure, contract type, payment method, monthly/total charges

## 🔍 Analysis Pipeline

```
Data Loading → Quality Check → EDA → Statistical Testing → Cohort Analysis → Segmentation → Prediction Model → Revenue Impact → Recommendations
```

### Key Findings

| Metric | Value |
|--------|-------|
| Overall Churn Rate | 26.5% |
| Avg Tenure (Churned) | 18.0 months |
| Avg Tenure (Retained) | 37.6 months |
| Monthly Charges (Churned) | €74.44 |
| Monthly Charges (Retained) | €61.27 |
| Month-to-month churn multiplier | **15.1x** higher than 2-year contracts |

### Statistical Tests Performed

| Test | Variable | Result |
|------|----------|--------|
| Independent t-test | Monthly Charges vs Churn | Significant (p < 0.001) |
| Chi-square test | Contract Type vs Churn | Significant (p < 0.001) |
| Chi-square test | Internet Service vs Churn | Significant (p < 0.001) |
| Independent t-test | Tenure vs Churn | Significant (p < 0.001) |

### Predictive Model Performance

| Metric | Score |
|--------|-------|
| ROC AUC | 0.839 |
| Accuracy | 77% |
| Precision (Churned) | 55% |
| Recall (Churned) | 69% |

### Revenue Impact

| Metric | Value |
|--------|-------|
| Monthly Revenue Lost to Churn | €139,131 |
| Projected Annual Revenue Loss | €1,669,570 |
| Avg Customer Lifetime Value | €2,550 |
| 10% Churn Reduction = Revenue Saved | €166,153/year |

## 💡 Business Recommendations

### 1. Contract Migration Program
> Customers without yearly contracts churn **15x more frequently**.

Offer 10-15% discount for annual contract upgrades. Converting 20% of month-to-month customers could reduce overall churn by ~30%.

### 2. Early Intervention (First 90 Days)
> New customers (0-3 months) have the highest churn risk.

Implement onboarding program: dedicated support contact, proactive check-ins at day 30/60/90, early loyalty reward at 3-month milestone.

### 3. Service Bundling
> Customers with multiple services churn significantly less.

Offer bundled packages (security + backup + support) at reduced rates to increase switching costs and perceived value.

### 4. Payment Method Optimization
> Electronic check users have the highest churn rate.

Incentivize auto-pay enrollment with €5/month discount to reduce payment friction and increase retention.

### 5. High-Value Customer Retention
> Premium customers who are new (< 12 months) = highest risk segment.

Assign dedicated account managers to this segment for proactive retention.

## 🛠️ Tech Stack

- **Python** (pandas, NumPy, matplotlib, seaborn)
- **scipy** (statistical testing: t-test, chi-square)
- **scikit-learn** (Random Forest classifier, evaluation metrics)
- **Business Intelligence** concepts (cohort analysis, KPI design)

## 📁 Project Structure

```
├── 02_customer_churn_analysis.ipynb    # Main analysis notebook
├── datasets/
│   └── churn/
│       └── WA_Fn-UseC_-Telco-Customer-Churn.csv
└── README.md
```

## 🚀 How to Run

```bash
# Clone the repository
git clone https://github.com/YOUR_USERNAME/customer-churn-analysis.git
cd customer-churn-analysis

# Install dependencies
pip install pandas numpy matplotlib seaborn scikit-learn scipy

# Download dataset from Kaggle
kaggle datasets download -d blastchar/telco-customer-churn -p datasets/churn/

# Run the notebook
jupyter notebook 02_customer_churn_analysis.ipynb
```

## 📈 Skills Demonstrated

- SQL-style data operations (groupby, window functions via pandas)
- Cohort analysis & retention curves
- Statistical hypothesis testing (t-test, chi-square)
- Customer segmentation (tenure × value matrix)
- Classification modeling (Random Forest)
- Revenue impact quantification
- KPI dashboard design
- Business storytelling & actionable recommendations
