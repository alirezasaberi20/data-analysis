## About the Dataset

### Context

This dataset is designed to help us discover the best strategies for improving future marketing campaigns. By analyzing data from a previous marketing campaign conducted by a financial institution, we aim to identify patterns and insights that can guide the development of more effective marketing strategies to increase deposits.

<img src='https://www.bankbound.com/wp-content/uploads/2022/11/8-Deposits-Strategies.jpg' width='600'>

### Source

The dataset is based on the work by [Moro et al., 2014](https://www.sciencedirect.com/science/article/abs/pii/S0167923614000192), where S. Moro, P. Cortez, and P. Rita presented a data-driven approach to predicting the success of bank telemarketing. The study is published in *Decision Support Systems*, Elsevier, 62:22-31, June 2014.

In addition, the dataset and related code can be found on Kaggle. You can explore the dataset at the following link:

- [Bank Marketing Dataset on Kaggle](https://www.kaggle.com/datasets/janiobachmann/bank-marketing-dataset/data)

### Model Used

For this analysis, we utilized the XGBoost algorithm as the main model. XGBoost is known for its performance and efficiency in classification tasks.

### Evaluation Metrics

The performance of the XGBoost model was evaluated using the following metrics:

- **AUC Score**: 0.912
- **Accuracy**: 0.848
- **F1 Score**: 0.839
- **Precision**: 0.817
- **Recall**: 0.862

These metrics provide a comprehensive overview of the model's effectiveness in predicting the success of the marketing campaigns.




