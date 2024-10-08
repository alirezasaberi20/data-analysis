## Heart disease prediction
## Context
This data set dates from 1988 and consists of four databases: Cleveland, Hungary, Switzerland, and Long Beach V. It contains 76 attributes, including the predicted attribute, but all published experiments refer to using a subset of 14 of them. The "target" field refers to the presence of heart disease in the patient. It is integer valued 0 = no disease and 1 = disease.

<img src='https://www.endocrine.org/-/media/endocrine/images/patient-engagement-webpage/condition-page-images/cardiovascular-disease/cardio_disease_t2d_pe_1796x943.jpg?w=2580&hash=C1E03C92FBE97D0B263E4B5A64B18280' width=500>

### Attribute Information:

        1.age
        2.sex
        3.chest pain type (4 values)
        4.resting blood pressure
        5.serum cholestoral in mg/dl
        6.fasting blood sugar > 120 mg/dl
        7.resting electrocardiographic results (values 0,1,2)
        8.maximum heart rate achieved
        9.exercise induced angina
        10.oldpeak = ST depression induced by exercise relative to rest
        11. the slope of the peak exercise ST segment
        12.number of major vessels (0-3) colored by flourosopy
        13.thal: 0 = normal; 1 = fixed defect; 2 = reversable defect
        The names and social security numbers of the patients were recently 
        removed from the database, replaced with dummy values.

## Evaluation Metrics:
- AUC Score: 0.989
- Accuracy: 0.971
- F1 Score: 0.970
- Precision: 0.980
- Recall: 0.961


The dataset can be found [here](https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset/data) on Kaggle.

