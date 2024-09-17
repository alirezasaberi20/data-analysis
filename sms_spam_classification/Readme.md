# SMS Spam Collection Dataset

## Overview

In this project, we leverage this dataset to build and evaluate models for classifying SMS messages. The dataset provides a rich set of examples for training machine learning algorithms, making it an excellent starting point for anyone looking to delve into the realm of text classification and spam filtering.

The data is neatly organized with labels and raw text, offering a straightforward yet powerful foundation for exploring various machine learning techniques.

## Dataset Content

The dataset files contain one message per line with two columns:

- `v1`: Label (either "ham" or "spam")
- `v2`: Raw text of the SMS message

## Sources

This dataset has been curated from several sources:

1. **Grumbletext Web Site**: A collection of 425 SMS spam messages extracted from a UK forum where users publicly report SMS spam. Visit the [Grumbletext Web Site](#) for more details.
   
2. **NUS SMS Corpus**: A subset of 3,375 legitimate messages from a larger dataset collected by the National University of Singapore. For more information, check out the [NUS SMS Corpus](#).

3. **Caroline Tag's PhD Thesis**: A list of 450 SMS ham messages collected for research purposes. Access the thesis [here](#).

4. **SMS Spam Corpus v.0.1 Big**: This includes 1,002 ham and 322 spam messages. The dataset is publicly available [here](#).

## Model

For training and classification, we used the `BernoulliNB` model. This Naive Bayes model is well-suited for binary/boolean features, which aligns well with the SMS spam classification task.

## Acknowledgements

The original dataset can be found [here](http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/). If you find this dataset useful for your research or projects, please reference the following paper:

- Almeida, T.A., Gómez Hidalgo, J.M., Yamakami, A. (2011). "Contributions to the Study of SMS Spam Filtering: New Collection and Results." Proceedings of the 2011 ACM Symposium on Document Engineering (DOCENG'11), Mountain View, CA, USA.


