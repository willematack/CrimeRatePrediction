# Crime Rate Analysis and Prediction
## Purpose
This was my term project for Methods of Applied Statistics (Fall 2023). Using a complex dataset containing environmental and demographic data of 2,000+ communities in the United States, and their associated rates of different crimes, the research question was two-fold:

- What are the most significant predictors of crime, and do these depend on crime type?
- How can we build a predictive model to predict crime rates for different crime types in new communities. In particular, I compared the performance of traditional linear regression techniques to modern tree-based models.

- My final report can be found [here](https://github.com/willematack/CrimeRatePrediction/blob/main/Deliverables/STA2101_Final_Report.pdf). It provides a detailed description of ethods used, and their justification, as well as a summary of results. My final presentation slides are [here](https://github.com/willematack/CrimeRatePrediction/blob/main/Deliverables/Presentation/PresentationSlides.pptx), which provide a brief summary of methods and results

## Skills Used

In a mix of R and Python:
Data cleaning, data pre-processing, EDA, statistical modelling, predictive modelling, data visualization

## Methods Used

Feature Selection - Criterion methods (AIC, BIC), LASSO, correlation analysis
Feature Importance - GINI Imbalance scores, SHAP values, t-values, bootstrapping, confidence intervals
Prediction (Traditional Statistics) - Linear Regression, LASSO, Ridge Regression, ElasticNet
Prediction (Tree-Based) - Random Forest, XGBoost, Light GBM

## Files
Data folder - contains raw and cleaned data, as well as some stored large intermediate variables
Deliverables folder - contains final report PDF as well as project proposal, EDA and literature review, and final presentation slides.
EDA_FeatureImportance folder - contains files needed for data cleaning and pre-processing, feature selection and feature importance tests, as well as a file where an EDA was performed with many visualizations of the data.
Preciction - contains files where predictive models were built
