# Titanic Dataset — R Data Cleaning & EDA (Week 1)

Data Analyst internship task: cleaning, preprocessing, and exploratory analysis  
of the Titanic passenger dataset using R.

## Files
- `week1_cleaning_analysis.R` — full R script (missing value imputation,  
  outlier detection & capping, normalization, categorical encoding, EDA + plots)
- `titanic.csv` — raw dataset (891 passengers, public dataset)
- `titanic_cleaned.csv` — cleaned output (18 columns, zero missing values)

## What was done
- Missing values handled: Age (median by Pclass+Sex), Embarked (mode),  
  Cabin (converted to Cabin_Known flag)
- Outlier detection via IQR method; Fare capped at 99th percentile
- Min-max normalization on Age and Fare
- Categorical encoding: Sex (binary), Pclass (factor), Embarked (one-hot)
- Exploratory analysis: summary statistics, correlation matrix,  
  survival rate by sex/class, visualizations (ggplot2)

## Key insight
Sex was the strongest predictor of survival (r = -0.54), followed by  
passenger class and fare.