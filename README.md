# Titanic Dataset — R Data Analysis (Week 1 & Week 2)

Data Analyst internship tasks: cleaning, preprocessing, exploratory analysis,
and data visualization of the Titanic passenger dataset using R.

---

## Week 1: Data Cleaning & Preprocessing

### Files
- `week1_cleaning_analysis.R` — full R script (missing value imputation,
  outlier detection & capping, normalization, categorical encoding, EDA + plots)
- `titanic.csv` — raw dataset (891 passengers, public dataset)
- `titanic_cleaned.csv` — cleaned output (18 columns, zero missing values)

### What was done
- Missing values handled: Age (median by Pclass+Sex), Embarked (mode),
  Cabin (converted to Cabin_Known flag)
- Outlier detection via IQR method; Fare capped at 99th percentile
- Min-max normalization on Age and Fare
- Categorical encoding: Sex (binary), Pclass (factor), Embarked (one-hot)
- Exploratory analysis: summary statistics, correlation matrix,
  survival rate by sex/class, visualizations (ggplot2)

---

## Week 2: Data Visualization & Insight Communication

### Files
- `week2_visualization.R` — 8 comprehensive ggplot2 visualizations
- `Week2_Data_Visualization_Report.docx` — full report with insights

### Visualizations Created
1. Overall Survival Count (Bar Chart)
2. Survival Rate by Sex (Grouped Bar Chart)
3. Survival Rate by Passenger Class (Stacked Bar Chart)
4. Age Distribution by Survival (Density Plot)
5. Fare Distribution by Class (Boxplot)
6. Survival Rate by Age Group (Grouped Bar Chart)
7. Survival Rate Heatmap — Sex x Passenger Class
8. Correlation Heatmap of all Numerical Variables

### Key Insights
- Sex was the strongest predictor of survival (r = -0.54)
- Female passengers had 74% survival rate vs 19% for males
- 1st class passengers had 63% survival vs 24% for 3rd class
- 1st-class females had 97% survival; 3rd-class males had only 14%
- Children (0-12) had the highest survival rate (~59%)
- Libraries used: ggplot2, dplyr, reshape2, gridExtra, scales

---

## Tech Stack
![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-E04D27?style=flat)
![GitHub](https://img.shields.io/badge/GitHub-181717?style=flat&logo=github&logoColor=white)

---

**Intern:** Kalyani Nemade | **Organization:** Unified Mentor Pvt. Ltd.