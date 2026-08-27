# Titanic Dataset — Complete R Data Analysis (4 Weeks)

End-to-end Data Analyst Internship project: cleaning, visualization,
statistical modeling and comprehensive reporting on the Titanic dataset using R.

![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-E04D27?style=flat)
![Status](https://img.shields.io/badge/Status-Completed-2ECC71?style=flat)

---

## Week 1: Data Cleaning & Preprocessing
- Age: Median imputation grouped by Pclass + Sex
- Cabin: Converted to Cabin_Known binary flag
- Outlier capping (Fare at 99th percentile)
- Min-max normalization on Age & Fare
- Categorical encoding (Sex, Pclass, Embarked)
- **Output:** 18 columns, 0 missing values

**File:** `week1_cleaning_analysis.R`

---

## Week 2: Data Visualization (ggplot2)
8 visualizations created: survival by sex/class/age/fare,
density plots, boxplots, heatmaps, correlation matrix.

**Key insight:** Females had 74% survival vs 19% for males.
1st-class females: 97% survival | 3rd-class males: 14%

**File:** `week2_visualization.R`

---

## Week 3: Statistical Analysis & Predictive Modeling
- 4 Hypothesis Tests (Chi-Square + t-Tests) — all significant
- Logistic Regression: **81.46% accuracy, AUC = 0.8721**
- Decision Tree comparison (79.21% accuracy)
- Feature importance: Sex > Pclass > Cabin_Known > FamilySize

**File:** `week3_statistical_modeling.R`

---

## Week 4: Comprehensive Final Report
Integrated 21-page final report covering all weeks, key findings,
lessons learned, challenges, and future recommendations.

---

## Key Findings
| Factor | Correlation | Survival Rate |
|--------|-------------|---------------|
| Sex (Female) | r = -0.54 | 74.2% |
| Passenger Class | r = -0.34 | 1st: 63%, 3rd: 24% |
| Fare | r = +0.26 | Survivors paid 2.2x more |
| Age | r = -0.08 | Children (0-12): 59% |

---

**Intern:** Kalyani Nemade | **Organization:** Unified Mentor Pvt. Ltd.