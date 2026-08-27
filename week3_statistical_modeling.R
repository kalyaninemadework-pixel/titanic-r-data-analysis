# ============================================================
# Week 3 Task: Statistical Analysis & Predictive Modeling
# Dataset: Titanic (continued from Week 1 & 2)
# Models: Logistic Regression + Decision Tree
# Author: Kalyani Nemade | Data Analyst Intern - Unified Mentor
# ============================================================

# ---- 1. Load Libraries ----
library(ggplot2)
library(dplyr)
library(caret)        # model training & evaluation
library(pROC)         # ROC curve
library(rpart)        # Decision Tree
library(rpart.plot)   # Plot decision tree
library(corrplot)     # Correlation plot

cat("=== WEEK 3: STATISTICAL ANALYSIS & PREDICTIVE MODELING ===\n\n")

# ---- 2. Load & Clean Data ----
titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# Impute Age (median by Pclass + Sex)
titanic$Age <- ave(titanic$Age, titanic$Pclass, titanic$Sex,
                   FUN = function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Fix Embarked
titanic$Embarked[titanic$Embarked == "" | is.na(titanic$Embarked)] <- "S"

# Cabin known flag
titanic$Cabin_Known <- ifelse(titanic$Cabin == "" | is.na(titanic$Cabin), 0, 1)

# Cap Fare at 99th percentile
fare_99 <- quantile(titanic$Fare, 0.99, na.rm = TRUE)
titanic$Fare[titanic$Fare > fare_99] <- fare_99

# Feature Engineering
titanic$FamilySize <- titanic$SibSp + titanic$Parch + 1
titanic$IsAlone    <- ifelse(titanic$FamilySize == 1, 1, 0)
titanic$Sex_num    <- ifelse(titanic$Sex == "male", 1, 0)
titanic$Embarked_Q <- ifelse(titanic$Embarked == "Q", 1, 0)
titanic$Embarked_S <- ifelse(titanic$Embarked == "S", 1, 0)

cat("Dataset Shape:", nrow(titanic), "rows x", ncol(titanic), "columns\n")
cat("Survival Rate:", round(mean(titanic$Survived) * 100, 2), "%\n\n")

# ============================================================
# PART A: STATISTICAL HYPOTHESIS TESTING
# ============================================================
cat("========================================\n")
cat("PART A: HYPOTHESIS TESTING\n")
cat("========================================\n\n")

# ---- Test 1: Chi-Square Test — Survival vs Sex ----
cat("--- Test 1: Chi-Square Test (Survival vs Sex) ---\n")
tbl1 <- table(titanic$Survived, titanic$Sex)
chi1 <- chisq.test(tbl1)
cat("Chi-Square Statistic:", round(chi1$statistic, 4), "\n")
cat("p-value:", format(chi1$p.value, scientific = TRUE), "\n")
cat("Result:", ifelse(chi1$p.value < 0.05,
    "REJECT H0 — Significant association between Sex and Survival",
    "FAIL TO REJECT H0"), "\n\n")

# ---- Test 2: Chi-Square Test — Survival vs Pclass ----
cat("--- Test 2: Chi-Square Test (Survival vs Pclass) ---\n")
tbl2 <- table(titanic$Survived, titanic$Pclass)
chi2 <- chisq.test(tbl2)
cat("Chi-Square Statistic:", round(chi2$statistic, 4), "\n")
cat("p-value:", format(chi2$p.value, scientific = TRUE), "\n")
cat("Result:", ifelse(chi2$p.value < 0.05,
    "REJECT H0 — Significant association between Pclass and Survival",
    "FAIL TO REJECT H0"), "\n\n")

# ---- Test 3: Independent t-Test — Age by Survival ----
cat("--- Test 3: Independent t-Test (Age by Survival) ---\n")
age_surv <- titanic$Age[titanic$Survived == 1]
age_dead  <- titanic$Age[titanic$Survived == 0]
ttest1    <- t.test(age_surv, age_dead)
cat("Mean Age (Survived):", round(mean(age_surv, na.rm = TRUE), 2), "\n")
cat("Mean Age (Not Survived):", round(mean(age_dead, na.rm = TRUE), 2), "\n")
cat("t-statistic:", round(ttest1$statistic, 4), "\n")
cat("p-value:", round(ttest1$p.value, 4), "\n")
cat("Result:", ifelse(ttest1$p.value < 0.05,
    "REJECT H0 — Significant age difference between survivors and non-survivors",
    "FAIL TO REJECT H0 — No significant age difference"), "\n\n")

# ---- Test 4: Independent t-Test — Fare by Survival ----
cat("--- Test 4: Independent t-Test (Fare by Survival) ---\n")
fare_surv <- titanic$Fare[titanic$Survived == 1]
fare_dead  <- titanic$Fare[titanic$Survived == 0]
ttest2    <- t.test(fare_surv, fare_dead)
cat("Mean Fare (Survived):", round(mean(fare_surv, na.rm = TRUE), 2), "\n")
cat("Mean Fare (Not Survived):", round(mean(fare_dead, na.rm = TRUE), 2), "\n")
cat("t-statistic:", round(ttest2$statistic, 4), "\n")
cat("p-value:", format(ttest2$p.value, scientific = TRUE), "\n")
cat("Result:", ifelse(ttest2$p.value < 0.05,
    "REJECT H0 — Survivors paid significantly higher fares",
    "FAIL TO REJECT H0"), "\n\n")

# ============================================================
# PART B: LOGISTIC REGRESSION MODEL
# ============================================================
cat("========================================\n")
cat("PART B: LOGISTIC REGRESSION MODEL\n")
cat("========================================\n\n")

# ---- Feature Selection ----
model_data <- titanic %>%
  select(Survived, Pclass, Sex_num, Age, Fare,
         FamilySize, IsAlone, Cabin_Known,
         Embarked_Q, Embarked_S) %>%
  na.omit()

# Train/Test Split (80/20)
set.seed(42)
train_idx <- createDataPartition(model_data$Survived, p = 0.8, list = FALSE)
train_data <- model_data[train_idx, ]
test_data  <- model_data[-train_idx, ]
cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n\n")

# ---- Fit Logistic Regression ----
log_model <- glm(Survived ~ ., data = train_data, family = binomial(link = "logit"))
cat("--- Logistic Regression Summary ---\n")
print(summary(log_model))

# ---- Predictions ----
pred_prob  <- predict(log_model, test_data, type = "response")
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)

# ---- Confusion Matrix ----
cat("\n--- Confusion Matrix ---\n")
cm <- confusionMatrix(factor(pred_class), factor(test_data$Survived), positive = "1")
print(cm)

# ---- Extract Key Metrics ----
accuracy  <- round(cm$overall["Accuracy"] * 100, 2)
precision <- round(cm$byClass["Precision"] * 100, 2)
recall    <- round(cm$byClass["Recall"] * 100, 2)
f1        <- round(cm$byClass["F1"] * 100, 2)
cat("\n--- Model Performance Metrics ---\n")
cat("Accuracy :", accuracy, "%\n")
cat("Precision:", precision, "%\n")
cat("Recall   :", recall, "%\n")
cat("F1 Score :", f1, "%\n\n")

# ---- ROC Curve ----
roc_obj <- roc(test_data$Survived, pred_prob)
auc_val <- round(auc(roc_obj), 4)
cat("AUC (Area Under ROC Curve):", auc_val, "\n\n")

# Save ROC plot
png("plot_roc_curve.png", width = 700, height = 550, res = 120)
plot(roc_obj, col = "#2ECC71", lwd = 2.5,
     main = paste("ROC Curve — Logistic Regression (AUC =", auc_val, ")"),
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")
abline(a = 0, b = 1, lty = 2, col = "#E74C3C")
legend("bottomright", legend = paste("AUC =", auc_val), col = "#2ECC71", lwd = 2)
dev.off()

# ---- Odds Ratios ----
cat("--- Odds Ratios (Key Predictors) ---\n")
odds_ratios <- exp(coef(log_model))
print(round(odds_ratios, 4))

# ============================================================
# PART C: DECISION TREE MODEL (Comparison)
# ============================================================
cat("\n========================================\n")
cat("PART C: DECISION TREE MODEL\n")
cat("========================================\n\n")

dt_model <- rpart(Survived ~ ., data = train_data, method = "class",
                  control = rpart.control(maxdepth = 5, minsplit = 20))

dt_pred_prob  <- predict(dt_model, test_data, type = "prob")[, 2]
dt_pred_class <- ifelse(dt_pred_prob >= 0.5, 1, 0)
dt_cm <- confusionMatrix(factor(dt_pred_class), factor(test_data$Survived), positive = "1")

dt_accuracy <- round(dt_cm$overall["Accuracy"] * 100, 2)
cat("Decision Tree Accuracy:", dt_accuracy, "%\n")
cat("Logistic Regression Accuracy:", accuracy, "%\n")
cat("Better Model:", ifelse(accuracy > dt_accuracy, "Logistic Regression", "Decision Tree"), "\n\n")

# Save Decision Tree plot
png("plot_decision_tree.png", width = 900, height = 600, res = 120)
rpart.plot(dt_model, type = 4, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree — Titanic Survival Prediction",
           box.palette = c("#E74C3C", "#2ECC71"))
dev.off()

# ============================================================
# PART D: FEATURE IMPORTANCE
# ============================================================
cat("========================================\n")
cat("PART D: FEATURE IMPORTANCE\n")
cat("========================================\n\n")

importance_df <- data.frame(
  Feature    = names(coef(log_model))[-1],
  Coefficient = abs(coef(log_model)[-1])
) %>% arrange(desc(Coefficient))

cat("Feature Importance (by absolute coefficient):\n")
print(importance_df)

# Save Feature Importance Plot
imp_plot <- ggplot(importance_df, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Coefficient)) +
  geom_bar(stat = "identity", color = "white") +
  coord_flip() +
  scale_fill_gradient(low = "#F39C12", high = "#E74C3C") +
  labs(
    title    = "Feature Importance — Logistic Regression",
    subtitle = "Based on absolute value of regression coefficients",
    x        = "Feature",
    y        = "Absolute Coefficient Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

ggsave("plot_feature_importance.png", imp_plot, width = 8, height = 5, dpi = 150)

# ============================================================
# FINAL SUMMARY
# ============================================================
cat("\n========================================\n")
cat("FINAL MODEL SUMMARY\n")
cat("========================================\n")
cat("Model              : Logistic Regression\n")
cat("Training Size      :", nrow(train_data), "records\n")
cat("Test Size          :", nrow(test_data), "records\n")
cat("Accuracy           :", accuracy, "%\n")
cat("Precision          :", precision, "%\n")
cat("Recall             :", recall, "%\n")
cat("F1 Score           :", f1, "%\n")
cat("AUC                :", auc_val, "\n")
cat("Best Predictors    : Sex, Pclass, Fare, Age\n")
cat("=========================================\n")
cat("All plots saved as PNG files.\n")
