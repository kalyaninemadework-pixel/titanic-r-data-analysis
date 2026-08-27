# ============================================================
# Week 2 Task: Data Visualization and Insight Communication
# Dataset: Titanic (continued from Week 1)
# Libraries: ggplot2, dplyr, reshape2, gridExtra
# Author: Kalyani Nemade | Data Analyst Intern - Unified Mentor
# ============================================================

# ---- 1. Load Libraries ----
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(scales)

# ---- 2. Load & Prepare Data ----
titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# Basic cleaning (from Week 1)
titanic$Age[is.na(titanic$Age)] <- ave(
  titanic$Age, titanic$Pclass, titanic$Sex,
  FUN = function(x) median(x, na.rm = TRUE)
)[is.na(titanic$Age)]

titanic$Embarked[titanic$Embarked == "" | is.na(titanic$Embarked)] <- "S"
titanic$Cabin_Known <- ifelse(titanic$Cabin == "" | is.na(titanic$Cabin), 0, 1)

# Fare outlier capping at 99th percentile
fare_99 <- quantile(titanic$Fare, 0.99, na.rm = TRUE)
titanic$Fare[titanic$Fare > fare_99] <- fare_99

# Factor conversions
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("Did Not Survive", "Survived"))
titanic$Pclass   <- factor(titanic$Pclass, levels = c(1, 2, 3), labels = c("1st Class", "2nd Class", "3rd Class"))
titanic$Sex      <- factor(titanic$Sex)

# Age groups
titanic$AgeGroup <- cut(titanic$Age,
  breaks = c(0, 12, 18, 35, 60, 100),
  labels = c("Child (0-12)", "Teen (13-18)", "Young Adult (19-35)", "Adult (36-60)", "Senior (60+)"),
  right  = FALSE
)

# ============================================================
# VISUALIZATION 1: Survival Count â€” Bar Chart
# ============================================================
plot1 <- ggplot(titanic, aes(x = Survived, fill = Survived)) +
  geom_bar(width = 0.5, color = "white") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#E74C3C", "#2ECC71")) +
  labs(
    title    = "Plot 1: Overall Survival Count",
    subtitle = "549 passengers did not survive; 342 survived",
    x        = "Survival Status",
    y        = "Number of Passengers",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        legend.position = "none")

ggsave("plot1_survival_count.png", plot1, width = 7, height = 5, dpi = 150)

# ============================================================
# VISUALIZATION 2: Survival Rate by Sex â€” Grouped Bar
# ============================================================
sex_surv <- titanic %>%
  group_by(Sex, Survived) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Sex) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

plot2 <- ggplot(sex_surv, aes(x = Sex, y = Percentage, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.6) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_dodge(width = 0.6), vjust = -0.4, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("#E74C3C", "#2ECC71")) +
  labs(
    title    = "Plot 2: Survival Rate by Sex",
    subtitle = "Females had a 74% survival rate vs. 19% for males",
    x        = "Gender",
    y        = "Survival Rate (%)",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15))

ggsave("plot2_survival_by_sex.png", plot2, width = 7, height = 5, dpi = 150)

# ============================================================
# VISUALIZATION 3: Survival Rate by Passenger Class â€” Stacked Bar
# ============================================================
class_surv <- titanic %>%
  group_by(Pclass, Survived) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Pclass) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

plot3 <- ggplot(class_surv, aes(x = Pclass, y = Percentage, fill = Survived)) +
  geom_bar(stat = "identity", color = "white", width = 0.6) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5), size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("#E74C3C", "#2ECC71")) +
  labs(
    title    = "Plot 3: Survival Rate by Passenger Class",
    subtitle = "1st class passengers had 63% survival; 3rd class had only 24%",
    x        = "Passenger Class",
    y        = "Percentage (%)",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15))

ggsave("plot3_survival_by_class.png", plot3, width = 7, height = 5, dpi = 150)

# ============================================================
# VISUALIZATION 4: Age Distribution by Survival â€” Density Plot
# ============================================================
plot4 <- ggplot(titanic, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.6, color = NA) +
  scale_fill_manual(values = c("#E74C3C", "#2ECC71")) +
  geom_vline(xintercept = median(titanic$Age[titanic$Survived == "Survived"], na.rm = TRUE),
             color = "#27AE60", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median(titanic$Age[titanic$Survived == "Did Not Survive"], na.rm = TRUE),
             color = "#C0392B", linetype = "dashed", linewidth = 1) +
  labs(
    title    = "Plot 4: Age Distribution by Survival Status",
    subtitle = "Children (age < 10) had higher survival rates; middle-aged adults had lower",
    x        = "Age (Years)",
    y        = "Density",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15))

ggsave("plot4_age_distribution.png", plot4, width = 8, height = 5, dpi = 150)

# ============================================================
# VISUALIZATION 5: Fare Distribution by Class â€” Boxplot
# ============================================================
plot5 <- ggplot(titanic, aes(x = Pclass, y = Fare, fill = Pclass)) +
  geom_boxplot(outlier.color = "darkred", outlier.shape = 16, alpha = 0.7) +
  scale_fill_manual(values = c("#3498DB", "#F39C12", "#9B59B6")) +
  labs(
    title    = "Plot 5: Fare Distribution by Passenger Class",
    subtitle = "1st class fares were significantly higher and more variable",
    x        = "Passenger Class",
    y        = "Fare (GBP)",
    fill     = "Class"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        legend.position = "none")

ggsave("plot5_fare_by_class.png", plot5, width = 7, height = 5, dpi = 150)

# ============================================================
# VISUALIZATION 6: Survival by Age Group â€” Grouped Bar
# ============================================================
age_surv <- titanic %>%
  filter(!is.na(AgeGroup)) %>%
  group_by(AgeGroup, Survived) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(AgeGroup) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

plot6 <- ggplot(age_surv, aes(x = AgeGroup, y = Percentage, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_dodge(width = 0.7), vjust = -0.4, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("#E74C3C", "#2ECC71")) +
  labs(
    title    = "Plot 6: Survival Rate by Age Group",
    subtitle = "Children had the highest survival rate; Seniors had the lowest",
    x        = "Age Group",
    y        = "Survival Rate (%)",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title  = element_text(face = "bold", size = 15),
        axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("plot6_survival_by_agegroup.png", plot6, width = 9, height = 5, dpi = 150)

# ============================================================
# VISUALIZATION 7: Heatmap â€” Survival Rate by Sex & Class
# ============================================================
heatmap_data <- titanic %>%
  group_by(Sex, Pclass) %>%
  summarise(SurvivalRate = mean(Survived == "Survived") * 100, .groups = "drop")

plot7 <- ggplot(heatmap_data, aes(x = Pclass, y = Sex, fill = SurvivalRate)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(round(SurvivalRate, 1), "%")), size = 6, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#E74C3C", high = "#2ECC71", name = "Survival %") +
  labs(
    title    = "Plot 7: Survival Rate Heatmap â€” Sex Ã— Passenger Class",
    subtitle = "1st class females had the highest survival (97%); 3rd class males had lowest (14%)",
    x        = "Passenger Class",
    y        = "Gender"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15))

ggsave("plot7_heatmap_sex_class.png", plot7, width = 8, height = 5, dpi = 150)

# ============================================================
# VISUALIZATION 8: Correlation Heatmap
# ============================================================
num_cols <- titanic %>%
  mutate(Survived_num = as.numeric(Survived) - 1,
         Sex_num      = ifelse(Sex == "male", 1, 0),
         Pclass_num   = as.numeric(Pclass)) %>%
  select(Survived_num, Pclass_num, Age, Fare, SibSp, Parch, Sex_num, Cabin_Known)

cor_matrix  <- round(cor(num_cols, use = "complete.obs"), 2)
cor_melted  <- melt(cor_matrix)

plot8 <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#E74C3C", mid = "white", high = "#2ECC71",
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  labs(
    title    = "Plot 8: Correlation Heatmap of Numerical Variables",
    subtitle = "Sex_num and Pclass_num show strongest correlation with survival",
    x = "", y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title  = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 35, hjust = 1))

ggsave("plot8_correlation_heatmap.png", plot8, width = 8, height = 7, dpi = 150)

# ============================================================
# DONE â€” All 8 plots saved as PNG files!
# ============================================================
cat("\nâœ… All 8 visualizations saved successfully!\n")
cat("Files: plot1 to plot8 PNG files in working directory\n")
