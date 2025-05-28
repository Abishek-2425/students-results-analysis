# === COLLEGE EXAM RESULTS ANALYSIS SCRIPT ===

# Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(gridExtra)
library(scales)

# === 1. Load Dataset ===
cat("=== LOADING DATASET ===\n")
data <- read.csv("G:/COLLEGE/SEM 4/FDSR/SEM PROEJCT/ABISHEK S/results.csv")
cat("Dataset loaded successfully!\n")

# === 2. Basic Dataset Info ===
cat("Number of students:", nrow(data), "\n")
cat("Number of subjects:", ncol(data) - 1, "\n\n")  # excluding Student_ID

# === 3. Dataset Overview ===
cat("=== DATASET OVERVIEW ===\n")
print(head(data, 10))
cat("\n")

# === 4. Total Marks and Result Classification ===
data$Total <- rowSums(data[ , 2:7])  # subjects are in columns 2 to 7

# Result: 1 = Pass, 0 = Fail (less than 35 in any subject)
data$Results <- ifelse(apply(data[, 2:7], 1, function(row) all(row >= 35)), 1, 0)

# Division Classification (only for Passed students)
data$Div <- ifelse(data$Results == 0, "No Division",
                   ifelse(data$Total >= 360, "First Division",
                          ifelse(data$Total >= 300, "Second Division", "Third Division")))

data$Results_Label <- factor(data$Results, levels = c(0, 1), labels = c("Failed", "Passed"))

# === 5. Summary Statistics ===
cat("=== STATISTICAL SUMMARY - SUBJECT SCORES ===\n")
print(summary(data[ , 2:7]))
cat("\n")

# === 6. Result and Division Analysis ===
cat("=== RESULTS DISTRIBUTION ===\n")
print(table(data$Results_Label))
cat("Pass Rate:", round(mean(data$Results) * 100, 1), "%\n\n")

cat("=== DIVISION DISTRIBUTION ===\n")
print(table(data$Div))
cat(round(prop.table(table(data$Div)) * 100, 1), "\n\n")

# === 7. Subject-wise Mean Performance ===
cat("=== SUBJECT-WISE PERFORMANCE ===\n")
print(data %>% 
        select(Hindi, English, Science, Maths, History, Geography) %>% 
        summarise(across(everything(), mean)))
cat("\n")

# === 8. Correlation Analysis ===
cat("=== CORRELATION ANALYSIS ===\n")
cor_matrix <- round(cor(data[ , 2:7]), 3)
print(cor_matrix)
cat("\n")

# === 9. Visualizations ===
# (1) Histogram of Total Marks
g1 <- ggplot(data, aes(x = Total)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Marks", x = "Total Marks", y = "Number of Students") +
  theme_minimal()

# (2) Bar Plot of Results
g2 <- ggplot(data, aes(x = Results_Label, fill = Results_Label)) +
  geom_bar() +
  labs(title = "Pass vs Fail Count", x = "Result", y = "Count") +
  theme_minimal() + scale_fill_manual(values = c("red", "green"))

# (3) Bar Plot of Divisions
g3 <- ggplot(data, aes(x = Div, fill = Div)) +
  geom_bar() +
  labs(title = "Division Classification", x = "Division", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (4) Boxplot for Subject Scores (to spot outliers)
g4 <- data %>%
  pivot_longer(cols = Hindi:Geography, names_to = "Subject", values_to = "Marks") %>%
  ggplot(aes(x = Subject, y = Marks, fill = Subject)) +
  geom_boxplot() +
  labs(title = "Boxplot of Subject Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (5) Heatmap for Correlation Matrix
library(reshape2)
cor_melt <- melt(cor_matrix)
g5 <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap of Subjects", x = "Subject", y = "Subject") +
  theme_minimal()

# (6) Pass Percentage by Subject
g6 <- data %>%
  mutate(across(Hindi:Geography, ~. >= 35)) %>%
  summarise(across(Hindi:Geography, ~mean(.))) %>%
  pivot_longer(cols = everything(), names_to = "Subject", values_to = "PassRate") %>%
  ggplot(aes(x = Subject, y = PassRate, fill = Subject)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Pass Percentage by Subject", x = "Subject", y = "Pass Rate") +
  theme_minimal()

# Arrange plots in grid
grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2)

cat("\n=== ANALYSIS COMPLETE ===\n")
