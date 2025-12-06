
library(caret)
library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(dplyr)
library(tidyverse)
library(randomForest)

setwd("C:/Users/matec/DataAnalytics/Assignment6")

df <- read.csv("C:/Users/matec/DataAnalytics/Assignment6/bank-additional-full.csv", sep=";")

df <- df %>%
  mutate(across(where(is.character), as.factor))

str(df)
summary(df)

# Code/reference for data transformation for the EDA plots
# tidyr::pivot_longer(): https://tidyr.tidyverse.org/reference/pivot_longer.html
num_vars <- df %>% select(where(is.numeric))

num_vars_long <- num_vars %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

# Plotting distributions with ggplot2 and faceting
# facet_wrap docs: https://ggplot2.tidyverse.org/reference/facet_wrap.html
ggplot(num_vars_long, aes(value)) +
  geom_histogram(bins = 30, color="black") +
  facet_wrap(~ variable, scales = "free")

cat_vars <- df %>% select(where(is.factor))

cat_vars_long <- cat_vars %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

# Plotting distributions with ggplot2 and faceting
# facet_wrap docs: https://ggplot2.tidyverse.org/reference/facet_wrap.html
ggplot(cat_vars_long, aes(value)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free")

ggplot(df, aes(y)) + geom_bar(fill = "steelblue")

# Convert 'y' to factor just in case
df$y <- as.factor(df$y)

# -----------------------------
# Split data into train/test
# -----------------------------
train_index <- createDataPartition(df$y, p = 0.7, list = FALSE)
train <- df[train_index, ]
test <- df[-train_index, ]


# 1. Random Forest Classification for y
rf_class <- randomForest(y ~ ., data = train, importance = TRUE, ntree = 500)

# Predictions
pred_train <- predict(rf_class, train)
pred_test <- predict(rf_class, test)


# Evaluate Training Set
cm_train <- confusionMatrix(pred_train, train$y)
accuracy_train <- cm_train$overall['Accuracy']
precision_train <- cm_train$byClass['Pos Pred Value']  # precision for positive class
recall_train <- cm_train$byClass['Sensitivity']       # recall for positive class
f1_train <- 2 * ((precision_train * recall_train) / (precision_train + recall_train))

cat("Classification Performance - Training Set\n")
print(cm_train$table)  # print confusion matrix
cat("Accuracy:", accuracy_train, "\n")
cat("Precision:", precision_train, "\n")
cat("Recall:", recall_train, "\n")
cat("F1 Score:", f1_train, "\n\n")


# Evaluate Test Set
cm_test <- confusionMatrix(pred_test, test$y)
accuracy_test <- cm_test$overall['Accuracy']
precision_test <- cm_test$byClass['Pos Pred Value']
recall_test <- cm_test$byClass['Sensitivity']
f1_test <- 2 * ((precision_test * recall_test) / (precision_test + recall_test))

cat("Classification Performance - Test Set\n")
print(cm_test$table)  # print confusion matrix
cat("Accuracy:", accuracy_test, "\n")
cat("Precision:", precision_test, "\n")
cat("Recall:", recall_test, "\n")
cat("F1 Score:", f1_test, "\n")

# 2. Logistic Regression for y
logit_model <- glm(y ~ ., data = train, family = binomial)

# Predictions (probabilities)
pred_prob_train_logit <- predict(logit_model, train, type = "response")
pred_prob_test_logit  <- predict(logit_model, test, type = "response")

# Convert probabilities to class
pred_train_logit <- ifelse(pred_prob_train_logit > 0.5, "yes", "no") %>% factor(levels = levels(df$y))
pred_test_logit  <- ifelse(pred_prob_test_logit > 0.5, "yes", "no") %>% factor(levels = levels(df$y))

# Evaluate Training Set
cm_train_logit <- confusionMatrix(pred_train_logit, train$y)
accuracy_train_logit <- cm_train_logit$overall['Accuracy']
precision_train_logit <- cm_train_logit$byClass['Pos Pred Value']
recall_train_logit <- cm_train_logit$byClass['Sensitivity']
f1_train_logit <- 2 * ((precision_train_logit * recall_train_logit) / (precision_train_logit + recall_train_logit))

cat("Logistic Regression - Training Set\n")
print(cm_train_logit$table)
cat("Accuracy:", accuracy_train_logit, "\n")
cat("Precision:", precision_train_logit, "\n")
cat("Recall:", recall_train_logit, "\n")
cat("F1 Score:", f1_train_logit, "\n\n")

# Evaluate Test Set
cm_test_logit <- confusionMatrix(pred_test_logit, test$y)
accuracy_test_logit <- cm_test_logit$overall['Accuracy']
precision_test_logit <- cm_test_logit$byClass['Pos Pred Value']
recall_test_logit <- cm_test_logit$byClass['Sensitivity']
f1_test_logit <- 2 * ((precision_test_logit * recall_test_logit) / (precision_test_logit + recall_test_logit))

cat("Logistic Regression - Test Set\n")
print(cm_test_logit$table)
cat("Accuracy:", accuracy_test_logit, "\n")
cat("Precision:", precision_test_logit, "\n")
cat("Recall:", recall_test_logit, "\n")
cat("F1 Score:", f1_test_logit, "\n\n")

# 3. Random Forest Regression for campaign
rf_reg <- randomForest(campaign ~ ., data = train, importance = TRUE, ntree = 500)

# Predictions
pred_train_reg <- predict(rf_reg, train)
pred_test_reg  <- predict(rf_reg, test)

# Evaluate regression
mse_train <- mean((train$campaign - pred_train_reg)^2)
mse_test  <- mean((test$campaign - pred_test_reg)^2)

cat("Regression MSE - Training Set:", mse_train, "\n")
cat("Regression MSE - Test Set:", mse_test, "\n")



