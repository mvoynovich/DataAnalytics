
library(caret)
library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)
library(dplyr)

setwd("C:/Users/matec/DataAnalytics/Assignment5")

city.data <- read.csv("C:/Users/matec/DataAnalytics/Assignment5/NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

manhattan.data <- city.data[city.data$BOROUGH == 'MANHATTAN', ]

num_cols <- sapply(manhattan.data, is.numeric)
numeric_data <- manhattan.data[, num_cols]

# Remove columns with all NAs or non-finite values
numeric_data <- numeric_data[, colSums(is.finite(as.matrix(numeric_data))) > 0]

#ggpairs(
#  numeric_data,
#  aes(color = 'blue')
#)

ggplot(numeric_data, aes(y = SALE.PRICE)) +
  geom_boxplot(outlier.colour = "red") +
  theme_minimal()

# Compute whiskers
Q1 <- quantile(manhattan.data$SALE.PRICE, 0.25, na.rm = TRUE)
Q3 <- quantile(manhattan.data$SALE.PRICE, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

# Whisker limits (exclude extreme outliers)
y_min <- Q1 - 1.5 * IQR_val
y_max <- Q3 + 1.5 * IQR_val

# Plot
ggplot(manhattan.data, aes(y = SALE.PRICE)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  coord_cartesian(ylim = c(y_min, y_max)) +  # set y-axis to whiskers
  theme_minimal() +
  labs(title = "Boxplot of SALE.PRICE Outliers hidden",
       y = "Sale Price")

lm_model <- lm(SALE.PRICE ~ ., data = numeric_data)

# Summary of the model
summary(lm_model)

manhatten_clean <- manhattan.data %>%
  filter(SALE.PRICE >= (Q1 - 1.5 * IQR_val) &
           SALE.PRICE <= (Q3 + 1.5 * IQR_val))

manhatten_clean <- manhattan.data %>%
  filter(SALE.PRICE > 0)

manhatten_clean <- manhatten_clean %>%
  mutate(
    NEIGHBORHOOD = as.factor(NEIGHBORHOOD),
    BUILDING.CLASS.CATEGORY = as.factor(BUILDING.CLASS.CATEGORY),
  )

manhatten_clean <- manhatten_clean %>%
  mutate(
    LAND.SQUARE.FEET = as.integer(LAND.SQUARE.FEET),
    GROSS.SQUARE.FEET = as.integer(GROSS.SQUARE.FEET)
  )

manhatten_clean <- manhatten_clean %>%
  mutate(logPrice = log(SALE.PRICE))

lm_log_model <- lm(SALE.PRICE ~ YEAR.BUILT + NEIGHBORHOOD + BUILDING.CLASS.CATEGORY + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = manhatten_clean)

summary(lm_log_model)

ggplot(manhatten_clean, aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point() +
  stat_smooth(method = "lm")+ labs(title='Linear Model')

ggplot(lm_log_model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

# Do Neighborhood Classification
manhatten_clean2 <- manhattan.data %>%
  filter(SALE.PRICE >= (Q1 - 1.5 * IQR_val) &
           SALE.PRICE <= (Q3 + 1.5 * IQR_val))

manhatten_clean2 <- manhatten_clean2 %>%
  filter(SALE.PRICE > 0)

manhatten_clean2 <- manhatten_clean2 %>%
  mutate(
    LAND.SQUARE.FEET = as.integer(LAND.SQUARE.FEET),
    GROSS.SQUARE.FEET = as.integer(GROSS.SQUARE.FEET)
  )

model_columns <- c("NEIGHBORHOOD", "YEAR.BUILT", "RESIDENTIAL.UNITS", 
                   "COMMERCIAL.UNITS", "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", 
                   "SALE.PRICE")

# 1. Select only the necessary columns and use na.omit() to remove any row 
#    that has an NA in ANY of these columns.
manhatten_clean2 <- manhatten_clean2 %>%
  select(all_of(model_columns)) %>%
  na.omit()

split.rat <- 0.7
train.indexes <- sample(3118,split.rat*3118)

train <- manhatten_clean2[train.indexes,]
test <- manhatten_clean2[-train.indexes,]

metric <- "Accuracy"

## train models
mod.knn <- train(NEIGHBORHOOD~YEAR.BUILT + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + LAND.SQUARE.FEET + GROSS.SQUARE.FEET + SALE.PRICE, data=train, method="knn", preProcess = c("center", "scale"), metric=metric)
mod.rf <- train(NEIGHBORHOOD~YEAR.BUILT + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + LAND.SQUARE.FEET + GROSS.SQUARE.FEET + SALE.PRICE, data=train, method="rf", metric=metric)
mod.knn2 <- train(NEIGHBORHOOD~YEAR.BUILT + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + SALE.PRICE, data=train, method="knn", preProcess = c("center", "scale"), metric=metric)
## see results (accuracy)
#results <- resamples(list(knn=mod.knn, knn=mod.knn, rf=mod.rf))
#summary(results)

knn.train.true <- train$NEIGHBORHOOD
knn.test.true  <- test$NEIGHBORHOOD

# Raw predictions
train.pred.raw <- predict(mod.knn, train[,-1])
test.pred.raw  <- predict(mod.knn, test[,-1])

# Ensure factor levels are consistent with the original training data
knn.train.predicted <- factor(train.pred.raw, levels = unique(train$NEIGHBORHOOD))
knn.test.predicted  <- factor(test.pred.raw,  levels = unique(train$NEIGHBORHOOD))

# --- Confusion Matrices ---
train.cm <- table(Actual = knn.train.true, Predicted = knn.train.predicted)
test.cm  <- table(Actual = knn.test.true,  Predicted = knn.test.predicted)

# Print matrices
print(train.cm)

# Pad confusion matrix if missing predicted levels
all_classes <- union(rownames(train.cm), colnames(train.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(train.cm), colnames(train.cm)] <- train.cm

# Now safely compute diag, precision, recall, f1
diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

accuracy <- sum(diag_vals) / sum(train.cm)
accuracy

precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
f1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                    NA, 2 * precision * recall / (precision + recall))

# Combine into dataframe
metrics <- data.frame(
  Class = all_classes,
  Recall = round(recall, 3),
  Precision = round(precision, 3),
  F1 = round(f1, 3)
)
# Print test cm
print(test.cm)

# Pad confusion matrix if missing predicted levels
all_classes <- union(rownames(test.cm), colnames(test.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(test.cm), colnames(test.cm)] <- test.cm

# Now safely compute diag, precision, recall, f1
diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

accuracy <- sum(diag_vals) / sum(test.cm)
accuracy

precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
f1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                    NA, 2 * precision * recall / (precision + recall))

# Combine into dataframe
metrics <- data.frame(
  Class = all_classes,
  Recall = round(recall, 3),
  Precision = round(precision, 3),
  F1 = round(f1, 3)
)

print(metrics)


# Evaluate RF
rf.train.true <- train$NEIGHBORHOOD
rf.test.true  <- test$NEIGHBORHOOD

# Raw predictions
train.pred.raw <- predict(mod.rf, train[,-1])
test.pred.raw  <- predict(mod.rf, test[,-1])

# Ensure factor levels are consistent with the original training data
rf.train.predicted <- factor(train.pred.raw, levels = unique(train$NEIGHBORHOOD))
rf.test.predicted  <- factor(test.pred.raw,  levels = unique(train$NEIGHBORHOOD))

# --- Confusion Matrices ---
train.cm <- table(Actual = rf.train.true, Predicted = rf.train.predicted)
test.cm  <- table(Actual = rf.test.true,  Predicted = rf.test.predicted)

# Print matrices
print(train.cm)


# Pad confusion matrix if missing predicted levels
all_classes <- union(rownames(train.cm), colnames(train.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(train.cm), colnames(train.cm)] <- train.cm

# Now safely compute diag, precision, recall, f1
diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

accuracy <- sum(diag_vals) / sum(train.cm)
accuracy

precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
f1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                    NA, 2 * precision * recall / (precision + recall))

# Combine into dataframe
metrics <- data.frame(
  Class = all_classes,
  Recall = round(recall, 3),
  Precision = round(precision, 3),
  F1 = round(f1, 3)
)
# Print test cm
print(test.cm)

# Pad confusion matrix if missing predicted levels
all_classes <- union(rownames(test.cm), colnames(test.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(test.cm), colnames(test.cm)] <- test.cm

# Now safely compute diag, precision, recall, f1
diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

accuracy <- sum(diag_vals) / sum(test.cm)
accuracy

precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
f1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                    NA, 2 * precision * recall / (precision + recall))

# Combine into dataframe
metrics <- data.frame(
  Class = all_classes,
  Recall = round(recall, 3),
  Precision = round(precision, 3),
  F1 = round(f1, 3)
)

print(metrics)


# Evaluate KNN without sqft
knn2.train.true <- train$NEIGHBORHOOD
knn2.test.true  <- test$NEIGHBORHOOD

# Raw predictions
train.pred.raw <- predict(mod.knn2, train[,-1])
test.pred.raw  <- predict(mod.knn2, test[,-1])

# Ensure factor levels are consistent with the original training data
knn2.train.predicted <- factor(train.pred.raw, levels = unique(train$NEIGHBORHOOD))
knn2.test.predicted  <- factor(test.pred.raw,  levels = unique(train$NEIGHBORHOOD))

# --- Confusion Matrices ---
train.cm <- table(Actual = knn2.train.true, Predicted = knn2.train.predicted)
test.cm  <- table(Actual = knn2.test.true,  Predicted = knn2.test.predicted)

# Print matrices
print(train.cm)

# Pad confusion matrix if missing predicted levels
all_classes <- union(rownames(train.cm), colnames(train.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(train.cm), colnames(train.cm)] <- train.cm

# Now safely compute diag, precision, recall, f1
diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

accuracy <- sum(diag_vals) / sum(train.cm)
accuracy

precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
f1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                    NA, 2 * precision * recall / (precision + recall))

# Combine into dataframe
metrics <- data.frame(
  Class = all_classes,
  Recall = round(recall, 3),
  Precision = round(precision, 3),
  F1 = round(f1, 3)
)
# Print test cm
print(test.cm)

# Pad confusion matrix if missing predicted levels
all_classes <- union(rownames(test.cm), colnames(test.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(test.cm), colnames(test.cm)] <- test.cm

# Now safely compute diag, precision, recall, f1
diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

accuracy <- sum(diag_vals) / sum(test.cm)
accuracy

precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
f1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                    NA, 2 * precision * recall / (precision + recall))

# Combine into dataframe
metrics <- data.frame(
  Class = all_classes,
  Recall = round(recall, 3),
  Precision = round(precision, 3),
  F1 = round(f1, 3)
)

print(metrics)


# Do for queens data set
queens.data <- city.data[city.data$BOROUGH == 'QUEENS', ]

queens.clean <- queens.data %>%
  filter(SALE.PRICE >= (Q1 - 1.5 * IQR_val) &
           SALE.PRICE <= (Q3 + 1.5 * IQR_val)) %>%
  filter(SALE.PRICE > 0) %>%
  mutate(
    NEIGHBORHOOD = as.factor(NEIGHBORHOOD),
    BUILDING.CLASS.CATEGORY = as.factor(BUILDING.CLASS.CATEGORY),
    LAND.SQUARE.FEET = as.integer(LAND.SQUARE.FEET),
    GROSS.SQUARE.FEET = as.integer(GROSS.SQUARE.FEET),
    logPrice = log(SALE.PRICE)
  )

queens.clean2 <- queens.clean %>%
  select(NEIGHBORHOOD, BUILDING.CLASS.CATEGORY, YEAR.BUILT, 
         RESIDENTIAL.UNITS, COMMERCIAL.UNITS, LAND.SQUARE.FEET, 
         GROSS.SQUARE.FEET, SALE.PRICE) %>%
  na.omit()

# Get rid of the factor columns as we cannot use those
lm_numeric <- lm(SALE.PRICE ~ YEAR.BUILT + RESIDENTIAL.UNITS + COMMERCIAL.UNITS + LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = manhatten_clean)

summary(lm_numeric)

queens.clean2$pred_sale_price <- predict(lm_numeric, newdata = queens.clean2)


ggplot(queens.clean2, aes(x = pred_sale_price, y = SALE.PRICE)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Queens: Predicted vs Actual SALE.PRICE",
    x = "Predicted SALE.PRICE (from Manhattan model)",
    y = "Actual SALE.PRICE"
  ) +
  theme_minimal()

queens.clean2$residuals <- queens.clean2$SALE.PRICE - queens.clean2$pred_sale_price

ggplot(queens.clean2, aes(x = pred_sale_price, y = residuals)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Queens: Residuals vs Fitted Values",
    x = "Predicted SALE.PRICE (Fitted Values)",
    y = "Residuals"
  ) +
  theme_minimal()

# True labels
knn.queens.true <- queens.clean2$NEIGHBORHOOD

# Raw predictions
knn.queens.pred <- factor(predict(mod.knn, queens.clean2[,-1]),
                          levels = unique(train$NEIGHBORHOOD))
rf.queens.pred  <- factor(predict(mod.rf, queens.clean2[,-1]),
                          levels = unique(train$NEIGHBORHOOD))
knn2.queens.pred <- factor(predict(mod.knn2, queens.clean2[,-1]),
                          levels = unique(train$NEIGHBORHOOD))

# KNN
knn.cm <- table(Actual = knn.queens.true, Predicted = knn.queens.pred)
all_classes <- union(rownames(knn.cm), colnames(knn.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(knn.cm), colnames(knn.cm)] <- knn.cm

cm_full

diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

knn_accuracy <- sum(diag_vals) / sum(cm_full)
knn_precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
knn_recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
knn_f1        <- ifelse(is.na(knn_precision) | is.na(knn_recall) | 
                          (knn_precision + knn_recall) == 0,
                        NA, 2 * knn_precision * knn_recall / (knn_precision + knn_recall))

knn_metrics <- data.frame(
  Class = all_classes,
  Recall = round(knn_recall, 3),
  Precision = round(knn_precision, 3),
  F1 = round(knn_f1, 3)
)

# RF
rf.cm <- table(Actual = knn.queens.true, Predicted = rf.queens.pred)
all_classes <- union(rownames(rf.cm), colnames(rf.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(rf.cm), colnames(rf.cm)] <- rf.cm

cm_full

diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

rf_accuracy <- sum(diag_vals) / sum(cm_full)
rf_precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
rf_recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
rf_f1        <- ifelse(is.na(rf_precision) | is.na(rf_recall) | 
                         (rf_precision + rf_recall) == 0,
                       NA, 2 * rf_precision * rf_recall / (rf_precision + rf_recall))

rf_metrics <- data.frame(
  Class = all_classes,
  Recall = round(rf_recall, 3),
  Precision = round(rf_precision, 3),
  F1 = round(rf_f1, 3)
)

# KNN 2
knn2.cm <- table(Actual = knn.queens.true, Predicted = knn2.queens.pred)
all_classes <- union(rownames(knn2.cm), colnames(knn2.cm))
cm_full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                  dimnames = list(all_classes, all_classes))
cm_full[rownames(knn2.cm), colnames(knn2.cm)] <- knn2.cm

cm_full

diag_vals <- diag(cm_full)
rowsums <- rowSums(cm_full)
colsums <- colSums(cm_full)

knn2_accuracy <- sum(diag_vals) / sum(cm_full)
knn2_precision <- ifelse(colsums == 0, NA, diag_vals / colsums)
knn2_recall    <- ifelse(rowsums == 0, NA, diag_vals / rowsums)
knn2_f1        <- ifelse(is.na(knn2_precision) | is.na(knn2_recall) | 
                          (knn2_precision + knn2_recall) == 0,
                        NA, 2 * knn2_precision * knn2_recall / (knn2_precision + knn2_recall))

knn2_metrics <- data.frame(
  Class = all_classes,
  Recall = round(knn2_recall, 3),
  Precision = round(knn2_precision, 3),
  F1 = round(knn2_f1, 3)
)



print(knn_metrics)
knn_accuracy


print(rf_metrics)
rf_accuracy

knn2_metrics
knn2_accuracy

