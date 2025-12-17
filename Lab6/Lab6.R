## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(randomForest)
library(rpart)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/matec/DataAnalytics/Lab6/")

df <- read.csv("C:/Users/matec/DataAnalytics/lab6/NY-House-Dataset.csv")

## keep only variables needed
df <- df[, c("PRICE", "PROPERTYSQFT")]

## remove missing values
df <- na.omit(df)

## remove outliers based on IQR
Q1 <- quantile(df$PRICE, 0.25)
Q3 <- quantile(df$PRICE, 0.75)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

df <- df[df$PRICE >= lower_bound & df$PRICE <= upper_bound, ]

# test train split
N <- nrow(df)
train.indexes <- sample(N,0.8*N)

train <- df[train.indexes,]
test <- df[-train.indexes,]

# Train a linear model
lm.mod <- lm(PRICE ~ ., data = train)

lm.pred <- predict(lm.mod, test)

# Train a Decision Tree
ctrl <- trainControl(method="cv", number=5)  # 5-fold cross-validation
tree.mod <- train(
  PRICE ~ ., 
  data = train,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
)

tree.pred <- predict(tree.mod, test)

# Train a SVM
svm.mod <- svm(
  PRICE ~ .,
  data = train,
  type = "eps-regression",
  kernel = "radial"
)

svm.pred <- predict(svm.mod, test)

# Linear model results
lm_mae  <- mean(abs(test$PRICE - lm.pred))
lm_mse  <- mean((test$PRICE - lm.pred)^2)
lm_rmse <- sqrt(lm_mse)
lm_mae
lm_mse
lm_rmse

# tree model results
tree_mae  <- mean(abs(test$PRICE - tree.pred))
tree_mse  <- mean((test$PRICE - tree.pred)^2)
tree_rmse <- sqrt(tree_mse)
tree_mae
tree_mse
tree_rmse

# SVM model results
svm_mae  <- mean(abs(test$PRICE - svm.pred))
svm_mse  <- mean((test$PRICE - svm.pred)^2)
svm_rmse <- sqrt(svm_mse)
svm_mae
svm_mse
svm_rmse

