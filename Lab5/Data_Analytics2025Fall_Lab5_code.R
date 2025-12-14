
## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/matec/DataAnalytics/Lab5/")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

features <- c("Alcohol", "Flavanoids", "Color Intensity", "Proline")
dataset <- wine[, c(features, "Type")]

N <- nrow(dataset)
train.indexes <- sample(N,0.8*N)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

gamma.range <- seq(0.1,10, .1)
gamma.range

Cost.range <- seq(1,20, 1)
Cost.range

tuned.svm <- tune.svm(Type~., data = train, kernel = 'linear',gamma = gamma.range, cost = Cost.range)
tuned.svm

svm.mod1 <- svm(Type ~ ., data = train, kernel = 'linear', gamma = 0.1, cost = 3)
svm.mod1

tuned.svm <- tune.svm(Type~., data = train, kernel = 'polynomial',gamma = gamma.range, cost = Cost.range)
tuned.svm

svm.mod2 <- svm(Type ~ ., data = train, kernel = 'polynomial', gamma = 0.4, cost = 7)
svm.mod2

grid <- expand.grid(k = c(3, 5, 7, 9, 11))  
ctrl <- trainControl(method = "cv", number = 10)
knn.mod <- train(Type~., data=train, method="knn",  preProcess = c("center", "scale"), tuneGrid = grid, metric="Accuracy", trControl=ctrl)
knn.mod$bestTune

# Test svm model 1
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

#Test svm model 2
test.pred <- predict(svm.mod2, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

# Test knn
test.pred <- predict(knn.mod, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

