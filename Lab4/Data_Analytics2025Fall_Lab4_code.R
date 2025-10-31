##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/matec/DataAnalytics/Lab4")

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

### use prcomp since its the modern version of princomp
Xmat <- as.matrix(wine[, -1])
Xc <- scale(Xmat, center = TRUE, scale = TRUE)

pca <- prcomp(Xc)
summary(pca)

pc_scores <- pca$x[, 1:2]
head(pc_scores)

pc_loadings <- pca$rotation[, 1:2]
pc_loadings

plot(pca, type = "l")
biplot(pca)

autoplot(pca, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

# Do KNN for all variables
split.rat <- 0.7
train.indexes <- sample(178,split.rat*178)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

ctrl <- trainControl(method = "cv", number = 10)

knn.all <- train(Type~., data=train, 
                         method="knn",  preProcess = c("center", "scale"), metric="Accuracy", trControl=ctrl)

knn.train.true <- train$Type
knn.test.true <- test$Type

knn.train.predicted <- predict(knn.all,train[,-1])
knn.test.predicted <- predict(knn.all,test[,-1])

train.cm = as.matrix(table(Actual = knn.train.true, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(train) 
train.accuracy

n = sum(train.cm) # number of instances
nc = nrow(train.cm) # number of classes
diag = diag(train.cm) # number of correctly classified instances per class 
rowsums = apply(train.cm, 1, sum) # number of instances per class
colsums = apply(train.cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n
accuracy

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

test.cm = as.matrix(table(Actual = knn.test.true, Predicted = knn.test.predicted))
test.cm 
test.accuracy <- sum(diag(test.cm))/nrow(test)
test.accuracy

n = sum(test.cm) # number of instances
nc = nrow(test.cm) # number of classes
diag = diag(test.cm) # number of correctly classified instances per class 
rowsums = apply(test.cm, 1, sum) # number of instances per class
colsums = apply(test.cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n
accuracy

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

# Do KNN for the first two principle components
pc.data <- as.data.frame(pca$x[, 1:2])
pc.data$Type <- as.factor(wine$Type)

split.rat <- 0.7
train.indexes <- sample(178,split.rat*178)

train <- pc.data[train.indexes,]
test <- pc.data[-train.indexes,]

knn.pca <- train(Type~., data=train, 
                 method="knn",  preProcess = c("center", "scale"), metric="Accuracy", trControl=ctrl)

knn.train.true <- train$Type
knn.test.true <- test$Type

knn.train.predicted <- predict(knn.pca,train[,-3])
knn.test.predicted <- predict(knn.pca,test[,-3])

train.cm = as.matrix(table(Actual = knn.train.true, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(train) 
train.accuracy

n = sum(train.cm) # number of instances
nc = nrow(train.cm) # number of classes
diag = diag(train.cm) # number of correctly classified instances per class 
rowsums = apply(train.cm, 1, sum) # number of instances per class
colsums = apply(train.cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n
accuracy

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

test.cm = as.matrix(table(Actual = knn.test.true, Predicted = knn.test.predicted))
test.cm 
test.accuracy <- sum(diag(test.cm))/nrow(test)
test.accuracy

n = sum(test.cm) # number of instances
nc = nrow(test.cm) # number of classes
diag = diag(test.cm) # number of correctly classified instances per class 
rowsums = apply(test.cm, 1, sum) # number of instances per class
colsums = apply(test.cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n
accuracy

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

