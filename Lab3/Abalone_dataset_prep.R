####################################
##### Abalone Data Preparation #####
####################################

library(caret)
library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

# read dataset
abalone.data <- read.csv("C:/Users/matec/DataAnalytics/Lab3/abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone.data$age.group[abalone.data$rings<=8] <- "young"
abalone.data$age.group[abalone.data$rings>8 & abalone.data$rings<=11] <- "adult"
abalone.data$age.group[abalone.data$rings>11 & abalone.data$rings<=35] <- "old"

## split train/test
split.rat <- 0.7
train.indexes <- sample(150,split.rat*150)

train <- abalone.data[train.indexes,]
test <- abalone.data[-train.indexes,]

# Set up the control for 10 fold cross validation to prevent overfitting when picking best k
ctrl <- trainControl(method = "cv", number = 10)

# KNN trained off of weights
# Use preprocess so all values weighted equally and scaled
mod.knn.weights <- train(age.group~whole_weight + shucked_wieght + viscera_wieght + shell_weight, data=train, 
                         method="knn",  preProcess = c("center", "scale"), metric="Accuracy", trControl=ctrl)

knn.train.true <- train[,10]
knn.test.true <- test[,10]

knn.train.predicted <- predict(mod.knn.weights,train[,-10])
knn.test.predicted <- predict(mod.knn.weights,test[,-10])

# Get accuracy for the weight based classifier
train.cm = as.matrix(table(Actual = knn.train.true, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(train) 
train.accuracy

test.cm = as.matrix(table(Actual = knn.test.true, Predicted = knn.test.predicted))
test.cm 
test.accuracy <- sum(diag(test.cm))/nrow(test)
test.accuracy

# KNN trained off of measurements
# Use preprocess so all values weighted equally and scaled
mod.knn.meas <- train(age.group~length + diameter + height + rings, data=train, method="knn",  
                      preProcess = c("center", "scale"), metric="Accuracy", trControl=ctrl)

knn.train.true <- train[,10]
knn.test.true <- test[,10]

knn.train.predicted <- predict(mod.knn.meas,train[,-10])
knn.test.predicted <- predict(mod.knn.meas,test[,-10])

knn.train.predicted <- predict(mod.knn.meas,train[,-10])
knn.test.predicted <- predict(mod.knn.meas,test[,-10])

# Get accuracy for measurement based classifier
train.cm = as.matrix(table(Actual = knn.train.true, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(train) 
train.accuracy

test.cm = as.matrix(table(Actual = knn.test.true, Predicted = knn.test.predicted))
test.cm 
test.accuracy <- sum(diag(test.cm))/nrow(test)
test.accuracy

# Pick best k for the KNN trained off of measurements
# Odd so there are no tie breakers
grid <- expand.grid(k = c(3, 5, 7, 9, 11))  

# Train with the list of k values and pick the best based on the accuracy
# Use preprocess so all values weighted equally and scaled
mod.knn <- train(
  age.group ~ length + diameter + height + rings,
  data = train,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = grid,
  metric="Accuracy",
  trControl=ctrl
)

# Best k for model
mod.knn$bestTune

# Get the accuracy
knn.train.predicted <- predict(mod.knn,train[,-10])
knn.test.predicted <- predict(mod.knn,test[,-10])

knn.train.predicted <- predict(mod.knn.meas,train[,-10])
knn.test.predicted <- predict(mod.knn.meas,test[,-10])

train.cm = as.matrix(table(Actual = knn.train.true, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(train) 
train.accuracy

test.cm = as.matrix(table(Actual = knn.test.true, Predicted = knn.test.predicted))
test.cm 
test.accuracy <- sum(diag(test.cm))/nrow(test)
test.accuracy

# Start clustering
# Train kMeans model for optimal k
k.list <- c(1,2,3,4,5,6,7)

wcss.list <- c()
si.list <- c()

for (k in k.list) {
  
  mod.km <- kmeans(abalone.data[,c(2, 3, 4, 9)], centers = k)
  
  wcss <- mod.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  if (k>1){
    
    si <- silhouette(mod.km$cluster, dist(abalone.data[c(2, 3, 4, 9)]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
}

plot(k.list,wcss.list,type = "b")

plot(k.list[-1],si.list,type = "b")

# Most optimal is k = 3 (elbow and has best silhouette before diminishing returns)
mod.km <- kmeans(abalone.data[,c(2, 3, 4, 9)], centers = 3)

wcss <- mod.km$tot.withinss
wcss

## get and plot clustering output 
assigned.clusters <- as.factor(mod.km$cluster)

ggplot(abalone.data, aes(x = rings, y = length, colour = assigned.clusters)) +
  geom_point()

## Silhouette Plot
sil <- silhouette(mod.km$cluster, dist(abalone.data[c(2, 3, 4, 9)]))
fviz_silhouette(sil)


# Train PAM model for optimal k
k.list <- c(1,2,3,4,5,6,7)

sumdiss.list <- c()
si.list <- c()

for (k in k.list) {
  
  mod.pam <- pam(abalone.data[,c(2, 3, 4, 9)], k)
  
  sumdiss <- mod.pam$objective[1]
  
  sumdiss.list <- c(sumdiss.list,sumdiss)
  
  if (k>1){
    si <- silhouette(mod.pam$cluster, dist(abalone.data[c(2, 3, 4, 9)]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
  
}

plot(k.list,sumdiss.list,type = "b")

plot(k.list[-1],si.list,type = "b")

# optimal number of clusters is 5 (based on the elbow and the silhouette)
mod.pam <- pam(abalone.data[,c(2, 3, 4, 9)], 5)

sumdiss <- mod.pam$objective[1]
sumdiss

## get and plot clustering output 
assigned.clusters <- as.factor(mod.pam$cluster)

ggplot(abalone.data, aes(x = rings, y = length, colour = assigned.clusters)) +
  geom_point()

## Silhouette Plot
sil <- silhouette(mod.pam$cluster, dist(abalone.data[c(2, 3, 4, 9)]))
fviz_silhouette(sil)
