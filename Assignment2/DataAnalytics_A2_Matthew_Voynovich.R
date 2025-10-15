library(readr)
library(ggplot2)
library(caret)
setwd("C:/Users/matec/DataAnalytics/Assignment2")

epi.data <- read_csv("epi_results_2024_pop_gdp_v2.csv")

# Get two regions sampled
east_europe_data <- epi.data[epi.data$region == "Eastern Europe", ]
west_data <- epi.data[epi.data$region == "Global West", ]

# Plot TBN New for eastern Europe
east_europe_data$TBN.new
NAs <- is.na(east_europe_data$TBN.new)
TBN1 <- east_europe_data$TBN.new[!NAs]

boxplot(TBN1)
x <- seq(0., 100., 5)
hist(TBN1, x, prob=TRUE)
lines(density(TBN1,na.rm=TRUE, bw="SJ"))


# Plot TBN New for global west
west_data$TBN.new
NAs <- is.na(west_data$TBN.new)
TBN2 <- west_data$TBN.new[!NAs]

boxplot(TBN2)
x <- seq(0., 100., 5)
hist(TBN2, x, prob=TRUE)
lines(density(TBN2,na.rm=TRUE, bw="SJ"))

qqplot(TBN1, TBN2, xlab = "Q-Q plot for TBN of eastern europe vs global west", ylab='Global West TBN') 
abline(0, 1, col = "red") # Put line to show what would theoretically be the same distribution

# Do linear model for gdp
TBN_lin_model_gdp <- lm(TBN.new~gdp, epi.data)
summary(TBN_lin_model_gdp)

ggplot(epi.data, aes(x = gdp, y = TBN.new)) +
  geom_point() +
  stat_smooth(method = "lm")+ labs(title='Linear Model - GDP')

ggplot(TBN_lin_model_gdp, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model with GDP as predictor)', x='Fitted Values', y='Residuals')

# Do linear model for population
TBN_lin_model_pop <- lm(TBN.new~population, epi.data)
summary(TBN_lin_model_pop)

ggplot(epi.data, aes(x = population, y = TBN.new)) +
  geom_point() +
  stat_smooth(method = "lm")+ labs(title='Linear Model - Population')

ggplot(TBN_lin_model_pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model with Population as predictor)', x='Fitted Values', y='Residuals')

# Do linear models again for Eastern Europe region
TBN_lin_model_gdp <- lm(TBN.new~gdp, east_europe_data)
summary(TBN_lin_model_gdp)

ggplot(east_europe_data, aes(x = gdp, y = TBN.new)) +
  geom_point() +
  stat_smooth(method = "lm")+ labs(title='Eastern Europe Linear Model - GDP')

ggplot(TBN_lin_model_gdp, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model for Eastern Europe with GDP as predictor)', x='Fitted Values', y='Residuals')

TBN_lin_model_pop <- lm(TBN.new~population, east_europe_data)
summary(TBN_lin_model_pop)

ggplot(east_europe_data, aes(x = population, y = TBN.new)) +
  geom_point() +
  stat_smooth(method = "lm")+ labs(title='Eastern Europe Linear Model - Population')

ggplot(TBN_lin_model_pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model for Eastern Europe with Population as predictor)', x='Fitted Values', y='Residuals')

# The best model is the linear model trained on gdp for the eastern Europe region. This is because it has the lowest p value (it is significant) and the highest R^2. 
# Even though it is the best model, it is not necessarily a great model as it still has decently sized residuals and only has a R^2 of 0.34. 
# Specifying by region likely helps to reduce the variability caused by other factors, thus why gdp for all regions is the second best model instead.


# Do KNN for regions clusters (first 3 variables)
ctrl <- trainControl(method = "cv", number = 10)
region.knn.one <- train(region~EPI.new + ECO.new + BDH.new, data=epi.data, 
                         method="knn",  preProcess = c("center", "scale"), metric="Accuracy", trControl=ctrl)

knn.true <- epi.data$region
knn.predicted <- predict(region.knn.one,epi.data[,c(8, 10, 12)])

train.cm = as.matrix(table(Actual = knn.true, Predicted = knn.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(epi.data) 
train.accuracy

# Repeat KNN for region clusters (another 3 variables)
epi.clean <- na.omit(epi.data[, c("region", "SPI.new", "TBN.new", "TKP.new")])
ctrl <- trainControl(method = "cv", number = 10)
region.knn.two <- train(region~SPI.new + TBN.new + TKP.new, data=epi.clean, 
                        method="knn",  preProcess = c("center", "scale"), metric="Accuracy", trControl=ctrl)

knn.true <- epi.clean$region
knn.predicted <- predict(region.knn.two,epi.clean[,c(2, 3, 4)])

train.cm = as.matrix(table(Actual = knn.true, Predicted = knn.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(epi.clean) 
train.accuracy

# Both models were evaluated with a k value of 3 (following R's default). The model that did better was the model based on EPI, ECO, and BDH. It has a better accuracy of ~.6277 over ~.5 for the other model. 