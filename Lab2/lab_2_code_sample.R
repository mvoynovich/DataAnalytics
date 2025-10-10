####### Data Analytics Fall 2025 Lab 1 ######

library(ggplot2)

### set working directory
setwd("C:/Users/matec/DataAnalytics/Lab2")

### read in data
epi.data <- read.csv("epi_results_2024_pop_gdp_v2.csv", header=TRUE)

View(epi.data)

#### Exploratory Analysis ####

EPI.new <- epi.data$EPI.new

## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
country <- epi.data$country

Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])

## summary stats
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

##################

### Comparing distributions of 2 variables
EPI.old <- epi.data$EPI.old

boxplot(EPI.old, EPI.new, names=c("EPI.old","EPI.new"))


### Quantile-quantile plots

qqplot(EPI.new,EPI.old)


#### GDP vs. EPI ####
gdp <- epi.data$gdp

ggplot(epi.data, aes(x = gdp, y = EPI.new, colour = region)) +
  geom_point()

## created linear model of EPI.new ~ gdp
lin.mod0 <- lm(EPI.new~gdp,epi.data)

summary(lin.mod0)

ggplot(epi.data, aes(x = gdp, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm using log 10 gdp
epi.data$log_gdp <- log10(gdp)

ggplot(epi.data, aes(x = log_gdp, y = EPI.new, colour = region)) +
  geom_point()

lin.mod1 <- lm(log_gdp~EPI.new,epi.data)

summary(lin.mod1)

ggplot(epi.data, aes(x = log_gdp, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

### subset by regions
summary(epi.data$region)

##convert region from strings to factors
epi.data$region <- as.factor(epi.data$region)

summary(epi.data$region)

epi.data.subset <- epi.data[! epi.data$region %in% c("Eastern Europe","Former Soviet States","Latin America & Caribbean"),]

ggplot(epi.data.subset, aes(x = log_gdp, y = EPI.new, colour = region, label=country)) +
  geom_point() + geom_text(hjust=0, vjust=0)

lin.mod2 <- lm(log_gdp~EPI.new,epi.data.subset)

summary(lin.mod2)

ggplot(epi.data.subset, aes(x = log_gdp, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

############################
# LAB 2 Student Work Start #
############################

# Start cleaning the data
# Remove any null values
Clean_NY_House <- na.omit(NY_House_Dataset)
NY_House_DF <- data.frame(Price = Clean_NY_House$PRICE, PropertySqFt = Clean_NY_House$PROPERTYSQFT, Beds = Clean_NY_House$BEDS, Bath = Clean_NY_House$BATH)
NY_House_DF$Bath <- round(NY_House_DF$Bath)
# Get just the variables we want to use for linear model
# Round Baths to be integers

# Start creating models
house_lin_model0 <- lm(Price~., NY_House_DF)
summary(house_lin_model0)

ggplot(NY_House_DF, aes(x = PropertySqFt, y = Price)) +
  geom_point() +
  stat_smooth(method = "lm")

# Outliers make it really hard. Repeat with their removal using z-score/standard deviation
meanVal <- mean(NY_House_DF$Price, na.rm = TRUE)
sdVal <- sd(NY_House_DF$Price, na.rm = TRUE)

NY_House_DF <- NY_House_DF[abs(NY_House_DF$Price - meanVal) <= 3*sdVal, ]

# Repeat creation of first model
house_lin_model0 <- lm(Price~., NY_House_DF)
summary(house_lin_model0)

ggplot(NY_House_DF, aes(x = PropertySqFt, y = Price)) +
  geom_point() +
  stat_smooth(method = "lm")+ labs(title='Linear Model 0')

ggplot(house_lin_model0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model 0)', x='Fitted Values', y='Residuals')

# Create model with log price
NY_House_DF$LogPrice <- log10(NY_House_DF$Price)

# Repeat Linear Model Now
house_lin_model1 <- lm(LogPrice~Bath + Beds + PropertySqFt, NY_House_DF)
summary(house_lin_model1)

ggplot(NY_House_DF, aes(x = Bath, y = LogPrice)) +
  geom_point() +
  stat_smooth(method = "lm") + labs(title='Linear Model 1')

ggplot(house_lin_model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model 1)', x='Fitted Values', y='Residuals')

# Remove beds because they have been insignificant
house_lin_model2 <- lm(Price~Bath + PropertySqFt, NY_House_DF)
summary(house_lin_model2)

ggplot(NY_House_DF, aes(x = PropertySqFt, y = Price)) +
  geom_point() +
  stat_smooth(method = "lm")+ labs(title='Linear Model 2')

ggplot(house_lin_model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Model 2)', x='Fitted Values', y='Residuals')

# Removing beds actually hurts the model slightly despite variable being insignificant

