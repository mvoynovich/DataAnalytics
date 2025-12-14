
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

setwd("C:/Users/matec/DataAnalytics/Project")

df <- read.csv("C:/Users/matec/DataAnalytics/europe_tourism_gdp.csv")

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
  geom_histogram(bins = 30, color="gray") +
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

ggplot(df, aes(GDP_constant_USD)) + geom_bar(fill = "steelblue")

# GDP vs Tourism Receipts
ggplot(df, aes(x = Tourism_receipts_USD, 
               y = GDP_constant_USD)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "GDP vs Tourism Receipts",
       x = "Tourism Receipts (USD)",
       y = "GDP (constant USD)") +
  theme_minimal()

# GDP vs Tourism Arrivals
ggplot(df, aes(x = Tourism_arrivals, 
               y = GDP_constant_USD)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "GDP vs Tourism Arrivals",
       x = "Tourism Arrivals",
       y = "GDP (constant USD)") +
  theme_minimal()


ggpairs(df %>% 
          select(GDP_constant_USD,
                 Tourism_receipts_USD,
                 Tourism_arrivals))
