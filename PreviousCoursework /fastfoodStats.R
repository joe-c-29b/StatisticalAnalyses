suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
suppressPackageStartupMessages(library(lm.beta))

fastfood <- openintro::fastfood

Q1 <- fastfood %>%
  filter(restaurant == 'Sonic' | restaurant == 'Subway' | restaurant == 'Taco Bell') %>%
  select(calories, total_fat, sugar, calcium) %>%
  na.omit() %>%
  cor() %>%
  round(digits = 2)

SMreg <- fastfood %>%
  filter(restaurant == 'Mcdonalds' | restaurant == 'Subway')%>%
  select(restaurant, calories, sodium, protein) %>%
  mutate(restbin = ifelse(restaurant == 'Mcdonalds', 1, 0))
SMregModel <- glm(restbin ~ calories + sodium + protein, binomial(), data = SMreg)
Q2 <- SMregModel$coefficients %>% round(digits = 2)


SMregModel2 <- glm(restbin ~ calories + protein, data = SMreg)
SMregModel2
Q3 <- AIC(SMregModel, k=2) %>% round(digits = 2)


calModel <- lm(calories ~ sat_fat + fiber + sugar, data = fastfood)
lm.beta(calModel)
Q4 <- calModel$coefficients [2] %>%
  round(digits = 2)

table(fastfood$restaurant) #gives counts of restaurant occurences
fiftyPlus <- fastfood %>%
  filter(restaurant == 'Arbys' | restaurant == 'Mcdonalds' | restaurant == 'Sonic') %>%
  select(total_fat, cholesterol, total_carb, vit_a, restaurant) 
fiftyPlus
TotFat <- lm(total_fat ~ cholesterol + total_carb + vit_a + restaurant, data = fiftyPlus)
summary(TotFat)
TotFat2 <- lm(total_fat ~ cholesterol + total_carb + restaurant, data = fiftyPlus)
Tots <- lm.beta(TotFat2)
Q5 <- Tots$standardized.coefficients [2] %>% round(digits = 2)
