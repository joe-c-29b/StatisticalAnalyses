suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))

fastfood <- openintro::fastfood

Q1 <- fastfood %>%
  filter(restaurant == 'Burger King' | restaurant == 'Chick Fil-A') %>%
  arrange(desc(calories)) %>%
  select(item) %>%
  head(1)

Q2 <- fastfood %>%
  filter(restaurant == 'Subway')%>%
  summarise(mean_sugar = mean(sugar, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>%
  round(digits = 2)
Q2
Q3 <- fastfood %>%
  filter(restaurant == 'Taco Bell')%>%
  summarise(mean_calories = mean(calories, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>%
  round(digits = 2)

Q4 <- fastfood %>%
  mutate(fatXsugar = total_fat * sugar) %>%
  select(restaurant, item, fatXsugar) %>%
  arrange(desc(fatXsugar)) %>%
  head(3)

Q5 <- fastfood %>%
  group_by(restaurant) %>%
  summarise(mean_sat_fat = mean(sat_fat, na.rm = TRUE), .groups = 'drop') %>%
  filter(mean_sat_fat > 10) %>%
  nrow()
