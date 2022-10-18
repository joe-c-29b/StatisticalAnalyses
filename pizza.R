suppressPackageStartupMessages(library(tidyverse))

pizza <- suppressMessages(read_csv('pizza.csv'))

Q1 <- pizza %>%
  filter(free_wine == 1 & discount_customer == 1 & pizzas > 4) %>%
  select(driver) 

Q2 <- pizza %>%
  summarise(mean_ratio = mean(bill / pizzas, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame()%>%
  round(digits = 2)

Q3 <- pizza %>%
  group_by(day) %>%
  summarise(var_pizzas = var(pizzas), .groups = 'drop') 

Q4 <- pizza %>%
  group_by(operator) %>%
  summarise(mean_bill = mean(bill), .groups = 'drop') %>%
  arrange(desc(mean_bill)) %>%
  select(operator) %>%
  head(1)

Q5 <- pizza %>%
  group_by(day, driver) %>%
  summarise(n = as.integer(sum(free_wine)), .groups = 'drop')%>%
  arrange(desc(n)) %>%
  select(day, driver, n) %>%
  head(1)
