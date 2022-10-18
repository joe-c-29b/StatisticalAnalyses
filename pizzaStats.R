suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))

pizza <- suppressMessages(read_csv('pizza.csv'))

Q1 <- pizza %>%
  select(temperature, bill, pizzas, got_wine) %>%
  cor() %>%
  round(digits = 2)


Q2 <- pizza %>%
  filter(operator == 'Laura' & branch == 'East') %>%
  select(time, temperature, bill, pizzas) %>%
  cor() %>%
  round(digits = 2)
 
wineMod <- glm(got_wine ~ temperature + bill + pizzas, binomial(), data = pizza)
summary(wineMod)
Q3 <- summary(wineMod)$coefficients %>%
  round(digits = 2)
Q3

billMod <- lm(bill ~ temperature + pizzas + got_wine, data = pizza)
coe <- lm.beta(billMod)
Q4 <- coe$standardized.coefficients[2:4]

pizza2 <- pizza %>%
  mutate(operator_bin = ifelse(operator == 'Laura', 1,0))
billMod2 <- update(billMod, .~. + operator_bin, data = pizza2)
Q5 <- AIC(billMod, k=2)
