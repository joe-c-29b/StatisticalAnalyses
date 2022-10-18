suppressPackageStartupMessages(library(tidyverse))
msleep


Q1 <- msleep %>%
  filter(vore == 'carni' & conservation == 'lc') %>%
  summarise(var = var(sleep_total, na.rm = 'TRUE'), .groups = 'drop') %>%
  as.data.frame() %>%
  round(digits = 2)

Q2 <- msleep %>%
  filter(order == 'Rodentia') %>%
  mutate(sleep_ratio = sleep_total / sleep_rem, na.rm = TRUE) %>%
  arrange(desc(sleep_ratio)) %>%
  select(name) %>%
  head(1)

Q3 <- msleep %>%
  filter(order == 'Primates') %>%
  mutate(weight_ratio = bodywt / brainwt, na.rm = TRUE) %>%
  filter(weight_ratio > 100) %>%
  nrow()

Q4 <- msleep %>%
  group_by(conservation) %>%
  summarise(mean_sleep = mean(sleep_total, na.rm = TRUE), 
            var_sleep = var(sleep_total, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 2)%>%
  head()
  
Q5 <- msleep %>%
  filter(conservation == 'domesticated' & vore == 'herbi' & sleep_total > 12) %>%
  select(name)