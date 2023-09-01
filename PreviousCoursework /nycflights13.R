suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))
data(flights)
flights

Q1 <- flights %>%
  group_by(carrier) %>%
  filter(carrier == 'AA' | carrier == 'EV' | carrier == 'FL') %>%
  summarise(mean_dist = mean(distance, na.rm = 'TRUE'), .groups = 'drop') %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 2)

Q2 <- flights %>%
  group_by(month) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(desc(n)) %>%
  head(1)

Q3 <- flights %>%
  group_by(origin, dest) %>%
  summarise(min_dist = mean(distance, na.rm = TRUE), .groups = 'drop') %>%
  arrange(min_dist) %>%
  head(5)

Q4 <- flights %>%
  filter(origin == 'JFK') %>%
  group_by(month, day) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(mean_distance)) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 2)%>%
  head(5)

Q5 <- flights %>%
  filter(dest == 'BOS' | dest == 'ATL') %>%
  group_by(dest) %>%
  summarise(max_arr_delay = max(arr_delay, na.rm = TRUE), .groups = 'drop')
