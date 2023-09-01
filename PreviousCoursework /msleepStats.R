suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))


##1
Q1 <- cor.test(msleep$sleep_total, msleep$bodywt)

##2
Q2 <- select(msleep, sleep_total, sleep_rem, brainwt, bodywt) %>%
  na.omit() %>%
  cor() %>%
  round(digits = 2)

##3
sleepModel <- lm(bodywt ~ vore, data = msleep)
Q3 <- round(sleepModel$coefficients, digits = 2)

##4
sleepModel2 <- lm(msleep$bodywt ~ msleep$vore + msleep$sleep_rem, data = msleep)

AICmod1 <- AIC(sleepModel, k = 1)
AICmod2 <- AIC(sleepModel2, k = 2)
Q4 <- AICmod2

##5
voreSleep <- msleep %>%
  select(vore, sleep_total) %>%
  filter(vore != "omni" & vore != "insecti") %>%
  mutate(vorebin = ifelse(vore == 'carni', 0, 1))
voreSleep
voreSleepMod <- glm(vorebin ~ sleep_total, binomial(), data = voreSleep)
Q5 <- voreSleepMod
Q5
