#library
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(olsrr))

#data
BRF <- suppressMessages(read_csv('BRFSS2015.csv'))

###########################################################
#Q1: How many people have any kind of health care coverage?
Q1 <- BRF %>%
  select(HLTHPLN1) %>%                    #isolate the required column
  filter(HLTHPLN1 == 1) %>%             #filter out any responses that are not 1(yes)
  nrow()                                #get a row count


#this also worked
QQ1 <- BRF %>%
  group_by(HLTHPLN1) %>%
  summarise(coverage = n(), .groups = 'drop') %>%
  head(1)
QQ1



###########################################################
#Q2: What is the average "Number of Days Mental Health Not Good" for those in Pennsylvania who have numeric data? Make sure to change the response corresponding to none to 0.
Q2 <- BRF %>%
  select(MENTHLTH) %>%                                                                      #isolate the required column
  filter(MENTHLTH != 77 | MENTHLTH != 99, is.numeric = TRUE, na.rm = TRUE) %>%            #get rid of refused and don't knows
  mutate(MENH = ifelse(MENTHLTH > 30, 0, 1)) %>%                                          #create column with the proper multipliers
  transmute(MENH30 = (MENTHLTH * MENH)) %>%                                               #convert 88(none) to 0s
  summarise(average = mean(MENH30), .groups = 'drop')                                     #average



###########################################################
#Q3: Compare only those who have and have not had some form of arthritis, rheumatoid arthritis, gout, etc. For those groupings, convert reported weight in kilograms to pounds.  Then, compute the mean and standard deviation of the newly created weight in pounds variable. Use the conversion 1KG = 2.20462 LBS. Make sure the units are in pounds, not two decimals implied. The names of the variables should be mean_weight and sd_weight. mean_weight should equal 183.04. The output for this should be a tibble/dataframe/table with two rows (one for the haves and one for the have-nots) and two columns (mean_weight and sd_weight). So four values all together:

BRF2 <- BRF %>%
  mutate(rep_lbs = WTKG3/ 100 * 2.20462) %>%                              #convert kilos to lbs
  select(HAVARTH3, WTKG3, rep_lbs) %>%                                    #columns we want
  filter(HAVARTH3 == 1 | HAVARTH3 == 2, na.rm = TRUE) %>%                 #only interested in these 2 groups
  group_by(HAVARTH3) %>%                                                  #group group by the haves and haves not to summarise
  summarise(                                                              #adding aggregates
    mean_weight = mean(rep_lbs, na.rm = TRUE), 
    sd_weight = sd(rep_lbs, na.rm = TRUE), 
    .groups = 'drop') %>% 
  select(mean_weight, sd_weight) %>%                                      #dropping HAVARTH3 column   
  as.data.frame() %>%
  round(digits = 2)
Q3 <- BRF2



###########################################################
#Q4: Remove outliers from minutes of total physical activity per week using 0.997 and 0.003 as criteria.  What percentage of observations remain?  Assign that value to Q4.
phy_act <- BRF %>%                                                                #select proper rows drop bad values
  select(MARITAL, PA1MIN_) %>%
  filter(MARITAL != 9) %>%
  mutate(MARITAL_ = as.factor(MARITAL)) %>%
  na.omit()

upperO <- quantile(phy_act$PA1MIN_, .997,na.rm = TRUE)                            #upper limit
lowerO <- quantile(phy_act$PA1MIN_, .003,na.rm = TRUE)                            #lower limit
outliers <- which(phy_act$PA1MIN_ > upperO | phy_act$PA1MIN_ < lowerO)            #identify outliers
#remove the outliers
Q4 <- ((nrow(phy_act) - length(outliers)) / nrow(phy_act) * 100)                  #percentage

M_P_noout <- phy_act[-outliers,]



###########################################################
#Q5:  Group by marital status and calculate the mean, standard deviation, minimum, and maximum of total exercise, to two decimals.
M_P_noout2 <- M_P_noout %>% select(MARITAL_, PA1MIN_)                               #drop extra marital column

Q5 <- M_P_noout2 %>%                                                                #assign
  group_by(MARITAL_) %>%                                                            #grouping by marital status
  summarise(
    mean_tot_exercise = mean(PA1MIN_, na.rm = TRUE),                                #creating aggregate values with summarise function
    sd_tot_exercise = sd(PA1MIN_, na.rm = TRUE),
    min_tot_exercise = min(PA1MIN_, na.rm = TRUE),
    max_tot_exercise = max(PA1MIN_, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>%                                                           #changing to data frame so that I will be able to round
  mutate_if(is.numeric, round, digits = 2)                                        #rounding output values



###########################################################
Q6 <- ggplot(M_P_noout2) +
  geom_boxplot(mapping = aes(PA1MIN_, MARITAL_))



###########################################################
model_Q7 <- lm(PA1MIN_ ~ MARITAL_, data = M_P_noout2) 
Q7 <- summary(model_Q7)



###########################################################
Q8 <- aov(PA1MIN_ ~ MARITAL_, data = M_P_noout2) %>%
  TukeyHSD()



###########################################################
#Q9: Run a regression as in Q7, but add total fruits consumed per day.  Based on the R-squared and AIC, what is the better model?  Assign the better AIC value to Q9.
Q9w <- BRF %>%
  rename(FRUTSUM = '_FRUTSUM') %>%                                 #renaming this ridiculous column
  select(MARITAL, PA1MIN_, FRUTSUM) %>%                            #adding fruit column to the df
  filter(MARITAL != 9) %>%
  mutate(MARITAL_ = as.factor(MARITAL)) %>%
  na.omit()

upperO4 <- quantile(Q9w$FRUTSUM, .997,na.rm = TRUE)                #checking fruit for outliers
lowerO4 <- quantile(Q9w$FRUTSUM, .003,na.rm = TRUE)                
outliers4 <- which(Q9w$FRUTSUM > upperO4 | Q9w$FRUTSUM < lowerO4) 
length(outliers4)

upperO2 <- quantile(Q9w$PA1MIN_, .997,na.rm = TRUE)                #upper limit
lowerO2 <- quantile(Q9w$PA1MIN_, .003,na.rm = TRUE)                #lower limit
outliers2 <- which(Q9w$PA1MIN_ > upperO2 | Q9w$PA1MIN_ < lowerO2)    #identify outliers
Q9works <- Q9w[-outliers2,]                                        #remove the outliers

model_Q9 <- lm(PA1MIN_ ~ MARITAL_ + FRUTSUM, data = Q9works)       #model w/fruit

summary(model_Q9)              #Rsq = .008
summary(model_Q7)              #Rsq = .005
AIC(model_Q9)                  #AIC = 4370601
AIC(model_Q7)                  #AIC = 4477305
Q9 <- AIC(model_Q9)            #model including fruit had a better lower AIC and higher Rsq, although both Rsq were garbage`



###########################################################
#Q10: Address the values of any variables.  For instance, is "none" equal to a value other than 0? Are there extra decimals implied?

QLK <- BRF %>%
  mutate(
    bmi = (BRF$'_BMI5' / 100),
    heavy_drinker = as.factor(BRF$'_RFDRHV5'),
    smoker_frq = as.factor(BRF$'_SMOKER3'),
    height = (BRF$'HTIN4')) %>%
  select(height, smoker_frq, heavy_drinker, bmi) %>%
  filter(heavy_drinker != 9 & smoker_frq != 9) %>% 
  mutate_if(is.numeric, round, digits = 2) %>%
  na.omit()

Q10 <- QLK


#Below are my chosen variables and their respective descriptions
#: _BMI5 - Body Mass Index (BMI); numeric value with two implied decimal places(3000=30), converted to a normal BMI value where 30=30, BLANK values were "Don't know/Refused/Missing" and omitted 
#_RFDRHV5 - Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week); converted to a factor 
#1=not heavy drinker, 
#2=heavy drinker, 
#9=Don't know/Not sure/Missing(filtered out)
#_SMOKER3 - Four-level smoker status: Everyday smoker, Someday smoker, Former smoker, Non-smoker; converted to a factor 
#1='Current smoker - now smokes every day', 
#2='Current smoker - now smokes some days', 
#3='Former smoker', 
#4='Never smoked', 
#9='Don't know/Not sure/Missing'(filtered out)
#: HTIN4 - Reported height in inches; BLANK values were "Don't know/Refused/Missing" and omitted 



###########################################################
#Q11


#began with the 'Longo Modified 3 Standard Deviation' method (LM3SD for short) because I was more easily able to steal the code chunk from an above question; I narrow to .15% on each end if there are any outliers identified
#bmi outliers
upperO3 <- quantile(QLK$bmi, .997,na.rm = TRUE)                            #upper bmi outlier limit
lowerO3 <- quantile(QLK$bmi, .003,na.rm = TRUE)                            #lower bmi outlier limit
outliers3 <- which(QLK$bmi > upperO3 | QLK$bmi < lowerO3)                  #bmi outliers if there are any
length(outliers3)                                                          #length=2312; narrowing to 0.15%
#height outliers
upperO5 <- quantile(QLK$height, .997,na.rm = TRUE)                         #upper height outlier limit
lowerO5 <- quantile(QLK$height, .003,na.rm = TRUE)                         #lower height outlier limit
outliers5 <- which(QLK$height > upperO5 | QLK$height < lowerO5)            #height outliers if there are any
length(outliers5)                                                          #length=1494; narrowing to 0.15%
#I am narrowing my outlier search to .15% because I only have one continuous variable I would like to maintain as many observations as possible, I will be creating models for both outlier sets to compare models
#bmi outliers
upperO4 <- quantile(QLK$bmi, .9985,na.rm = TRUE)                           #upper bmi outlier limit @ .015%
lowerO4 <- quantile(QLK$bmi, .0015,na.rm = TRUE)                           #lower bmi outlier limit @ .015%
outliers4 <- which(QLK$bmi > upperO4 | QLK$bmi < lowerO4) 
length(outliers4)                                                          #length=1130
#height outliers
upperO6 <- quantile(QLK$height, .9985,na.rm = TRUE)                        #upper height outlier limit @ .015%
lowerO6 <- quantile(QLK$height, .0015,na.rm = TRUE)                        #lower height outlier limit @ .015%
outliers6 <- which(QLK$height > upperO6 | QLK$height < lowerO6) 
length(outliers6)                                                          #length=887
upperO3                                                                    #=63.20; bmi
lowerO3                                                                    #=16.47; bmi
upperO4                                                                    #=79.29; bmi
lowerO4                                                                    #=15.73; bmi
upperO5                                                                    #=78; height
lowerO5                                                                    #=57; height
upperO6                                                                    #=79; height
lowerO6                                                                    #=56; height

QLK2 <- QLK[-outliers3,][-outliers5,]      #model without .3% outliers

QLK3 <- QLK[-outliers4,][-outliers6,]      #model without .15% outliers
Q11 <- QLK3

#I am will be comparing QLK2 and QLK3 data sets as I move forward to see what the different outlier sets would do to model fit
#It is worth noting that only the lower bmi quantiles have outliers; both height upper and lower quantiles have outliers
#will not be testing the others because they are factor-types;_CHLDCNT/kids_n min=1 and max=6(removed the 9s); _SMOKER3/smoker_frq min=1 and max=4(removed the 9s); _RFDRHV5/heavy_drinker min=1 and max=2(anything else was BLANK and omitted)


###########################################################
#Q12
###########################################################
#Q12
###########################################################
#Q12: Complete exploratory analyses doing appropriate visualizations with ggplot2.
#DISTRIBUTIONS OF EACH VARIABLE
height_dist <-ggplot(data = QLK3, aes(x = height)) +
  stat_count()
#height follows a relatively normal distribution

bmi_dist <- ggplot(data = QLK3, aes(x = bmi)) +
  stat_count()
#this is difficult to get anything from so I will be transforming bmi by rounding, first to one decimal, then to none.
bmi_dist2 <- ggplot(data = QLK3, aes(x = round(bmi, digits = 1))) +
  stat_count()
#better
bmi_dist3 <- ggplot(data = QLK3, aes(x = round(bmi, digits = 0))) +
  stat_count()
#distribution appears to skew pretty severely to the right

smoker_dist <-ggplot(data = QLK3, aes(x = smoker_frq)) +
  stat_count()
#simple summation, there are more non-smokers than smokers

drinker_dist <-ggplot(data = QLK3, aes(x = heavy_drinker)) +
  stat_count()
#simple summation, there are more non-heavy drinkers than heavy-drinkers

#CORRELATION MATRICES
QLK2_ <- QLK2 %>%
  mutate_if(is.factor, as.numeric)                          #prepping for cor()
cor(QLK2_)
rcorr(as.matrix(QLK2_))
QLK3_ <- QLK3 %>%
  mutate_if(is.factor, as.numeric)                         #prepping for cor() 
QLK3_cor <- cor(QLK3_)
rcorr(as.matrix(QLK3_))

cor.test(QLK3_$height, QLK3_$bmi)                          #double checking the p-values from the rcorr matrices
cor.test(QLK2_$height, QLK2_$bmi)                          #they were correct
cor.test(QLK3_$heavy_drinker, QLK3_$smoker_frq)            #wanted the exact p-value for my variable with the strongest correlation to
#my outcome variable; p=2.2e-16
#Correlations were all pretty weak, but they were all significant except for height and bmi in QLK2 which is the one which excluded more observations as outliers
#For the time being I will be dropping QLK2 from my graphics and descriptive stats unless I get a better AIC in Q14 when I compare all models.  Should QLK3 have a better AIC than QLK2, I will come back and create additional graphics.
#STRONGEST CORRELATION: heavy_drinker vs. Smoker_frq
#CORRELATION DIRECTIONS:
#height_smoker: negative
#height_drinker: positive
#height_bmi: positive
#smoker_drinker: negative
#smoker_bmi: positive
#drinker_bmi: negative

#below is a visual of the correlations between the variables
corrplot(QLK3_cor, method = 'color')                            
#I have the option to add/adjust some aesthetics to this plot, but I do not feel that will add any value to demonstrate the already weak correlations

#COMPARATIVE GRAPHICS
#In the below boxplots I am working with bmi when factoring in heavy drinking alone and the heavy drinking and smoking
box_bmi <- ggplot(data = QLK3, aes(heavy_drinker, bmi)) +
  geom_boxplot()
#it appears that heavy drinkers(2), on average, have a slightly lower bmi than non-heavy drinkers, this is interesting to me and I would be curious to see how gender plays into this

box_bmi_facet <- ggplot(data = QLK3, aes(heavy_drinker, bmi)) +
  geom_boxplot() +
  facet_wrap(~ smoker_frq)
#adding smoker to this graph added next to nothing


box_height <- ggplot(data = QLK3, aes(heavy_drinker, height)) +
  geom_boxplot()
#this graph shows a smaller overall range for heavy drinkers(2) compared to non-heavy drinkers, just like above, I would be curious to see how gender plays into this

box_height_facet <- ggplot(data = QLK3, aes(heavy_drinker, height)) +
  geom_boxplot() +
  facet_wrap(~ smoker_frq)
#adding smokers as an additional factor does not show much, but it does indicate that there is a wider dispersion in the height of smokers vs non-smokers
#additionally, the tallest observations are on the "not a current smoker" graphs, and the shortest was in the category "never a smoker & not a heavy drinker"


count_drinker <- ggplot(data = QLK3) +
  stat_count(aes(x = heavy_drinker, color = smoker_frq))
#this graph is not particularly helpful because of the difference in counts between drinker categories
#why are you keeping this graph then, Joe?
#I'm glad you asked, below, I switch the variables and it is significantly more illuminating

count_smoker <- ggplot(data = QLK3) +
  stat_count(aes(x = smoker_frq, color = heavy_drinker))
#this histogram demonstrates that while there are similar counts of heavy drinkers within each smoking group, the groups that identify as smokers have a higher percentage of heavy drinkers than those that do not currently smoke


#originally tried this graph with bmi assigned to color, but it was less than illuminating so I changed it to smoker_frq
point_height_bmi_smoker <- ggplot(data = QLK3) +
  geom_point(aes(x = height, y = heavy_drinker, color = smoker_frq), position = "jitter")
#this graph shows a few things:
#heavy drinkers have a higher occurrence of smokers(both frequent and infrequent)
#having being taller is not the best indicator for determining heavy drinking, but... 
#heavy drinking is a good indicator that someone is over 60inches



#initially included heavy_drinker as a color variable, but it turned out very poorly and wasn't helpful
point_height_bmi <- ggplot(data = QLK3) +
  geom_point(aes(x = height, y = bmi))
#honestly still not incredibly helpful as it is just too busy, but it does show that while there is a bmi drop off as height increases, it is not quite the same in the other direction, with a spike around the 48in mark

QLK_drinker <- QLK3 %>%
  filter(heavy_drinker == 2)

QLK_nondrinker <- QLK3 %>%
  filter(heavy_drinker == 1)

point_drinker_height_smoker <- ggplot(data = QLK3) +
  geom_col(aes(x = smoker_frq, y = height)) +
  facet_wrap(~ heavy_drinker)
#while I like having the graphs side by side, the scale difference is an issue

point_drinker_bmi_smoker1 <- ggplot(data = QLK_drinker) +
  geom_point(aes(x = smoker_frq, y = bmi), position = "jitter")
point_drinker_bmi_smoker2 <- ggplot(data = QLK_nondrinker) +
  geom_point(aes(x = smoker_frq, y = bmi), position = "jitter")
point_drinker_bmi_smoker2
point_drinker_bmi_smoker1
#biggest takeaway here is that regardless of drinking habits, smokers(particularly "sometimes smokers") trend towards having slightly lower BMIs compared to non-smokers

point_drinker_height_smoker1 <- ggplot(data = QLK_drinker) +
  geom_point(aes(x = smoker_frq, y = height), position = "jitter")
point_drinker_height_smoker2 <- ggplot(data = QLK_nondrinker) +
  geom_point(aes(x = smoker_frq, y = height), position = "jitter")
point_drinker_height_smoker2
point_drinker_height_smoker1
#minor takeaway that non-smokers have a larger range of height going higher and lower than those that do smoke.

###########################################################
###########################################################
###########################################################
###########################################################
#Q13: Run basic descriptive statistics
#correlation matrices
QLK2_ <- QLK2 %>%
  mutate_if(is.factor, as.numeric)                          #prepping for cor()
cor(QLK2_)
rcorr(as.matrix(QLK2_))
QLK3_ <- QLK3 %>%
  mutate_if(is.factor, as.numeric)                         #prepping for cor() 
QLK3_cor <- cor(QLK3_)
rcorr(as.matrix(QLK3_))

cor.test(QLK3_$height, QLK3_$bmi)                          #double checking the p-values from the rcorr matrices
cor.test(QLK2_$height, QLK2_$bmi)                          #they were correct
cor.test(QLK3_$heavy_drinker, QLK3_$smoker_frq)            #wanted the exact p-value for my variable with the strongest correlation to
#my outcome variable; p=2.2e-16
#Correlations were all pretty weak, but they were all significant except for height and bmi in QLK2 which is the one which excluded more observations as outliers
#For the time being I will be dropping QLK2 from my graphics and descriptive stats unless I get a better AIC in Q14 when I compare all models.  Should QLK3 have a better AIC than QLK2, I will come back and create additional graphics.
#displaying mean, sd, min, and max for BMI, height, and smoker type while grouping by heavy_drinker (my outcome variable)
QLK_stats <- QLK3 %>%                                                                #assign
  group_by(heavy_drinker) %>%                                                            #grouping by marital status
  summarise(
    mean_bmi = mean(bmi, na.rm = TRUE),                                #creating aggregate values with summarise function
    sd_bmi = sd(bmi, na.rm = TRUE),
    min_bmi = min(bmi, na.rm = TRUE),
    max_bmi = max(bmi, na.rm = TRUE),
    mean_height = mean(bmi, na.rm = TRUE),                                #creating aggregate values with summarise function
    sd_height = sd(bmi, na.rm = TRUE),
    min_height = min(bmi, na.rm = TRUE),
    max_height = max(bmi, na.rm = TRUE),
    mean_smoker_frq = mean(bmi, na.rm = TRUE),                                #creating aggregate values with summarise function
    sd_smoker_frq = sd(bmi, na.rm = TRUE),
    min_smoker_frq = min(bmi, na.rm = TRUE),
    max_smoker_frq = max(bmi, na.rm = TRUE),
    .groups = 'drop') %>%
  as.data.frame() %>%                                                           #changing to data frame so that I will be able to round
  mutate_if(is.numeric, round, digits = 2)                                        #rounding output values
QLK_stats

#also including this summary because it gives the counts for my drinker and smoker categories
summary(QLK3)


#individual regressions against my outcome variable to look at the coefficients
BMImod <- glm(heavy_drinker ~ bmi, binomial(), data = QLK3)
smokermod <- glm(heavy_drinker ~ smoker_frq, binomial(), data = QLK3)
heightmod <- glm(heavy_drinker ~ height, binomial(), data = QLK3)
BMImod$coefficients
smokermod$coefficients
heightmod$coefficients
summary(BMImod)
summary(smokermod)
summary(heightmod)
#was hoping to get some p-values from these summaries, but it looks like they are all very small and thus their meager correlations are significant

#honestly I regret taking the time to do this when I was able to do the significance from a model summary, kinda a dummy for that, but I'm leaving it
BMImod.chi <- BMImod$null.deviance - BMImod$deviance
BMImod.df <- BMImod$df.null - BMImod$df.residual
cat("p-value", 1 - pchisq(BMImod.chi, BMImod.df))


QLKmod <- glm(heavy_drinker ~ bmi + smoker_frq + height, binomial(), data = QLK3)
QLKmod$coefficients


###########################################################
###########################################################
###########################################################
#Q14: Finally, run an appropriate regression predicting one of those variables.  Identify the best model.
#have to do glm(logistic regression) because my outcome variable is discrete
QLKmod <- glm(heavy_drinker ~ bmi + smoker_frq + height, binomial(), data = QLK3)
ols_step_all_possible(QLKmod) #this will not work because it is a logistic regression
#creating a bunch of models to compare AICs
BMImod2 <- update(BMImod, .~. + smoker_frq, binomial())
BMImod3 <- update(BMImod2, .~. + height, binomial())
BMImod4 <- update(BMImod, .~. + height, binomial())
BMImod5 <- update(BMImod4, .~. + smoker_frq, binomial())
smokermod2 <- update(smokermod, .~. + height, binomial())
smokermod3 <- update(smokermod2, .~. + bmi, binomial())
smokermod4 <- update(smokermod, .~. + bmi, binomial())
smokermod5 <- update(smokermod4, .~. + height, binomial())
heightmod2 <- update(heightmod, .~. + bmi, binomial())
heightmod3 <- update(heightmod2, .~. + smoker_frq, binomial())
heightmod4 <- update(heightmod, .~. + smoker_frq, binomial())
heightmod5 <- update(heightmod4, .~. + bmi, binomial())

#running AICs for all models to compare
AIC(BMImod)         #162916
AIC(smokermod)      #158554.1
AIC(heightmod)      #163260
AIC(BMImod2)        #157736.3
AIC(smokermod2)     #158236.5
AIC(heightmod2)     #162403.2
AIC(BMImod3)        #157396.8
AIC(smokermod3)     #157396.8
AIC(heightmod3)     #157396.8
AIC(BMImod4)        #162403.2
AIC(smokermod4)     #157736.3
AIC(heightmod4)     #158236.5
AIC(BMImod5)        #157396.8
AIC(smokermod5)     #157396.8
AIC(heightmod5)     #157396.8

AIC(QLKmod)         #157396.8


#these are all the same as the QLKmod which includes all three predictors
AIC(BMImod3)        #157396.8
AIC(smokermod3)     #157396.8
AIC(heightmod3)     #157396.8
AIC(BMImod5)        #157396.8
AIC(smokermod5)     #157396.8
AIC(heightmod5)     #157396.8

#The QLKmod was the best model with the lowest AIC.
#The second best model was BMImod2 which only had the smoker_frq and bmi predictors
#The best single predictor model was the smokermod
#below is my best model
QLKmod <- glm(heavy_drinker ~ bmi + smoker_frq + height, binomial(), data = QLK3)
AIC(QLKmod)
QLKmod$coefficients
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
#I feel like gender may have helped improve my model, so I want to try it.  If you don't care, no need to keep reading, if you are curious as well then carry on

excess <- BRF %>%
  mutate(
    gender = as.factor(BRF$SEX),
    bmi = (BRF$'_BMI5' / 100),
    heavy_drinker = as.factor(BRF$'_RFDRHV5'),
    smoker_frq = as.factor(BRF$'_SMOKER3'),
    height = (BRF$'HTIN4')) %>%
  select(height, smoker_frq, heavy_drinker, bmi, gender) %>%
  filter(heavy_drinker != 9 & smoker_frq != 9) %>% 
  mutate_if(is.numeric, round, digits = 2) %>%
  na.omit()


excess_mod <- glm(heavy_drinker ~ gender + bmi + smoker_frq + height, binomial(), data = excess)
excess_mod2 <- glm(heavy_drinker ~ bmi + smoker_frq + height, binomial(), data = excess)

AIC(excess_mod)
AIC(excess_mod2)

excess %>%
  mutate_if(is.factor, as.numeric) %>%                          #prepping for cor()
  cor()
#well, while it did improve my model to include gender and it also had some pretty good correlation to height, it was not nearly the additional predictor that I thought it would be
