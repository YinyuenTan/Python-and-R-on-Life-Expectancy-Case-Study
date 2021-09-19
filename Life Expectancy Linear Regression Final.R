library(tidyverse)

#Read csv file
mydata <- read.csv ("Life Expectancy.csv", header = TRUE)

#Filter null values
mydata = mydata %>%
filter(!is.na(Life.expectancy),
         !is.na(Adult.Mortality),
         !is.na(Alcohol),
         !is.na(Hepatitis.B),
         !is.na(BMI),
         !is.na(Polio),
         !is.na(Total.expenditure),
         !is.na(Diphtheria),
         !is.na(GDP),
         !is.na(Population),
         !is.na(thinness..1.19.years),
         !is.na(thinness.5.9.years),
         !is.na(Income.composition.of.resources),
         !is.na(Schooling))
summary(mydata)

#Model That Iucludes Every Variables
full_initial_model <- lm(Life.expectancy ~ Adult.Mortality + infant.deaths + Alcohol + percentage.expenditure + Hepatitis.B
                         + Measles + BMI + under.five.deaths +  Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP 
                         + Population + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources 
                         + Schooling, data = mydata)

print(summary(full_initial_model),digits=5)

#All The Simple Linear Regression of Each Independent Variable
simple_model_1 <- lm(Life.expectancy ~ Adult.Mortality, data = mydata)
summary(simple_model_1)

simple_model2 <- lm(Life.expectancy ~ infant.deaths, data = mydata)
summary(simple_model_2)

simple_model_3 <- lm(Life.expectancy ~ Alcohol, data = mydata)
summary(simple_model_3)

simple_model_4 <- lm(Life.expectancy ~  percentage.expenditure, data = mydata)
summary(simple_model_4)

simple_model_5 <- lm(Life.expectancy ~ Hepatitis.B, data = mydata)
summary(simple_model_5)

simple_model_6 <- lm(Life.expectancy ~ BMI, data = mydata)
summary(simple_model_6)

simple_model_7 <- lm(Life.expectancy ~ under.five.deaths, data = mydata)
summary(simple_model_7)

simple_model_8 <- lm(Life.expectancy ~ Polio, data = mydata)
summary(simple_model_8)

simple_model_9 <- lm(Life.expectancy ~ Total.expenditure, data = mydata)
summary(simple_model_9)

simple_model_10 <- lm(Life.expectancy ~ Diphtheria, data = mydata)
summary(simple_model_10)

simple_model_11 <- lm(Life.expectancy ~ HIV.AIDS, data = mydata)
summary(simple_model_11)

simple_model_12 <- lm(Life.expectancy ~ GDP, data = mydata)
summary(simple_model_12)

simple_model_13 <- lm(Life.expectancy ~ Population, data = mydata)
summary(simple_model_13)

simple_model_14 <- lm(Life.expectancy ~ thinness..1.19.years, data = mydata)
summary(simple_model_14)

simple_model_15 <- lm(Life.expectancy ~ thinness.5.9.years, data = mydata)
summary(simple_model_15)

simple_model_16 <- lm(Life.expectancy ~ Income.composition.of.resources, data = mydata)
summary(simple_model_16)

#The Chosen Simple Linear Regression Model
simple_model_17 <- lm(Life.expectancy ~ Schooling, data = mydata)
summary(simple_model_17)

#Multiple Linear Regression

model_2 <- lm(Life.expectancy ~ Schooling + Hepatitis.B + Diphtheria + Polio  + Measles + HIV.AIDS, data = mydata)
summary(model_2)

model_3 <- lm(Life.expectancy ~ Schooling + Hepatitis.B + Diphtheria + Polio  + Measles + HIV.AIDS + BMI + Alcohol +  percentage.expenditure, data = mydata)
summary(model_3)

#Check For Best Model

#Compare Between Multiple Linear Regression of Model 2 and Multiple Linear Regression of Model 3
#Sig test
# H0: b7 = b8 = b9 = 0
# H1: bi != 0 for at least one i, where i = 7, 8, 9
anova(model_2, model_3)
# p-value = 2.2e-16 (near 0) < 0.05
# Reject H0 at 5% sig level.
# Regressors X7, X8 and X9 cannot be removed together.

#Compare Between Multiple Linear Regression of Model 3 and Simple Linear Regression Model
#Sig test
# H0: b2 = b3 = b4 =b5 = b6 = b7 = b8 = b9 = 0
# H1: bi != 0 for at least one i, where i = 2, 3, 4, 5, 6, 7, 8, 9
anova(simple_model_17, model_3)
# p-value = 2.2e-16 (near 0) < 0.05
# Reject H0 at 5% sig level.
# Regressors X7, X8 and X9 cannot be removed together.

#Compare Between Multiple Linear Regression of Model 2 and Simple Linear Regression Model
#Sig test
# H0: b2 = b3 = b4 =b5 = b6 = 0
# H1: bi != 0 for at least one i, where i =  2, 3, 4, 5, 6,
anova(simple_model_17, model_2)
# p-value = 2.2e-16 (near 0) < 0.05
# Reject H0 at 5% sig level.
# Regressors X7, X8 and X9 cannot be removed together.

#Best Model is model_3

#Prediction Interval using the Best Model: model_3
#Using Central Tendency = Mean As A Suitable Set of X Values
test <- data.frame(Schooling = 12.119891, Hepatitis.B = 79.217708, Diphtheria = 84.155246, Polio = 83.564585, 
                   Measles = 2224.494239, HIV.AIDS = 1.983869, BMI = 38.128623, Alcohol = 4.533196, percentage.expenditure = 698.973558)
predict.lm(model_3, newdata = test, interval = "prediction", level = 0.95)
#The 95 CI of Y = [60.89602 77.70859]
