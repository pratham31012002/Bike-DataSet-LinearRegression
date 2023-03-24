rm(list = ls())
setwd("~/Downloads") # Note: Set the working directory to the path where you download the dataset
data = read.csv('day.csv') 
#data = data[-c(442), ] #Removing outliers for Model 1
data = data[-c(2, 27, 65, 66, 239, 249, 250, 328, 668), ] #Removing outliers for the final model
attach(data)

#Declaring categorical variables
sea = factor(season)
wday = factor(workingday)
weatsit = factor(weathersit)

#Summarizing response variable to show if its suitable to fit a linear
#regression model or not
summary(cnt)
sd(cnt)
hist(cnt)
boxplot(cnt)

#Checking association between response and explanatory variables
cor(temp, cnt)
plot(temp, cnt)
hist(temp)

cor(hum, cnt)
plot(hum, cnt)
hist(hum)

cor(windspeed, cnt)
plot(windspeed, cnt)
hist(windspeed)

table(sea)
barplot(table(sea))
boxplot(cnt ~ season)

table(weatsit)
barplot(table(weatsit))
boxplot(cnt ~ weatsit)

table(wday)
barplot(table(wday))
boxplot(cnt ~ wday)

#Propose an initial model
M1 = lm(cnt ~ temp + hum + windspeed + sea + wday + weatsit, data = data)
summary(M1)

#Checking assumptions using residual plots to determine the 
#adequacy of the model
SR = rstandard(M1)
hist(SR)
qqnorm(SR)
qqline(SR)
plot(M1$fitted.values, SR)

plot(temp, SR)
plot(hum, SR)
plot(windspeed, SR)

#determining outliers and influential points
which(SR > 3 | SR < -3)
C = cooks.distance(M1)
which(C > 1)

#testing the significance of categorical variables sea and weatsit
anova(lm(cnt ~ temp + hum + windspeed + wday + weatsit + sea))
anova(lm(cnt ~ temp + hum + windspeed + sea + wday + weatsit))

#As the scatterplot of SR and fitted values showed a funnel shape, 
#we log the response variable and remove workingday(refer to 5.). 
#We also remove the outlier from the data.
M2 = lm(log(cnt) ~ temp + hum + windspeed + sea + weatsit, data = data)
summary(M2)

#We try to explore the significance of interaction terms, by taking 
#all pairs possible. We also include the workingdays(to test its 
#relation with other variables, refer to 5.).
M3 = lm(
  log(cnt) ~ temp + hum + windspeed + sea + wday + weatsit + temp * hum + temp *
    windspeed + temp * wday + temp * weatsit + temp * sea + hum * windspeed + hum *
    wday + hum * weatsit + hum * sea + windspeed * wday + windspeed * weatsit + windspeed *
    sea + wday * weatsit + wday * sea + weatsit * sea,
  data = data
)
summary(M3)

#Final Model
#We remove insignificant interaction terms(use intuition and trial 
#and error to see change in adjusted R^2 value and observe changes 
#in the corresponding plots) and also remove the outliers of the new model.
M4 = lm(
  log(cnt) ~ temp + hum + windspeed + sea + wday + weatsit + temp * sea + hum * weatsit
  + windspeed * weatsit + wday * weatsit,
  data = data
)
summary(M4)

#Determining outliers and influential points and observing changes on 
#removing them(Check line 5)
SR = rstandard(M4)
which(SR > 3 | SR < -3)

C = cooks.distance(M4)
which(C > 1)

#Checking assumptions using residual plots to determine the 
#adequacy of the model
SR = rstandard(M4)
hist(SR)
qqnorm(SR)
qqline(SR)
plot(M4$fitted.values, SR)

plot(temp, SR)
plot(hum, SR)
plot(windspeed, SR)
