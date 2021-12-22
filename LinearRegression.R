library(tidyverse)
library(dummies)
library(corrplot)
library(stats)
library(dplyr)

bikes <- read_csv("C:/Users/User/Downloads/Full Book/Chapter 4 Code/Student/Data/bikes.csv")

#Covariance among rentals and humidity
cov(bikes$humidity, bikes$rentals)

#Standard Deviation
sd(bikes$humidity)

sd(bikes$rentals)

#Pearson Correlation
cor(bikes$humidity, bikes$rentals)

#Visualize all the correlations
#Remove non-numeric date values

bikenumeric <- bikes %>%
  select(-date)


bike_correlations <- cor(bikenumeric)


corrplot(bike_correlations)



#Retrieving linear regression
bikes_mod1 <- lm(data = bikes, rentals ~ temperature)
summary(bikes_mod1)


#Multiple linear regression
bikes_mod2 <- lm(data = bikes, rentals ~ temperature + windspeed + humidity)
summary(bikes_mod2)


#Check for quadratic relationship - polynomial regression
bikes2 <- bikes %>%
  mutate(humidity2 = humidity^2) %>%
  mutate(windspeed2 = windspeed^2) %>%
  mutate(temperature2 = temperature^2)


#Create a model with the new variables
bikes3_mod <- lm(data=bikes2, rentals~humidity+windspeed+temperature+humidity2+windspeed2 + temperature2)
summary(bikes3_mod)


#Dummifying the categorical values
bikes2 <- bikes2 %>%
  mutate(season=recode(season, c("1"="Winter", "2"="Spring", 
                                  "3"="Summer", "4"="Fall")))




