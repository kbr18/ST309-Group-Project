a=1
a=2
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library(dplyr)
install.packages("tree")
install.packages("ISLR")
library(tree)
library(ISLR)


JPRF <- read.csv("JP RF.csv") #Importing the risk-free data
JPN225 <- read.csv("JPMKT.csv") #Importing the market data (Nikkei 225 index)
View(JPRF)
View(JPN225)
dim(JPRF)
dim(JPN225)

#Creating variables to add to the dataset

JPRF$Return <- (JPRF$Close - JPRF$Open)/JPRF$Open #Calculating the risk-free return

JPN225$Return <- (JPN225$Close - JPN225$Open)/JPN225$Open #Calculating the market index return

EPJPRF <- JPRF[1:38,] #Creating a subset from the risk-free data, i.e. our Estimation Period, from 9th May to 29th June

EWJPRF <- JPRF[39:88,] #Creating a subset from the risk-free data, i.e. our Event Window, from 2nd July to 7th September

EPJPN225 <- JPN225[1:38,] #Creating a subset from the market index data, i.e. our Estimation Period, from 9th May to 29th June

EWJPN225 <- JPN225[39:88,] #Creating a subset from the market index data, i.e. our Event Window, from 2nd July to 7th September

EPJPN225$DateNum <- as.numeric(EPJPN225$Date)
EWJPN225$DateNum <- as.numeric(EWJPN225$Date)

View(EPJPN225)
View(EWJPN225)

### PLOTTING

EPJPN225$MRP<- (EPJPN225$Return- EPJPRF$Return)

plot(EPJPN225$DateNum, EPJPN225$MRP)

EWJPN225$MRP<- (EWJPN225$Return- EWJPRF$Return)

View(EPJPN225)
View(EWJPN225)

y<- EPJPN225$MRP
x<- EPJPN225$DateNum

xsq<- x^2
xcub<- x^3
xquar<- x^4

plot(x,y)

fit1<- lm(y~x)
anova(fit1)
abline(fit1, col="red")

fit4<-lm(y~x+xsq+xcub+xquar)
summary(fit4)
anova(fit4)

xv<-seq(min(x), max(x), 0.01)

yv<-predict(fit4, list(x=xv, xsq = xv^2, xcub = xv^3, xquar = xv^4))

lines(xv,yv, col="green")

#GREEN IS WHAT WE ESTIMATE

#Data manipulation and extraction:

summary(EPJPN225$MRP)

View(EPJPN225)

#TREE TRIALS

#SAME ERROR: factor predictors must have at most 32 levels

#LOGISTIC REGRESSIONS IN R to model the probability MRP belongs to a particular class

summary(EPJPN225)
HighMRP=ifelse(EPJPN225$MRP<=-0.002545, "No", "Yes")
EPJPN225.2=data.frame(EPJPN225,HighMRP)
View(EPJPN225.2)
names(EPJPN225.2)
dim(EPJPN225.2)
summary(EPJPN225.2)

install.packages("randomForest")
require(randomForest)

summary(tree.EPJPN225.2)


logistic.EPJPN225.2=glm(HighMRP~Open+High+Low+Close+Adj.Close+Volume+Return, data=EPJPN225.2,family=binomial)
summary(logistic.EPJPN225.2)

#predict can be used to predict the probability that the market will have HighMRP , given values of the predictors

pred.EPJPN225.2=predict(logistic.EPJPN225.2, type="response")

pred.EPJPN225.2HighMRP=rep("No", 38) # a sequence of "No" repeated 38 times, predicting for when market is high

pred.EPJPN225.2HighMRP[pred.EPJPN225.2>0.5]="High"
table(pred.EPJPN225.2HighMRP, HighMRP)

summary(EPJPN225.2)
View(EPJPN225.2)
