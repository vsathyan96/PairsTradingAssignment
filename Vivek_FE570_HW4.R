#Vivek Sathyanarayana
#FE 570 Spring 2019
#HW4 Problem 1

library("stats")
library("tseries")
library("dplyr")

#Read file from text file
dataCVX <- read.csv("CVX.csv", head=T)
dataXOM <- read.csv("XOM.csv", head=T)

#Create the log return vector for CVX 
y<-vector(mode="numeric",length=(nrow(dataCVX)-1))
for(i in 1:(nrow(dataCVX)-1))
{
  y[i]<-log(dataCVX$Close[i+1])-log(dataCVX$Close[i])
}

#Create the log return vector for XOM
x<-vector(mode="numeric",length=(nrow(dataXOM)-1))
for(i in 1:(nrow(dataXOM)-1))
{
  x[i]<-log(dataXOM$Close[i+1])-log(dataXOM$Close[i])
}

#Part (1)
#Linear Regression 
fit <- lm(y~x)
summary(fit)

#Extract coefficients from regression
e <- as.numeric(fit$residuals)
c <- fit$coefficients[[1]]
a <- fit$coefficients[[2]]

#Part (2)
test <- adf.test(e)

#Print adf test results
test

#Result shows that residuals are stationary

#Part (3)
z <- y - (a*x) + c
delta <- 2 * sd(z)

#Declare time and order type vectors
t = vector(mode="integer",length = 0)
port.order <- vector(mode="character",length = 0)

for (i in 1:length(x)) {
#round() is used on the valuese in the conditions to obtain good trade signals  
  
  #Condition for Short portfolio
  if(round(y[i]-(a*x[i]),digits = 2)==round(c+delta,digits = 2)) {
    if(length(t)==0) {
      t=rbind(t,i)
      port.order <- rbind(port.order,"SHORT")
      
    }
    #Additional condition to ensure alternating buy/sell strategy
    if((length(t)>0)&(port.order[length(port.order)]=="LONG")) {
      t=rbind(t,i)
      port.order <- rbind(port.order,"SHORT")
    }
  }
  
  #Condition for Long Portfolio
  if((round(y[i]-(a*x[i]),digits = 2)==round(c-delta,digits = 2))) {
    if(length(t)==0) {
      t=rbind(t,i)
      port.order <- rbind(port.order,"LONG")
    }
    #Additional condition to ensure alternating buy/sell strategy
    if((length(t)>0)&(port.order[length(port.order)]=="SHORT")) {
      t=rbind(t,i)
      port.order <- rbind(port.order,"LONG")
    }
  }
}

#Compile data frame with trade data
trade.info <- data.frame(t,port.order)
colnames(trade.info) <- c("Time","Portfolio Order Type")

#Create vector to number rows
vec1 <- seq(1,length(trade.info[,1]),1)
rownames(trade.info) <- c(vec1)

#Print trades
trade.info