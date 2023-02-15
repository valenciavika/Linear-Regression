#2440062123-Vika Valencia Susanto
getwd()
setwd("C:/Users/62821/Documents/Binus Semester 4/Linear Regression")

real_estate <- read.csv("Real_estate.csv")
head(real_estate)

#Model using OLS
model = lm(Y.house.price.of.unit.area~X2.house.age+
             X3.distance.to.the.nearest.MRT.station+X4.number.of.convenience.stores, 
           data = real_estate)
summary(model)


#Find the residual of the model
error =resid(model)
error

library(car)
car::vif(model)
#None of the VIF is more than 10, no multicollinearity

#Kolmogorov Smirnov Test
#H0: The errors are normally distributed
#H1: The errors are not normally distributed
library(nortest)
lillie.test(error)
#p-value (9.90 * 10^-10) < 0.05, reject H0/null hypotesis, 
#The errors are not normally distributed

#Breusch Pagan Test
#H0: The errors are homoscedastic (constant variance)
#H1: The errors are heteroscedastic
library(lmtest)
bptest(model)
#p-value (0.3489) > 0.05, fail to reject H0/null hypotesis, 
#The errors are homoscedastic (constant variance)

#Durbin Watson Test
#H0: no autocorrelation exists (independent)
#H1: autocorrelation exists
dwtest(model)
#p-value (0.8808) > 0.05, fail to reject H0/null hypotesis,
#no autocorrelation exists (independent)

library(MASS)
RobustModel = rlm(Y.house.price.of.unit.area~X2.house.age+
                    X3.distance.to.the.nearest.MRT.station+X4.number.of.convenience.stores, 
                  data = real_estate)
summary(RobustModel)

error2 = resid(RobustModel)
lillie.test(error2)

#RSE
summary(model)$sigma
summary(RobustModel)$sigma

