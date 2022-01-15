#install.packages("MASS")
#install.packages("ISLR")
library(MASS)
library(ISLR)

rm(list = ls())

View(Boston)
lm_fit = lm(medv ~ lstat, data = Boston)
summary(lm_fit)

attach(Boston)

## predict using new data ##

predict(lm_fit, data.frame(lstat = c(5, 10, 15)))
  # predicts new data given (model, new x variable data)

predict(lm_fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")


predict(lm_fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

#plot
plot(lstat, medv)
abline(lm_fit)
abline(lm_fit, lwd = 3)
abline(lm_fit, lwd = 3, col = "red")

plot(lstat, medv, col = "red", pch = "+")



#Diagnostics - NICE - normal, independent, constant variance, E(e) = 0
par(mfrow = c(2,2))
plot(lm_fit)




