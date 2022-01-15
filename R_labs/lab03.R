## Lab 4: Cross validation and bootstrap
library(ISLR)
Auto = Auto
attach(Auto)
plot(mpg ~ horsepower, data = Auto)
dim(Auto)


#########################################################################################
## Validation set approach: randomly split data, use half as training and half as test ##
#########################################################################################

set.seed(1234)
train = sample(x = dim(Auto)[1], size = dim(Auto)[1]/2) #set half as training data

### Linear model: regress mpg on horsepower in linear model ###
lm_mpg = mpg ~ horsepower
lm_fit = lm(lm_mpg, data = Auto, subset = train)
summary(lm_fit)

# MSE of observations in test(validation) data set #
y_hat = predict(lm_fit, newdata = Auto) 
  # use newdata instead of data to get predictions on entire 
  # Auto datset rather than just training dataset
MSE_test_lm = mean((mpg - y_hat)[-train] ^ 2)



### linear model, with quadratic term ###
lm_mpg_2 = mpg ~ poly(horsepower, 2)
lm_fit_2 = lm(lm_mpg_2, data = Auto, subset = train)
summary(lm_fit_2)

#MSE of observations in validation set
y_hat_2 = predict(lm_fit_2, newdata = Auto)
MSE_test_lm_2 = mean((mpg - y_hat_2)[-train]^2) #test MSE is smaller with quadratic term, so prediction is better than before


### linear model, with cubic term ###
lm_mpg_3 = mpg ~ poly(horsepower, 3)
lm_fit_3 = lm(lm_mpg_3, data = Auto, subset = train)
summary(lm_fit_3)

#MSE of observations in validation set
y_hat_3 = predict(lm_fit_3, newdata = Auto)
MSE_test_lm_3 = mean((mpg - y_hat_3)[-train]^2) #model is now both more complex and worse MSE


### plot of MSE vs complexity ###
plot(x = c(1,2,3) #x is number of predictors in model
     , y = c(MSE_test_lm, MSE_test_lm_2, MSE_test_lm_3) #y is the MSE of each model
     , type = 'l', 
     main = "Test MSE vs Complexity"
     , ylab = "Test MSE"
     , xlab = "Degree of Poly")




#########################################################################################
## LOOCV Apprach ########################################################################
#########################################################################################
library(boot)

#use glm under default linear family, because cross validation package works only for glm
glm_fit = glm(lm_mpg, data = Auto)

#CV using LOOCV
loocv = cv.glm(data = Auto, glm_fit) #default K value is LOOCV
loocv_MSE = loocv$delta[2]
loocv_MSE


# LOOCV MSE shortcut: write a function to use shortcut formula
  # LOOCV can be computationally intensive. A shortcut can be used for linear models
  # see pg. 12 Lecture 3
loocv = function(fit){
  h = lm.influence(fit)$hat 
    #lm.influence returns $hat (diagonal of the "hat" matrix), $coefficients, $sigma, $wt.res
  MSE = mean((residuals(fit)/(1-h)) ^ 2)
  return(MSE)
}

loocv(glm_fit)


### Plot CV MSE vs degree of polynomial ###
cv_error1 = rep(0, 5) #MSE using formula
cv_error2 = rep(0, 5) #MSE using LOOCV

#calculating MSE for degrees 1 to 5:
degree = 1:5
for (i in degree){
  glm_fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  
  #CV using formula:
  cv_error1[i] = loocv(glm_fit) #loocv() is a function of the shortcut formula
  
  #CV using LOOCV:
  cv_error2[i] = cv.glm(data = Auto, glm_fit)$delta[2]
}

cv_error1
cv_error2

plot(degree, cv_error1, type = "b") #creates graph of MSE using formula
lines(degree, cv_error2, col = "red") #add a line for MSE using LOOCV


#########################################################################################
## 5-fold CV ############################################################################
#########################################################################################
cv_error3 = rep(0, 5)

#calculating MSE for degrees 1 to 5:
for (i in degree){
  glm_fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  
  #CV using 5-fold cv:
  cv_error3[i] = cv.glm(data = Auto, glm_fit, K = 5)$delta[2]
}

lines(degree, cv_error3, col = "blue")


