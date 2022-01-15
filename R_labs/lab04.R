## Lab 4: Cross Validation and Bootstrap

## Cross Validation
library(ISLR)
Auto = Auto
plot(mpg ~ horsepower, data = Auto)
dim(Auto)
##############################################################################################################################

attach(Auto)

## linear model ##

# split half into training and validation sets
set.seed(1234)
train = sample(x = 392, size = 392/2)

# linear model: regress mpg on horsepower using training data set
lm_mpg = mpg ~ horsepower
lm_fit = lm(lm_mpg, data = Auto, subset = train)
summary(lm_fit)

# calculate MSE of validation set
MSE_1 = mean((mpg - predict(lm_fit, newdata = Auto))[-train]^2)



## polynomial model ##
## loop method: from degree 1 to 5 ##

MSE_vector = rep(0,5) #vector of MSE for degrees 1 - 5

#run a loop from degree 1 - 5 and generate MSE
degree = 1:5
for (i in degree){
  lm_fit = lm(mpg ~ poly(horsepower, i),
              data = Auto,
              subset = train
              )
  MSE_vector[i] = mean((mpg - predict(lm_fit, newdata = Auto))[-train]^2)
}
  
## plot MSE vs complexity for polynomial model ##
plot(x = degree, y = MSE_vector, type = "b")


##############################################################################################################################
### LOOCV ###
library(boot)

cv_error_loocv = rep(0,5)
cv_error_k = rep(0,5)



# `cv.glm()` can be used to perform cv, if we use `glm()` to fit a model without passing in the family argument, then it performs linear regression like `lm()` function

degree = 1:5
for (i in degree){
  glm_fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  
  #loocv error for model with complexity of degree i 
  cv_error_loocv[i] = cv.glm(Auto, glm_fit)$delta[1]
  
  #10-fold cv error for model with complexity of degree i 
  cv_error_k[i] = cv.glm(Auto, glm_fit, K = 10)$delta[1]
}

# Plot the cv errors v.s. degree of the polynomial:
plot(x = degree, y = cv_error_loocv, type = "l")
lines(x = degree, y = cv_error_k, col = "red")



##############################################################################################################################
## Bootstrap ###

# We use Portfolio data set in the ISLR package
# alpha_fn() function takes as input the (X,Y) data as well as a vector indicating which observations should be used to estimate alpha
  # Note that $\alpha = \min \text{var}[\alpha X + (1 - \alpha Y)]$.

library(ISLR)

alpha_fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  # solution of alpha s.t. min. var
  alpha = (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
  return(alpha)
}

# randomly select 100 observations from 1 to 100 with replacement, i.e. constract a new bootstrap data and compute the corresponding alpha
set.seed(1)



#-------------------- LOGIC -----------------------------------#
# You collect a sample of 100 from the population. You use this sample to estimate alpha^, but you want to know the bias and variance of your estimate 
# (note that each run of alpha^ below will be different, because alpha is a function of RVs)


#this is alpha^
#you can't actually find the bias or std error of this finite sample estimator because you only get 1 sample from the population
#but you can sometimes find the asymptotic bias or std error 
alpha = alpha_fn(data = Portfolio, index = sample(x = 1:100, size = 100, replace = T))




# Bootstrap using "for" loop:
boot_alpha = rep(0, 1000)
for(i in 1:1000){
  boot_index = sample(x = 1:100, size = 100, replace = T)
  boot_alpha[i] = alpha_fn(data = Portfolio, index = boot_index)
}
mean(boot_alpha)
sd(boot_alpha)


# Use boot() function to produce R=1000 bootstrap estimates for alpha's bias and variance
boot(Portfolio, alpha_fn, R = 1000)
#bootstrapped estimator alpha* = 0.5758
#bias of alpha* = -0.001352
#std error of alpha*

#-------------------- LOGIC END -----------------------------------#




################################################################################################################################
### Estimating the accuracy of a linear regression model ###
################################################################################################################################

#this is your object of interest. it returns the intercept and beta coefficient
boot_fn = function(data, index) {   
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}

# This returns the intercept and slope estimates for the linear regression model, given this sample. 
# (if you sample again from the population, you'll get a different estimate)
boot_fn(Auto, 1:392)



set.seed(1234)
boot_fn(Auto, sample(392, 392, replace = T)) # this is like a single bootstrap result



# Now we use boot() to compute the standard errors of 1000 bootstrap estimates for the intercept and slope
  # we're interested in bootstrap estimates of the BIAS and STD ERROR of the coefficients!!!
library(boot)



boot(Auto, boot_fn, R = 1000)



# Compare with standard formula results for the regression coefficients in a linear model
summary(lm(mpg ~ horsepower, data = Auto))$coef


