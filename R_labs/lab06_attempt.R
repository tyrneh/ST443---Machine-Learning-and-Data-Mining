########################################################################################
## Lab 6.1: Ridge regression
## We will perform ridge regression and Lasso to predict Salary on the Hitters data

rm(list = ls()) 

#install.packages("glmnet")
library(glmnet)
#install.packages("ISLR")
library(ISLR)

# overview of dataset 
dim(Hitters)
summary(Hitters)

# deal with nulls
sum(is.na(Hitters))
Hitters = na.omit(Hitters)
attach(Hitters)

# we need to create a design matrix. 
# Remove first column because we don't want intercept column, as glmnet auto creates intercept column
x = model.matrix(Salary ~ ., data = Hitters)[,-1]
y = Hitters$Salary

## glmnet() does not use formula language
## glmnet() function has an alpha argument that determines what type of model is fit
## alpha=0 corresponds to the ridge regression and alpha=1 corresponds to Lasso by default
## glmnet() function standardizes the variables so that they are on the same scale. 
fit_ridge = glmnet(x, y, alpha = 0)
plot(fit_ridge, xvar = "lambda")
plot(fit_ridge, xvar = "dev")



## k-fold cross validation for to determine the optimal tuning parameter, lambda. By default k is set to be 10
cv.ridge = cv.glmnet(x, y, alpha=0, k = 10)
names(cv.ridge)
#minimum MSE lambda
cv.ridge$lambda.min
#lambda that is within 1SE of min MSE lambda
cv.ridge$lambda.1se

## Plot of CV mse vs log (lambda)
plot(cv.ridge)
## Coefficent vector corresponding to the mse which is within one standard error of the lowest mse using the best lambda.
coef(cv.ridge)
## Coefficient vector corresponding to the lowest mse using the best lambda
  #you used CV to find the minimum MSE lambda. Now you can specify this min lambda value to use in the lasso regression
coef(glmnet(x,y,alpha=0, lambda=cv.ridge$lambda.min))







#######################################################################################
## Lab 6.2: LASSO
fit_lasso <-glmnet(x,y, alpha = 1)
plot(fit_lasso, xvar="lambda", label= TRUE)
plot(fit_lasso, xvar="dev", label= TRUE)

cv_lasso = cv.glmnet(x, y, alpha = 1, k = 10)
plot(cv_lasso)


## coefficients - default coef() takes lambda at 1SE
coef(cv_lasso)

## coefficients - coef of min MSE lambda
  #you used CV to find the minimum MSE lambda. 
  #Now you can specify this min lambda value to use in the lasso regression
coef(glmnet(x,y, alpha = 1, k = 10, lambda = cv_lasso$lambda.min))


#once you use lasso to choose variables, you should refit using linear regression to avoid bias
#actually glmnet will give the same coefficients





#######################################################################################
## Lab 6.3: Principal Component Regression
#install.packages("pls")
library(pls)

set.seed(123)
train = sample(1:nrow(Hitters),180)

# "pcr()" to perform Principal Component Regression. 
# If "validation = `CV`": 10fold cross-validation is performed. If "validation = `LOO`", leave-one-out cross-validation is performed.
# If "scale" is TRUE, X is scaled by dividing each variable by its sample standard deviation
  #by setting scale=T, your predictor becomes X/sd, and the coefficient you get is sd*Beta
  #to recover betahat, do coef/sd (e.g. sd*beta/sd)
fit_pcr = pcr(Salary ~ ., data = Hitters[train,], validation = "LOO", scale = T) 
summary(fit_pcr)

# "selectNcomp()" Choosing the best number of components in PCR.
# "method = `onesigma`" implies the "1 stdandard error rule".
selectNcomp(fit_pcr, method = "onesigma", plot = TRUE)




##optional plot ##

# plot "Number of Components" vs "Standardized Coefficients"
bhats = fit_pcr$coefficients[,1,]
plot(c(1, 19), range(bhats), type = "n",
     xlab = "Number of Components", ylab = "Standardized Coefficients")
for(j in 1:4){
  lines(1:19, bhats[j,], type = "S", col = j, lty = j, lwd = 3)
}
legend(x = "topleft", legend = rownames(bhats)[1:4], col = 1:4, lty = 1:4, lwd = 3)
for(j in 5:19){
  lines(1:19, bhats[j,], type = "S", col = "gray", lty = 1, lwd = 1)
}




# Prediction on test set.
pmse = rep(NA, 19)
for(j in 1:19){
  yhat = predict(fit_pcr, ncomp = j, newdata = Hitters[-train,])
  pmse[j] = mean((yhat - Hitters$Salary[-train])^2)
}
plot(pmse, type = "o", xlab = "Number of Components", ylab = "PMSE")





## Recover the original coefficients.
fit_pcr = pcr(Salary ~ ., data = Hitters, scale = T)
bhat_pcr = fit_pcr$coefficients[,1,19]
sd = apply(x, 2, sd) #2 means column - each column is 1 variable
sd
bhat_pcr_rec = bhat_pcr/sd

# Comparing to lm():
bhat_lm = coef(lm(Salary ~ ., data = Hitters))[-1]
cbind(bhat_pcr, bhat_pcr_rec, bhat_lm)
