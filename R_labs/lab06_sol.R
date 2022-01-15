## Lab 6: Ridge Regression and Lasso
## we will use glmnet package to perfrom ridge regression and lasso
library(glmnet)
library(ISLR)
summary(Hitters)

## Lab 6.1: Ridge regression
## We will perform ridge regression and Lasso to predict Salary on the Hitters data
## Be sure to remove the missing values
Hitters <-na.omit(Hitters)
## glmnet does not use formula language
# x <-model.matrix(Salary~.-1, data=Hitters)
x <-model.matrix(Salary~., data=Hitters)[,-1]
y <-Hitters$Salary

## glmnet() function has an alpha argument that determines what type of model is fit
## alpha=0 corresponds to the ridge regression and alpha=1 corresponds to Lasso by default
## glmnet() function standardizes the variables so that they are on the same scale. 
fit.ridge <-glmnet(x, y, alpha=0)
## plot of the solution path, i.e. estimated coefficients vs log (lambda), where lambda is the tuning parameter
plot(fit.ridge, xvar="lambda", label= TRUE)
plot(fit.ridge, xvar="dev", label= TRUE)

## k-fold cross validation for to determine the optimal tuning parameter, lambda. By default k is set to be 10
cv.ridge <-cv.glmnet(x, y, alpha=0)
## Plot of CV mse vs log (lambda)
plot(cv.ridge)
## Coefficent vector corresponding to the mse which is within one standard error of the lowest mse using the best lambda.
coef(cv.ridge)
## Coefficient vector corresponding to the lowest mse using the best lambda
coef(glmnet(x,y,alpha=0, lambda=cv.ridge$lambda.min))

## Of course, you can try a range of lambda values for the ridge regression
## Here we try a grid of values ranging from lambda=10^10 to lambda=10^{-2}, 100 different lambda values
grid <-10^seq(10, -2, length=100)
fit.ridge1 <-glmnet(x,y,alpha=0, lambda=grid)
## 21 rows (one for each predictor plus an intercept) and 100 columns (one for each value of lambda)
dim(coef(fit.ridge1))
## What is the 50th lambda value and the corresponding estimated coefficients by the ridge regression?
fit.ridge1$lambda[50]
coef(fit.ridge1)[,50]

## Lab 6.2: LASSO
fit.lasso <-glmnet(x,y)
plot(fit.lasso, xvar="lambda", label= TRUE)
plot(fit.lasso, xvar="dev", label= TRUE)
cv.lasso <-cv.glmnet(x, y)
plot(cv.lasso)
## coefficent vector corresponding to the mse which is within one standard error of the lowest mse using the best lambda.
coef(cv.lasso)
## coefficient vector corresponding to the lowest mse using the best lambda
coef(glmnet(x,y, lambda=cv.lasso$lambda.min))

## Validation set approach to select best lambda in Lasso
set.seed(1)
train <-sample(seq(263), 180, replace=FALSE)
lasso.train <-glmnet(x[train,], y[train])
lasso.train
pred.test <-predict(lasso.train, x[-train,])
dim(pred.test)
rmse <-sqrt(apply((y[-train]-pred.test)^2,2,mean))
plot(log(lasso.train$lambda), rmse, type="b", xlab="Log(lambda)")
lambda.best <-lasso.train$lambda[order(rmse)[1]]
lambda.best

## Lab 6.3: Principal Component Regression
library(pls)

set.seed(123)
train = sample(1:nrow(Hitters), 180)

# "pcr()" to perform Principal Component Regression. 
# If "validation = `CV`": cross-validation is performed. If "validation = `LOO`", leave-one-out cross-validation is performed.
# If "scale" is TRUE, X is scaled by dividing each variable by its sample standard deviation
fit.pcr = pcr(Salary ~ ., data = Hitters[train,], validation = "LOO", scale = T)
summary(fit.pcr)

 

# plot "Number of Components" vs "Standardized Coefficients"
bhats = fit.pcr$coefficients[,1,]
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
  yhat = predict(fit.pcr, ncomp = j, newdata = Hitters[-train,])
  pmse[j] = mean((yhat - Hitters$Salary[-train])^2)
}
plot(pmse, type = "o", xlab = "Number of Components", ylab = "PMSE")

## Recover the original coefficients.
fit.pcr = pcr(Salary ~ ., data = Hitters, scale = T)
bhat.pcr = fit.pcr$coefficients[,1,19]
sd = apply(x, 2, sd)
bhat.pcr.rec = bhat.pcr/sd
# Comparing to lm():
bhat.lm = coef(lm(Salary ~ ., data = Hitters))[-1]
cbind(bhat.pcr, bhat.pcr.rec, bhat.lm)
