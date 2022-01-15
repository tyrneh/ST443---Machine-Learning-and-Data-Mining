# Lab 5 on Best Subset Selection, Forward and Backward Stepwise Selection,
# Validation Set Approach and K fold Cross Validation

rm(list = ls()) 

library(ISLR)
? Hitters

summary(Hitters)
head(Hitters$Salary)
## Some missing values in Salary
sum(is.na(Hitters$Salary))

## Remove those missing values
Hitters = na.omit(Hitters)
dim(Hitters)
## Check any missing values?
sum(is.na(Hitters))

head(Hitters)


############################################## Lab 5_1: Best Subset Selection
## regsubsets() function perfroms best subset selection by identifying the best model that contains a given number of predictors, where best is quantified by RSS

#install.packages('leaps')
library(leaps)

regfit_full = regsubsets(Salary ~ ., data = Hitters)
reg_summary = summary(regfit_full)
reg_summary

## It gives by default best-subsets up to size 8; let us increase to 19
regfit_full = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg_summary = summary(regfit_full)
reg_summary
names(reg_summary)
reg_summary$rss


## Plot RSS, adjusted Rsq, Cp and BIC for all of the models at once, this would help us decide which model to select
par(mfrow=c(2,2))
plot(reg_summary$rss, xlab="Number of Variables", ylab="RSS")


## Plot adjusted R2 vs Number of Variables
plot(reg_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")

## which_max() function is used to identify the location of the maximum point of a vector
best_model = which.max(reg_summary$adjr2)
best_model

## Plot a red dot to indicate the model with the largest adjusted R2
points(best_model, reg_summary$adjr2[best_model], col="red", cex=2, pch=20)


## In a similar fashion, we can plot Cp and BIC
plot(reg_summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
(best_model = which.min(reg_summary$cp))
points(best_model, reg_summary$cp[best_model], col="red", cex=2, pch=20)

plot(reg_summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
(best_model = which.min(reg_summary$bic))
points(best_model, reg_summary$bic[best_model], col="red", cex=2, pch=20)


## regsubsets() function has a build-in plot() command which can display the selected variable for the best model with a given number of predictors
par(mfrow=c(2,2))
plot(regfit_full, scale="r2")
plot(regfit_full, scale="adjr2")
plot(regfit_full, scale="Cp")
plot(regfit_full, scale="bic")



## Top row of each plot contains a black square for each variable selected according to the optimal model associated with that statistic
## e_g_ BIC choose six-variable model we use coef() to see the coefficient estimates associated with this model
coef(regfit_full, id = 6)















################################################ Lab 5_3 Choosing among models using Validation Set Approach and Cross-Validation
set.seed(1)

###########################################################################################################################################################
# 1: Validation set approach:
# randomly split the Hitters data into Train and Validation datasets. Then use Validation dataset to choose the best variables that maximize validation R^2
###########################################################################################################################################################

dim(Hitters) #Hitters has 263 observations. We use half of these to be Training data

train = sample(x = nrow(Hitters), size = nrow(Hitters)/2, replace = FALSE)
train


## We now apply resubsets() on the training data to perform best subset selection
regfit_1 = regsubsets(Salary ~ .
                      , data = Hitters[train,]
                      , nvmax = 19
                      , method = "exhaustive"
                      )
summary(regfit_1)


##################################################
#method 1 for generating MSE of validation data #
#################################################

##
## we need to manually create the predicted Y_hat values using test data
## so first, we need to create the design matrix, also known as model matrix or regressor matrix, often denoted by X
##

## We make a design (or model) matrix from the testing data
test_mat = model.matrix(Salary ~ ., data = Hitters[-train, ]) #model.matrix automatically converts data to design matrix 
                                                                #(adds intercept column, converts string columns to binary)
head(test_mat)
dim(test_mat)

#initialize an error vector to save MSE of every model complexity into this vector
val_errors = rep(0, 19)




## Try all models with size i ranges from 1 to 19
for(i in 1:19){
  
  ## Get the coefficients (b0, b1, ..., bi) estimates associated with model (size i) 
  coef_i = coef(regfit_1, id = i)
  
  ## Get the prediction for the tesing data using the corresponding columns in the design matrix, matrix-multiplied (%*%) by the estimated coefficients in coef_i 
  pred_test = test_mat[, names(coef_i)] %*% coef_i
  
  ## Compute the mean square error
  val_errors[i] = mean((Hitters$Salary[-train] - pred_test) ^ 2)
}


#find optimal model size that minimizes test MSE, and display which predictors are used
optimal_size = which.min(val_errors)
coef(regfit_1, id = optimal_size)

# plot MSE against model complexity for validation data
par(mfrow=c(1,1))
plot(x = seq(19), y = sqrt(val_errors), type = 'l', xlab = 'num of predictors', ylab = 'Root MSE', ylim = c(200,450))
points(x = optimal_size, y = sqrt(val_errors[optimal_size]), pch = 20, col = 'red', cex = 2)

## Plot of Root MSE vs model size for training data
points(sqrt(regfit_1$rss[-1]/length(train)), col="red", pch=19, type="l")
                            # [-1] because you don't want to display MSE of 0 predictors
                            # divide by length of data because $rss is the model's sum of rss from all observations in the training data

legend("topright",legend=c("Validation","Training"), col=c("black","red"), pch=19)




##################################################
#method 2 for generating MSE of validation data #
#################################################

## There is no predict() method for regsubsets(), we summarize the steps for our computation above and write our own version of the predict function as
predict_regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]]) #draw formula from the object input. $call retruns the entire regsubsets() code, and $call[2] returns the y ~ x object
  mat = model.matrix(form, newdata) #creates design matrix
  coef_i = coef(object, id = id) #stores coefficient estimates
  mat[, names(coef_i)] %*% coef_i #matrix multiplication of design matrix * coefficient matrix to generate predicted y_hat values
}


## Repeat the above steps for computing test error rate for models with size from 1 to 19
val_errors2 =rep(0, 19)
for(i in 1:19){
  val_errors2[i] =mean((Hitters$Salary[-train] - predict_regsubsets(regfit_1, Hitters[-train,], id=i))^2)  
}

## Check whether our written function could provide the same result or not
sum(abs(val_errors2 - val_errors))















###########################################################################################################################################################
# 2: k-fold cross validation approach:
###########################################################################################################################################################

## K-Cross Validation Approach using forward stepwise selection (FSS), this part is very important, since it provides you a sample code of writing K-fold CV
K =10
set.seed(11)
folds = sample( rep(1:10, length = nrow(Hitters)) )
  # rep(x, length = 100) means it repeats x until the rep length is 100
  # rep(x, times = 100) means it repeats x 100 times, so if x is a vector > len(1), then the length of the rep will be higher than 100

  ### "so rep(1:10, length = nrow(Hitters)) will repeat (1,2,3,...,10,1,2,...,10,1,2,...,10...) 
    #  until the length of this rep vector reaches the number of observations in Hitters"

  ### then 'folds' samples using x = rep(), and size defaults to same length of rep()


folds
  ### so folds ends up randomly assigning observations into their own K-group 

table(folds)




## We initialize a error matrix with row (10 different folds) and column (19 different predictors)
cv_errors = matrix(0, 10, 19)


## We write a for loop that performs cross-validation, in the kth fold, the elements of folds that equal k are in the test set and the remainder are in the training set
for(k in 1:10){
  fit_fwd = regsubsets(Salary~., data=Hitters[folds!=k,], nvmax=19, method="forward")
  
  for(d in 1:19){
    pred = predict_regsubsets(fit_fwd, Hitters[folds==k,], id = d)
    cv_errors[k,d] = mean((Hitters$Salary[folds==k]-pred)^2)
  }
}
cv_errors

## Average of the cv_error over all 10 folds
rmse_cv = sqrt(apply(cv_errors,2,mean)) #apply mean function over columns (2) of cv_errors matrix
rmse_cv



## Plot of Root MSE vs model size and choose the optimal model size
plot(rmse_cv, ylab = 'Root MSE', xlab = 'Complexity', type = 'l')
optimal_size_cv = which.min(rmse_cv)
points(x = optimal_size_cv, y = rmse_cv[optimal_size_cv], pch = 20, col = 'red', cex = 2)
