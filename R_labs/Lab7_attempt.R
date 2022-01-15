## Lab 6: Moving Beyond Linearity  ############################################################################
rm(list = ls()) 

library(ISLR)
?Wage
attach(Wage)
summary(Wage)

#check for missing values
sum(is.na(Wage))

################################################### Lab 6.1: Polynomial Regression and Step Functions  ############################################################################
## Fit a polynomial regression with degree=4
par(mfrow = c(1,1))
plot(age,wage)
plot(age,(wage>250))


fit_degree4 = lm(wage ~ poly(age, 4), data = Wage)
summary(fit_degree4)


## Let us create a grid of values for age at which we want predictions 
age_grid = seq(from = range(age)[1], to = range(age)[2], by = 1)

preds = predict(fit_degree4, newdata = list(age=age_grid), se.fit=T)
se_bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(x = age, y = wage, col="darkgrey")
title("Degree 4 Polynomial")
points(x = age_grid, y = preds$fit, type = 'l', col = 'blue', lwd = 2)
matlines(x = age_grid, y = se_bands, lwd = 1, col = 'blue', lty=2)




## Some other ways of doing this in R. For example
## poly() function generates a basis of orthogonal polynomials
## We generate a basis of polynomials
fit.a <-lm(wage~ age + I(age^2)+ I(age^3)+ I(age^4), data=Wage)
summary(fit.a)
## Two methods provide the same fitted values
plot(fitted(fit), fitted(fit.a))



######################################################################
### cross validation to choose best poly degree model ###########
library(boot)

cv_error = rep(0,10)

degree = 1:length(cv_error)
for (i in degree) {
  glm_fit = glm(wage ~ poly(age, i), data = Wage)
  
  #cv using k-fold:
  cv_error[i] = cv.glm(data = Wage, glm_fit, K = 10)$delta[2]
}

# poly of degree 6 gives minimum CV error
cv_error
which.min(cv_error)



### adjr2 of each poly model #############################
adjr2 = rep(0,10)

degree = 1:length(cv_error)

for (i in degree) {
  lm_fit = lm(wage ~ poly(age, i), data = Wage)
  
  #adjr2 
  adjr2[i] = summary(lm_fit)$adj.r.squared
}

#poly of degree 9 gives maximum adjr2
adjr2
which.max(adjr2)


###### plot #######################
par(mfrow = c(1,2))

plot(x = degree, y = cv_error, type = 'b')
title("CV Error")

plot(x = degree, y = adjr2, type = 'b')
title("Adjusted R Squared")

######################################################################



## Polynomial logistic regression
fit.logit <-glm(I(wage>250) ~ poly(age, 4), data= Wage, family="binomial")
summary(fit.logit)
## Further details can be learnt from textbook ISLR: PAGE 292






## Step function
table(cut(age,4)) # cut - cuts domain into 4 equal intervals Number of observations in each interval will be different of course
fit.cut <-lm(wage~ cut(age, 4), data= Wage)
summary(fit.cut)











####################################################### Lab 6.2: Splines ############################################################################
#install.packages("splines")
library(splines)

##------ Fit wage to age using a regression spline -------##
# if we choose K = 3 knots, then our splines model has K+4=7 df
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage) #bs() is regression splines; default is degree = 3
pred <- predict(fit, newdata = list(age = age.grid), se.fit = TRUE)

par(mfrow = c(1,1))
plot(age, wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

## prespecified knots at age 25, 40 and 60 produces a spline with six basis functions (Recall that a cubic spline with three knots has seven degree of freedoms)
dim(bs(age, knots=c(25, 40, 60)))
## We could use the df option to produce a spline with knots at uniform quantiles of the data
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")


## Fit wage to age using a regression spline; let R auto choose knots##
# degrees of freedom in bs() is K + 4 - 1, because intercept takes away 1 degree of freedom
fit = lm(wage ~ bs(x = age, df = 6), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se.fit = T)

plot(age, wage, col = 'grey') 
lines(x = age.grid, y = pred$fit, lwd = 2)
lines(x = age.grid, y = pred$fit + 2*pred$se, lty = "dashed")
lines(x = age.grid, y = pred$fit - 2*pred$se, lty = "dashed")






##----- Now we fit a natural spline with four degrees of freedom in R-----##
# degrees of freedom of natural spline = K knots. So in R, if df = 4, then there will be K=5 knots 
#     (3 internal knots and 2 boundary knots). Which means there are 6 regions
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age.grid), se.fit = T)
lines(x = age.grid, y = pred2$fit, lwd = 2, col = 'red')
lines(x = age.grid, y = pred2$fit + 2*pred2$se, lty = 'dashed', col = 'red')
lines(x = age.grid, y = pred2$fit - 2*pred2$se, lty = 'dashed', col = 'red')







##-------- To fit a smoothing spline, we use the smooth.spline() function -------##
# see pg. 33 of lecture on non-linear models. Remember smoothing spline avoids knot selection issue
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df=16)
fit2 <- smooth.spline(age, wage, cv = TRUE) #cv selects best effective df 
fit2$df 
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.8 DF"), col=c("red","blue"), lty=1, lwd=2, cex=0.8)






############################################ Lab 6.3: Generalized Additive Models (GAM)  ############################################################################
## We now fit a GAM to predict wage using natural spline functions of year and age treating education as a qualitative predictor
library(gam)
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data=Wage)
par(mfrow=c(1,3))
plot.Gam(gam1, se=TRUE, col="red")


## We next fit a GAM using smoothing splines rather than natural splines, here we need to use the gam library in R
## s() function, which is part of gam library, is used to indicate that we would like to use a smoothing spline
gam2 <- gam(wage ~ s(year,4) + s(age,5) +education, data=Wage)
summary(gam2)
plot(gam2, se=TRUE, col="blue")



## we can make predictions from gam objects
pred <-predict(gam2, newdata=Wage) #note there was no test-train split here. So to use predict, we should actually have test data