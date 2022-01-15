rm(list=ls())

## Lab 10 Unsupervised Learning Methods

## Lab 10.1: Principal Component Analysis ##############--------------------------------------------------------------------------
## We perform PCA on the USArrests data set.
View(USArrests)
summary(USArrests)

## The row of the data set contain 50 states, in alphabetical order.
states <-row.names(USArrests)
states


## The column of the data contain the four variables.
names(USArrests)


## 4 variables have vastly different means and variance, hence it is important to standarize the variables to have mean zero and std one before performing PCA.
apply(USArrests, 2, mean) #this calculates mean of columns (2 indicates column)
apply(USArrests, 2, var) #this calculates var of columns (2 indicates column)


## prcomp() function is used to perform PCA
## By default, the prcomp() function centers the variables to have mean zero.
## By using the option scale=TRUE, we scale the variables to have deviation one.
pr.out <-prcomp(USArrests, scale. = TRUE)
names(pr.out)


## The center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA.
pr.out$center #center is the mean of the variables
pr.out$scale #scale is the S.D. of the variables

## The rotation matrix provides the principal component loadings, each column contaisn the corresponding PC loading vector.
pr.out$rotation

## The 50 by 4 matrix 'x' has its columns the principal component score vectors, i.e. the kth column is the kth PC score vector.
dim(pr.out$x)
pr.out$x





## We can plot the first two principal components as follows
## The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings,
## other values for scale give slightly different biplots with different interpretations.
biplot(pr.out, scale=0, cex=0.7)



## prcomp() function aslo outputs the standard deviation of each principal component.
pr.out$sdev

## The variance explained by each principal component.
pr.var <-pr.out$sdev^2

## The proportion of variance explained by each principal component.
pve <-pr.var/sum(pr.var)
pve
cumsum(pr.var)/sum(pr.var)

## We can plot the PVE explained by each component, as well as the cumulative PVE, as follows.
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type='b')







## Lab 10.2: K-means clustering ####################################---------------------------------------------------------------
## Simulation data: two clusters; the first 25 observations have a mean shift relative to the next 25 observations.
set.seed(2)

x <-matrix(rnorm(50*2), ncol=2) #generate x and y coordinates with ~normal(0,1)
x[1:25, 1] <- x[1:25, 1] +3  #make half of observations have x:0+3 mean
x[1:25, 2] <- x[1:25, 2] -4 #make that half of observations have y:0-4 mean
plot(x, xlab="", ylab="",pch=20, cex=2)


## The cluster assignments of the 50 observations are contained.
## kmeans() performs K-means clustering in R.
## If a value of nstart greater than one used, then K-means clustering will be performed using multiple random assignments for the starting values.
km.out <-kmeans(x, 2, nstart=20) #we specify 2 clusters
km.out$cluster #this is the best results - vector of classifications of each observation

## We plot the data, with each observation colored according to its cluster assignment
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2) #coloring classifications differently



## We redo everything setting the number of clusters to be 3
set.seed(4)
km.out <-kmeans(x, 3, nstart=20) 
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)



## Here we compare using nstart=1 to nstart=20
## Note km.out$tot.withinss is the total within-cluster sum of squares, which we seek to minimize by performing K-means clustering.
set.seed(4)
km.out <-kmeans(x, 3, nstart=1)
km.out$tot.withinss
km.out <-kmeans(x, 3, nstart=20)
km.out$tot.withinss
## We strongly recommend always running K-menas clustering with a large value of nstart, such as 20 or 50, since otherwise an undesirable local optimum may be obtained.






## Lab 10.3: Hierarchical clustering ################---------------------------------------------------------------------------------------------------
## hclust() function implements hierarchical clustering in R.
## dist() function is used to compute the 50 by 50 inter-observation Euclidean distance matrix.
hc.complete <-hclust(dist(x), method="complete")
hc.average <-hclust(dist(x), method="average")
hc.single <-hclust(dist(x), method="single")


par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", ylab="", cex=0.9)
plot(hc.average, main="Average Linkage", xlab="", ylab="", cex=0.9)
plot(hc.single, main="Single Linkage", xlab="", ylab="", cex=0.9)



## To determine the cluster labels for each obervations associated with a given cut of the dendrogram, we can use the cutree() function.
## cuttree(model, x) means you want x clusters
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

## Complete and average linkage generally separate the observations into their correct groups.
## Single linkage identifies one points as belonging to its own cluster. 
## A more sensible answer is obtained when four clusters are selected, although there are still two singletons.
cutree(hc.single, 4)
cutree(hc.complete, 4)



## !!!!!!!!! To scale the variables before performing hierarchical clustering of the observations, we use scale() function.!!!!!!!!!
## we skipped this step because we used rnorm(0,1) to generate our observations
xsc <-scale(x)
par(mfrow=c(1,1))
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")





## Correlation-based distance can be computed using the as.dist() function, which converts an arbitrary square symmetric matrix into a form 
## that the hclust() function recognizes as a distance matrix.
## However this only makes sense for data with at least three features since the abslute correlation between any two observations with measurements on two features is always 1
## Hence we will cluster a three-dimensioinal data set.
x <-matrix(rnorm(30*3), ncol=3)
plot(x)



## Similar shape corresponds to the correlation closer to one, hence we consider one minus the correlation, The closer to zero this value is, the more similarity we have.
dd <-as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
