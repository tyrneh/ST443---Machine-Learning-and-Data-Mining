# Lab 1.1 Basic Commands
x<-c(1,3,2,5)
x
x=c(1,6,2)
x
y=c(1,4,3)
x+y
ls()
rm(x,y) ## rm(list=ls(all=TRUE)) ##remove all variables (clean up)
ls()
x=matrix(c(1,2,3,4),2,2)
x
x=matrix(c(1,2,3,4),2,2,byrow = T)
x
sqrt(x)
x^2
x=rnorm(50)

y=x+rnorm(50,sd=.1)
cor(x,y)
mean(y)
var(y)
sqrt(var(y))

# Lab 1.2 Graphics
plot(x,y)
plot(x,y,xlab="X Variable", ylab="Y Variable", main="Plot of X vs Y",pch=20)
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=15,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,f)
image(x,y,fa)
persp(x,y,f)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)

# Lab 1.3 Indexing Data
A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)


# Lab 1.4 Loading Data
auto=read.table("auto_mpg.data")
View(auto)
auto=read.table("auto_mpg.data",na.strings="?")
View(auto)
auto=read.csv("auto_mpg.csv",header=F,na.strings="?")
View(auto)
dim(auto)
auto=na.omit(auto)
dim(auto)
names(auto)

# Lab 1.5 Additional Graphical and Numerical Summaries
plot(Cylinders,Mpg)
plot(auto$Cylinders,auto$Mpg)
attach(auto)
plot(Cylinders,Mpg)
Cylinders=as.factor(Cylinders)
plot(Cylinders,Mpg)
plot(Cylinders,Mpg,col="red")
plot(Cylinders,Mpg,col="red",varwidth=T)
plot(Cylinders,Mpg,col="red",varwidth=T,horizontal=T)
plot(Cylinders,Mpg,col="red",varwidth=T,xlab="Cylinders",ylab="MPG")
hist(Mpg)
hist(Mpg,col=2)
hist(Mpg,col=2,breaks=35)
pairs(auto)
pairs(~Mpg+Displacement+Horsepower+Weight+Acceleration,auto)
plot(Horsepower,Mpg)
identify(Horsepower,Mpg,Name)
summary(auto)
summary(Mpg)