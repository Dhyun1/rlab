# Singular value decomposition --------------------------------------------

## Data import
set.seed(1)
data<-read.table("/users/kyr0302/Desktop/lab_1/T4-3.dat")
colnames(data)<-c("x1","x2","x3","x4","dsq")
X<-data[,-5]

## Data description
head(X)
str(X)

## Singular value decomposition
svd(X)
svd(X,1,1)

# Sample covariance -------------------------------------------------------

## Making true covariance
library(mvtnorm)
A<-rmvnorm(10,mean=rep(0,10),sigma=diag(10))
B<-cov(A)

## Generate random vector with n=100, p=10, multivariate normal
tv<-B
A<-rmvnorm(100,mean=rep(0,10),sigma=tv)

## Calculate sample covariance matrix 
scv<-cov(A) 

## Comparison
head(tv); head(scv);
eigen(scv)$values; eigen(tv)$values;
abs(t(eigen(scv)$vectors[,1])%*%eigen(tv)$vectors[,1]);

## Repeat 500 
theta<-rep(0,500);
evl<-matrix(0,nrow=500,ncol=5);
eig.res.tr<-eigen(tv)
for(i in 1:500){
  A<-rmvnorm(100,mean=rep(0,10),sigma=tv)
  scv<-cov(A)
  eig.res<-eigen(scv)
  theta[i]<-abs(t(eig.res$vectors[,1])%*%eig.res.tr$vectors[,1])
  evl[i,]<-eig.res$values[1:5]
}

# boxplot (eigenvalues)
boxplot(evl)
x0s <- 1:5 - 0.5
x1s <- 1:5 + 0.5
y0s <- eig.res.tr$values[1:5]
segments(x0 = x0s, x1 = x1s, y0 = y0s, col = "red")

# boxplot (angle between eigenvectors)
boxplot(theta)

# Ex 4.15 -----------------------------------------------------------------

## Calculating measures (standardized measurements & square of distances)
xbar<-colMeans(X)
var<-cov(X)
Xbarmat<-matrix(rep(xbar,30),nrow=30,ncol=4,byrow=TRUE)
t(t(X-Xbarmat)/sqrt(diag(var)))
dsq<-rep(0,30)
dsq<-as.matrix(X-Xbarmat)%*%solve(var)%*%t(X-Xbarmat)
diag(dsq)

## Scatter plot
library(car)
pairs(~x1+x2+x3+x4,data=X)

