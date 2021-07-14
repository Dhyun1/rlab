#install.packages("leaps")
#install.packages("glmnet")
#install.packages("ISLR")
library(ISLR)
library(leaps)
library(glmnet)

getwd()
setwd("C:/Users/DaeHyun/Desktop/Study/●Datamining")

#preprocessing
iris<-read.csv("iris.csv")
dim(iris)
str(iris)

iris$Species<-factor(iris$Species)
str(iris)
sum(is.na(iris$Sepal.Width))

#test train split
set.seed(1)
smp_size <- floor(0.7 * nrow(iris))
train_ind<-sample(seq_len(nrow(iris)),size=smp_size)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

####Ridge Regression alpha=0####
#lambda grid
dim(model.matrix(Sepal.Width~.,train))
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(model.matrix(Sepal.Width~.,train),train$Sepal.Width,
                 alpha=0,lambda=grid)
#default 10-fold
set.seed(1)
cv.out=cv.glmnet(model.matrix(Sepal.Width~.,train),
                 train$Sepal.Width,alpha=0)
plot(cv.out)
(bestlam=cv.out$lambda.min)

#after determining lambda, conduct ridge regression on entire train data
ridge.pred=predict(ridge.mod,s=bestlam,
                   newx=model.matrix(Sepal.Width~.,train))
#training MSE using tuning parameter=bestlam
mean((ridge.pred-train$Sepal.Width)^2)

ridge.pred=predict(ridge.mod,s=bestlam,
                   newx=model.matrix(Sepal.Width~.,test))
#test MSE using tuning parameter=bestlam
mean((ridge.pred-test$Sepal.Width)^2)

####Lasso Regression alpha=1####
#lambda grid
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(model.matrix(Sepal.Width~.,train),train$Sepal.Width,
                 alpha=1,lambda=grid)
#default 10-fold
set.seed(1)
cv.out=cv.glmnet(model.matrix(Sepal.Width~.,train),
                 train$Sepal.Width,alpha=1)
plot(cv.out)
(bestlam=cv.out$lambda.min)

#after determining lambda, conduct lasso regression 
#on entire train data
lasso.pred=predict(lasso.mod,s=bestlam,
                   newx=model.matrix(Sepal.Width~.,train))
#training MSE using tuning parameter=bestlam
mean((lasso.pred-train$Sepal.Width)^2)

lasso.pred=predict(lasso.mod,s=bestlam,
                   newx=model.matrix(Sepal.Width~.,test))
#test MSE using tuning parameter=bestlam
mean((lasso.pred-test$Sepal.Width)^2)
out=glmnet(model.matrix(Sepal.Width~.,iris),iris$Sepal.Width,
           alpha=1,lambda=grid)

lasso.coef=predict(out,type="coefficients",s=bestlam)[1:7,]
lasso.coef
