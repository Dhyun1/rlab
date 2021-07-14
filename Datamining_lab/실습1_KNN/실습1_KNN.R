library(ISLR)
library(MASS)
library(class)
library(dplyr)

setwd("C:/Users/DaeHyun/Desktop/Study")
getwd()

iris<-read.csv("./●데이터마이닝/data/iris.csv")

names(iris)
dim(iris)
str(iris)
table(iris$Species)

# train/test random split (7:3)
smp_size <- floor(0.7 * nrow(iris))

set.seed(1) 
train_ind<-sample(seq_len(nrow(iris)),size=smp_size)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

str(train)
str(test)
head(train)
head(test)
###K-Nearest Neighbors###
#특징:No assumption, hyperparameters(training data 내에서 CV을 통해 결정)
#linear regression=>parameters(데이터에 의해 정해지는 값)
#KNN은 choice of k가 매우 중요.
set.seed(1)
#knn(train$x변수,예측할data,train$y변수)
#training accuracy를 보기 위해서 예측할data는 train[,-5]
#k=1은 임의로 설정
knn.pred=knn(train[,-5],train[,-5],train$Species,k=1)
 
# Confusion matrix - 행이 predict, 열이 true값
#table(knn.pred, train data의 true Y value)
table(knn.pred,train$Species) 
mean(knn.pred==train$Species) #training accuracy

set.seed(1)
#test accuracy 보기 위해 예측할data는 test[,-5]
knn.pred=knn(train[,-5],test[,-5],train$Species,k=1)
#table(knn.pred, test data의 true Y value)
table(knn.pred,test$Species)
mean(knn.pred==test$Species) #test accuracy

#when k=3
set.seed(1)
knn.pred=knn(train[,-5],train[,-5],train$Species,k=3)
table(knn.pred,train$Species)
mean(knn.pred==train$Species) #training accuracy

set.seed(1)
knn.pred=knn(train[,-5],test[,-5],train$Species,k=3)
table(knn.pred,test$Species)
mean(knn.pred==test$Species) #test accuracy

##Smarket:주식데이터##
names(Smarket) #Direction이 설명변수(UP/DOWN)
dim(Smarket)
summary(Smarket)
pairs(Smarket)#Scatter Plot 생성
cor(Smarket[,-9])#설명변수 제외 Pearson's Cor matrix


#train-test split
train=Smarket %>%
  filter(Year<=2004)
test=Smarket %>%
  filter(Year>2004) #시계열자료는 시점을 기준으로 split

#k=1
set.seed(1)
knn.pred=knn(train[,-9],train[,-9],train$Direction,k=1)
table(knn.pred,train$Direction)
mean(knn.pred==train$Direction) #training accuracy

set.seed(1)
knn.pred=knn(train[,-9],test[,-9],train$Direction,k=1)
table(knn.pred,test$Direction)
mean(knn.pred==test$Direction) #test accuracy

#k=3
set.seed(1)
knn.pred=knn(train[,-9],train[,-9],train$Direction,k=3)
table(knn.pred,train$Direction)
mean(knn.pred==train$Direction) #training accuracy

set.seed(1)
knn.pred=knn(train[,-9],test[,-9],train$Direction,k=3)
table(knn.pred,test$Direction)
mean(knn.pred==test$Direction) #test accuracy

##Caravan데이터##
dim(Caravan)
head(Caravan)
attach(Caravan)
summary(Purchase) #Purchase는 binary response var.

#standardizing continous variable for scale issue
#머신러닝에서 보통 연속형 변수를 standardize 처리함.
#평균이 0이고, 분산이 1이 되도록 한다
standardized.X=scale(Caravan[,-86])

#분산 1 check
var(standardized.X[,1])
var(standardized.X[,2])


# training set's predictors
test=1:1000
train.X=standardized.X[-test,] #1001~ training set
# test set's predictors
test.X=standardized.X[test,] #1~1000 test set

# training set's response variable
train.Y=Purchase[-test]
# test set's response variable
test.Y=Purchase[test]

#k=1
set.seed(1)
knn.pred=knn(train.X,train.X,train.Y,k=1)
table(knn.pred,train.Y)
mean(knn.pred==train.Y) #training accuracy


set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred,test.Y)
mean(test.Y==knn.pred) #test accuracy

#k=3
set.seed(1)
knn.pred=knn(train.X,train.X,train.Y,k=3)
table(knn.pred,train.Y)
mean(train.Y==knn.pred) #training accuracy

set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y) 
mean(test.Y==knn.pred) #test accuracy
