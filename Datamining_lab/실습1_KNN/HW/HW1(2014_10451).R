library(ISLR)
library(MASS)
library(class)
library(dplyr)

getwd()
setwd("../Desktop/Study/●데이터마이닝/3주차/3주차 실습자료 (KNN)") 
iris <- read.csv("iris.csv")

## Question1 ##
smp_size <- floor(0.8 * nrow(iris))
set.seed(1)

train_ind <- sample(seq_len(nrow(iris)), size = smp_size)

train <- iris[train_ind, ]
test <- iris[-train_ind, ]

str(train) 
str(test)  

# training accuracy, test accuracy for k=2
set.seed(1)
knn.pred=knn(train[,-5],train[,-5],train$Species,k=2)
table(knn.pred,train$Species) 
mean(knn.pred==train$Species) ## training accuracy 0.983

set.seed(1)
knn.pred=knn(train[,-5],test[,-5],train$Species,k=2)
table(knn.pred,test$Species) 
mean(knn.pred==test$Species) ## test accuracy 0.967

# training accuracy, test accuracy for k=4
set.seed(1)
knn.pred=knn(train[,-5],train[,-5],train$Species,k=4)
table(knn.pred,train$Species) 
mean(knn.pred==train$Species) ## training accuracy 0.967

set.seed(1)
knn.pred=knn(train[,-5],test[,-5],train$Species,k=4)
table(knn.pred,test$Species) 
mean(knn.pred==test$Species) ## test accuracy 0.967

# training accuracy, test accuracy for k=6
set.seed(1)
knn.pred=knn(train[,-5],train[,-5],train$Species,k=6)
table(knn.pred,train$Species) 
mean(knn.pred==train$Species) ## training accuracy 0.983

set.seed(1)
knn.pred=knn(train[,-5],test[,-5],train$Species,k=6)
table(knn.pred,test$Species) 
mean(knn.pred==test$Species) ## test accuracy 0.967


## Question2 ##
smp_size <- floor(0.5 * nrow(iris))
set.seed(1)

train_ind <- sample(seq_len(nrow(iris)), size = smp_size)

train <- iris[train_ind, ]
test <- iris[-train_ind, ]

str(train) 
str(test)  

# training accuracy, test accuracy for k=2
set.seed(1)
knn.pred=knn(train[,-5],train[,-5],train$Species,k=2)
table(knn.pred,train$Species) 
mean(knn.pred==train$Species) ## training accuracy 0.973

set.seed(1)
knn.pred=knn(train[,-5],test[,-5],train$Species,k=2)
table(knn.pred,test$Species) 
mean(knn.pred==test$Species) ## test accuracy 0.973

# training accuracy, test accuracy for k=4
set.seed(1)
knn.pred=knn(train[,-5],train[,-5],train$Species,k=4)
table(knn.pred,train$Species) 
mean(knn.pred==train$Species) ## training accuracy 0.96

set.seed(1)
knn.pred=knn(train[,-5],test[,-5],train$Species,k=4)
table(knn.pred,test$Species) 
mean(knn.pred==test$Species) ## test accuracy 0.973

# training accuracy, test accuracy for k=6
set.seed(1)
knn.pred=knn(train[,-5],train[,-5],train$Species,k=6)
table(knn.pred,train$Species) 
mean(knn.pred==train$Species) ## training accuracy 0.973

set.seed(1)
knn.pred=knn(train[,-5],test[,-5],train$Species,k=6)
table(knn.pred,test$Species) 
mean(knn.pred==test$Species) ## test accuracy 0.947
