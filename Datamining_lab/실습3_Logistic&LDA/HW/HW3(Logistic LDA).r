library(ISLR)
library(MASS)
library(pROC)
getwd()
setwd("C:/Users/DaeHyun/Desktop/Study/●데이터마이닝/5주차")

iris <- read.csv("iris.csv")
iris[iris=="setosa"]<-1
iris[iris=="versicolor"|iris=="virginica"]<-0
iris$Species<-factor(iris$Species)

smp_size <- floor(0.7 * nrow(iris))
set.seed(1)
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

glm.fits<-glm(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train,
              family=binomial)
summary(glm.fits)

glm.probs<-predict(glm.fits,train,type="response")
glm.pred <- rep("0",smp_size)
glm.pred[glm.probs>.5] <- 1
table(glm.pred,train$Species)
mean(glm.pred==train$Species)

glm.probs<-predict(glm.fits,test,type="response")
glm.pred <- rep("0",nrow(iris)-smp_size)
glm.pred[glm.probs>.5] <- 1
table(glm.pred,test$Species)
mean(glm.pred==test$Species)

lda.fit <-lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train)

lda.pred <- predict(lda.fit, train)
lda.class <- lda.pred$class
table(lda.class,train$Species)
mean(lda.class==train$Species)

lda.pred1 <- predict(lda.fit, test)
lda.class1 <- lda.pred1$class
table(lda.class1,test$Species)
mean(lda.class1==test$Species)
