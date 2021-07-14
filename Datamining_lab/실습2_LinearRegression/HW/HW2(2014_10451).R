library("ggplot2")

getwd()
iris <- read.csv("iris.csv")

##represent Species using dummy variable##
levels(factor(iris$Species))
length(which(iris$Species=="setosa"))
length(which(iris$Species=="versicolor"))
length(which(iris$Species=="virginica"))

coded_setosa<-cbind(rep(1,50),rep(0,50))
coded_versi<-cbind(rep(0,50),rep(1,50))
coded_virgi<-cbind(rep(0,50),rep(0,50))
coded_Species<-rbind(coded_setosa,coded_versi,coded_virgi)
colnames(coded_Species)<-c("d1","d2")

iris_aug<-cbind(iris[,-5],coded_Species)

##represent Species using allocated code : setosa=1, versicolor=2, virginica=3##
alloc<-c(rep(1,50),rep(2,50),rep(3,50))
iris_aug2<-cbind(iris[,-5],alloc)

##plotting before regression##
ggplot(data = iris_aug) + 
  geom_point(mapping = aes(x = Sepal.Width, y = Sepal.Length))
ggplot(data = iris_aug) + 
  geom_point(mapping = aes(x = Petal.Length, y = Sepal.Length))
ggplot(data = iris_aug) + 
  geom_point(mapping = aes(x = Petal.Width, y = Sepal.Length))

##Question1_using dummy varaible##
smp_size <- floor(0.7 * nrow(iris_aug))
set.seed(1)

train_ind <- sample(seq_len(nrow(iris_aug)), size = smp_size)
train <- iris_aug[train_ind, ]
test <- iris_aug[-train_ind, ]

str(train)
str(test)
lm_train<-lm(Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width+d1+d2, 
            data = train)

train_mse<-mean(lm_train$residuals^2)
train_mse

x<-test[,-1]
p <- predict(lm_train, data.frame(x))
test_mse<-mean((test[,1]-p)^2)
test_mse

##Question1_using allocated code##
smp_size <- floor(0.7 * nrow(iris_aug2))
set.seed(1)

train_ind <- sample(seq_len(nrow(iris_aug2)), size = smp_size)
train <- iris_aug2[train_ind, ]
test <- iris_aug2[-train_ind, ]

lm_train<-lm(Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width+alloc, 
             data = train)

train_mse<-mean(lm_train$residuals^2)
train_mse

x<-test[,-1]
p <- predict(lm_train, data.frame(x))
test_mse<-mean((test[,1]-p)^2)
test_mse

##Question2_using dummy variable##
smp_size <- floor(0.6 * nrow(iris_aug))
set.seed(1)

train_ind <- sample(seq_len(nrow(iris_aug)), size = smp_size)
train <- iris_aug[train_ind, ]
test <- iris_aug[-train_ind, ]

str(train)
str(test)
lm_train<-lm(Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width+d1+d2, 
             data = train)

train_mse<-mean(lm_train$residuals^2)
train_mse

x<-test[,-1]
p <- predict(lm_train, data.frame(x))
test_mse<-mean((test[,1]-p)^2)
test_mse

##Question2_using allocated code##
smp_size <- floor(0.6 * nrow(iris_aug2))
set.seed(1)

train_ind <- sample(seq_len(nrow(iris_aug2)), size = smp_size)
train <- iris_aug2[train_ind, ]
test <- iris_aug2[-train_ind, ]

str(train)
str(test)
lm_train<-lm(Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width+alloc, 
             data = train)

train_mse<-mean(lm_train$residuals^2)
train_mse

x<-test[,-1]
p <- predict(lm_train, data.frame(x))
test_mse<-mean((test[,1]-p)^2)
test_mse
