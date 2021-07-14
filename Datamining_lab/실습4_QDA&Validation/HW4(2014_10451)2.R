getwd()
iris<-read_csv("./●데이터마이닝/iris.csv")

set.seed(1)
iris0<-iris%>%
  mutate(Species0=(as.numeric(Species=="setosa")))%>%
  select(-Species)
sample_size=floor(0.7*nrow(iris0))
train_ind<-sample(seq_len(nrow(iris0)),sample_size)
train<-iris0[train_ind,]
test<-iris0[-train_ind,]

str(train)
str(test)

qda.fit<-qda(as.factor(Species0)~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
             data=as.data.frame(iris0),subset=train_ind)

qda.pred <- predict(qda.fit, train[,-5])
qda.class <- qda.pred$class
#Confusion matrix : table(pred,참값)
table(qda.class,as.factor(train[,5]))

mean(qda.class==Direction.2005)


