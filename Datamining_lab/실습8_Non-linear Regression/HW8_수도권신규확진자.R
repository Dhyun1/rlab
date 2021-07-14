library(ggplot2)
library(hms)
library(caret)
library(boot)

par(mfrow=c(1,1))
par(mar=c(4,4,1,1))
setwd("C:/Users/DaeHyun/Desktop/Study/●Datamining/실습8_Non-linear Regression")
data<-read.csv("COVID_19_Confirmed_Korea_DataMining_201127.csv")

#train test split
train<-data%>%
  filter(ymd(Date)<=ymd(20201120))%>%
  select(Days_after_First_Cases,수도권.신규확진자)
test<-data%>%
  filter(ymd(Date)>=ymd(20201121)&ymd(Date)<=ymd(20201127))%>%
  select(Days_after_First_Cases,수도권.신규확진자)
fulldata<-rbind(train,test)

#10-fold cv to determine polynomial regression degree on 2:5
k <- 10
set.seed(1)
folds <- caret::createFolds(1:nrow(train),k = 10,list = TRUE,returnTrain=FALSE) #construct 10-fold dataset for training data

deg <- seq(2, 5, by = 1)
cv <- vector("double",length(deg)) #4 CV to be calculated at degree = 2~5
mse <- vector("double",k) #10 MSE to be calculated for each degree

for (j in deg){  
  for (i in seq_len(k)){ #i=1,2,3, .. ,9,10
    fit<- lm(수도권.신규확진자~poly(Days_after_First_Cases,j),data=train[unlist(folds[-i]),])
    val_i<-train[unlist(folds[i]),]
    pred <- predict(fit, newdata=list("Days_after_First_Cases"=val_i$Days_after_First_Cases),se=TRUE)$fit
    mse[i] <- mean((pred - val_i$수도권.신규확진자)^2)
  }
  cv[j-1]<- mean(mse)
}

plot(deg, cv) #choose degree = 5

poly_fit=lm(수도권.신규확진자~poly(Days_after_First_Cases,5),data=train) 
(training_MSE<-mean((train$Days_after_First_Cases-poly_fit$fit)^2))
poly_train_fitted<-poly_fit$fit
poly_test_fitted<-predict(poly_fit,newdata=list("Days_after_First_Cases"=test$Days_after_First_Cases),se=TRUE)$fit #se=TRUE, $fit required
(test_MSE<-mean((test$Days_after_First_Cases-poly_test_fitted)^2))

poly_fitted<-data.frame(c(poly_train_fitted,poly_test_fitted))
colnames(poly_fitted)<-"poly"
poly_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=poly,colour="a"),data=poly_fitted)+
  scale_color_discrete(name = "Model", labels = c("poly"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(polynomial)")
ggsave(file="수도권(1120)_polynomial.jpg",width=30,height=15)
dev.off()

#10-fold cv to determine spar for smooth splines
set.seed(1)
smooth.spline(train$Days_after_First_Cases,train$수도권.신규확진자,cv=TRUE) #choose spar=0.12
smooth_fit<-smooth.spline(train$Days_after_First_Cases,train$수도권.신규확진자, spar = 0.12)
smooth_train_fitted<-predict(smooth_fit,train$Days_after_First_Cases)$y
(training_MSE<-mean((train$수도권.신규확진자-smooth_train_fitted)^2))
smooth_test_fitted<-predict(smooth_fit,test$Days_after_First_Cases)$y
(test_MSE<-mean((test$수도권.신규확진자-smooth_test_fitted)^2))

smooth_fitted<-data.frame(c(smooth_train_fitted,smooth_test_fitted))
colnames(smooth_fitted)<-"smooth"
smooth_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=smooth,colour="a"),data=smooth_fitted)+
  scale_color_discrete(name = "Model", labels = c("smooth"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(smooth)")
ggsave(file="수도권(1120)_smooth.jpg",width=30,height=15)
dev.off()

#10-fold cv to determine span for loess
k <- 10
set.seed(1)
folds <- caret::createFolds(1:nrow(train),k = 10,list = TRUE,returnTrain=FALSE) #construct 10-fold dataset for training data
deg <- seq(0.1, 1.0, by = 0.1)
cv <- vector("double",length(deg))
mse <- vector("double",k)

for (j in 1:length(deg)){  
  for (i in seq_len(k)){
    fit<- loess(수도권.신규확진자~Days_after_First_Cases,data=train[unlist(folds[-i]),],span=deg[j],
                   control=loess.control(surface="direct")) 
    val_i<-train[unlist(folds[i]),]
    pred <- predict(fit, newdata=data.frame("Days_after_First_Cases"=val_i$Days_after_First_Cases))
    mse[i] <- mean((pred - val_i$수도권.신규확진자)^2)
  }
  cv[j]<- mean(mse)
}
plot(deg,cv) #choose span=0.1

loess_fit=loess(수도권.신규확진자~Days_after_First_Cases,data=train,span=0.1,
                   control=loess.control(surface="direct")) 
loess_train_fitted<-loess_fit$fit
(training_MSE<-mean((train$Days_after_First_Cases-loess_train_fitted)^2))

loess_test_fitted<-predict(loess_fit,newdata=data.frame("Days_after_First_Cases"=test$Days_after_First_Cases)) #se=FALSE, no $fit required
(test_MSE<-mean((test$Days_after_First_Cases-loess_test_fitted)^2))

loess_fitted<-data.frame(c(loess_train_fitted,loess_test_fitted))
colnames(loess_fitted)<-"loess"
loess_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=loess,colour="a"),data=loess_fitted)+
  scale_color_discrete(name = "Model", labels = c("loess"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(loess)")
ggsave(file="수도권(1120)_loess.jpg",width=30,height=15)
dev.off()


#GAM
#natural splines (df=2:5)
library(gam)
cv<-rep(0,4)
df<-seq(2,5,by=1)
k <- 10
set.seed(1)
folds <- caret::createFolds(1:nrow(train),k = 10,list = TRUE,returnTrain=FALSE)

df<-seq(2,5,by=1)
for (j in df){  
  for (i in seq_len(k)){ #i=1,2,3, .. ,9,10
    fit<- gam(수도권.신규확진자~ns(Days_after_First_Cases,j),data=train[unlist(folds[-i]),]) 
    val_i<-train[unlist(folds[i]),]
    pred <- predict(fit, newdata=list("Days_after_First_Cases"=val_i$Days_after_First_Cases)) #se=FALSE, no $fit required
    mse[i] <- mean((pred - val_i$수도권.신규확진자)^2)
  }
  cv[j-1]<- mean(mse)
}
plot(df,cv) #choose df=4

gam1_fit<-glm(수도권.신규확진자~ns(Days_after_First_Cases,4),data=train)
gam1_train_fitted<-gam1_fit$fitted.values
(training_MSE=mean((train$수도권.신규확진자-gam1_train_fitted)^2))
gam1_test_fitted<-predict(gam1_fit,newdata=list("Days_after_First_Cases"=test$Days_after_First_Cases))
(test_MSE=mean((test$수도권.신규확진자-gam1_test_fitted)^2))

gam1_fitted<-data.frame(c(gam1_train_fitted,gam1_test_fitted))
colnames(gam1_fitted)<-"gam1"
gam1_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=gam1,colour="a"),data=gam1_fitted)+
  scale_color_discrete(name = "Model", labels = c("gam1"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(gam1)")
ggsave(file="수도권(1120)_gam1.jpg",width=30,height=15)
dev.off()

#step function with cut=2:5
#use boot::cv.glm

cut <- seq(2, 5, by = 1)
cv<-c(0,0,0,0)
set.seed(1)

for (i in cut){
  train$tmp <- cut(train$Days_after_First_Cases,i)
  step_fit = glm(수도권.신규확진자~tmp, data = train)
  cv[i-1] <- cv.glm(train,step_fit, K= 10)$delta[1]
}
plot(cut,cv) #cut=3

set.seed(1)
train$tmp <- cut(train$Days_after_First_Cases,3)
step_fit = glm(수도권.신규확진자~tmp, data = train)
step_train_fitted<-step_fit$fitted.values
(training_MSE<-mean((train$Days_after_First_Cases-step_train_fitted)^2))

(step_fit$coefficients[3]+step_fit$coefficients[1])
step_test_fitted<-rep(102,7)
(test_MSE<-mean((test$Days_after_First_Cases-step_test_fitted)^2))
train<-train[,-3]

step_fitted<-data.frame(c(step_train_fitted,step_test_fitted))
colnames(step_fitted)<-"step"
step_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=step,colour="a"),data=step_fitted)+
  scale_color_discrete(name = "Model", labels = c("step"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(step)")
ggsave(file="수도권(1120)_step.jpg",width=30,height=15)
dev.off()

#poisson regression (note 수도권신규확진자 is non-negative integer)

poisson_fit = glm(수도권.신규확진자~Days_after_First_Cases, family = 'poisson', data = train, maxit = 100)
poisson_train_fitted<-poisson_fit$fitted.values
(training_MSE<-mean((train$수도권.신규확진자-poisson_train_fitted)^2))
poisson_test_fitted<-predict(poisson_fit,data.frame("Days_after_First_Cases"=test$Days_after_First_Cases),
                             type = "r")
(test_MSE<-mean((test$수도권.신규확진자-poisson_test_fitted)^2))

poisson_fitted<-data.frame(c(poisson_train_fitted,poisson_test_fitted))
colnames(poisson_fitted)<-"poisson"
poisson_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=poisson,colour="a"),data=poisson_fitted)+
  scale_color_discrete(name = "Model", labels = c("poisson"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(poisson)")
ggsave(file="수도권(1120)_poisson.jpg",width=30,height=15)
dev.off()

#plot all fitted values for entire data set for each model
fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=poly,colour="a"),data=poly_fitted,)+
  geom_line(aes(y=smooth,colour="b"),data=smooth_fitted)+
  geom_line(aes(y=loess,colour="c"),data=loess_fitted)+
  geom_line(aes(y=gam1,colour="d"),data=gam1_fitted)+
  geom_line(aes(y=step,colour="e"),data=step_fitted)+
  geom_line(aes(y=poisson,colour="f"),data=poisson_fitted)+
  scale_color_discrete(name = "Model", labels = c("poly","smooth","loess","gam1","step","poisson"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(종합)")
ggsave(file="수도권(1120).jpg",width=30,height=15)
dev.off()

#Further analysis : change date, construct new set of train, test data
#7:3 split, do same analysis above
train1<-fulldata[1:round(nrow(fulldata)*0.7),]
test1<-fulldata[-(1:round(nrow(fulldata)*0.7)),]
nrow(train1) #1~219, 220~313 split dat=2020/08/25

#10-fold cv to determine polynomial regression degree on 2:5
k <- 10
set.seed(1)
folds <- caret::createFolds(1:nrow(train1),k = 10,list = TRUE,returnTrain=FALSE) 

deg <- seq(2, 5, by = 1)
cv <- vector("double",length(deg)) 
mse <- vector("double",k)

for (j in deg){  
  for (i in seq_len(k)){ #i=1,2,3, .. ,9,10
    fit<- lm(수도권.신규확진자~poly(Days_after_First_Cases,j),data=train1[unlist(folds[-i]),])
    val_i<-train1[unlist(folds[i]),]
    pred <- predict(fit, newdata=list("Days_after_First_Cases"=val_i$Days_after_First_Cases),se=TRUE)$fit
    mse[i] <- mean((pred - val_i$수도권.신규확진자)^2)
  }
  cv[j-1]<- mean(mse)
}
plot(deg, cv) #choose degree = 5

poly_fit=lm(수도권.신규확진자~poly(Days_after_First_Cases,5),data=train1) 
(training_MSE<-mean((train1$Days_after_First_Cases-poly_fit$fit)^2))
poly_train1_fitted<-poly_fit$fit
poly_test1_fitted<-predict(poly_fit,newdata=list("Days_after_First_Cases"=test1$Days_after_First_Cases),se=TRUE)$fit #se=TRUE, $fit required
(test_MSE<-mean((test1$Days_after_First_Cases-poly_test1_fitted)^2))

poly_fitted<-data.frame(c(poly_train1_fitted,poly_test1_fitted))
colnames(poly_fitted)<-"poly"
poly_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=poly,colour="a"),data=poly_fitted)+
  scale_color_discrete(name = "Model", labels = c("poly"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(polynomial)")
ggsave(file="수도권(0825)_polynomial.jpg",width=30,height=15)
dev.off()

#10-fold cv to determine spar for smooth splines
set.seed(1)
smooth.spline(train1$Days_after_First_Cases,train1$수도권.신규확진자,cv=TRUE) 
smooth_fit<-smooth.spline(train1$Days_after_First_Cases,train1$수도권.신규확진자,
                          spar = 0.3)
smooth_train1_fitted<-predict(smooth_fit,train1$Days_after_First_Cases)$y
(training_MSE<-mean((train1$수도권.신규확진자-smooth_train1_fitted)^2))
smooth_test1_fitted<-predict(smooth_fit,test1$Days_after_First_Cases)$y
(test_MSE<-mean((test1$수도권.신규확진자-smooth_test1_fitted)^2))

smooth_fitted<-data.frame(c(smooth_train1_fitted,smooth_test1_fitted))
colnames(smooth_fitted)<-"smooth"
smooth_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=smooth,colour="a"),data=smooth_fitted)+
  scale_color_discrete(name = "Model", labels = c("smooth"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(smooth)")
ggsave(file="수도권(0825)_smooth.jpg",width=30,height=15)
dev.off()

#10-fold cv to determine span for loess
k <- 10
set.seed(1)
folds <- caret::createFolds(1:nrow(train1),k = 10,list = TRUE,returnTrain=FALSE)
deg <- seq(0.1, 1.0, by = 0.1)
cv <- vector("double",length(deg)) #4 CV to be calculated at degree = 2~5
mse <- vector("double",k) #10 MSE to be calculated for each degree

for (j in 1:length(deg)){  
  for (i in seq_len(k)){ #i=1,2,3, .. ,9,10
    fit<- loess(수도권.신규확진자~Days_after_First_Cases,data=train1[unlist(folds[-i]),],span=deg[j],
                   control=loess.control(surface="direct")) 
    val_i<-train1[unlist(folds[i]),]
    pred <- predict(fit, newdata=data.frame("Days_after_First_Cases"=val_i$Days_after_First_Cases)) #se=FALSE, no $fit required
    #used predict() instead of @fitted model$fit
    mse[i] <- mean((pred - val_i$수도권.신규확진자)^2)
  }
  cv[j]<- mean(mse)
}
plot(deg,cv) #choose span=0.1

loess_fit=loess(수도권.신규확진자~Days_after_First_Cases,data=train1,span=0.1,
                   control=loess.control(surface="direct")) 
loess_train1_fitted<-loess_fit$fit
(training_MSE<-mean((train1$Days_after_First_Cases-loess_train1_fitted)^2))

loess_test1_fitted<-predict(loess_fit,newdata=data.frame("Days_after_First_Cases"=test1$Days_after_First_Cases)) #se=FALSE, no $fit required
(test_MSE<-mean((test1$Days_after_First_Cases-loess_test1_fitted)^2))

loess_fitted<-data.frame(c(loess_train1_fitted,loess_test1_fitted))
colnames(loess_fitted)<-"loess"
loess_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=loess,colour="a"),data=loess_fitted)+
  scale_color_discrete(name = "Model", labels = c("loess"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(loess)")
ggsave(file="수도권(0825)_loess.jpg",width=30,height=15)
dev.off()

#2-(d) GAM
#natural splines (df=2:5)
library(gam)
cv<-rep(0,4)
df<-seq(2,5,by=1)
k <- 10
set.seed(1)
folds <- caret::createFolds(1:nrow(train1),k = 10,list = TRUE,returnTrain=FALSE)

df<-seq(2,5,by=1)
for (j in df){  
  for (i in seq_len(k)){ #i=1,2,3, .. ,9,10
    fit<- gam(수도권.신규확진자~ns(Days_after_First_Cases,j),data=train1[unlist(folds[-i]),]) 
    val_i<-train1[unlist(folds[i]),]
    pred <- predict(fit, newdata=list("Days_after_First_Cases"=val_i$Days_after_First_Cases)) #se=FALSE, no $fit required
    mse[i] <- mean((pred - val_i$수도권.신규확진자)^2)
  }
  cv[j-1]<- mean(mse)
}
plot(df,cv) #choose df=5

gam1_fit<-glm(수도권.신규확진자~ns(Days_after_First_Cases,5),data=train1)
gam1_train1_fitted<-gam1_fit$fitted.values
(training_MSE=mean((train1$수도권.신규확진자-gam1_train1_fitted)^2))
gam1_test1_fitted<-predict(gam1_fit,newdata=list("Days_after_First_Cases"=test1$Days_after_First_Cases))
(test_MSE=mean((test1$수도권.신규확진자-gam1_test1_fitted)^2))

gam1_fitted<-data.frame(c(gam1_train1_fitted,gam1_test1_fitted))
colnames(gam1_fitted)<-"gam1"
gam1_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=gam1,colour="a"),data=gam1_fitted)+
  scale_color_discrete(name = "Model", labels = c("gam1"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(gam1)")
ggsave(file="수도권(0825)_gam1.jpg")
dev.off()


#step function with cut=2:5
#use boot::cv.glm

cut <- seq(2, 5, by = 1)
cv<-c(0,0,0,0)
set.seed(1)

for (i in cut){
  train1$tmp <- cut(train1$Days_after_First_Cases,i)
  step_fit = glm(수도권.신규확진자~tmp, data = train1)
  cv[i-1] <- cv.glm(train1,step_fit, K= 10)$delta[1]
}
plot(cut,cv) #cut=5

set.seed(1)
train1$tmp <- cut(train1$Days_after_First_Cases,3)
step_fit = glm(수도권.신규확진자~tmp, data = train1)
step_train1_fitted<-step_fit$fitted.values
(training_MSE<-mean((train1$Days_after_First_Cases-step_train1_fitted)^2))
step_test1_fitted<-rep(49,94)
(test_MSE<-mean((test1$Days_after_First_Cases-step_test1_fitted)^2))
train1<-train1[,-3]

step_fitted<-data.frame(c(step_train1_fitted,step_test1_fitted))
colnames(step_fitted)<-"step"
step_fitted$Days_after_First_Cases<-1:313

fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=step,colour="a"),data=step_fitted)+
  scale_color_discrete(name = "Model", labels = c("step"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(step)")
ggsave(file="수도권(0825)_step.jpg",width=30,height=15)
dev.off()

#poisson regression 
poisson_fit = glm(수도권.신규확진자~Days_after_First_Cases, family = 'poisson', data = train1, maxit = 100)
poisson_train1_fitted<-poisson_fit$fitted.values
(training_MSE<-mean((train1$수도권.신규확진자-poisson_train1_fitted)^2))
poisson_test1_fitted<-predict(poisson_fit,data.frame("Days_after_First_Cases"=test1$Days_after_First_Cases),
                              type = "r")
(test_MSE<-mean((test1$수도권.신규확진자-poisson_test1_fitted)^2))

poisson_fitted<-data.frame(c(poisson_train1_fitted,poisson_test1_fitted))
colnames(poisson_fitted)<-"poisson"
poisson_fitted$Days_after_First_Cases<-1:313
fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=poisson,colour="a"),data=poisson_fitted)+
  scale_color_discrete(name = "Model", labels = c("poisson"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(poisson)")
ggsave(file="수도권(0825)_poisson.jpg",width=30,height=15)
dev.off()

#plot all fitted values for entire data set for each model
fulldata%>%
  ggplot(aes(x=Days_after_First_Cases))+
  geom_line(aes(y=수도권.신규확진자))+
  geom_line(aes(y=poly,colour="a"),data=poly_fitted,)+
  geom_line(aes(y=smooth,colour="b"),data=smooth_fitted)+
  geom_line(aes(y=loess,colour="c"),data=loess_fitted)+
  geom_line(aes(y=gam1,colour="d"),data=gam1_fitted)+
  geom_line(aes(y=step,colour="e"),data=step_fitted)+
  geom_line(aes(y=poisson,colour="f"),data=poisson_fitted)+
  scale_color_discrete(name = "Model", labels = c("poly","smooth","loess","gam1","step","poisson"))+
  labs(x="Days after first cases",y="수도권신규확진자",title="수도권신규확진자(종합)")+
  coord_cartesian(ylim=c(0,500))
ggsave(file="수도권(0825).jpg",width=30,height=15)
dev.off()
