library(ISLR)
library(MASS)
library(pROC)

attach(Smarket)

# Train set & Test set
train <- (Year<2005)
Smarket.2005 <- Smarket[!train,] 
Direction.2005 <- Direction[!train]

# QDA fitting (subset=train)
qda.fit <- qda(Direction~Lag1+Lag2,data=Smarket,subset=train)

#Test set accuracy 계산
#predict(qda명,testset명)$class 이용
qda.pred <- predict(qda.fit, Smarket.2005)
qda.class <- qda.pred$class
#Confusion matrix : table(pred,참값)
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005) #Test accuracy

#ROC Curve
library(pROC)
#roc(test set 참값, qda posterior 확률)
qda.roc<-roc(Direction.2005,qda.pred$posterior[,2])
#필요한 것 4개만 추출
qda.roc <-qda.roc[c("specificities","sensitivities","thresholds","auc")]
plot(y=qda.roc$sensitivities,x=(1-qda.roc$specificities),col = "darkgreen",type="l", main="ROC curve of QDA")

# AUC 계산
qda.roc$auc
