library(glmnet)
library(MASS)

glm.fits <- glm(formula.rfe,data=train,family=binomial)
summary(glm.fits)

#test set에 대한 prediction
glm.probs <- predict(glm.fits,test[,-1],type="response")
#Confusion matrix
glm.pred <- rep(0,nrow(test))
glm.pred[glm.probs>.5] <- 1
#test set에 대한 confusion matrix
table(glm.pred,test[,1])
mean(glm.pred==test[,1]) #Accuracy

############LDA#####################
lda.fit <- lda(formula.rfe, data=train)

#Predictions on test set
lda.pred <- predict(lda.fit, test)
#Predicted Class
lda.class <- lda.pred$class

table(lda.class,test[,1])
mean(lda.class==test[,1]) #Accuracy

#test set의 분류확률 출력
lda.pred$posterior
sum(lda.pred$posterior[,2]>=.5) #Predicted as 1
sum(lda.pred$posterior[,2]<.5)#Predicted as 0

#ROC
glm.roc<-roc(test[,1],glm.probs)
lda.roc<-roc(test[,1],lda.pred$posterior[,2])
glm.roc <-glm.roc[c("specificities","sensitivities","thresholds","auc")]
lda.roc <-lda.roc[c("specificities","sensitivities","thresholds","auc")]

glm.roc$auc 
lda.roc$auc

par(mfrow=c(1,1))
plot(x=(1-glm.roc$specificities),y=glm.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.roc$sensitivities,x=(1-lda.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")
