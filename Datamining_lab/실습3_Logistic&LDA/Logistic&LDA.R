library(ISLR)
library(MASS)
#pROC추가
library(pROC)


names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach(Smarket)
head(Smarket)

#Logistic regression
#glm(이항범주형변수~설명변수,data명,family=binomial)
glm.fits<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Volume,
              data=Smarket,family=binomial)
summary(glm.fits)
#AIC는 낮을수록 좋다
#p-value 값이 작은게 영향력이 큰 것임

#회귀계수 log-odds 출력
coef(glm.fits) #부호가 +면 +효과, -면 -효과

#p-value만 출력
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

#Prediction of logit model(=>Prob. of Y=1)
#predict(glm모델명,type="response")
glm.probs<-predict(glm.fits,type="response")
glm.probs[1:10] 
#default threshold=0.5 이상이면 Y=1로 예측


glm.pred <- rep("Down",nrow(Smarket))
glm.pred[glm.probs>0.5] <- "Up"
#Confusion Matrix : table(predict,truevalue)
table(glm.pred,Direction)
mean(glm.pred==Direction) #Accuracy

##정석:Train Test set의 이용##

#Split Train & Test set
train <- (Year<2005)
#Test set & test set에 대한 반응변수 저장
Smarket.2005 <- Smarket[!train,] 
Direction.2005 <- Direction[!train]

#logit model using glm(subset=train)
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=Smarket,family=binomial,subset=train)
summary(glm.fits)

#test set에 대한 prediction
glm.probs <- predict(glm.fits,Smarket.2005,type="response")
# Confusion matrix
glm.pred <- rep("Down",nrow(Smarket))
glm.pred[glm.probs>.5] <- "Up"

#test set에 대한 confusion matrix
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) #Accuracy


# New sample prediction (n=2)
# Lag1 Lag2 변수만을 사용하여 glm했다고 가정
glm.fits <- glm(Direction~Lag1+Lag2,data=Smarket,
                family=binomial,subset=train)
predict(glm.fits,data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")
#새 관측치 2개 모두 Down으로 predict됨

############LDA######################
#함수 lda vs glm
lda.fit <- lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
#Prior probabilities of groups 분석
#Down=0.49,Up=0.51(proportion-Down 49%, Up 51%)
#Group means 분석
#Coefficients

#Predictions on test set
lda.pred <- predict(lda.fit, Smarket.2005)
#Predicted Class
lda.class <- lda.pred$class
lda.class

table(lda.class,Direction.2005)
mean(lda.class==Direction.2005) #Accuracy

#test set의 분류확률 출력
lda.pred$posterior
#Number of data Posterior prob. of Up >=0.5 
sum(lda.pred$posterior[,2]>=.5)
#252개의 test set 중 182개가 Up으로 분류
#Number of data Posterior prob of Up < 0.5 
sum(lda.pred$posterior[,2]<.5)#Predicted as Down
#252개의 test set 중 70개가 Down으로 분류

# ROC CURVE로 모델 비교
#roc(true값,fitted model의 prob.값)
glm.roc<-roc(Direction.2005,glm.probs)
lda.roc<-roc(Direction.2005,lda.pred$posterior[,2])
names(glm.roc) #여러가지 변수가 저장되어있음

#plotting ROC CURVE
#필요한 4가지 값만 roc에서 가져오자
glm.roc <-glm.roc[c("specificities","sensitivities","thresholds","auc")]
lda.roc <-lda.roc[c("specificities","sensitivities","thresholds","auc")]

#본격적인 plotting
plot(x=(1-glm.roc$specificities),y=glm.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="1-Specificity",ylab = "Sensitivity",
     main = "ROC curve of LR") 
plot(x=(1-lda.roc$specificities),y=lda.roc$sensitivities,
     col="red",type="l",main="ROC curve of LDA")

#GLM과 LDA의 ROC Curve를 비교하기 위해 겹쳐그리자
plot(x=(1-glm.roc$specificities),y=glm.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) #새로운 그림을 겹쳐그려라.
plot(y=lda.roc$sensitivities,x=(1-lda.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

# Test AUC of Logit model = 0.5197
glm.roc$auc
# Test AUC of LDA model = 0.5584
lda.roc$auc
