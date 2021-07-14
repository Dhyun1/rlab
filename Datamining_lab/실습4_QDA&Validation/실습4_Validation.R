library(glmnet)
library(MASS)
library(leaps)
library(AUC)
library(ISLR)
library(boot) 
par(mfrow=c(1,1))
names(Auto)
summary(Auto)
attach(Auto)

##1)Validation Set Approach
#데이터의 절반을 랜덤으로 나눔
set.seed(1)
train <- sample(1:nrow(Auto),nrow(Auto)/2) 

# Linear
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train)
# Validation MSE 계산(validation test [-train] 이용!)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Second order
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# Third order
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#seed=2 validation approach (sensitive)
set.seed(2)
train <- sample(1:nrow(Auto),nrow(Auto)/2)

#Linear
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Second order
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# Third order
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#data가 어떻게 나오느냐에 따라서, validation mse
#값을 평균이 아니라 한번만 사용하므로 굉장히 sensitive
#seed=2일 때와 seed=1일 때 validation mse의 최솟값이 나오는
#model이 다르다!

#2) LOOCV : 랜덤성이 없다. 몇번을 해도 결과는 똑같다
#n번 모델을 적합시켜야 하므로 computation이 길다
#sample 수가 많을 때는 너무 오래걸림

glm.fit <- glm(mpg~horsepower,data=Auto)
#default family="gaussian"
summary(glm.fit)
coef(glm.fit)
lm.fit <- lm(mpg~horsepower,data=Auto)

#cv.glm()의 활용
#cv.glm(데이터,glmfit) 
#default=LOOCV(옵션 k=n)
cv.err <- cv.glm(Auto,glm.fit)
cv.err$delta #1번째 값만 중요(2번째는 adjusted임)

#본격적으로, poly=1,2,3,4,5에 대한 cv.error 계산
#cv.glm()값의 .$delta[1]만 중요!
cv.error <- rep(0,5)
for (i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i] <- cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
#LOOCV이기 때문에 결과가 바뀌지 않는다.
#degree=5일 때 cv.error[5] 값이 전체 중 제일 작았다.

#3)K-Fold
#LOOCV와 동일, cv.glm()에서 K=10 옵션만 추가
cv.error.10 <- rep(0,10)
set.seed(17)

for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
rank(cv.error.10) #k=9일 때 best

####Variable Selection#####
##test error 추정과 더불어
##+++tuning parameter lambda를 CV로 결정하는게 중요
##Ridge regression, Lasso regression
help(Boston) #DATA 변수 설명

data(Boston)
names(Boston)
Boston <- Boston[,order(colnames(Boston))] #변수 알파벳 순서로 정렬

#Train : Test = 2:1 with 1/3 ceiling(nrow)
set.seed(201910)
ind <- sample(1:nrow(Boston),ceiling(nrow(Boston)/3)) # 1:2 selection
boston_train<-Boston[-ind,]
boston_test<-Boston[ind,]

####Ridge Regression####
#Choosing Tuning Parameter lambda
#lambda는 데이터로 추정되는 것이 아니라
#user가 정하는 hyperparameter로, 이는 CV로 정한다.

#cv.glmnet의 활용
#cv.glmnet(y를제외한matrix,y,alpha,nfolds)
ridge_cv<-cv.glmnet(x = as.matrix(boston_train[,-8]),y = boston_train$medv,
                    alpha = 0,nfolds = nrow(boston_train))
#alpha=0은 Ridge Reg., alpha=1은 Lasso Reg.
#nfolds=nrow(train)은 LOOCV를 의미

ridge_cv$lambda.min #mse가 최소인 lambda값
ridge_cv$lambda.1se #lambda값 + stanard error값

plot(ridge_cv) #mse가 최소인 지점을 찾는다

#mse가 최소인 lambda를 찾았으므로,
#그 lambda에 대한 test error를 구한다.
#glmnet의 활용(lambda 구할 때는 cv.glmnet 썼음)
model<-glmnet(x=as.matrix(boston_train[,-8]),y=boston_train$medv,
              alpha = 0,lambda = ridge_cv$lambda.min)
summary(model)
model$beta #beta값이 0으로 shrinkage 되지만, 0이 되지는 않는다

#test error = mean( model로 predict-test set의 y참값)
#1. cv.glmnet으로 lambda 찾기
#2. cv.glmnet$lambda.min을 구하기
#3. 구한 lambda.min을 넣어 glmnet 도출(1과 cv빼고 동일)
#4. glmnet$beta를 구해 shrinkage 관찰(아그렇구나정도)
#5. 최적화된 model을 구했고, 이제 test error 구하기
#5. (test set)prediction = predict(glmnet,newx=test_set[,-$y])
#6. (test set)true value = test_set[,$y]
#6. test error = (prediction - true value)^2의 평균
error.ridge<-mean((predict(model, newx = as.matrix(boston_test[,-8]))-boston_test$medv)^2)
error.ridge

####Lasso Regression####
#Ridge와 차이 : 안 중요한 변수의 계수를 0으로 만듦
#L1 penalty.

#코드 동일, alpha=1만 바꿈
lasso_cv <- cv.glmnet(x = as.matrix(boston_train[,-8]),
                      y = boston_train$medv,alpha = 1)
#nfolds의 default 값은 nrow()이므로
#앞서 Ridge와는 다르게 굳이 안 썼었음.

lasso_cv$lambda.min
lasso_cv$lambda.1se 
#1se-min이 0과 가깝다면 se가 매우 작은 것

plot(lasso_cv)

#glmnet으로 모델 확정
model_min<-glmnet(x = as.matrix(boston_train[,-8]),
                  y = boston_train$medv,alpha = 1,
                  lambda = lasso_cv$lambda.min)
model_min$beta#beta=0으로 추정되는 변수가 2개이다.

#test error 계산
#prediction of test set(f(x))-true value of test set(y)
#의 제곱의 평균
mean((predict(model_min, newx = as.matrix(boston_test[,-8]))-boston_test$medv)^2)

#lambda = lambda.min + 1se일 때도 해보자
model_1se<-glmnet(x = as.matrix(boston_train[,-8]),y = boston_train$medv,alpha = 1,lambda = lasso_cv$lambda.1se)
model_1se$beta #beta=0으로 추정되는 변수가 4개나 된다!
#lambda를 크게 잡았기 때문(penalty를 많이 주었다)
#test error
mean((predict(model_1se, newx = as.matrix(boston_test[,-8]))-boston_test$medv)^2)
#당연히 lambda.min의 경우보다 크게 나온다.