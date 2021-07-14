setwd("C:/Users/DaeHyun/Desktop/Study")
getwd()
#data=read.csv(".\\●데이터마이닝\\")

library(MASS)
library(ISLR)
library(car)

dim(Boston)
names(Boston)
str(Boston)
head(Boston)

#attach:사용데이터를 Boston으로 fix
attach(Boston)

#lstat(x)과 medv(y)간 관계 plotting
par(mfrow=c(1,1))
plot(lstat,medv,pch=20)

lm.fit=lm(medv~lstat,data=Boston)
summary(lm.fit)
#전체 model 유의성 검정 F-test
#각 coeff. 유의성 검정 t-test
#각 coeff. 부호와 값 확인

names(lm.fit) #lm fitting 했을 때 얻을 수 있는 값들 확인
coef(lm.fit) #coeff. 값 확인 
confint(lm.fit) #coef C.I 확인-0이 신뢰구간에 포함되면 H0기각

#predict(fit명, dataframe명, interval="confidene" or "prediction") 
#predictor lstat=c(5,10,15)일 때 fitted value, 95% C.I.
predict(lm.fit,data.frame(lstat=c(5,10,15)),
        interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),
        interval="prediction")

abline(lm.fit,lwd=2,col="red")

#fitted model plot, 4가지 plot 도출
par(mfrow=c(2,2))
plot(lm.fit) 

par(mfrow=c(1,1))
#1)Residual vs. Fitted : 등분산성, random성(패턴이 있으면 안됨)
plot(predict(lm.fit),residuals(lm.fit))
#2)Normal Q-Q Plot : 정규성
qqnorm(rstandard(lm.fit))
qqline(rstandard(lm.fit))
#3)Standardized Residual Plot : residual plot과 비슷
plot(predict(lm.fit),rstandard(lm.fit))
#4)Leverage Plot : influential point 분석
#5)Studentized Residual Plot : residual plot과 비슷
plot(predict(lm.fit),rstudent(lm.fit))

#influential point
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) 
#hatvalues가 가장 큰 데이터가 influential point임을 확인


lm.fit2=lm(medv~lstat+age,data=Boston)
summary(lm.fit2)

lm.fitall=lm(medv ~.,data=Boston)
#note:age변수가 full model에서 유의성이 사라짐.
#이는 age와 다른 변수의 multicollinearity 때문
#=>vif로 확인 
vif(lm.fitall) #vif score가 10 이상이면 multicol. 발생하여 삭제
#vif 9.0인 tax를 제거하여 다시 fitting

lm.fittax=lm(medv~.-tax,data=Boston)
vif(lm.fittax) #rad 변수의 vif score가 확실히 줄어듦
summary(lm.fittax)


##interaction##
#interaction term을 포함하면 individual effect도 반드시 고려
summary(lm(medv~lstat*age,data=Boston))
#model에 안 써놔도 summary에 individual이 들어가있음

##Non-linear transformation of predictors##
#반드시 제곱할 때 I() 사용해야함
lm.fit_full<-lm(medv~lstat+I(lstat^2),data=Boston)
summary(lm.fit_full)

#제곱TERM의 유의성을 보기 위해 Full model, reduced model 비교
#anova(reduced model,full model)
anova(lm.fit,lm.fit_full) #F-test p-value가 매우 작다
#따라서 제곱TERM을 넣는 모델이 유의하다.

par(mfrow=c(2,2))
plot(lm.fit_full)

detach(Boston)
##categorical variable##
names(Carseats)
table(Carseats$Urban)
contrasts(Carseats$Urban) #factor화했을때 Yes가 1

lm.fit=lm(Sales~Urban,data=Carseats)
summary(lm.fit)

#relevel()을 이용해 factor 변수의 reference level을 default="Yes"로
Carseats$Urban<-relevel(Carseats$Urban,ref="Yes")
contrasts(Carseats$Urban)

lm.fit=lm(Sales~Urban,data=Carseats)
summary(lm.fit) #relevel전과 결과는 똑같음, 회귀계수 부호만 반대

#full model with interaction term
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

