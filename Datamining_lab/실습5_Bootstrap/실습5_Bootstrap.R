#Bootstrap
names(Portfolio)
str(Portfolio) #100x2 dataframe
dim(Portfolio)
head(Portfolio)
#상황:alpha만큼 X, 나머지 1-alpha를 Y에 투자

#1) Estimating alpha(ISLR p.187)

#bootstrap function 정의(alpha_hat 추정식)
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

#conduct bootstrap
alpha.fn(Portfolio,1:100) 
#모든 data를 사용하므로 set.seed 필요없음

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
#모든 data를 사용하되, 복원추출 sampling
#alpha 값이 커졌다. X의 투자 비율을 높게 추정하였다.

#boot()의 활용
#boot(Data,모수추정함수,반복횟수R)
boot(Portfolio,alpha.fn,R=1000)
#original과의 bias가 -0.001로 매우 작다(거의 정확히 추정)
#standard error=0.093

#2)LR Accuracy 추정
#bootstrap function의 인자는 data와 index!
boot.fn=function(data,index){
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}
dim(Auto)
boot.fn(Auto,1:nrow(Auto))
#data set 전체를 1번 이용하는 경우 beta는 -0.15

set.seed(1) #항상 sample 앞에는 set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
#seed=1 bootstrap 1번 했을 때 beta는 -0.163

#bootstrap 1번 하는것은 의미가 없고, 여러번 해야함
#variability를 추정할 수 있기 때문
boot(Auto,boot.fn,1000)

#원래 data set과 bootstrap 결과 se 비교
summary(lm(mpg~horsepower,data=Auto))$coef

#boot()결과와 summary()$coef결과를 비교하면
#추정치는 완전 같고, se는 boot결과가 좀 더 컸다.
#더 신뢰할 수 있는 값은 bootstrap의 se이다.
#이유1.LR에서는 sigma^2를 MSE로 추정하는데,
#이 추정의 variance가 se의 variance에 영향을 준다.
#이유2.LR에서는 X가 given이라고 설정, variance
#가 오차항 e_i에서만 온다고 생각하지만 이 가정이 틀릴 수 있다.
#bootstrap에는 이러한 assumption들을 필요로 하지 않으므로 더 신뢰할 수 있음
#bootstrap은 또한 LR과 같이 se를 계산하는 formula가 없는 경우
#se을 구하는 방법으로 유용하다.
#그러나 단점:LR의 경우와 비교했을 때 계산이 많다.

#poly=2일 때 bootstrap
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)
#boot(데이터,boot함수,1000)
boot(Auto,boot.fn,1000)
#단순 LR과 비교
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

