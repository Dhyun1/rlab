getwd()
iris<-read.csv("./●데이터마이닝/data/iris.csv")
library("ggplot2")

# Data input
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

relation <-lm(y~x)
summary(relation)

#predict(fitted model이름,dataframe이름)
#predict할때는 반드시 x를 dataframe으로 저장!
result<-predict(relation,data.frame(x=170))
print(result)

#내보낼 file 이름 선언
png(file="Linear_regression.png")

#plot() 함수와 그 인자들
#abline(fitted model명) cex점크기 pch점모양 main제목 xlab,ylab
plot(x,y,col = "blue",main = "Height & Weight Regression",
     abline(relation),cex = 1.3,pch = 16,xlab = "Height in cm",ylab = "Weight in kg")

#dev.off()로 파일저장
dev.off()

#95% C.I. for new data(in dataframe)
#predict(fittedmodel명,predict할 dataframe명,interval)
predict(relation,data.frame(x=170),interval="confidence")
predict(relation,data.frame(x=170),interval="prediction")


#모든 기존 x값에 대한 P.I. 도출
pred.int <- predict(relation, interval = "prediction")
mydata <- as.data.frame(cbind(y,x,pred.int)) 

#회귀선과 모든 기존 x값에 대한 C.I. 도출
#ggplot, geom_point(), stat_smooth() 이용
p <- ggplot(mydata, aes(x, y)) +
  geom_point() +
  stat_smooth(method = lm)

#geom_line을 이용해 앞서 구한 P.I.을 plot에 같이 표시
#geom_line(aes(y=lwr))+geom_line(aes(y=upr))
p + geom_line(aes(y=lwr),color="red",linetype="dashed")+
  geom_line(aes(y=upr),color="red",linetype="dashed")


model <-lm(dist~speed,data=cars)
summary(model)

new.speeds <- data.frame(
  speed = c(12, 19, 24)
)

predict(model, new.speeds, interval = "confidence")
predict(model, new.speeds, interval = "prediction")

#전체 data에 대한 P.I.
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(cars, pred.int) # 저장

#C.I. 표시
#ggplot, geom_point(), stat_smooth() 이용
p <- ggplot(mydata, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm)

#P.I. 구한 것을 plot에 표시
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")
