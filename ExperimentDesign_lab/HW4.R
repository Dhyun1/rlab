setwd("C:/Users/DaeHyun/Desktop/ExpDesign/R")

yex4<-c(73,68,74,71,67,73,67,75,72,70,75,68,78,73,68,73,71,75,75,69)
trtex4<-c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
boltex4<-c(rep(seq(1:5),4))

dataex4<-data.frame(yex4,trtex4,boltex4)
dataex4$trt<-factor(dataex4$trt)
dataex4$bolt<-factor(dataex4$bolt)

interaction.plot(dataex4$bolt,dataex4$trt,yex4)
dataex4
lmex4<-lm(yex4~trt+bolt,dataex4)

anova(lmex4)

qqnorm(resid(lmex4), datax=TRUE)		#normal Q-Q plot 정규성 검정
qqline(resid(lmex4), datax=TRUE)

plot(fitted(lmex4),resid(lmex4))		#등분산성 및 독립성 검정
abline(h=0)

plot(trtex4,resid(lmex4))
plot(boltex4,resid(lmex4))

##----------------------------------------

yex10<-c(0.500,0.634,0.487,0.329,0.512,0.535,0.675,0.520,0.435,0.540,0.513,0.595,0.488,0.400,0.510)
trtex10<-c(rep(1,5),rep(2,5),rep(3,5))
blockex10<-c(rep(seq(1:5),3))

dataex10<-data.frame(yex10,trtex10,blockex10)
dataex10$trtex10<-as.factor(dataex10$trtex10)
dataex10$blockex10<-as.factor(dataex10$blockex10)

interaction.plot(dataex10$blockex10,dataex10$trtex10,yex10)

lmex10<-lm(yex10~trtex10+blockex10,dataex10)

anova(lmex10)

aggregate(dataex10$yex10~dataex10$trtex10, FUN=mean)

qqnorm(resid(lmex10), datax=TRUE)		#normal Q-Q plot 정규성 검정.
qqline(resid(lmex10), datax=TRUE)

plot(fitted(lmex10),resid(lmex10))		#등분산성 및 독립성 검정
abline(h=0)

plot(trtex10,resid(lmex10))
plot(blockex10,resid(lmex10))

##----------------------------------------
yex22<-c(10,14,7,8,7,18,11,8,5,10,11,9,10,10,12,14)
orderex22<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
operex22<-c(rep(seq(1,4),4))
assemex22<-c("C","D","A","B","B","C","D","A","A","B","C","D","D","A","B","C")

dataex22<-data.frame(yex22,orderex22,operex22,assemex22)
dataex22$orderex22<-as.factor(dataex22$orderex22)
dataex22$operex22<-as.factor(dataex22$operex22)
dataex22$assemex22<-as.factor(dataex22$assemex22)

lmex22<-lm(yex22~orderex22+operex22+assemex22,dataex22)			

anova(lmex22)

qqnorm(resid(lmex22), datax=TRUE)
qqline(resid(lmex22), datax=TRUE)

plot(fitted(lmex22),resid(lmex22))
abline(h=0)

plot(dataex22$yex22,resid(lmex22))

plot(dataex22$orderex22,resid(lmex22))
plot(dataex22$operex22,resid(lmex22))
plot(dataex22$assemex22,resid(lmex22))

##-----------------------------------------
yex36<-c(11,10,14,8,8,12,10,12,9,11,7,15,9,8,18,6)
orderex36<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
operex36<-c(rep(seq(1,4),4))
assem1ex36<-c("C","B","D","A","B","C","A","D","A","D","B","C","D","A","C","B")
assem2ex36<-c("beta","gamma","delta","alpha","alpha","delta","gamma","beta","delta","alpha","beta","gamma",
              "gamma","beta","alpha","delta")

dataex36<-data.frame(yex36,orderex36,assem1ex36,assem2ex36)
dataex36$orderex36<-as.factor(orderex36)
dataex36$operex36<-as.factor(operex36)
dataex36$assem1ex36<-as.factor(assem1ex36)
dataex36$assem2ex36<-as.factor(assem2ex36)

lmex36<-lm(yex36~orderex36+operex36+assem1ex36+assem2ex36,dataex36)			

anova(lmex36)

qqnorm(resid(lmex36), datax=TRUE)
qqline(resid(lmex36), datax=TRUE)

plot(fitted(lmex36),resid(lmex36))
abline(h=0)

plot(dataex36$yex36,resid(lmex36))

plot(dataex36$orderex36,resid(lmex36))
plot(dataex36$operex36,resid(lmex36))
plot(dataex36$assem1ex36,resid(lmex36))
plot(dataex36$assem2ex36,resid(lmex36))

##---------------------------------------

yex40<-c(17,14,13,12,14,14,13,10,12,13,12,9,13,11,11,12,11,12,10,8)
addex40<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4))
carex40<-c(2,3,4,5,1,2,4,5,1,3,4,5,1,2,3,4,1,2,3,5)
sum(yex40^2)-mean(yex40)^2*20
dataex40<-data.frame(yex40,addex40,carex40)
dataex40$addex40<-factor(addex40)
dataex40$carex40<-factor(carex40)

lmex40<-lm(yex40~addex40+carex40,dataex40)

anova(lmex40)

interaction.plot(dataex40$addex40,dataex40$carex40,yex40)

qqnorm(resid(lmex40), datax=TRUE)	
qqline(resid(lmex40), datax=TRUE)

plot(fitted(lmex40),resid(lmex40))	
abline(h=0)

plot(dataex40$addex40,resid(lmex40))
plot(dataex40$carex40,resid(lmex40))

