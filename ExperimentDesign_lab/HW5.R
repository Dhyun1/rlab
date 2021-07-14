Temp=as.factor(c(rep("T1",8),rep("T2",8),rep("T3",8),rep("T4",8)))
Co= as.factor(c(rep(c("C1", "C2", "C3", "C4"),8)))
y = c(17,16,24,28,20,21,22,27,12,18,17,27,9,13,
      12,31,16,18,25,30,12,21,23,23,21,23,23,29,
         17,21,22,31)
dataex7<-data.frame(Temp,Co,y)

lmex7<-lm(y~Temp*Co,data=dataex7)
anova(lmex7)

qqnorm(resid(lmex7), datax=TRUE)
qqline(resid(lmex7), datax=TRUE)

plot(fitted(lmex7),resid(lmex7))
plot(dataex7$Temp,resid(lmex7))
plot(dataex7$Co,resid(lmex7))

by(dataex7, dataex7[,"Co"], function(x) predict.lm(lm(y~Temp, data=x), interval="confidence"))
predict.lm(lm.ex7, interval="confidence") 

library(BHH2)
anovaPlot(lm.ex7,labels = TRUE)

interaction.plot(dataex7$Temp,dataex7$Co,y)

##-------------------------------------------------
Tempex13=c(rep("T1",6),rep("T2",6),rep("T3",6))
Posex13=c(rep(c("P1","P2"),9))
yex13=c(571,530,564,548,584,520,1063,988,1080,1026,
        1043,1004,565,525,511,538,588,530)
dataex13<-data.frame(Tempex13,Posex13,yex13)
lmex13<-lm(yex13~Tempex13+Posex13,data=dataex13)
anova(lmex13)

qqnorm(resid(lmex13), datax=TRUE)
qqline(resid(lmex13), datax=TRUE)

plot(fitted(lmex13),resid(lmex13))
plot(dataex13$Tempex13,resid(lmex13))
plot(dataex13$Posex13,resid(lmex13))

interaction.plot(dataex13$Tempex13,dataex13$Posex13,yex13)

##---------------------------------------------------
dataex7$Block<-as.factor(rep(c(rep("A",4),rep("B",4)),4))
lm..ex7<-lm(y~Co*Temp+Block,dataex7)
anova(lm..ex7)

qqnorm(resid(lm..ex7), datax=TRUE)
qqline(resid(lm..ex7), datax=TRUE)

plot(fitted(lm..ex7),resid(lm..ex7))
plot(dataex7$Temp,resid(lm..ex7))
plot(dataex7$Co,resid(lm..ex7))
plot(dataex7$Block,resid(lm..ex7))
