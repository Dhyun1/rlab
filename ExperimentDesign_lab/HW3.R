
y1<-c(3135,3001,2865,2890)
y2<-c(3210,3311,2975,3150)
y3<-c(2815,2910,2985,3050)
y4<-c(2619,2706,2612,2765)
y<-c(y1,y2,y3,y4)
lab<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
trt<-factor(lab)
data<-data.frame(y,trt)
lm.ex7<-lm(y~trt)
data.aov<- anova(lm.ex7)

library(BHH2)
anovaPlot(lm.ex7,labels = TRUE)

aggregate(y~trt, FUN=mean)
qt(0.975, 12, lower.tail = TRUE)

fit.ex7<-fitted(lm.ex7)
res.ex7<-resid(lm.ex7)


plot(fit.ex7,res.ex7, color=trt)
install.packages("lazyeval")
library(ggplot2)
data.mod = data.frame(fit.ex7,res.ex7, trt)
ggplot(data.mod, aes(fit.ex7, res.ex7, colour = trt))
qqnorm(res.ex7)
qqline(res.ex7)

