##6.1, 6.2, 6.15, 6.20, 6.26

a1<-as.factor(rep(c(-1,+1),8))
b1<-as.factor(rep(c(-1,-1,+1,+1),4))

r1<-c(18.2,27.2,15.9,41.0)
r2<-c(18.9,24.0,14.5,43.9)
r3<-c(12.9,22.4,15.1,36.3)
r4<-c(14.4,22.5,14.2,39.9)
y1<-c(r1,r2,r3,r4)
data1<-data.frame(cbind(a1,b1,y1))
data1
lm1<-aov(y1 ~ a1 * b1, data1)
summary(lm1)

qqnorm(resid(lm1), datax=TRUE)
qqline(resid(lm1), datax=TRUE)
plot(fitted(lm1),resid(lm1))

interaction.plot(data1$a1,data1$b1,data1$y1)

##-------------------------------------------------
a2<-as.factor(rep(c(-1,+1),12))
b2<-as.factor(rep(c(-1,-1,+1,+1),6))
c2<-as.factor(rep(c(-1,-1,-1,-1,+1,+1,+1,+1),3))
r1<-c(21,33,34,56,43,41,61,40)
r2<-c(31,43,34,47,45,37,50,41)
r3<-c(25,29,50,46,38,36,54,47)

x<-rowSums(cbind(r1,r2,r3))

y2<-c(r1,r2,r3)
mean(y2)
data2<-data.frame(cbind(a2,b2,c2,y2))
lm2<-aov(y2~a2*b2*c2,data2)
summary(lm2)
lm2$coefficients
(effa<-((sum(x)-(x[1]+x[3]+x[5]+x[7]))-(x[1]+x[3]+x[5]+x[7]))/12)
(effb<-((x[3]+x[4]+x[7]+x[8])-(sum(x)-(x[3]+x[4]+x[7]+x[8])))/12)
(effc<-((x[5]+x[6]+x[7]+x[8])-(sum(x)-(x[5]+x[6]+x[7]+x[8])))/12)
(effab<-((x[1]+x[4]+x[5]+x[8])-(sum(x)-(x[1]+x[4]+x[5]+x[8])))/12)
(effbc<-((x[1]+x[2]+x[7]+x[8])-(sum(x)-(x[1]+x[2]+x[7]+x[8])))/12)
(effca<-((x[1]+x[3]+x[6]+x[8])-(sum(x)-(x[1]+x[3]+x[6]+x[8])))/12)
(effabc<-((x[2]+x[3]+x[5]+x[8])-(sum(x)-(x[2]+x[3]+x[5]+x[8])))/12)

qqnorm(resid(lm2), datax=TRUE)
qqline(resid(lm2), datax=TRUE)
plot(fitted(lm2),resid(lm2))

interaction.plot(data2$a2,data2$c2,data2$y2)
##----------------------------------------------------------
a3<-as.factor(rep(c(-1,+1),16))
b3<-as.factor(rep(c(-1,-1,+1,+1),8))
c3<-as.factor(rep(c(-1,-1,-1,-1,+1,+1,+1,+1),4))
d3<-as.factor(rep(c(-1,-1,-1,-1,-1,-1,-1,-1,+1,+1,+1,+1,+1,+1,+1,+1),2))
r1<-c(7.037,14.707,11.635,17.273,10.403,4.368,9.360,13.440,8.561,16.867,13.876,19.824,11.846,6.125,11.190,15.653)
r2<-c(6.376,15.219,12.089,17.815,10.151,4.098,9.253,12.923,8.951,17.052,13.658,19.639,12.337,5.904,10.935,15.053)
y3<-c(r1,r2)
xx<-rowSums(cbind(r1,r2))

(data3<-data.frame(cbind(a3,b3,c3,d3,y3)))
lm3<-aov(y3~a3*b3*c3*d3,data3)
summary(lm3)
qqnorm(resid(lm3), datax=TRUE)
qqline(resid(lm3), datax=TRUE)
plot(fitted(lm3),resid(lm3))
plot(a3,fitted(lm3))
plot(b3,fitted(lm3))
plot(c3,fitted(lm3))
plot(d3,fitted(lm3))
interaction.plot(a3,b3,y3)
interaction.plot(a3,c3,y3)
interaction.plot(b3,c3,y3)

(effa<-(2*sum(x3[c(2,4,6,8,10,12,14,16)])-sum(x3))/16)
(effb<-(2*sum(x3[c(3,4,7,8,11,12,15,16)])-sum(x3))/16)
(effc<-(2*sum(x3[c(5,6,7,8,13,14,15,16)])-sum(x3))/16)
(effd<-(2*sum(x3[c(9,10,11,12,13,14,15,16)])-sum(x3))/16)
(effab<-(2*sum(x3[c(1,4,5,8,9,12,13,16)])-sum(x3))/16)
(effbc<-(2*sum(x3[c(1,2,7,8,9,10,15,16)])-sum(x3))/16)
(effca<-(2*sum(x3[c(1,3,6,8,9,11,14,16)])-sum(x3))/16)
(effad<-(2*sum(x3[c(1,3,5,7,10,12,14,16)])-sum(x3))/16)
(effac<-(2*sum(x3[c(1,3,6,8,9,11,14,16)])-sum(x3))/16)
(effbd<-(2*sum(x3[c(1,2,5,6,11,12,15,16)])-sum(x3))/16)
(effabc<-(2*sum(x3[c(2,3,5,8,10,11,13,16)])-sum(x3))/16)
(effabd<-(2*sum(x3[c(2,3,6,7,9,12,13,16)])-sum(x3))/16)
(effbcd<-(2*sum(x3[c(3,4,5,6,9,10,15,16)])-sum(x3))/16)
(effacd<-(2*sum(x3[c(2,4,5,7,9,11,14,16)])-sum(x3))/16)
(effabcd<-(2*sum(x3[c(1,4,6,7,10,11,13,16)])-sum(x3))/16)
mean(y3)
##---------------------------------------
a4<-as.factor(rep(c(-1,+1),8))
b4<-as.factor(rep(c(-1,-1,+1,+1),4))
c4<-as.factor(rep(c(-1,-1,-1,-1,+1,+1,+1,+1),2))
r1<-c(-3,0,-1,2,-1,2,1,6)
r2<-c(-1,1,0,3,0,1,1,5)
x<-rowSums(cbind(r1,r2))
r4<-c(r1,r2)
data4<-data.frame(cbind(a4,b4,c4,r4))
lm4<-aov(r4~a4*b4*c4,data4)
summary(lm4)

(effa<-((sum(x)-(x[1]+x[3]+x[5]+x[7]))-(x[1]+x[3]+x[5]+x[7]))/8)
(effb<-((x[3]+x[4]+x[7]+x[8])-(sum(x)-(x[3]+x[4]+x[7]+x[8])))/8)
(effc<-((x[5]+x[6]+x[7]+x[8])-(sum(x)-(x[5]+x[6]+x[7]+x[8])))/8)
(effab<-((x[1]+x[4]+x[5]+x[8])-(sum(x)-(x[1]+x[4]+x[5]+x[8])))/8)
(effbc<-((x[1]+x[2]+x[7]+x[8])-(sum(x)-(x[1]+x[2]+x[7]+x[8])))/8)
(effca<-((x[1]+x[3]+x[6]+x[8])-(sum(x)-(x[1]+x[3]+x[6]+x[8])))/8)
(effabc<-((x[2]+x[3]+x[5]+x[8])-(sum(x)-(x[2]+x[3]+x[5]+x[8])))/8)

qqnorm(resid(lm4), datax=TRUE)
qqline(resid(lm4), datax=TRUE)
plot(fitted(lm4),resid(lm4))
contour(rep(c(-1,+1),8),rep(c(-1,-1,+1,+1),4),r4)


##----------------------------------------
a5<-as.factor(rep(c(-1,+1),8))
b5<-as.factor(rep(c(-1,-1,+1,+1),4))
c5<-as.factor(rep(c(-1,-1,-1,-1,+1,+1,+1,+1),2))
d5<-as.factor(rep(c(-1,-1,-1,-1,-1,-1,-1,-1,+1,+1,+1,+1,+1,+1,+1,+1),1))
r5<-c(11,18,12,16,17,15,22,15,9,27,13,25,19,22,17,24)

data5<-data.frame(cbind(a5,b5,c5,d5,r5))
lm5<-aov(r5~a5*b5*c5*d5,data5)
summary(lm5)

qqnorm(resid(lm5), datax=TRUE)
qqline(resid(lm5), datax=TRUE)
plot(fitted(lm5),resid(lm5))
128*((42.5)^2)/36
6422.222/13.3
