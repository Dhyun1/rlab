
## 6.7
A<-c(71,57,85,67,66,79)
B<-c(76,94,61,36,42,49)
C<-c(80,104,101,90,93,85)
x<-c(A,B,C)
trt<-c(rep(1,length(A)),rep(2,length(B)),rep(3,length(C)))
block<-c(rep(1:6,3))
install.packages("clinfun")
library(clinfun)
jonckheere.test(x,trt,"increasing")
summary(anova(lm(x ~ trt + block)))
## 6.11
install.packages("pgirmess",dependencies = TRUE)
library(pgirmess)
y<-c(C,B,A)
trt0<-c(rep(1,length(C)),rep(2,length(B)),rep(3,length(A)))

kruskalmc(y, as.factor(trt0), probs = 0.15, cont="one-tailed")
kruskalmc(y, as.factor(trt0), probs = 0.15, cont="two-tailed")

## 7.2
A2<-c(-0.08,0.21,0.50,0.14)
B2<-c(0.01,0.17,-0.11,0.07)
C2<-c(0.06,0.19,0.34,0.14)
x2<-c(A2,B2,C2)
trt2<-factor(c(rep("A2",length(A2)),rep("B2",length(B2)),rep("C2",length(C2))))
block2<-factor(c(rep(1:4,3)))
friedman.test(x2,trt2,block2)
## 7.5
A3<-c(3.5,3.7,1.6,2.5,2.8,2.0,5.9,2.5)
B3<-c(5.9,8.1,8.1,8.6,8.1,5.9,9.5,7.9)
C3<-c(13.9,12.6,8.1,6.8,14.3,4.2,14.5,7.9)
x3<-cbind(A3,B3,C3)
library(crank)
page.trend.test(x3,rank=FALSE)
## 7.9
A4<-c(22.6,53.1,8.3,21.6,13.3,37.0,14.8,14.8)
B4<-c(23.1,57.6,10.5,23.6,11.9,54.6,21.0,20.3)
C4<-c(22.7,53.2,9.7,19.6,13.8,47.1,13.1,23.6)
D4<-c(22.5,53.7,10.8,21.1,13.7,39.2,13.7,16.3)
x4<-c(A4,B4,C4,D4)
trt4<-factor(c(rep("A4",length(A4)),rep("B4",length(B4)),rep("C4",length(C4)),rep("D4",length(D4))))
block4<-factor(c(rep(1:8,4)))
friedman.test(x4,trt4,block4)

library(PMCMR)
posthoc.friedman.nemenyi.test(x4, trt4, block4, probs = 0.05, cont="one-tailed")

## 8.2
x1<-c(20,17,15,19,23,14,27,17,18,15,15,23,21,16,12,19,18,19,16,17,26,21)
y1<-c(90,94,100,103,103,106,108,109,109,112,112,113,114,118,119,120,124,132,133,141,155,157)
cor.test(x1, y1, method="kendall", alternative="greater")
cor.test(x1, y1, method="spearman", alternative="greater")

## 8.4
x2<-c(1:13)
y2<-c(6.1,7.5,7.7,5.9,5.2,6.1,5.3,4.5,4.9,4.6,3.0,4.0,3.7)
cor.test(x2, y2, method="kendall", alternative="less")
cor.test(x2, y2, method="spearman", alternative="less")

## Extra 2, lambda=1
trt<-c(1,1,1,1,2,2,2,2,3,3,3,3)
j<-matrix(c(1,1,1,1,2,2,2,2,3,3,3,3),nrow=4)
N=1000
n.f=0
n.k=0
set.seed(0312)
for(i in 1:N){
  data<-matrix(c(rep(0,12)),nrow=4)
  e<-matrix(rt(12,1),nrow=4)
  data<-j+e
  x<-c(data[,1],data[,2],data[,3])
  t<-colSums(data)
  b<-rowSums(data)
  SSTrt<-sum(t^2)/4-sum(data)^2/12
  SST<-sum(data^2)-sum(data)^2/12
  SSE<-SST-SSTrt
  F0<-(SSTrt/2)/(SSE/9)
    if(F0>qf(0.95,2,9)){
      n.f=n.f+1
    }
    if(kruskal.test(x,trt)$p.value<0.05){
      n.k=n.k+1
    }
}
n.f/N;n.k/N

## lambda = 10
n.f=0
n.k=0
set.seed(0312)
for(i in 1:N){
  data<-matrix(c(rep(0,12)),nrow=4)
  e<-matrix(rt(12,10),nrow=4)
  data<-j+e
  x<-c(data[,1],data[,2],data[,3])
  t<-colSums(data)
  b<-rowSums(data)
  SSTrt<-sum(t^2)/4-sum(data)^2/12
  SST<-sum(data^2)-sum(data)^2/12
  SSE<-SST-SSTrt
  F0<-(SSTrt/2)/(SSE/9)
  if(F0>qf(0.95,2,9)){
    n.f=n.f+1
  }
  if(kruskal.test(x,trt)$p.value<0.05){
    n.k=n.k+1
  }
}
n.f/N;n.k/N

## lambda = 100
n.f=0
n.k=0
set.seed(0312)
for(i in 1:N){
  data<-matrix(c(rep(0,12)),nrow=4)
  e<-matrix(rt(12,100),nrow=4)
  data<-j+e
  x<-c(data[,1],data[,2],data[,3])
  t<-colSums(data)
  b<-rowSums(data)
  SSTrt<-sum(t^2)/4-sum(data)^2/12
  SST<-sum(data^2)-sum(data)^2/12
  SSE<-SST-SSTrt
  F0<-(SSTrt/2)/(SSE/9)
  if(F0>qf(0.95,2,9)){
    n.f=n.f+1
  }
  if(kruskal.test(x,trt)$p.value<0.05){
    n.k=n.k+1
  }
}
n.f/N;n.k/N


## Extra 3
N=1000
beta<-matrix(rep(c(-0.63,0.18,-0.84,1.60,-0.31),3),nrow=5)
set.seed(0312)
k<-c(1.5,3.5,6,6.5,12.5)
cnt<-c(0,0,0,0,0)
for(i in 1:N){
  data<-beta+5
  index<-NULL
  
  e<-matrix(runif(15,-0.5,0.5),nrow=5)
  data<-data+e
  x<-c(data[,1],data[,2],data[,3])
  trt<-factor(c(rep(1,5),rep(2,5),rep(3,5)))
  block<-factor(c(rep(1:5,3)))
  index<-which(friedman.test(x,trt,block)$statistic>k)
  cnt[index]<-cnt[index]+1
}
cnt/N
