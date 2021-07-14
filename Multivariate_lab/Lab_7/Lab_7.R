### data import

setwd("/Users/kyr0302/Dropbox/2016-2/다변량/Lab_7")
data<-read.table("T11-2.dat")
colnames(data)<-c("region","gender","Freshwater","Marine")

x1bar<-colMeans(data[data$region==1,3:4])
x2bar<-colMeans(data[data$region==2,3:4])
S1<-cov(data[data$region==1,3:4])
S2<-cov(data[data$region==2,3:4])
n1<-50;n2<-50;n=n1+n2;

plot(data[data$region==1,3:4],col="red",type="p",xlim=c(30,200),ylim=c(150,550),pch=16)
points(data[data$region==2,3:4],col="blue",pch=16)
legend("bottomright",c("Alaskan","Canadian"), col=c("Red","Blue"), text.col = "black", 
       pch=c(16,16,16), merge = TRUE)

## Equal variance 가정
S_pooled<-(n1-1)/(n-2)*S1+(n2-1)/(n-2)*S2
S_pooled.inv<-solve(S_pooled)

A<-(x1bar-x2bar)%*%S_pooled.inv
B<-(x1bar-x2bar)%*%S_pooled.inv%*%(x1bar+x2bar)/2
c1.2=1; c2.1=1; p1=0.5; p2=0.5
threshold<-log(c1.2/c2.1*p2/p1)
class<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(A%*%t(data[i,3:4])-B>threshold) class[i]<-1
  else class[i]<-2;
}

table(data[,1],class)

## Unequal variacne 가정
S1.inv<-solve(S1);
S2.inv<-solve(S2);
A<-x1bar%*%S1.inv-x2bar%*%S2.inv;
B<-S1.inv-S2.inv;
k<-(log(det(S1)/det(S2))+x1bar%*%S1.inv%*%x1bar-x2bar%*%S2.inv%*%x2bar)/2;
c1.2=1; c2.1=1; p1=0.5; p2=0.5;
threshold<-log(c1.2/c2.1*p2/p1);
class<-rep(0,nrow(data));

for(i in 1:nrow(data)){
  x0<-as.numeric(data[i,3:4])
  if(-x0%*%B%*%x0/2+A%*%x0-k>threshold) class[i]<-1
  else class[i]<-2;
}

table(data[,1],class)

# LOOCV -------------------------------------------------------------------

## Eqaul
S_pooled<-(n1-1)/(n-2)*S1+(n2-1)/(n-2)*S2
S_pooled.inv<-solve(S_pooled)
c1.2=1; c2.1=1; p1=0.5; p2=0.5
threshold<-log(c1.2/c2.1*p2/p1)
class<-rep(0,nrow(data))
nk<-c(n1,n2);xbar<-rbind(x1bar,x2bar)

for(i in 1:nrow(data)){
  xh<-data[i,3:4]
  if(data[i,"region"]==1) {
    k<-1
    xbar.new<-xbar;
    xbar.new[1,]<-(xbar[1,]*n1-as.numeric(xh))/(n1-1)
    }
  else {
    k<-2
    xbar.new<-xbar;
    xbar.new[2,]<-(xbar[2,]*n1-as.numeric(xh))/(n2-1)
    }
  df<-as.numeric(xh-xbar[k,])
  c_k<-nk[k]/((nk[k]-1)*(n-2))
  temp<-c_k/(1-c_k*df%*%S_pooled.inv%*%df)
  S_H.pooled.inv<-(n-3)/(n-2)*(S_pooled.inv+(S_pooled.inv%*%df%*%df%*%S_pooled.inv)*as.numeric(temp))
  A=(xbar.new[1,]-xbar.new[2,])%*%S_H.pooled.inv%*%as.numeric(xh)
  B=(xbar.new[1,]-xbar.new[2,])%*%S_H.pooled.inv%*%(xbar.new[1,]+xbar.new[2,])/2
  if(A-B>threshold) class[i]<-1
  else class[i]<-2;
}

table(data[,1],class)

## Unequal case
S1.inv<-solve(S1);
S2.inv<-solve(S2);
c1.2=1; c2.1=1; p1=0.5; p2=0.5;
threshold<-log(c1.2/c2.1*p2/p1);
class<-rep(0,nrow(data));
nk<-c(n1,n2);xbar<-rbind(x1bar,x2bar)

for(i in 1:nrow(data)){
  xh<-as.numeric(data[i,3:4])
  if(data[i,"region"]==1) {
    k<-1
    xbar.new<-xbar;
    xbar.new[1,]<-(xbar[1,]*n1-as.numeric(xh))/(n1-1)
    df<-as.numeric(xh)-xbar[1,];
    S1.inv.new<-(n1-2)/(n1-1)*(S1.inv+(S1.inv%*%df%*%df%*%S1.inv)/as.numeric((n1-1-t(df)%*%S1.inv%*%df)))
    S2.inv.new<-S2.inv
    S1.new<-(n1-1)/(n1-2)*S1-df%*%t(df)/(n1-2)
    S2.new<-S2
  }
  else {
    k<-2
    xbar.new<-xbar;
    xbar.new[2,]<-(xbar[2,]*n1-as.numeric(xh))/(n2-1)
    df<-as.numeric(xh)-xbar[2,];
    S2.inv.new<-(n2-2)/(n2-1)*(S2.inv+(S2.inv%*%df%*%df%*%S2.inv/as.numeric((n2-1-t(df)%*%S2.inv%*%df))))
    S1.inv.new<-S1.inv
    S2.new<-(n2-1)/(n2-2)*S2-df%*%t(df)/(n2-2)
    S1.new<-S1.new
  }
  k<-(log(det(S1.new)/det(S2.new))+xbar.new[1,]%*%S1.inv.new%*%xbar.new[1,]
      -xbar.new[2,]%*%S2.inv.new%*%xbar.new[2,])/2;
  A<-S1.inv.new-S2.inv.new; B<-xbar.new[1,]%*%S1.inv.new-xbar.new[2,]%*%S2.inv.new;
  if(-xh%*%A%*%xh/2+B%*%xh-k>threshold) class[i]<-1
  else class[i]<-2
}

table(data[,1],class)

