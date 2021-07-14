## data import
data<-read.table("/Users/kyr0302/Desktop/Lab_3/T7-5.dat",header=FALSE,sep="");
colnames(data)<-c("CR","DR","Depth","Temp","Volt","Cycles")

## replicate 
set.seed(1024)
Fake<-data$CR+rnorm(nrow(data),sd=sqrt(var(data$CR)))
data_rep<-cbind(data[,-6],Fake)
y<-data[,6]
                
## centered
y.cen<-y-mean(y)
X.cen<-t(t(data_rep)-colMeans(data_rep))
cov.cen<-cov(X.cen)
eig.res.cen<-eigen(cov.cen)
p=6;n=nrow(X.cen)

## r=p
r=p
Z.cen<-as.matrix(X.cen)%*%eig.res.cen$vectors
gammahat.cen=solve(t(Z.cen)%*%Z.cen)%*%t(Z.cen)%*%y.cen
# lm(y.cen~.+0,data=as.data.frame(Z.cen))
betahat.cen=eig.res.cen$vectors%*%gammahat.cen

temp=matrix(0,nrow=r,ncol=r)
for(i in 1:r){
  temp=temp+(eig.res.cen$vectors[,i]%*%t(eig.res.cen$vectors[,i]))/(eig.res.cen$values[i]*(n-1))
}
temp%*%t(X.cen)%*%y.cen

fit<-lm(y.cen~CR+DR+Depth+Temp+Volt+Fake+0,data=as.data.frame(X.cen))
# fit1<-lm(y.cen~CR+DR+Depth+Temp+Volt+0,data=as.data.frame(X.cen))
sum.fit<-summary(fit);
# b<-summary(fit1);

sum.fit$sigma*temp
sum.fit$cov.unscaled

## r<p
r=5
Z.cen<-as.matrix(X.cen)%*%eig.res.cen$vectors[,1:r]
gammahat.cen=solve(t(Z.cen)%*%Z.cen)%*%t(Z.cen)%*%y.cen
# lm(y.cen~.+0,data=as.data.frame(Z.cen))
betahat.cen=eig.res.cen$vectors[,1:r]%*%gammahat.cen

temp=matrix(0,nrow=p,ncol=p)
for(i in 1:r){
  temp=temp+(eig.res.cen$vectors[,i]%*%t(eig.res.cen$vectors[,i]))/(eig.res.cen$values[i]*(n-1))
}
temp%*%t(X.cen)%*%y.cen
sum.fit$sigma*temp
fit
# fit1<-lm(y.cen~CR+DR+Depth+Temp+Volt+0,data=as.data.frame(X.cen))
sum.fit<-summary(fit);
# b<-summary(fit1);
