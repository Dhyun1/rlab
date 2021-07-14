# Ex 5.9 ------------------------------------------------------------------

## data import
data<-read.csv("/users/kyr0302/Desktop/lab_2/P5-9.dat",header=FALSE,sep="");
xbar<-c(95.92,164.38,55.69,93.39,17.98,31.13);
cov<-matrix(rep(0,6*6),nrow=6)
cov<-data[1:6,];cov[6,6]<-21.26;
cov[upper.tri(cov,diag=F)]=0
cov<-cov+t(cov)-diag(c(3266.46,721.91,179.28,474.98,9.95,21.26))

xbar;cov;

## (a)
n=61; p=6; lb<-rep(0,p); ub<-rep(0,p);
for(i in 1:p){
  # a<-sqrt(p*(n-1)*qf(0.95,p,n-p)*cov[i,i]/(n*(n-p))); # no large sample
  a<-sqrt(qchisq(0.95,p)*cov[i,i]/n); # large sample
  lb[i]<-xbar[i]-a; ub[i]<-xbar[i]+a
}
ci<-cbind(lb,ub)
ci


## (b) 
partxbar<-xbar[c(1,4)]; partcov<-cov[c(1,4),c(1,4)]
library(car)
plot(0,0,xlim=c(60,130),ylim=c(70,110))
ellipse(partxbar, as.matrix(partcov), sqrt(qchisq(0.95,p)/n), center.pch=19,         center.cex=1, 
        segments=51, add=TRUE, xlab="", ylab="", 
        las=par('las'), col=palette()[2], lwd=2, lty=1)
rect(ci[1,1],ci[4,1],ci[1,2],ci[4,2],lty=2)
abline(v=ci[1,1],lty=3); abline(v=ci[1,2],lty=3);
abline(h=ci[4,1],lty=3); abline(h=ci[4,2],lty=3);

## (c)

bflb<-rep(0,p); bfub<-rep(0,p);
for(i in 1:p){
  # a<-qt(0.05/(2*p),n-1,lower.tail=FALSE)*sqrt(cov[i,i]/n) ; no large sample
  a<-qnorm(0.05/(2*p),lower.tail=FALSE)*sqrt(cov[i,i]/n) ; large sample
  bflb[i]<-xbar[i]-a; bfub[i]<-xbar[i]+a
}
bfci<-cbind(bflb,bfub)
bfci

## (d)
plot(0,0,xlim=c(60,130),ylim=c(70,110))
ellipse(partxbar, as.matrix(partcov), sqrt(qchisq(0.95,p)/n), center.pch=19,         center.cex=1, 
        segments=51, add=TRUE, xlab="", ylab="", 
        las=par('las'), col=palette()[2], lwd=2, lty=1)
rect(bfci[1,1],bfci[4,1],bfci[1,2],bfci[4,2],lty=2)
rect(ci[1,1],ci[4,1],ci[1,2],ci[4,2],lty=3)
abline(v=ci[1,1],lty=3); abline(v=ci[1,2],lty=3);
abline(h=ci[4,1],lty=3); abline(h=ci[4,2],lty=3);
abline(v=bfci[1,1],lty=2); abline(v=bfci[1,2],lty=2);
abline(h=bfci[4,1],lty=2); abline(h=bfci[4,2],lty=2);

##(e)
dif<-xbar[5]-xbar[6];
a<-qnorm(0.05/(2*7))*sqrt(dif^2*(cov[5,5]+cov[6,6]-2*cov[5,6])/n)
bfub<-dif+a ; bflb<-dif-a;
bfci<-cbind(bflb,bfub)
bfci

# Ex 5.16 -----------------------------------------------------------------

## data import
obs<-c(105,119,56,25,50); sp<-c(0.3,0.33,0.16,0.07,0.14);

## (a)
sigma<-diag(sp)-sp%*%t(sp);
q=4; n=355; p=q+1;
lb<-rep(0,p); ub<-rep(0,p);
for(i in 1:p){
  a<-sqrt(qchisq(0.95,q)*sigma[i,i]/n);
  lb[i]<-sp[i]-a; ub[i]<-sp[i]+a
}
ci<-cbind(lb,ub)
ci

## (b)
dif<-sp[1]-sp[2];
a<-sqrt(qchisq(0.95,1)*(sigma[1,1]+sigma[2,2]-2*sigma[1,2])/355);
lb<-dif-a; ub<-dif+a;
ci<-cbind(lb,ub);
ci


# 5.17 --------------------------------------------------------------------
## data import
obs<-c(117,62,21); sp<-obs/sum(obs);

## Construct CI
sigma<-diag(sp)-sp%*%t(sp);
q=2; n=sum(obs); p=q+1;
lb<-rep(0,p); ub<-rep(0,p);
for(i in 1:p){
  a<-sqrt(qchisq(0.95,q)*sigma[i,i]/n);
  lb[i]<-sp[i]-a; ub[i]<-sp[i]+a
}
ci<-cbind(lb,ub)
ci


# Example 6.10 ------------------------------------------------------------

## data import
n1=271; n2=138; n3=107; n=n1+n2+n3;
x1bar<-c(2.066,0.480,0.082,0.360);
x2bar<-c(2.167,0.596,0.124,0.418);
x3bar<-c(2.273,0.521,0.125,0.383);
S1<-matrix(0,4,4)
S1[lower.tri(S1,diag=TRUE)]<-c(0.291,-0.001,0.002,0.010,0.011,0.000,0.003,0.001,0.000,0.010)
S1<-S1+t(S1)-diag(diag(S1))
S2<-matrix(0,4,4)
S2[lower.tri(S2,diag=TRUE)]<-c(0.561,0.011,0.001,0.037,0.025,0.004,0.007,0.005,0.002,0.019)
S2<-S2+t(S2)-diag(diag(S2))
S3<-matrix(0,4,4)
S3[lower.tri(S3,diag=TRUE)]<-c(0.261,0.030,0.003,0.018,0.017,-0.000,0.006,0.004,0.001,0.013)
S3<-S3+t(S3)-diag(diag(S3))

## Calculate matrices
W<-(n1-1)*S1+(n2-1)*S2+(n3-1)*S3;
xbar<-(n1*x1bar+n2*x2bar+n3*x3bar)/n;
B=n1*(x1bar-xbar)%*%t(x1bar-xbar)+n2*(x2bar-xbar)%*%t(x2bar-xbar)+n3*(x3bar-xbar)%*%t(x3bar-xbar);
W;B;

## Calculate Wilk's lambda
lambda=det(W)/det(B+W);

##test
(n-p-2)*(1-sqrt(lambda))/(p*sqrt(lambda))
qf(0.99,2*p,2*(n-p-2))


# Example 6.11 ------------------------------------------------------------

## calculate CI
g=3;
th1=x1bar-xbar; th2=x2bar-xbar; th3=x3bar-xbar;
dif12<-(th1-th2)[3]; dif13<-(th1-th3)[3]; dif23<-(th2-th3)[3];
a12<-qt(0.05/(p*g*(g-1)),n-g,lower.tail=FALSE)*sqrt((1/n1+1/n2)*W[3,3]/(n-g));
a13<-qt(0.05/(p*g*(g-1)),n-g,lower.tail=FALSE)*sqrt((1/n1+1/n3)*W[3,3]/(n-g));
a23<-qt(0.05/(p*g*(g-1)),n-g,lower.tail=FALSE)*sqrt((1/n2+1/n3)*W[3,3]/(n-g));
lb12<-dif12-a12; ub12<-dif12+a12; ci12<-cbind(lb12,ub12);
lb13<-dif13-a13; ub13<-dif13+a13; ci13<-cbind(lb13,ub13);
lb23<-dif23-a23; ub23<-dif23+a23; ci23<-cbind(lb23,ub23);

