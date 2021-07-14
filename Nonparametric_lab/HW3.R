install.packages("zyp")
install.packages("Kendall")
library(zyp)
library(Kendall)
##9.3
x<-c(200,300,400,400,500,600)
y<-c(10000,12000,20000,25000,25000,30000)

print(zyp.sen(y~x))

##9.5
x<-c(0,5000,10000,15000,20000,25000,30000,100000)
y<-c(0.924,0.988,0.992,1.118,1.133,1.145,1.157,1.357)
print(zyp.sen(y~x))
summary(Kendall(x,y))
Kendall(x,y,alternative="greater")
##9.7
x<-c(3.81,2.10,0.79,1.99,1.03,2.07,0.74,3.88,1.43,0.41)
y<-c(1.90,1.03,0.44,1.18,0.62,1.29,0.39,2.30,0.93,0.23)
fit<-print(zyp.sen(y~x))
summary(Kendall(x,y))

##9.8
print(confint.zyp(fit,level=0.928))

##Ex 15.2
x=c(11,13,14,32,55,58,61,67,69,73,73,89,90,93,94,94,95,96,99,99)
B=50000;n=length(x);alpha=0.05
set.seed(1)
tm<-c()
for(i in 1:B){
index=sample(c(1:n),size=n,replace=T)
min=min(rank(index));max=max(rank(index))
trimmed.index=intersect(which(5<=rank(index)),which(rank(index)<=15))
tm<-c(tm,mean(x[trimmed.index]))
}

which(rank(tm)==B*alpha/2)
which(rank(tm)==B*(1-alpha/2))
##Ex 15.7
x<-c(140,90,125,130,95,121,85,97,131,110)
y<-c(138,87,110,132,96,120,86,90,129,100)
n=10000
t0<-mean(x-y)/sqrt((var(x)+var(y))/10)
t<-c();cnt=0
data<-c(x,y)
set.seed(1)
for(i in 1:n){
index=sample(c(1:20),size=20,replace=F)
xx<-data[index[1:10]]
yy<-data[index[11:20]]
tobs<-mean(xx-yy)/sqrt((var(xx)+var(yy))/10)
cnt=cnt+(abs(tobs)>t0)
t<-c(t,tobs)
}
hist(t);cnt/n
