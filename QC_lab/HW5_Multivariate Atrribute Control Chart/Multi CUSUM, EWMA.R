install.packages("MSQC")
library(MSQC)
data("carbon1")
data("carbon2")

carbon1
class(carbon1)
dim(carbon1) #30x3, 8 sample

m1<-dim(carbon1)[1] # 30 subsample size
m2<-dim(carbon2)[1] # 25 subsample size
p<-dim(carbon1)[2]  # 3 variable
n<-dim(carbon1)[3]  # 8 samples

mu0<-apply(carbon1, 2, mean) #mean by each variable for all list
Sig0<-Reduce("+",lapply(1:m1,function(i){
  cov(t(carbon1[i,,])) # S_i, DO TRANSPOSE WHEN SELECTING A LIST
}))/m1 #sigma_hat=(sum S_i)/(subsample size)
iSig0<-solve(Sig0) #to reduce time in inverse 
Xbar<-apply(carbon2, c(1,2), mean)
# 2=mean by variable(column), 1=throughout all list elements

k<-0.5
h<-5.5
MC_vec<-NULL
MCi<-0
ni<-1

for(i in 1:m2){
  if(MCi>0){
    ni<-ni+1
  }else{
    ni<-1
  }
  if(ni==1){
    Ci<-Xbar[i,]-mu0 #if ni==1, apply() in below gives error.
  }else{
    Ci<-apply(Xbar[(i-ni+1):i,],2,sum)-ni*mu0
  }
  MCi<-max(0,sqrt(n*t(Ci)%*%iSig0%*%Ci)-k*ni)
  MC_vec<-c(MC_vec,MCi) #add results
}

libray(ggplot2)
df_MCUSUM<-data.frame(MC=MC_vec,index=1:m2)
ggplot(df_MCUSUM,aes(x=index,y=MC))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=h)+ #UCL
  coord_cartesian(ylim=c(0,6))+
  theme_classic()

####Multi-EWMA####
lambda<-0.1
Zi<-mu0
T2_vec<-NULL
for(i in 1:m2){
  Zi<-lambda*Xbar[i,]+(1-lambda)*Zi
  Ti2<-(n*(2-lambda))/(lambda*(1-(1-lambda)^(2*i)))*
    t(Zi-mu0)%*%iSig0%*%(Zi-mu0)
  T2_vec<-c(T2_vec,Ti2)
}
alpha<-0.05
UCL<-qchisq(1-alpha,p)

df_MEWMA<-data.frame(T2=T2_vec,index=1:m2)
ggplot(df_MEWMA,aes(x=index,y=T2))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=UCL)+
  coord_cartesian(ylim=c(0,10))+
  theme_classic()
