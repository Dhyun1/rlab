library(MASS)
data(birthwt)
colnames(birthwt)
str(birthwt)

normal=birthwt[birthwt[,"low"]==0,"age"]
normal #정상체중 아기의 산모 체중 데이터 추출


case=birthwt[birthwt[,"low"]==1,"age"]
case #저체중 아기의 산모 체중 데이터 추출

t.test(normal,case) 
real_test=t.test(normal,case)$statistic
real_test 

##Permutation test 수행##
getwd()
source("permfunc.R")
set.seed(1) 
tperm=perm.test(normal,case,n.perm=5000)

hist(tperm) #t-statistic의 분포
abline(v=abs(real_test),lty=2,col=2) 

#Empirical p-value
pvalue=mean(abs(tperm)>=abs(real_test)) 
pvalue







normal=birthwt[birthwt[,"low"]==0,"lwt"]
normal #정상체중 아기의 산모 체중 데이터 추출


case=birthwt[birthwt[,"low"]==1,"lwt"]
case #저체중 아기의 산모 체중 데이터 추출

t.test(normal,case) #분산 가정 없으므로 Welch 검정
real_test=t.test(normal,case)$statistic
real_test #threshold로 저장

##Permutation test 수행##
getwd()
source("permfunc.R")
set.seed(1) 
tperm=perm.test(normal,case,n.perm=5000)

hist(tperm) #t-statistic의 분포
abline(v=abs(real_test),lty=2,col=2) 

#Empirical p-value
pvalue=mean(abs(tperm)>=abs(real_test)) 
pvalue

#=0.014, original p-value=0.01308과 유사, alpha=0.05보다 작음
#저체중 여부에 따라 산모의 체중의 모평균은 통계적으로 유의미한 차이를 보인다.