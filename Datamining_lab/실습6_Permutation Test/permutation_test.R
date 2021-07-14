
# reference: https://bioinformaticsandme.tistory.com/10

library(MASS)
data(birthwt)
colnames(birthwt)
str(birthwt)
#low=0(정상체중) 1(저체중) binary 
#lwt는 산모의 체중

# 2. 정상군과 실험군 분류
normal=birthwt[birthwt[,"low"]==0,"lwt"]
normal #정상체중 아기의 산모 체중 데이터 추출
str(normal)

case=birthwt[birthwt[,"low"]==1,"lwt"]
case #저체중 아기의 산모 체중 데이터 추출
str(case)

#저체중, 정상체중 집단 간 산모 체중 모평균 차이 검정
t.test(normal,case) #분산 가정 없으므로 Welch 검정
real_test=t.test(normal,case)$statistic
real_test #threshold로 저장

##Permutation test 수행##
getwd()
source("permfunc.R") #source 파일이 getwd() 내에 있어야 작동함

set.seed(1) #permutation 수행 전에 반드시 set.seed
tperm=perm.test(normal,case,n.perm=1000) # 1000개의 t 통계량 계산

hist(tperm) #t-statistic의 분포
abline(v=abs(real_test),lty=2,col=2) #threshold를 도시
#분포의 극단치에 threshold가 위치, reject H0로 생각

#Empirical p-value
pvalue=mean(abs(tperm)>=abs(real_test)) #histogram에서 우측에 있는 t-statistic의 비율
pvalue
#=0.017, original p-value=0.01308과 유사, alpha=0.05보다 작음
#저체중아 여부에 따라 산모의 체중의 모평균은 통계적으로 유의미한 차이를 보인다.


