###################################################################################################################################
###################################################################################################################################
##############################                 CHAPTER TWO-LEVEL FACTORIAL DESIGNS                   ##############################
###################################################################################################################################
###################################################################################################################################



# Table 6.1  	#책의 238쪽에 있는 Table.6.1을 만들고자 합니다.
#이를 진행하기 위해 234쪽에 있는 data를 입력합시다.
data <- c(28, 36, 25, 32, 27, 32, 18, 31, 19, 30, 23, 29)

#A와 B의 Low와 High level 별로 data를 지정하기 위해 다음과 같이
#A와 B의 level을 factor로 저장합니다.
A <- c("Alow", "Ahigh")
B <- c("Blow", "Bhigh")

tm1 <- gl(2, 1, 12, A)	#gl(level의 개수, 각 level 별 반복 개수, 전체 길이, 집어넣는 대상)
tm1
tm2 <- gl(2, 6, 12, factor(B))	#factor로 직접 집어넣어도 됩니다.
tm2
##다른 방법
##tm1 <- as.factor(rep(A,6))
##tm2 <- as.factor(rep(B,each=6))

data.frame(data,tm1,tm2)		#이걸 만든겁니다!(책 234쪽의 표를 참고하세요.)

interaction.plot(tm1,tm2,data)  	#시각적으로는, A와 B의 효과에 따라 결과값이
#영향을 받는 것으로 볼 수 있고, interaction은
#없는 것으로 볼 수 있습니다. 그러면 이를
#통계적으로 확인하기 위해 다음 과정을 진행합시다.

lm.ex6 <- lm(data~tm1*tm2)		#앞서의 실습 때 말씀드렸듯이, interaction을 고려한
#fitting입니다. 그에 대한 anova 결과를 봅시다.

anova(lm.ex6) # Table 6.1	#앞에서 interaction plot에서 볼 수 있었던 것처럼
#A와 B의 level에 따른 영향은 매우 강하게 있고, 반면
#interation은 비교적 그 영향력이 미미하다는 것을
#알 수 있습니다.

########## Regression Model(p.238~) ##########
x1 <- ifelse(tm1=="Alow", -1, +1)   #Alow이면 -1로, Ahigh이면 +1로 변경
x2 <- ifelse(tm2=="Blow", -1, +1)

##ifelse(조건문, 조건문이 성립하는 자리에 넣을 값, 그렇지 않은 자리에 넣을 값)

lm.ex6_reg <- lm(data~x1+x2)		#interaction이 없다고 할 수 있었으므로,
#이번에는 interaction term을 고려하지
#않고 fitting합니다.
lm.ex6_reg
#interaction plot에서 볼 수 있듯이, A의 level이 높아짐에 따라
#결과값은 증가하지만 B의 level이 높아짐에 따라 결과값은 감소하므로,
#이에 따른 회귀계수를 확인할 수 있습니다.

########## Response Surface(p.240~) ##########
Conc <- ifelse(tm1=="Alow", 15, 25) 	#책의 240쪽의 밑에서 두번째 줄의 Conc부분은
Catalyst <- ifelse(tm2=="Blow", 1, 2)	#(15,25) -> (-1,1)이 되게 선형변환한 것
#입니다. Catalyst부분 역시, (1,2)->(-1,1)
#이 되게 선형변환한 것입니다.

lm.ex6_nat <- lm(data~Conc+Catalyst)   #natural factor levels
lm.ex6_nat

#이를 통해 residual analysis를 진행합니다.

qqnorm(resid(lm.ex6_nat),datax=T)   #Figure 6.2; 정확히는 몇 개의 점 위치가 조금
qqline(resid(lm.ex6_nat),datax=T)	#다릅니다. 하지만 책에서 구한 결과가 잘못 나온 것
plot(fitted(lm.ex6_nat),resid(lm.ex6_nat))	#으로 보입니다. 실제로 확인해보면
abline(h=0)					#figure 6.2(b)에서 가장 오른쪽의 점이 2개밖에
#생기지 않음을 알 수 있습니다. 다음을 확인해보세요.
cbind(fitted(lm.ex6_nat),resid(lm.ex6_nat))	#여기에서 4행과 6행이 동일합니다. -> 점이 겹침을 확인 할 수 있습니다.


Conc_seq <- seq(15,25,by=1)
Catalyst_seq <- seq(1,2,by=0.1)

newdat <- expand.grid(Conc= Conc_seq, Catalyst=Catalyst_seq)

##expand.grid(a,b) : a와 b를 합치는데, b의 각 level당 모든 a를 나열하는 식으로 합니다.
##newdat의 결과를 확인하면 알 수 있을겁니다!
newdat
# expand.grid() function creates a data frame from all combinations of the supplied vectors or factors.

newdat$pd <- predict(lm.ex6_nat, newdata=newdat)

##predict(lm 결과(회귀계수), newdata=새로운 X(design matrix)) : X를 lm 결과에
##fitting하여 prediction을 얻습니다.

pd <- matrix(newdat$pd, 11, 11)	#각 Catalyst의 level당 Conc의 level들에 따른
#prediction 결과를 배치하여 matrix로 저장합니다.
#시각적으로 표현하자면 이런 table을 만든겁니다. (참고로 matrix() option에서 byrow=T로 하면 pd의 vector 입력값이 row별로 들어가므로 생성한 dlatl low, column name을 바꿔주면 됩니다. )
dlatl <- data.frame(pd,row.names=Conc_seq)
names(dlatl) <- Catalyst_seq
dlatl

#이렇게 정리한 prediction 결과를 이용하여 response surface를 얻을 수 있습니다.
#theta, phiangles defining the viewing direction. theta gives the azimuthal direction and phi the colatitude.
persp(Conc_seq, Catalyst_seq, pd, theta=300, phi=20, ticktype="detailed")      
#Figure 6.3(a)

#install.packages("GA")  
library(GA)
persp3D(Conc_seq, Catalyst_seq, pd, theta=300, phi=20, ticktype="detailed")   ## 더 이쁘지 않나요...?
#Figure 6.3(a)

contour(Conc_seq, Catalyst_seq, pd)    #Figure 6.3(b)

### Figure 6.3(a) 다른방법 ###
# plot3Drgl 패키지 설치
#install.packages("plot3Drgl")
library(rgl)
with(plot3d(Conc,Catalyst,data))
with(newdat, surface3d(unique(Conc),unique(Catalyst),pd,alpha=0.3,front="line"))
# 그림에 대고 클릭 후 드래그 하면 그림이 회전합니다(신기신기)
setwd("/Volumes/USB/2017-1/실험계획법 및 실습/실습/실습3(최종)")
# Example 6.2
ex6.2 <-read.csv("table6_10.csv",header=T)
ex6.2$fa <- as.factor(ex6.2$fa)
ex6.2$fb <- as.factor(ex6.2$fb)
ex6.2$fc <- as.factor(ex6.2$fc)
ex6.2$fd <- as.factor(ex6.2$fd)
attach(ex6.2)
lmex6.2 <- lm(trt~fa*fb*fc*fd)
anova(lmex6.2)  # Table 6.12

lmex6.2  # Table 6.12의 estimation과 다릅니다. 그 이유는 lm에 내장된 constraint가
#저희가 사용하는 constraint와 다르기 때문입니다. 정확하게 구하는 방법은 다음처럼,




detach(ex6.2)
ex6.2 <-read.csv("table6_10.csv",header=T)
attach(ex6.2)
A <- diff(by(trt,fa,mean))
B <- diff(by(trt,fb,mean))
C <- diff(by(trt,fc,mean))
D <- diff(by(trt,fd,mean))
AB <- diff(by(trt,fa*fb,mean))	#이렇게 하나씩 구할 수도 있구요..
#코딩을 이용하여 좀 더 세련되게 구할 수도 있습니다.
#어느 방법을 사용할지는 여러분 취향에 맞게..!
binary <- function(n){
  if(n==0 | n==1){ return(n)
  }else return(c(n%%2,binary(floor(n/2))))
}

res <- vector()
for(i in 1:15){
  if(ncol((ex6.2[,-1])[,which(binary(i)==1),drop=F])==1){ ## drop=F means it will reduce its result to a vector if there is only one column.
    res[i] <- diff(by(trt,(ex6.2[,-1])[,which(binary(i)==1)],mean))
  } else {
    res[i] <- diff(by(trt,apply((ex6.2[,-1])[,which(binary(i)==1)],1,prod),mean))
  }
}
res
SS <- anova(lmex6.2)$Sum[-16]		#error가 뜨지만, MSE를 구할 수 없기 때문에 생긴
#error이므로 그냥 진행하셔도 됩니다. 참고로
#여기서 그러한 문제가 생긴 이유는 replication이 없었기 때문입니다!
cbind(Estimate=res,SS,Percent=SS/sum(SS)*100)		#Table.6.12 를 구현했습니다.


## or you can use 
effects(lmex6.2, set.sign=TRUE)[2:16]/2
# Example 6.1; 책 245쪽
#이 예시에서는 data를 불러들이지 않고 직접 입력합니다! level을 지정하는 것은 앞에서 했던 것과
#똑같이 A,B,C 각각 high와 low로 지정하기 위해 다음과 같이 합니다!
#이 예시는 이전의 예시와 다르게 replication이 존재합니다.
data1 <- c(550, 669, 604, 650, 633, 642, 601, 635, 1037, 749, 1052, 868, 1075, 729, 1063, 860)
A <- c("Alow", "Ahigh")
B <- c("Blow", "Bhigh")
C <- c("Clow", "Chigh")

tm11 <- gl(2, 1, 16, factor(A)) # n=2 observations per treatment
tm11
tm12 <- gl(2, 4, 16, factor(B))
tm12
tm13 <- gl(2, 8, 16, factor(C))
tm13
lm.ex1 <- lm(data1~tm11*tm12*tm13)		#interaction들을 모두 고려하여 fitting합니다.
anova(lm.ex1) # Table 6.6

### Regression Model and Response Surface(p.247)###
x11 <- ifelse(tm11=="Alow", -1, +1) #Alow이면 -1로, Ahigh이면 +1로 변경
x12 <- ifelse(tm12=="Blow", -1, +1)
x13 <- ifelse(tm13=="Clow", -1, +1)

lm.ex1_reg <- lm(data1~x11*x13) # reduced model(두번째(tm12) factor는 영향을 미치지
#않는다고 볼 수 있다고, 앞의 anova 결과가 알려줍니다!)
lm.ex1_reg # reduced model 계수

Gap <- ifelse(tm11=="Alow", 0.8,1.2) 	#책 245쪽의 table.6.4를 참고합시다.
Power <- ifelse(tm13=="Clow", 275, 325)

lm.ex1_nat <- lm(data1~Gap*Power) #natural factor levels
lm.ex1_nat		#책의 250쪽 아래에서 두 번째 뭉치(?)의 값을 얻을 수 있습니다.

Gap_seq <- seq(0.8,1.2,by=0.05)	#책의 248쪽에 있는 response surface를 그리기
Power_seq <- seq(275,325,by=5)	#위한 작업입니다.
newdat1 <- expand.grid(Gap=Gap_seq, Power=Power_seq)
newdat1$pd <- predict(lm.ex1_nat,newdata=newdat1)
pd1 <- matrix(newdat1$pd, 9, 11)

persp(Gap_seq, Power_seq, pd1, theta=45, phi=20, ticktype="detailed") #Figure 6.7(a)
### 앞에서 했던 예쁜 그림들 역시 그릴 수 있겠죠??
contour(Gap_seq, Power_seq, pd1) #Figure 6.7(b)

summary(lm.ex1) #full moedel의 R-squared
anova(lm.ex1_reg) # Table 6.7 (reduced)
summary(lm.ex1_reg) #reduced moedel의 R-squared
#R-squared 값이 0.9661에서 0.9608로 감소했지만 큰 변화가 없습니다. 따라서
#tm12, 즉 B는 제외시켜도 큰 문제가 없는 factor임을 알 수 있습니다.


# Example 6.2

# A, tm 등 변수를 앞과 조금 다르게 설정했습니다

data2 <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)
Af <- c("", "A")
Bf <- c("", "B")
Cf <- c("", "C")
Df <- c("", "D")

A <- gl(2, 1, 16, factor(Af))
B <- gl(2, 2, 16, factor(Bf))
C <- gl(2, 4, 16, factor(Cf))
D <- gl(2, 8, 16, factor(Df))

lm.ex2 <- lm(data2~A*B*C*D)
anova(lm.ex2) 

# Figure 6.12
# tapply() 는 그룹별 처리를 위한 apply 함수로서 tapply(데이터,색인,함수)의 형태로 호출됩니다. 여기서 "색인"은 데이터가 어느 그룹에 속하는지를 표한하기 위한 factor형 데이터 입니다.
ex.tapply(1:10,1:10%%2==0,sum)
plot(tapply(data2,A,mean),type='l', ylim=c(50,90), ylab="Average filtration rate", xlab="A")
plot(tapply(data2,C,mean),type='l', ylim=c(50,90), ylab="Average filtration rate", xlab="C")
plot(tapply(data2,D,mean),type='l', ylim=c(50,90), ylab="Average filtration rate", xlab="D")
interaction.plot(A,C,data2,ylim=c(40,100))
interaction.plot(A,D,data2,ylim=c(40,100))	#책이랑 조금 다릅니다..!(Y LAB과 비교해 보세요 )그런데 다음처럼
#확인해보면 A와 D가 모두 "-"일 때 관측치의 평균이
#60.25로 나오는 것으로부터, 책의 그래프가 잘못된
#것이라고 볼 수 있을 것 같습니다.
mean(data2[(A=="" & D=="")])
mean(data2[(A=="" & D=="D")])


ef <- effects(lm.ex2, set.sign=TRUE)[2:15]    # effect 계산(intercept 제외하고)
anova(lm.ex2)
cbind(ef/2,anova(lm.ex2)$"Sum Sq") # Table 6.12 여기서 D 의 경우 책에서는 effect가 14.625로 나타났으나 R 결과에서는 -14.625로 나타남 

# D의 효과 
(sum(data2[9:16])-sum(data2[1:8]))/8
## R effects 함수가 조금 이상하네요 ㅠㅠ....이유는...??????

np <- qqnorm(ef, datax=TRUE)
qqline(ef, datax=TRUE)				#Figure.6.11
identify(np$x, np$y, names(ef)) ## 궁금한 점을 마우스로 찍은 후 Finish를 꾹 눌러줍시다. 참고로 ef D의 부호가 반대임을 앞에서 확인하실 수 있습니다. 



# Table 6.11 Contrast Constants matrix 생성
n=4
Cont <- matrix(1, 2^n, 2^n-1) 
for(i in 1:2^n){
  Cont[i,1] <- ifelse(i%%2==0, 1, -1) # A열에 -1, +1 번갈아가며 입력
}
for(i in 1:(n-1)){
  for(k in 1:16){
    Cont[k, 2^i] <- (-1)^((k-1)%/%(2^i)+1)
  } ###  B , C , D 열 생성 
  for(j in (2^i+1):(2^(i+1)-1)){
    for(k in 1:16){
      Cont[k,j] <- Cont[k, j-2^i]*Cont[k, 2^i]
    }
  }
}
Cont

# Term 이름과 그 effect로 이루어진 행렬 생성
eff <- matrix(" ",15, 2)
colnames(eff) <- c("Term", "effect")
for(i in 1:15){
  eff[i,1] <- paste(A[i+1], B[i+1], C[i+1], D[i+1])
  ef <- 0
  for(j in 1:16){
    ef <- ef+data2[j]*Cont[j,i]
  }
  eff[i,2] <- ef/8
}
eff

#Term 이름과 effect로 이루어진 수열 생성
effec <- as.numeric(c(eff)[16:30]) ## == as.numeric(eff[,2])
names(effec) <- eff[1:15]

np <- qqnorm(effec, datax=TRUE) #Figure 6.11
qqline(effec, datax=TRUE)
identify(np$x, np$y, names(effec))
#그래프에서 label을 알고 싶은 점들을 클릭 후 오른쪽 위 Finish를 누르면 label 표시


x21 <- ifelse(A=="A", 1, -1) #Alow이면 -1로, Ahigh이면 +1로 변경
x22 <- ifelse(B=="B", 1, -1)
x23 <- ifelse(C=="C", 1, -1)
x24 <- ifelse(D=="D", 1, -1)

lm.ex2_reg <- lm(data2~x21+x23+x24+x21*x23+x21*x24) # reduced model(p.261)
lm.ex2_reg # reduced model 계수

qqnorm(resid(lm.ex2_reg), datax=TRUE) #Figure 6.13
qqline(resid(lm.ex2_reg), datax=TRUE)

A_seq <- seq(-1,1,by=0.5)
C_seq <- seq(-1,1,by=0.5)
D_seq <- seq(-1,1,by=0.5)

newdat2 <- expand.grid(x21= A_seq, x23=C_seq, x24=1)	#p.262
newdat2$pd <- predict(lm.ex2_reg, newdata=newdat2)
pd2 <- matrix(newdat2$pd, 5, 5)
contour(A_seq, C_seq, pd2) #Figure 6.14(a)

newdat3 <- expand.grid(x21= 1, x23=C_seq, x24=D_seq)
newdat3$pd <- predict(lm.ex2_reg, newdata=newdat3)
pd3 <- matrix(newdat3$pd, 5, 5)
contour(C_seq, D_seq, pd3) #Figure 6.14(b)

###################################################################################################################################
###################################################################################################################################
##########################       CHAPTER 7 BLOCKING AND CONFOUNDING SYSTEMS FOR TWO-LEVEL FACTORIALS       ########################
###################################################################################################################################
###################################################################################################################################
# Example 7.1 

data1 <- c(28, 36, 25, 32, 27, 32, 18, 31, 19, 30, 23, 29)
Af <- c("", "A")
Bf <- c("", "B")
Blockf <- c("Block1", "Block2", "Block3")

A <- gl(2, 1, 12, factor(Af))
B <- gl(2, 6, 12, factor(Bf))
Block <- gl(3, 2, 12, factor(Blockf))

data.frame(data1,A,B,Block)		#이런 data를 만든겁니다!(Table.7.1)

lm.ex1 <- lm(data1~Block+A*B)
anova(lm.ex1) # Table 7.2

#Table 7.3은 아래의 Table 7.4 과정에서 n=2로만 바꾸어주면 됨

# Table 7.4 생성 (마지막 열은 무시)
n=3
Cont <- matrix(1, 2^n, 2^n) 
for(i in 1:2^n){
  Cont[i,1] <- ifelse(i%%2==0, 1, -1) # A열에 -1, +1 번갈아가며 입력
}
for(i in 1:(n-1)){
  for(k in 1:2^n){
    Cont[k, 2^i] <- (-1)^((k-1)%/%(2^i)+1)
  }
  for(j in (2^i+1):(2^(i+1)-1)){
    for(k in 1:2^n){
      Cont[k,j] <- Cont[k, j-2^i]*Cont[k, 2^i]
    }
  }
}
Cont

# 마지막열에 Block 번호 표시
for(i in 1:2^n){
  Cont[i, 2^n] <- ifelse(Cont[i,2^n-1]==1, 2, 1)
}
Cont

colnames(Cont) <- c("A", "B", "AB", "C", "AC", "BC", "ABC", "Block")
rownames(Cont) <- c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc")
Cont

# Example 7.2 

data2 <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)
Af <- c("", "A")
Bf <- c("", "B")
Cf <- c("", "C")
Df <- c("", "D")

A <- gl(2, 1, 16, factor(Af))
B <- gl(2, 2, 16, factor(Bf))
C <- gl(2, 4, 16, factor(Cf))
D <- gl(2, 8, 16, factor(Df))

n=4
Cont <- matrix(1, 2^n, 2^n) 
for(i in 1:2^n){
  Cont[i,1] <- ifelse(i%%2==0, 1, -1) # A열에 -1, +1 번갈아가며 입력
}
for(i in 1:(n-1)){
  for(k in 1:2^n){
    Cont[k, 2^i] <- (-1)^((k-1)%/%(2^i)+1)
  }
  for(j in (2^i+1):(2^(i+1)-1)){
    for(k in 1:2^n){
      Cont[k,j] <- Cont[k, j-2^i]*Cont[k, 2^i]
    }
  }
}
Cont
# ABCD가 confound되므로 ABCD열의 부호에 따라 마지막열에 Block 번호 표시
for(i in 1:2^n){
  Cont[i, 2^n] <- ifelse(Cont[i,2^n-1]==1, 1, 2)
}
Cont

#Block1에 -20
for(i in 1:2^n){
  data2[i] <- ifelse(Cont[i,2^n]==1, data2[i]-20, data2[i])
}

Block <- as.factor(Cont[1:2^n, 2^n])
lm.ex2 <- lm(data2~A*B*C*D+Block-A:B:C:D) 
anova(lm.ex2) # Table 7.6 _ SS

# Term 이름과 그 effect로 이루어진 행렬 생성
eff <- matrix(" ",15, 2)
colnames(eff) <- c("Term", "effect")
for(i in 1:15){
  eff[i,1] <- paste(A[i+1], B[i+1], C[i+1], D[i+1])
  ef <- 0
  for(j in 1:16){
    ef <- ef+data2[j]*Cont[j,i]
  }
  eff[i,2] <- ef/8
}
eff

#Term 이름과 effect로 이루어진 수열 생성
effec <- as.numeric(c(eff)[16:30])
names(effec) <- eff[1:15]
effec # Table 7.6 _ effect est
effec/2  # Table 7.6 _ regression coeff

lm.ex2_2 <- lm(data2~A*C*D+Block-A:C:D-C:D)
anova(lm.ex2_2) # Table 7.7




#######################################################
###################### Chapter 8 ######################
#######################################################

n=3
Cont <- matrix(1, 2^n, 2^n-1) 
for(i in 1:2^n){
  Cont[i,1] <- ifelse(i%%2==0, 1, -1) # A열에 -1, +1 번갈아가며 입력
}
for(i in 1:(n-1)){
  for(k in 1:2^n){
    Cont[k, 2^i] <- (-1)^((k-1)%/%(2^i)+1)
  }
  for(j in (2^i+1):(2^(i+1)-1)){
    for(k in 1:2^n){
      Cont[k,j] <- Cont[k, j-2^i]*Cont[k, 2^i]
    }
  }
}
Cont

colnames(Cont) <- c("A", "B", "AB", "C", "AC", "BC", "ABC")
rownames(Cont) <- c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc")
Cont

plma <- as.data.frame(Cont)  # 행렬을 데이터 프레임으로 변환 
plma <- plma[order(-plma$ABC),] # ABC기준으로 정렬  --> Table 8.1
plma

###################### Example 8.1 ######################

data <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)

n=4
Cont <- matrix(1, 2^n, 2^n) 
for(i in 1:2^n){
  Cont[i,1] <- ifelse(i%%2==0, 1, -1) # A열에 -1, +1 번갈아가며 입력
  Cont[i,2^n] <- data[i]
}
for(i in 1:(n-1)){
  for(k in 1:2^n){
    Cont[k, 2^i] <- (-1)^((k-1)%/%(2^i)+1)
  }
  for(j in (2^i+1):(2^(i+1)-1)){
    for(k in 1:2^n){
      Cont[k,j] <- Cont[k, j-2^i]*Cont[k, 2^i]
    }
  }
}
colnames(Cont) <- c("A", "B", "AB", "C", "AC", "BC", "ABC", "D", "AD", "BD", "ABD", "CD", "ACD", "BCD", "ABCD", "FR")
rownames(Cont) <- c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc", "d", "ad", "bd", "abd", "cd", "acd", "bcd", "abcd")
Cont

plma1 <- as.data.frame(Cont) # 행렬을 데이터 프레임으로 변환
plma1 <- subset(plma1, subset=(ABCD==1)) # ABCD=1인 행만 선택
plma1 <- as.matrix(plma1) # 데이터 프레임을 다시 행렬로 변환
plma1
plma1 <- plma1[,-15] #ABCD 제거
plma1

eff <- matrix(" ",2^n-2, 2)
colnames(eff) <- c("Term", "effect")
for(i in 1:(2^n-2)){
  ef <- 0
  for(j in 1:2^(n-1)){
    ef <- ef+plma1[j,15]*plma1[j,i]
  }
  eff[i,2] <- ef/2^(n-2)
  eff[i,1] <- colnames(plma1)[i]
}
eff #Table 8.4


###################### Example 8.2 ###################### 


data2 <- c(8, 9, 34, 52, 16, 22, 45, 60, 6, 10, 30, 50, 15, 21, 44, 63)

n=5
Cont2 <- matrix(1, 2^n, 2^n) 
for(i in 1:2^n){
  Cont2[i,1] <- ifelse(i%%2==0, 1, -1) # A열에 -1, +1 번갈아가며 입력
}
for(i in 1:(n-1)){
  for(k in 1:2^n){
    Cont2[k, 2^i] <- (-1)^((k-1)%/%(2^i)+1)
  }
  for(j in (2^i+1):(2^(i+1)-1)){
    for(k in 1:2^n){
      Cont2[k,j] <- Cont2[k, j-2^i]*Cont2[k, 2^i]
    }
  }
}
colnames(Cont2) <- c("A", "B", "AB", "C", "AC", "BC", "ABC", "D", "AD", "BD", "ABD", "CD", "ACD", "BCD", "ABCD",
                     "E", "AE", "BE", "ABE", "CE", "ACE", "BCE", "ABCE", "DE", "ADE", "BDE", "ABDE", "CDE", "ACDE", "BCDE", "ABCDE","Yield")
rownames(Cont2) <- c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc", "d", "ad", "bd", "abd", "cd", "acd", "bcd", "abcd",
                     "e", "ae", "be", "abe", "ce", "ace", "bce", "abce", "de", "ade", "bde", "abde", "cde", "acde", "bcde", "abcde")
Cont2

plma2 <- as.data.frame(Cont2) # 행렬을 데이터 프레임으로 변환
plma2 <- subset(plma2, subset=(ABCDE==1)) # ABCDE=1인 행만 선택
plma2 <- plma2[order(plma2$A),] 
plma2 <- plma2[order(plma2$B),] 
plma2 <- plma2[order(plma2$C),] 
plma2 <- plma2[order(plma2$D),] #Table 8.5의 Treatment Combination 순서와 같게 정렬
plma2 <- as.matrix(plma2) # 데이터 프레임을 다시 행렬로 변환
plma2 <- plma2[,-31] #ABCDE열 제거
plma2
for(i in 1:2^(n-1)){
  plma2[i,2^n-1] <- data2[i]
}
plma2
eff2 <- matrix(" ",2^(n-1)-1, 2)
colnames(eff2) <- c("Term", "effect")
k=1
for(i in 1:(2^n-2)){
  if(nchar(colnames(plma2)[i])<3){
    ef <- 0
    for(j in 1:2^(n-1)){
      ef <- ef+plma2[j,2^n-1]*plma2[j,i]
    }
    eff2[k,2] <- ef/2^(n-2)
    eff2[k,1] <- colnames(plma2)[i]
    k <- k+1
  }
}
eff2  #Table 8.6 _ Effect


effec <- as.numeric(eff2[,2])
names(effec) <- eff2[,1]
effec
np <- qqnorm(effec, datax=TRUE) #Figure 8.6
qqline(effec, datax=TRUE)
identify(np$x, np$y, names(effec))





Af <- c("", "A")
Bf <- c("", "B")
Cf <- c("", "C")
Df <- c("", "D")

A <- gl(2, 1, 16, factor(Af))
B <- gl(2, 2, 16, factor(Bf))
C <- gl(2, 4, 16, factor(Cf))
D <- gl(2, 8, 16, factor(Df))

lm.table.8.7 <- lm(data2~A*B+C) 
anova(lm.table.8.7) # Table 8.7


#그래프에서 label을 알고 싶은 점들을 클릭 후 오른쪽 위 Finish를 누르면 label 표시



###################### Example 8.3 ######################


plma3 <- as.data.frame(Cont) # 행렬을 데이터 프레임으로 변환
plma3 <- subset(plma3, subset=(ABCD==-1)) # ABCD=-1인 행만 선택
plma3 <- as.matrix(plma3) # 데이터 프레임을 다시 행렬로 변환
plma3 <- plma3[,-15] #ABCD 제거
plma3
n <- 4
eff3 <- matrix(" ",2^n-1, 2)
colnames(eff3) <- c("Term", "effect")
for(i in 1:(2^n-1)){
  ef <- 0
  for(j in 1:2^(n-1)){
    ef <- ef+plma3[j,15]*plma3[j,i]
  }
  eff3[i,2] <- ef/2^(n-2)
  eff3[i,1] <- colnames(plma3)[i]
}
eff3 <- eff3[-15,]

effect <- (as.numeric(eff[,2])+as.numeric(eff3[,2]))/2
names(effect) <- eff[,1]
effect   #332쪽의 오른쪽 아래 Table 결과가 나왔습니다.


###수고하셨습니다!