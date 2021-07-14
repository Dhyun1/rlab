# Example 5.1 (p. 192)
ex5.1 <-read.csv("ex_5_1.csv",header=T)	#파일을 불러옵니다.(자료는 책의 예.5.1)
head(ex5.1)		#어떻게 구성되어 있는지 확인해봅시다.
names(ex5.1)
life			#이렇게 실행하면 오류가 납니다. 지난 시간에도 말씀드렸다시피, ex5.1의
			#이름들을 불러오려면 '$'표시를 이용한 형태인, ex5.1$life 와 같이
ex5.1$life		#불러와야 합니다.
attach(ex5.1)	#하지만 매번 그렇게 하기에는 불편함이 따르기 때문에, attach 함수를 이용
life			#하여 ex5.1의 이름을 쉽게 불러오게 합시다.


#우리는 이 자료에서, life가 temp와 type에 대해 어떤 관계가 있는지를 확인해보고자 합니다.
#자료는, 각 level별로 4번의 반복을 통하여 얻은, factorial design으로 확인할 수 있습니다.


# function factor 를 이용하여 label 생성하기
#각 수준들을 factor로써 저장하도록 합시다. 물론, as.factor를 이용하여 저장할 수도 있습니다!
typef <- factor(type, labels=c("1","2","3"))
tempf <- factor(temp, labels=c("15","70","125"))
typef
tempf

lmex5.1 <- lm(life~typef*tempf)		#회귀모형으로 적합시킵니다.
			#여기서 주안점으로 보아야 할 것은, 이전과는 달리 '+'가 아닌
			#life~typef*tempf
			#의 형태로 코딩하였다는 것입니다. 이렇게 함으로써 전의 실습 시간에
			#다루었던 것에 더하여, 두 변수의 interaction의 영향까지도 확인할
			#수 있게 됩니다. 이를 코딩하는 방법은 여러가지가 있습니다.
anova(lm(life~typef+tempf+typef*tempf))
anova(lm(life~(typef+tempf)^2))
anova(lm(life~typef+tempf+typef:tempf))
anova(lm(life~typef:tempf))
anova(lmex5.1)  # Table 5.5를 얻을 수 있습니다.


interaction.plot(tempf,typef,life)  # Figure 5.9(interaction plot을 그려봅시다.)
qqnorm(resid(lmex5.1),datax=TRUE) # Figure 5.11(정규성 가정을 확인하기 위해!)
qqline(resid(lmex5.1),datax=TRUE) #정규성 가정에 큰 무리는 없어보입니다.
plot(fitted(lmex5.1),resid(lmex5.1))  # Figure 5.10, 5.12
abline(h=0)
#위의 fitted value와 residual을 plotting하는 이유는, 우리의 모형에 대한 가정이 맞다면
#fitted value와 residual은 독립적이어야 하기 때문에, 정말 독립인가를 확인하기 위함입니다.

plot(type,resid(lmex5.1)) # Figure 5.13
abline(h=0)
plot(typef,resid(lmex5.1))
plot(temp,resid(lmex5.1)) # Figure 5.14
abline(h=0)
plot(tempf,resid(lmex5.1))
#위에서 fitted value에 대한 서술과 마찬가지로, residual이 어느 level을 따라서도
#어떠한 경향성이나 의존성이 보이지 않는지를 확인하기 위해 이와 같은 plotting을 합니다.

detach(ex5.1)


# Data file을 읽어 들이는 대신 직접 입력하기 
t1 = c(130, 155, 74, 180, 34, 40, 80, 75, 20, 70, 82, 58)
t2 = c(150, 188, 159, 126, 136, 122, 106, 115, 25, 70, 58, 45)
t3 = c(138, 110, 168, 160, 174, 120, 150, 139, 96, 104, 82, 60)
life = c(t1, t2, t3)
life			#직접 life를 만들었습니다! 이와 같이, data를 외부 파일에서 불러오지
			#않고 직접 만들어 활용할 수도 있습니다. 다만, 외부 파일에서 불러오는 이유는
			#그렇게 하는 편이 data을 만드는 데 훨씬 용이하기 때문입니다.
			#또는 이미 저장된 data를 손으로 일일이 기입한다면 그 노동력과 시간 낭비가
			#상당할 것입니다..!


# function gl을 이용하여 label 생성하기(ex.5.1에서와 같은 것을 직접 입력하는 중입니다!)
?gl		#언제나, 잘 모르는 함수가 있다면 ?gl 같이 help를 요청하거나 구글링을 합니다!
#gl(n=level의 개수, k=반복하는 개수, length=n*k(전체 길이), labels=level들의 이름)
#여기서 length를 꼭 n*k로 하지 않아도 됩니다. R에서 사용되는 '재사용 원칙'이 적용되어,
#length로 정해준 길이만큼 level의 값들이 반복되어 추가됩니다.

typef <- gl(3,12,36, labels=c("1","2","3"))	#type를 정해줍니다.
tempf <- gl(3,4,36, labels=c("15","70","125"))	#temp를 정해줍니다.
gl(3,3,19,labels=c("a","b","c"))	#19는 3*3=9의 배수가 아니기 때문에, 마지막에
				#부자연스럽게 보이지만, a라는 level 값 하나가 19번째를 채우기
				#위해 뿅 하고 나타나있는 것을 알 수 있습니다!

#이렇게 만들어진 typef와 tempf별 life의 값이 어떠한지를 boxplot을 그려 확인해봅시다.
boxplot(life~typef, ylab="life in hours", main="material type")
boxplot(life~tempf, ylab="life in hours", main="temperature")
#대충, typef가 증가함에 따라서는 life가 대체적으로 증가하는 것 같고, tempf가 증가함에 따라서는
#life가 감소하는 것으로 보입니다!


#직접 입력한 자료를 이용하여 anova test를 하는 과정입니다.
anova5.1 <- aov(life ~ typef*tempf)
summary(anova5.1)

#참고로, 지금의 경우, anova(lm())으로 한 결과와 똑같습니다!
anova(lm(life~typef*tempf))


#life가 temp에 따라 어떻게 변하는지를 그래프로 그리는 방법은 다음의 두 가지가 있습니다.
#결과가 똑같죠!
plot(life~tempf, ylab="life in hours", main="temperature")
plot(tempf,life, ylab="life in hours", main="temperature")

interaction.plot(tempf, typef, life)

lmex5.1 <- lm(life ~ tempf*typef)
anova(lmex5.1)


# Example 5.3 (p. 208)
#이번엔 factor가 3개인 경우에 대한 예시입니다.
#bottle을 만드는데, operating pressure와 carbonation 비율, line 속도에 따라
#fill height의 편차가 어떻게 되는지, 각 level별로 그 차이가 있는지에 대한 실험입니다.
ex5.3 <-read.csv("ex_5_3.csv",header=T)
head(ex5.3)
attach(ex5.3)
Af <- factor(carbon,labels=c("10","12","14"))
Bf <- factor(press, labels=c("25","30"))
Cf <- factor(speed, labels=c("200","250"))
Af
Bf
Bf

#### as.factor 쓰는 방법 #####
Ac<-as.character(carbon)
Af<-as.factor(carbon)
Bc<-as.character(press)
Bf<-as.factor(press)
Cc<-as.character(speed)
Cf<-as.factor(speed)

lmex5.3 <- lm(dev~Af*Bf*Cf)
anova(lmex5.3) #Table 5.14

#tapply(변수A, 변수B, 변수B별로 변수A에 적용하고 싶은 함수)
tapply(dev,Af,mean)		#Af level별 dev의 mean을 얻어내는 함수입니다.
tapply(dev,Af,sd)			#Af level별 dev의 sd를 얻어내는 함수입니다.

#이렇게 각 level별로의 평균을 plotting하여, 각 factor의 level별 dev의 변화 양상을
#확인해봅시다.
plot(tapply(dev,Af,mean),type='l', ylab="Average fill deviation", 
	xlab="carbonation") #Figure 5.16
plot(tapply(dev,Bf,mean),type='l', ylab="Average fill deviation", 
	xlab="pressure",ylim=c(-2,8))
plot(tapply(dev,Cf,mean),type='l', ylab="Average fill deviation",
	xlab="speed", ylim=c(-2,6))
interaction.plot(Af,Bf,dev)


##### Figure 5.16 다른 방법 #####
plot.design(dev~Af+Bf+Cf)		#Af, Bf, Cf의 level 별 dev의 mean을
				#한꺼번에 그려주는 코드입니다. 한 눈에 확인할 수 있죠!
				#물론, interaction을 확인하기는 무리입니다.

#특별히, press=25, speed=250인 경우에, carbon의 level에 따른 dev의 mean의 양상을
#확인해봅시다. 이는 one factor at a time을 확인하는 것으로 볼 수 있습니다.
hslp <- subset(ex5.3, press==25&speed==250, select=c(dev, carbon, 
	press, speed)) 
plot(tapply(hslp$dev, factor(hslp$carbon), mean), type='l',
	ylab="Average fil deviation at high speed and low pressure", 	xlab="carbonation") #Figure 5.17

detach(ex5.3)

# Example 5.4 (p. 211)(자료는 ex5.1과 같습니다.)
#이 예시에서는, Ex.5.1에서의 자료를 그대로 사용하면서, 이번엔 temperature value의
#이차항까지 고려하여 ANOVA를 진행합니다.


#(수정):이 다음 코드에서, 지난 실습 때 as.factor()를 이용하여 A와 A2를 저장해야 한다고
#하였습니다. 그런데 이 실험과정에서 요구한 것이, type의 level에 따라, life를 temp에 대해
#이차곡선으로 적합하고자 하는 것이었기 때문에 A와 A2는 factor로써의 질적 변수가 아닌
#그 값 자체로의 양적 변수로 대입하여야 하므로, as.factor()를 사용하지 않아야 합니다.
#즉, numeric vector로써 그대로 대입하여 결과를 얻어야 하며, 그렇게 얻은 결과는
#Table.5.15와 같습니다.


ex5.4 <-read.csv("ex_5_1.csv",header=T)
head(ex5.4)
attach(ex5.4)
A <- temp
A2 <- temp*temp		#원소별 곱을 이용하여, temp의 원소들을 각각 제곱합니다.
B <- factor(type, labels=c("1","2","3"))
lmex5.4 <- lm(life~A*B+A2+A2*B)
anova(lmex5.4) #Table 5.15
summary(lmex5.4)


#책이 잘못 나와서 다시 수정하는 결과를 보여드리겠습니다!
#책에서 table.5.15가 잘못나왔습니다. 제대로 구한 결과는 다음과 같습니다.
head(ex5.4)
ex5.4$temp2 <- ex5.4$temp^2
ex5.4.1 <- ex5.4[type==1,]
ex5.4.2 <- ex5.4[type==2,]
ex5.4.3 <- ex5.4[type==3,]
lm(life~temp+temp2,data=ex5.4.1)
lm(life~temp+temp2,data=ex5.4.2)
lm(life~temp+temp2,data=ex5.4.3)


f1 <- function(x) 169.38017 - 2.50145*x + 0.01285*x^2		#table5.15의 함수를 입력했습니다.
f2 <- function(x) 159.62397 - 0.173347*x - 0.005661*x^2
f3 <- function(x) 132.76240 + 0.90289*x - 0.01025*x^2


#(수정):위 함수 f1, f2, f3을 구하는 방법에 대해 말씀드리지 않았습니다. 우리가 fitting한
#결과를 갖고 있으므로 굳이 위의 type level 별로 data를 따로 끊어서 각각 적합하는 과정을
#밟지 않아도 됩니다. f1~f3의 계수들은 위의 fitting한 결과로부터, 즉 summary(lmex5.4)에서
#얻은 결과에서의 Estimate 부분으로부터 바로 얻어낼 수 있습니다. 그 방법은,
#먼저 f1은 type level이 1인 경우의 curve이므로,
#f1(x)=(Intercept)+A*x+A2*x
#로 구할 수 있습니다. 한편 f2는 type level이 2인 경우의 curve이므로,
#f2(x)=(Intercept)+B2+(A+A:B2)*x+(A2+A2:B2)*x^2
#으로 구할 수 있고 같은 방법으로 f3는
#f3(x)=(Intercept)+B3+(A+A:B3)*x+(A2+A2:B3)*x^2
#으로 구할 수 있습니다.


plot(temp, life, ylab="life", xlab="temperature",ylim=c(20,190))
par(new=T)		#이 코드를 통해, plot에 다음 plot들을 덮어쓸 수 있게 됩니다.
curve(f1, 15, 125, axes = FALSE, ylab=" ", xlab=" ",ylim=c(20,190))
par(new=T)
curve(f2, 15, 125, axes = FALSE, ylab=" ", xlab=" ",ylim=c(20,190))
par(new=T)
curve(f3, 15, 125, axes = FALSE, ylab=" ", xlab=" ",ylim=c(20,190)) 

interaction.plot(A,B,life)		#책의 figure.5.18의 curve들을 부분부분 직선
				#인 그래프로 그리게 됨을 알 수 있습니다.

detach(ex5.4)


# Example 5.5 (p. 214)
#이 예시에서는 angle과 speed의 제곱항까지 모두 고려합니다.
ex5.5 <-read.csv("ex_5_5.csv",header=T)
ex5.5
rm(life)		#앞에서 정의한 life가 따로 있었기 때문에 이 변수를 없애줍니다!
attach(ex5.5)
angle2 <- angle*angle
speed2 <- speed*speed
anglef <- factor(angle, labels=c("15","20","25"))
angle2f <- factor(angle2, labels=c("225","400","625"))
speedf <- factor(speed, labels=c("125","150","175"))
speed2f <- factor(speed2, labels=c("15625","22500","30625"))


#(수정):다음 줄에서, table.5.17을 얻기 위해 제곱항을 제외하고 나머지를 factor로 대입합니다.
anova(lm(life~anglef*speedf))  # Table 5.17


#(수정):다음 줄에서, table.5.17의 그래프를 얻기 위한 과정으로, 위와 같은 방법으로 lm을 실행합니다.
lmex5.5 <- lm(life~anglef*speedf)
plot(fitted(lmex5.5),life)
abline(a=0,b=1)   # y=bx+a -> 이렇게 설정하는 이유는, fitting이 정말 잘 된 것이라면
			#hat(y) = y에 가깝게 되기 때문입니다.

#모형 적합성에 대한 그래프 확인입니다.
plot(fitted(lmex5.5),resid(lmex5.5))
abline(h=0)


#제곱항을 고려하기 때문에 변수별 collinearity가 생길 수 있어, centering을 해줍니다.
anglec <- angle-20
speedc <- speed-150
anglec2 <- anglec*anglec
speedc2 <- speedc*speedc

lmex5.5n <- lm(life~angle+speed+anglec:speedc+anglec2+speedc2)
summary(lmex5.5n)		# Table 5.18
plot(fitted(lmex5.5n),life)
abline(a=0,b=1)


#(수정):다음 줄들은 제외시켰습니다.
#anglec <- as.factor(anglec)
#speedc <- as.factor(speedc)
#anglec2 <- as.factor(anglec2)
#speedc2 <- as.factor(speedc2)
#lmex5.5n <- lm(life~anglec*speedc+anglec2+speedc2)
#anova(lmex5.5n)		#앞에서 봤던 p-value는 변하지 않네요!
#summary(lmex5.5n)  # Table 5.18


#(수정):위에서 table.5.17과 table.5.18은 그 방향이 달랐습니다. 5.17의 경우는
#각 요인들을 factor로써 질적 변수로 적용시켜 각 level에 따른 작용효과를 적합한 경우이고
#5.18의 경우는 numeric vector 그대로 적용시켜 curve를 구하는 과정을 얻어낸 것입니다.


detach(ex5.5)

#수고하셨습니다!