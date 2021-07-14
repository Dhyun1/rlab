library(readxl)
library(dplyr)
library(tseries)
library(stringr)
library(ggplot2)
library(spc)
library(forecast)
library(ggfortify)
#paste Month+year
Month_2014to2019<-sapply(2014:2019,function(x){
  return (paste(x,str_sub(month.name,1,3),sep=""))
}) 
Month_2020<-paste(2020,str_sub(month.name[1:10],1,3),sep="")
Month<-factor(c(Month_2014to2019,Month_2020))

#title centering
theme_update(plot.title = element_text(hjust = 0.5)) 

#EWMA

EWMA<-function(data,L,lambda,mu,sigma){
  Z<-UCL<-LCL<-vector("double",length(data))
  n=length(data)
  for (i in 1:n){
    constant=lambda*(1-(1-lambda)^(2*i))/(2-lambda)
    UCL[i]<-mu+L*sigma*sqrt(constant)
    LCL[i]<-mu-L*sigma*sqrt(constant)
    Z[i]<-lambda*data[i]+(1-lambda)*(ifelse(i==1,mu,Z[i-1]))
  }
  return (list("Z"=Z,"UCL"=UCL,"LCL"=LCL,"index"=1:length(data)))
}

################################################################

getwd()
setwd("C:/Users/DaeHyun/Desktop/Study/●Quality Control/report")


jeju<-read_xls("./Passengers_Jeju.xls",skip=2)
names(jeju)<-c("Airport","Year","Month",
               "flights.arrival.","flights.departure.", "flights.total.",
               "passengers.arrival.","passengers.departure.", "passengers.total.",
               "cargos.arrival.", "cargos.departure.", "cargos.total.")

###II-1. check missing value, Data Preprocessing 
#(1) delete strange rows, check NAs
jeju<-jeju%>%
  filter(!is.na(Month),!is.na(Year))

sum(is.na(jeju))

#(2) remove and rename some columns
passengers<-jeju%>%
  dplyr::select(Year:Month,passengers.total.)%>%
  dplyr::rename(total=passengers.total.)
head(passengers)

#(3) EDA
passengers_ts<- ts(passengers$total,start=c(2014,1),end=c(2020,10),
                   frequency=12)

ggseasonplot(passengers_ts, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Passengers") +
  xlab("Time(Month)") +
  theme_update(plot.title = element_text(hjust = 0.5))+
  ggtitle("Seasonal plot : Jeju airport passengers")
ggsave(file="Seasonal plot (total passengers).jpg",width=15,height=6)
dev.off()

ggseasonplot(passengers_ts, polar=TRUE) +
  ylab("Passengers") +
  xlab("Time(Month)") +
  theme_update(plot.title = element_text(hjust = 0.5))+
  ggtitle("Seasonal plot : Jeju airport passengers")
ggsave(file="Seasonal plot2 (total passengers).jpg",width=15,height=6)
dev.off()

ggsubseriesplot(passengers_ts) +
  ylab("Passengers") +
  xlab("Time(Month)") +
  theme_update(plot.title = element_text(hjust = 0.5))+
  ggtitle("Seasonal subseries plot : Jeju airport passengers")
ggsave(file="Seasonal subseries plot (total passengers).jpg",width=15,height=6)
dev.off()

ggAcf(passengers_ts)
ggsave(file="ACF(total passengers).jpg",width=15,height=6)

###2. Data Analyzing

#1. Model based approach
 #(1) fit time series model for Phase I data
 # 1) Box-cox transformation
passengers_phase1<- ts(passengers$total,start=c(2014,1),end=c(2020,1),
                   frequency=12)
(lambda<-BoxCox.lambda(passengers_phase1)) #lambda=1.6
passengers_new<-BoxCox(passengers_phase1, lambda)

# 2) plot data(transformed, not transformed), choose what to use in analysis
par(mfrow=c(1,1),mar=c(4.5, 4.5, 0, 1))
plot.ts(passengers_new,xlab="Time",ylab="Passengers(box-cox lambda=1.6")

# 3) decompose data
plot(decompose(passengers_new))

# 4) conduct unit root test : the number of differences required to be made stationary

#Dickey-Fuller test to check mean stationary
adf.test(passengers_new) 

#Augmented Dickey-Fuller Test

#data:  passengers_new
#Dickey-Fuller = -3.9948, Lag order = 4, p-value = 0.01482
#alternative hypothesis: stationary

#p=0.01, reject H0, mean stationary

 # 5) plot ACF, PACF
ggtsdisplay(passengers_new) 
#The spike at lag 12 indicates a positive relationship with the 12 month cycle.

 # 6) auto.arima() : find best ARIMA model
(arima_fit_season<-auto.arima(passengers_new,stepwise=FALSE,approximation=FALSE))
#Series: passengers_new 
#ARIMA(0,1,1)(0,1,1)[12] 

#Coefficients:
#  ma1     sma1
#-0.6469  -0.6241
#s.e.   0.1112   0.1938

#sigma^2 estimated as 9.105e+17:  log likelihood=-1327.92
#AIC=2661.83   AICc=2662.26   BIC=2668.12

 # 7) plot residuals and conduct portmanteau test
passengers_new %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()
checkresiduals(arima_fit_season) 

#Ljung-Box test
#data:  Residuals from ARIMA(0,1,1)(0,1,1)[12]
#Q* = 9.0856, df = 13, p-value = 0.7664

#Model df: 2.   Total lags used: 15
#accept H0 : residual is white noise.

par(mar=c(3,3,2,2))
qqnorm(arima_fit_season$residuals)
qqline(arima_fit_season$residuals)

 # 8) quality Control Chart Analysis####
#(p. 433)The EWMA is used extensively in time series modeling and in forecasting
#(p. 465)An approach that has proved useful in dealing with autocorrelatd data
#is to diretly model the correlative structure with an appropriate time series model
#and use that model to remove the autocorrelation from the data
#and apply control chart to the residuals.

resid_phase1<-as.vector(residuals(arima_fit_season))

true_box_phase2<-((passengers[74:82,]$total)^(lambda)-1)/(lambda)
pred_phase2<-predict(arima_fit_season, n.ahead = 9)$pred
resid_phase2<-(true_box_phase2-pred_phase2)

resid_df<-as.data.frame(c(resid_phase1,resid_phase2))
colnames(resid_df)<-"Residual"

Time_Index<-1:nrow(resid_df)
resid_df<-cbind(resid_df,Time_Index)

split<-c(2020,2) #split Phase I, Phase II data with date 2020 Feb 
breakpoint<-which(jeju$Year==split[1]&jeju$Month==split[2])
Phase1_index<-1:(breakpoint-1)
Phase2_index<-(breakpoint):nrow(resid_df)
Phase1_data<-resid_df$Residual[Phase1_index]
Phase2_data<-resid_df$Residual[Phase2_index]
full_data<-(data.frame(Residuals=c(Phase1_data,Phase2_data),index=c(Phase1_index,Phase2_index)))

#Method 1. Shewhart control chart for time series residuals
#MR control chart
d2_2 <- 1.128
d3_2 <- 0.853
MR <- abs(Phase1_data[Phase1_index[-1]] - 
            Phase1_data[Phase1_index[-(breakpoint-1)]])
MR_bar <- mean(MR)

#set up control limits
UCL_MR <- MR_bar + 3*MR_bar*d3_2/d2_2
CL_MR <- MR_bar
LCL_MR <- max((MR_bar - 3*MR_bar*d3_2/d2_2),0)

#construct a MR chart
par(mfrow=c(1,1))
par(mar=c(3,3,3,3))
ggplot(data.frame(MR,index=Phase1_index[-1]),aes(x=Phase1_index[-1],y=MR))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=UCL_MR)+
  geom_abline(slope=0,intercept=CL_MR)+
  geom_abline(slope=0,intercept=LCL_MR)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Moving range chart (time series residual)")
ggsave(file="Moving range chart (time series residual).jpg",width=15,height=6)
dev.off()

Month[which(MR>UCL_MR|MR<LCL_MR)] #outliers : 2015May(18th) 2019Sep(69)

#Individual X chart
UCL_X_bar <- X_bar + 3*MR_bar/d2_2
CL_X_bar <- X_bar
LCL_X_bar <- X_bar - 3*MR_bar/d2_2

ggplot(data.frame(index=Phase1_index,Residuals=Phase1_data),aes(x=index,y=Residuals))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=UCL_X_bar)+
  geom_abline(slope=0,intercept=CL_X_bar)+
  geom_abline(slope=0,intercept=LCL_X_bar)+
  coord_cartesian(ylim=c(-1.2*10^(9.5),1.2*10^(9.5)))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Individual X chart (time series residual)")
ggsave(file="Individual X chart (time series residual).jpg",width=15,height=6)
dev.off()

Month[which(Phase1_data>UCL_X_bar|Phase1_data<LCL_X_bar)] # outlier : 2015Jun(18)

#remove outliers
out<-c(18,69)

#estimate true mean and variance
Phase1_data_remove<-Phase1_data
Phase1_data_remove[out]<-NA

mu_hat<-mean(Phase1_data_remove,na.rm=TRUE)
MR <- abs(Phase1_data_remove[2:73] - Phase1_data_remove[1:72])

sigma_hat<-mean(MR,na.rm=TRUE)/d2_2

#monitor Phase II data 
UCL_X_bar2<-mu_hat+3*sigma_hat
CL_X_bar2<-mu_hat
LCL_X_bar2<-mu_hat-3*sigma_hat

ggplot(full_data,aes(x=index,y=Residuals))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=UCL_X_bar2)+
  geom_abline(slope=0,intercept=CL_X_bar2)+
  geom_abline(slope=0,intercept=LCL_X_bar2)+
  geom_vline(xintercept=breakpoint-0.5,lty="dotted")+
  coord_cartesian(ylim=c(-1.2*10^(10),1.2*10^(10)))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Individual X chart (time series residual)")
ggsave(file="Individual X chart2(time series residual).jpg",width=15,height=6)
dev.off()

Month[73+which(Phase2_data>UCL_X_bar2|Phase2_data<LCL_X_bar2)] #all Phase 2 are outliers

##Method 2. EWMA chart for time series residuals
#choosing lambda
#choose big lambda to detect large mean shift
lambda<-0.65 
xewma.crit(l=0.65,L0=370,mu=0)
L<-2.85 #choose L which makes ARL0=370 (ARL0 of 3-sigma rule in Shewhart chart)

mu_hat<-mean(Phase1_data)
sigma_hat<-sd(Phase1_data)

#Calculate EWMA Statistic Z and UCL, LCL
EWMA_Phase1<-EWMA(Phase1_data,L,lambda,mu_hat,sigma_hat)

ggplot(data.frame(EWMA_Phase1),aes(x=index))+
  geom_point(aes(y=Z))+
  geom_line(aes(y=Z))+
  geom_line(aes(y=UCL))+
  geom_line(aes(y=LCL))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="EWMA chart (time series residual)")
ggsave(file="EWMA chart (time series residual).jpg",width=15,height=6)
dev.off()
#stable process : no outliers.

#estimate mean, variance
mu_hat<-mean(Phase1_data)
sigma_hat<-sd(Phase1_data)

#monitor Phase II data
EWMA_Full<-EWMA(c(Phase1_data,Phase2_data),L,lambda,mu_hat,sigma_hat)

ggplot(data.frame(EWMA_Full),aes(x=index))+
  geom_point(aes(y=Z))+
  geom_line(aes(y=Z))+
  geom_line(aes(y=UCL))+
  geom_line(aes(y=LCL))+
  geom_vline(xintercept=breakpoint-0.5,lty="dotted")+
  coord_cartesian(ylim=c(-1.2*10^(10),1.2*10^(10)))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="EWMA chart (time series residual)")
ggsave(file="EWMA chart2(time series residual).jpg",width=15,height=6)
dev.off() #all Phase II data are outliers.

##Method 3. An Approximate EWMA Procedure for Correlated Data (p. 468~) 
#First-order integrated moving average model x_t=x_(t-1)+epsilon_t-theta*e_(t-1) describes non-stationary behavior
#EWMA with lambda = 1-theta is the optimal one-step-ahead forecast for this process
#e_t=x_t - z_(t-1) is one-step-ahead prediction error(or model residual) 
#with i.i.d. mean zero.
#The parameter lambda would be found by minimizing residual sum of squares.

#use original data instead of residuals of time series
Phase1_datax<-passengers$total[Phase1_index]
Phase2_datax<-passengers$total[Phase2_index]

#select lambda which minimizes RSS of first-order integrated moving average model 
mu_hat<-mean(Phase1_datax)
sigma_hat<-sd(Phase1_datax)

e<-vector("double",length(Phase1_datax)-1) #e_2,... e_73
Z<-vector("double",length(Phase1_datax))
lambda_seq=seq(0,1,by=0.05)

df<-as.matrix(sapply(lambda_seq,function(x,data=Phase1_datax){
  n=length(data)
  mu=mean(data)
  for (i in 1:n){
    Z[i]<-x*data[i]+(1-x)*(ifelse(i==1,mu,Z[i-1]))
  }
  RSS<-sum((data[-1]-Z[-n])^2)
  return (c(format(x,scientific=FALSE),format(RSS,scientific=TRUE,digits=3)))
}))
df<-as.data.frame(t(df))%>%
  rename("lambda"="V1","RSS"="V2")

df%>%
  ggplot(aes(x=lambda,y=RSS))+
  geom_line(aes(group=1))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="RSS versus lambda")
ggsave(file="RSS versus lambda.jpg",width=15,height=6) #lambda=0.65

#construct Shewhart Chart for residuals from first-order integrated moving average model  
#calculate Z and residual 
lambda=0.65
mu_hat<-mean(Phase1_datax)
Z<-vector("double",length(Phase1_datax))

for (i in 1:length(Phase1_datax)){
  Z[i]<-lambda*Phase1_datax[i]+(1-lambda)*(ifelse(i==1,mu_hat,Z[i-1]))
}
e<-(Phase1_datax[-1]-Z[-length(Phase1_datax)]) #e_2, ... e_73

d2_2 <- 1.128
d3_2 <- 0.853
MR <- abs(e[2:72] - e[1:71])
MR_bar <- mean(MR)

#Individual X chart
X_bar <- mean(e)
UCL_X_bar <- X_bar + 3*MR_bar/d2_2
CL_X_bar <- X_bar
LCL_X_bar <- X_bar - 3*MR_bar/d2_2

ggplot(data.frame(Residuals=e,index=2:73),aes(x=index,y=Residuals))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=UCL_X_bar)+
  geom_abline(slope=0,intercept=CL_X_bar)+
  geom_abline(slope=0,intercept=LCL_X_bar)+
  coord_cartesian(ylim=c(-700000,700000))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Individual X chart (first order integrated moving average residual)")
ggsave(file="Individual X chart (first order integrated moving average residual).jpg",width=15,height=6)
dev.off() #no outliers

par(mar=c(3,3,2,2))
qqnorm(e)
qqline(e,col="blue")

#from above results, we can use first-order integrated moving average model 
#in terms of RSS, stability of process, and normality assumptions. 

#construct moving center-line EWMA Chart (p. 470)
#UCL_(t+1)=Z_t+3*sigma, LCL_(t+1)=Z_t-3*sigma
#observation would be compared to these limits
sigma_hat_resid<-sd(e) #sigma is estimated by the standard deviation of the residuals

UCL<-(Z+3*sigma_hat_resid)[-length(Phase1_datax)] #UCL_2,..., UCL_73
CL<-Z[-1]                                         #CL_2, ..., CL_73
LCL<-(Z-3*sigma_hat_resid)[-length(Phase1_datax)] #LCL_2,..., LCL_73

par(mfrow=c(1,1))
par(mar=c(3,3,3,3))
ggplot(data.frame(Passengers.Z=Phase1_datax[-1],index=2:73),aes(x=index))+
  geom_point(aes(y=Passengers.Z))+
  geom_line(aes(y=Passengers.Z))+
  geom_line(aes(y=UCL))+
  geom_line(aes(y=CL),color="blue")+
  geom_line(aes(y=LCL))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Moving center-line EWMA control chart")
ggsave(file="Moving center-line EWMA control chart.jpg",width=15,height=6)
dev.off() #no outliers

#estimate true mean and sigma
sigma_hat_resid<-sd(e)

#monitor Phase II data
Z_full<-EWMA(c(Phase1_datax,Phase2_datax),L,lambda,mu_hat,sigma_hat)$Z

UCL_full<-c(NA,Z_full[1:81]+3*sigma_hat_resid)
LCL_full<-c(NA,Z_full[1:81]-3*sigma_hat_resid)
#UCL_2<-(Z_full[72:81]+3*sigma_hat_resid)
#LCL_2<-(Z_full[72:81]-3*sigma_hat_resid)
#UCL_full<-c(UCL,UCL_2)
#LCL_full<-c(LCL,LCL_2) #LCL_1 not defined ,UCL_2 ~ 82 = Z_1 ~ Z_81 -3sigma
CL_full<-c(NA,Z_full[1:81]) #CL_1 not defined, CL_2 ~ CL_82 = Z_1 ~ Z_81 

df<-data.frame(Passengers=c(Phase1_datax,Phase2_datax),index=c(Phase1_index,Phase2_index),
              UCL_full,CL_full,LCL_full)

p<-ggplot(df,aes(x=index))+
  geom_point(aes(y=Passengers))+
  geom_line(aes(y=Passengers))+
  geom_line(aes(y=UCL_full))+
  geom_point(aes(y=CL_full),shape=21)+
  geom_line(aes(y=CL_full),colour="blue")+
  geom_vline(xintercept=breakpoint-0.5,lty="dotted")+
  geom_line(aes(y=LCL_full))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Moving center-line EWMA control chart")
p
ggsave(file="Moving center-line EWMA control chart(total).jpg",width=15,height=6)
dev.off()

p+
  coord_cartesian((xlim=c(70,82.5)))
ggsave(file="Moving center-line EWMA control chart(Phase II).jpg",width=15,height=6)
#2020 Feb, Mar, August are outliers.

##Method 4. A Model-Free approach 
#The Batches Mean Control Chart##
#The batch size should be selected so as to reduce the lag 1 autocorrelation of the batch means to approximately 0.10.
#start with b=1, doubling b until autocorrelation is sufficiently small.
#(p. 473) The batch means can be plotted and analyzed on a standard individual control chart.

#b=1(original data)
par(mar=c(3,3,2,2))
acf(Phase1_datax)[1] #lag 1 autocorrelation 0.708

#b=2
batch<-c(rep(1:35, each=2),rep(36, 3))
Phase1_datax_batch<-as.data.frame(cbind(Phase1_datax,batch))
colnames(Phase1_datax_batch)<-c("Passengers","batch")

xbar<-do.call(rbind,lapply(1:36,function(j){ #apply function() for all batches, then rbind
  mean(Phase1_datax_batch[Phase1_datax_batch$batch==j,1]) #apply mean by column for batch number j sample
})) 
acf(xbar)[1] # lag 1 autocorrelation 0.704

#b=4
batch<-c(rep(1:17, each=4),rep(18, 5))
Phase1_datax_batch<-as.data.frame(cbind(Phase1_datax,batch))
colnames(Phase1_datax_batch)<-c("Passengers","batch")

xbar<-do.call(rbind,lapply(1:18,function(j){ 
  mean(Phase1_datax_batch[Phase1_datax_batch$batch==j,1]) 
})) 
acf(xbar)[1] #0.546

#b=8
batch<-c(rep(1:8, each=8),rep(9, 9))
Phase1_datax_batch<-as.data.frame(cbind(Phase1_datax,batch))
colnames(Phase1_datax_batch)<-c("Passengers","batch")

xbar<-do.call(rbind,lapply(1:9,function(j){ 
  mean(Phase1_datax_batch[Phase1_datax_batch$batch==j,1]) 
})) 
acf(xbar)[1] #0.501

#b=16
batch<-c(rep(1:4, each=16),rep(5,9))
Phase1_datax_batch<-as.data.frame(cbind(Phase1_datax,batch))
colnames(Phase1_datax_batch)<-c("Passengers","batch")

xbar<-do.call(rbind,lapply(1:5, function(j){ 
  mean(Phase1_datax_batch[Phase1_datax_batch$batch==j,1]) 
})) 
acf(xbar)[1] #0.136, set 16 for batch size

#construct MR chart
d2_2 <- 1.128
d3_2 <- 0.853
batch_means<-xbar
MR <- abs(batch_means[2:length(batch_means)] - 
            batch_means[1:(length(batch_means)-1)])
MR_bar <- mean(MR)

UCL_MR <- MR_bar + 3*MR_bar*d3_2/d2_2
CL_MR <- MR_bar
LCL_MR <- max(0,MR_bar - 3*MR_bar*d3_2/d2_2)

par(mfrow=c(1,1))
par(mar=c(3,3,3,3))
ggplot(data.frame(MR,index=2:5),aes(x=index,y=MR))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=UCL_MR)+
  geom_abline(slope=0,intercept=CL_MR)+
  geom_abline(slope=0,intercept=LCL_MR)+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim=c(2,5),ylim=c(-100,7*10^5))+
  labs(title="Moving range chart (batches individual)")
ggsave(file="Moving range chart (batches individual).jpg",width=15,height=6)
dev.off() #no outlier

#Individual X chart 
X_bar<-mean(xbar)
UCL_X_bar <- X_bar + 3*MR_bar/d2_2
CL_X_bar <- X_bar
LCL_X_bar <- X_bar - 3*MR_bar/d2_2

ggplot(data.frame(X=xbar[-1],index=2:5),aes(x=index,y=X))+
  geom_point()+
  geom_line()+
  geom_abline(slope=0,intercept=UCL_X_bar)+
  geom_abline(slope=0,intercept=CL_X_bar)+
  geom_abline(slope=0,intercept=LCL_X_bar)+
  coord_cartesian(ylim=c(1.5*10^(6),2.7*10^(6)))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Individual X chart (batches individual)")
ggsave(file="Individual X chart (batches individual).jpg",width=15,height=6)
dev.off()

#estimate true mean and variance 
mu_hat<-X_bar
sigma_hat<-MR_bar/d2_2

##monitor Phase II data##
#not enough samples for Phase II in order to make a single batch