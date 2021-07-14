library(tidyverse)
library(dplyr)
library(mice)
library(ggcorrplot)

#Data Import
setwd("C:/Users/DaeHyun/Desktop/Study/●Datamining/FINAL(report)")
input<-read.csv("TIC_new.csv", header = TRUE)
class(input);dim(input) #dataframe with 1002 observations, 49 variables 

#Remove irrelevant column(Institution, Pt_No)
data<-input%>%
  dplyr::select(-Institution,-Pt_No)

#Factorize all categorical variables except response variable
conti_ind<-c(1,2,10,12:17,23,24)
data[,-conti_ind] <- lapply(data[,-conti_ind],factor)

#Multiple imputation data
imp=mice(data,seed=1,defaultMethod = c("pmm", "logreg_2", "polyreg")) #total of 5 imputated datasets
MIdata<-complete(imp,"long")[,c(-1,-2)]

smp_size <- floor(0.7 * nrow(MIdata))
set.seed(1)
train_ind <- sample(seq_len(nrow(MIdata)), size = smp_size)
train.MI <- MIdata[train_ind, ]
test.MI <- MIdata[-train_ind, ]

train.MI.factor<-train.MI%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI.factor<-test.MI%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#NA omit data
data.omit<-na.omit(data)
set.seed(1)
smp_size <- floor(0.7 * nrow(data.omit))
train_ind <- sample(seq_len(nrow(data.omit)), size = smp_size)
train.omit <- data.omit[train_ind, ]
test.omit <- data.omit[-train_ind, ]


