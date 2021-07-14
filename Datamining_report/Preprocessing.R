library(tidyverse)
library(dplyr)
library(mice)
library(ggcorrplot)

#####Data Importing#####
setwd("C:/Users/DaeHyun/Desktop/Study/●Datamining/FINAL(report)")
input<-read.csv("TIC_new.csv", header = TRUE)
class(input);dim(input) #dataframe with 1002 observations, 49 variables 

#####Data Preprocessing#####
#Remove irrelevant column(Institution, Pt_No)
data<-input%>%
  dplyr::select(-Institution,-Pt_No)

#Factorize all categorical variables
conti_ind<-c(2,10,12:17,23,24)
data[,-conti_ind] <- lapply(data[,-conti_ind],factor)

#Multiple imputation
imp=mice(data,seed=1,defaultMethod = c("pmm", "logreg_2", "polyreg")) #total of 5 imputated datasets
MIdata<-complete(imp,"long")[,c(-1,-2)]

