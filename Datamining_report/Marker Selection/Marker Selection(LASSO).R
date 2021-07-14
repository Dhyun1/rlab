library(glmnet)

smp_size <- floor(0.7 * nrow(MIdata))
set.seed(1)
train_ind <- sample(seq_len(nrow(MIdata)), size = smp_size)
train <- MIdata[train_ind, ]
test <- MIdata[-train_ind, ]
str(train)
str(test)

x<-model.matrix(Platinum_sensitivity_in_platinum_users1~.,train)[,-1]
y<-train$Platinum_sensitivity_in_platinum_users1 
grid=10^seq(10,-8,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid,family="binomial")

set.seed(1)
cv.lasso=cv.glmnet(x,y,alpha=1,family="binomial")
plot(cv.lasso)
(bestlam=cv.lasso$lambda.min) #0.0028
(selam=cv.lasso$lambda.1se) #0.008741

par(mfrow = c(1,1), mar = c(3.5,3.5,2,1), mgp = c(2, 0.6, 0), cex = 0.8, las = 1)
plot(lasso.mod, "lambda", label = TRUE,
     xlim=c(-7,-1))
abline(v=log(bestlam),col="blue",lty=2)
abline(v=log(selam),col="blue",lty=2)

#coefficients
(predict(lasso.mod,type="coefficients",s=bestlam))
(predict(lasso.mod,type="coefficients",s=selam))

#removed 13 variables from lambda 1se
#Parity, Diabetes, Personal_history_of_breast_cancer, Familial_history_of_breast_cancer
#No_of_family_member_with_gynecologic, Origin, Grade, LN_dissection, Large_bowel_resection,
#Upper_abdominal_surgery, Other_colon_except_rectosigmoid, Supraclavicular_LN,
#X1st_Regimen

var_remove<-c("Parity", "Diabetes", "Personal_history_of_breast_cancer", 
              "Familial_history_of_breast_cancer", "No_of_family_member_with_gynecologic",
              "Origin", "Grade", "LN_dissection", "Large_bowel_resection", "Upper_abdominal_surgery", "Other_colon_except_rectosigmoid", "Supraclavicular_LN", "X1st_Regimen")

train.lasso<-train%>%
  dplyr::select(-var_remove)
test.lasso<-test%>%
  dplyr::select(-var_remove)

formula.lasso<-as.formula(paste("Platinum_sensitivity_in_platinum_users1","~",paste(setdiff(names(data)[-1],var_remove),collapse=" + ")))