# 11???? ?Ç½?: Variable selection

# 6.5 Lab1: Subset Selection Methods
# 6.5.1 Best Subset Selection (reference: ISLR pdf ???? p.254)
install.packages("ISLR")
install.packages("leaps")
install.packages("glmnet")
library(ISLR)
library(leaps)
library(glmnet)

data(Hitters)

# 322 by 20
dim(Hitters)
str(Hitters)
names(Hitters)

# ?????? È®?? : 59 obs
sum(is.na(Hitters$Salary))

# ?????? Á¦??
Hitters <- na.omit(Hitters)

# 263 by 20
dim(Hitters)

# ?????? Á¦?? È®?? : 0 obs
sum(is.na(Hitters))

# regsubsets : performs the best subset selection method
# by identifying the best model that contains a given number of predictors,
# where best is quantified using RSS.
# An * indicates that a given variable is included in the corresponding model
# For instance, this output indicates that the best two-variable model contains
# only "Hits" and "CRBI"
# By default, regsubsets() only reports results up to the best eight-variable model.
# But the "nvmax" option can be used in order to return
# as many variables as are desired.

# Default: Up to 8-variables model
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

# Option: Up to 19-variables model
regfit.full=regsubsets(Salary~.,Hitters,nvmax=19)
reg.summary=summary(regfit.full)
reg.summary

# the R squared statistic increase from 32%, when only one variable in included in the model,
# to almost 55%, when all variables(19-variables) are included 
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
# adjr2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

# ????Â°, ?? ?? predictor?? ?????? ?????? best???? adjr2?? ???? È®??
which.max(reg.summary$adjr2)

points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)
# cp
plot(reg.summary$cp,xlab="Number of Variables", ylab="Cp",type="l")

which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
# bic

plot(reg.summary$bic,xlab="Number of Variables", ylab="BIC", type="l")
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# We can use the coef() function to see the coefficient estimates associated with this model.
coef(regfit.full,6)

#############################################################################################

# 6.5.2 Forward and Backward Stepwise Selection (reference: ISLR pdf ???? p.257)

# We can also use the regsubsets() function to perform forward/backward selection
# using the argument method="forward" or method="backward".

# 1. forward selection
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

# 2. backward selection
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

# For instance, we see that using forward selection,
# the best one-variable model contains only "CRBI",
# And the best two-variable model additionally includes Hits

# For this data, the best seven-variable models identified by forward selection,
# backward selection, and the best subset are different.

# 1. best subset model
coef(regfit.full,7)
# 2. forward selection model
coef(regfit.fwd,7)
# 3. backward selection model
coef(regfit.bwd,7)

#############################################################################################

# 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation 
# (reference: ISLR pdf ???? p.258)

# In order for these approaches to yield accurate estimates of the test error,
# we "must" use only the training observations to perform all aspects of model-fitting
# including variable selection.
# If the full data set is used to perform the best subset selection step,
# the validation set errors and cross-validation errors that we obtain
# will not be accurate estimates of the test error.

# In order to use the validation approach, we begin by splitting data into training and test set.
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

train
test

# Now, we apply regsubsets() to the training set in order to perform best subset selection.
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# We now compute the validation set error for the best model of each model size
# We first make a model matrix from the test data
# The model.matrix function is used in many regressoin packages for bulding an "X" matrix from data.
test.mat=model.matrix(Salary~.,data=Hitters[test,])
test.mat

val.errors=rep(NA,19)
val.errors
for(i in 1:19){
  coefi=coef(regfit.best,id=i) 
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)}

val.errors

# 7
which.min(val.errors)

coef(regfit.best,7)

## We now try to choose among the models of different sizes using cross-validation
## We must perform best subset selection "within each of the k training sets."
# k-fold validation
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))
cv.errors
# row(10): fold index, column(19): variable index
cv.errors

# predict ?Ô¼? Á¤?? 
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


for (j in 1:k) { 
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
  }

cv.errors
?apply
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

which.min(mean.cv.errors)

# we can see that cross-validation selects an 10-variable model.
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

# We now perform best subset selection on the full data set in order to obtain
# the 10-variable model.

reg.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,10)

#############################################################################################

# 6.6 Lab2: Ridge Regression and the Lasso

# We will use the glmnet packages in order to perform ridge regression 
# and the lasso.
# Before proceeding, ensure that the missing values have been removed from data
# ,as described in Section 6.5.

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

x
y
# 6.6.1 Ridge Regression (reference: ISLR pdf ???? p.261)
# The glmnet() function has an alpha argument that determines what type of model is fit.
# If alpha=0 then a ridge regression model is fit, and if alpha=1 then a lasso models is fit.
# We first fit a ridge regression model.
grid=10^seq(10,-2,length=100)
grid
# the glmnet() function standardizes the variables so that they are on the same scale.
# To turn off this default setting, use the argument standardize=FALSE.
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

# 20 by 100 : 20 rows(one for each predictor, plus an intercept)
# and 100 columns (one for each value of lambda)
dim(coef(ridge.mod))

# 50-th lambda: 11498
ridge.mod$lambda[50]
# corresponding coef with the lambda value
coef(ridge.mod)[,50]

# 60-th lambda: 705
ridge.mod$lambda[60]
# corresponding coef with the lambda vlaue
coef(ridge.mod)[,60]

# prediction
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
# 14226.5
mean((ridge.pred-y.test)^2)

# We can use cv.glmnet function (more convenient and general)
set.seed(1)
# default: 10-fold cv within train set
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
# 139833.6
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)

# coef corresponding to best lambda obtained by "cv.glmnet"
predict(out,type="coefficients",s=bestlam)[1:20,]

################################################################
# 6.6.2 The Lasso (reference: ISLR pdf ???? p.265)

# argument: alpha=1
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

# we can see form the coefficient plot that depending on the choice of tuning parameter
# some of the coefficients will be exactly equal to zero.
# We now perform cross-validation and compute the associated test error.
# using cv.glmnet
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
# ?? plot?? ?ß¿? 
plot(cv.out)
# ???? lambda ?? 
bestlam=cv.out$lambda.min

# ???? lambda ??À¸?? ?????? ?????? test mse
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
# 143673.6
mean((lasso.pred-y.test)^2)

# lasso has a substantial advantage over ridge regression
# in that the resulting coefficient estimates are sparse.

# ???? lambda ??À¸?? ?????? ???? ???Ãµ? ????Á¶?? È®?? 
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef

