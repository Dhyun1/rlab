
# 13ÁÖÂ÷ ½Ç½À: PCR,PLS,non-linear regression

install.packages("pls")
library(pls)

# 1. PCR(Principal Component Regression) : p.256

set.seed(1)
idx_train = sample(1:nrow(iris), size = nrow(iris)*0.8)
train = iris[idx_train, ]
test=iris[-idx_train,]

#120 obs, 5 variables
str(train)

# 30 obs, 5 variables
str(test)

# Choosing number of components 
set.seed (1)
pcr_model <- pcr(Sepal.Length~., data = train, scale = TRUE, validation = "CV")

# RMSEP: root mean squared error obtaine by CV
summary(pcr_model)

# Plot the root mean squared error
validationplot(pcr_model)

# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")

# Plot the R2
validationplot(pcr_model, val.type = "R2")

set.seed(1)
pcr.fit=pcr(Sepal.Length~.,data=train,ncom=3)
pcr.pred=predict(pcr.fit,test,ncom=3)
pcr.pred

#0.1056
mean((pcr.pred-test$Sepal.Length)^2)


# use all the variables for comparison
lm.fit<-glm(Sepal.Length~.,data=train)
lm.pred=predict(lm.fit,test)
lm.pred

# 0.0807
mean((lm.pred-test$Sepal.Length)^2)

# 2. PLS(Partial Least Squares) : p.258

set.seed(1)
pls.fit=plsr(Sepal.Length~.,data=train,scale=TRUE,validation="CV")
summary(pls.fit)

validationplot(pls.fit,val.type="MSEP")

# 0.1231
pls.pred=predict(pls.fit,test,ncom=3)
mean((pls.pred-test$Sepal.Length)^2)


# Polynomial Regression and Step functions
library(ISLR)
data(Wage)

# 7.1 Polynomial Regression  : p.266

attach(Wage)
# poly(age,4) : contains age, age^2, age^3 and age^4
fit=lm(wage~poly(age,4),data=Wage)
# Degree, Estimate, P-value
coef(summary(fit))

# We now create a grid of values for age at which we want predictions
agelims=range(age)
agelims

age.grid=seq(from=agelims[1],to=agelims[2])
age.grid

preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
preds

se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands

par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
# scatter plot
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
# plot fitted line
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# ANOVA for model comparision
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# The p-value comparing the linear Model 1 to the quadratic Model 2 is
# essentially zero (<2.2e-16), indicating that a linear fit is not sufficient. Similarly
# the p-value comparing the quadratic Model 2 to the cubic Model 3
# is very low (0.0017), so the quadratic fit is also insufficient. The p-value
# comparing the cubic and degree-4 polynomials, Model 3 and Model 4, is approximately
# 5% while the degree-5 polynomial Model 5 seems unnecessary
# because its p-value is 0.37. Hence, either a cubic or a quartic polynomial
# appear to provide a reasonable fit to the data, but lower- or higher-order
# models are not justified.


# *logistic polynomial regression

fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
fit

preds=predict(fit,newdata=list(age=age.grid),se=T)

pfit=exp(preds$fit)/(1+exp(preds$fit))
pfit

se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))
se.bands

# In order to fit a step function, as discussed in Section 7.2, we us the cut() function.

# Step function as a predictor(dummary variable): p.292
# baseline: (17.9,33.5], then three betas to be estimated
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))


#############################################################################
################ Splines ####################################################

install.packages("gam")
library(gam)
library(ISLR)
library(splines)

data(Wage)
attach(Wage)
# 7.8.2 Splines
# In order to fit regression splines in R, we use the splines library. In Section
# 7.4, we saw that regression splines can be fit by constructing an appropriate
# matrix of basis functions. The bs() function generates the entire matrix of
# bs()
# basis functions for splines with the specified set of knots. By default, cubic
# splines are produced. Fitting wage to age using a regression spline is simple:

# We now create a grid of values for age at which we want predictions
agelims=range(age)
agelims

age.grid=seq(from=agelims[1],to=agelims[2])
age.grid

# spline with three knots
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
fit

pred=predict(fit,newdata=list(age=age.grid),se=T)
pred

plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

# In order to instead fit a natural spline, we use the ns() function. Here
# ns()
# we fit a natural spline with four degrees of freedom.
fit2=lm(wage~ns(age,df=4),data=Wage)
fit2
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
pred2
lines(age.grid,pred2$fit,col="red",lwd=2)

# As with the bs() function, we could instead specify the knots directly using
# the knots option.
# In order to fit a smoothing spline, we use the smooth.spline() function.
# smooth.
# Figure 7.8 was produced with the following code:

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")

title("Smoothing Spline")
# we can specify df rather than lambda
fit=smooth.spline(age,wage,df=16)
# validated df/lambda = 6.79
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# Notice that in the first call to smooth.spline(), we specified df=16. The
# function then determines which value of Î» leads to 16 degrees of freedom. In
# the second call to smooth.spline(), we select the smoothness level by crossvalidation;
# this results in a value of Î» that yields 6.8 degrees of freedom.
# In order to perform local regression, we use the loess() function.

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
# span: the parameter alpha which controls the degree of smoothing
# the smaller alpha(span=0.2) the more wigly the function is
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)

lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# Here we have performed local linear regression using spans of 0.2 and 0.5:
# that is, each neighborhood consists of 20% or 50% of the observations. The
# larger the span, the smoother the fit. The locfit library can also be used
# for fitting local regression models in R.

# 7.8.3 GAMs
# We now fit a GAM to predict wage using natural spline functions of year
# and age, treating education as a qualitative predictor, as in (7.16). Since
# this is just a big linear regression model using an appropriate choice of
# basis functions, we can simply do this using the lm() function.

# natrual spline with degree=4 for year, with degree 5 for age
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
summary(gam1)
# smoothing spline with df=4 for year, with df=5 for age
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
summary(gam.m3)
gam.m3
