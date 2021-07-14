library(tidyverse)
library(dplyr)
library(mice)
library(caret)
library(randomForest)
library(pROC)
library(MLeval)
library(MASS)
library(ROCR)
####################some functions##########################
mice.impute.logreg_2 <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  
  # augment data in order to evade perfect prediction
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w
  
  # fit model
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(x = x[ry, , drop = FALSE], 
                             y = y[ry], 
                             family = quasibinomial(link = logit), 
                             weights = w[ry],
                             control = list(maxit = 100)))
  fit <- eval(expr)
  fit.sum <- summary.glm(fit)
  beta <- coef(fit)
  rv <- t(chol(fit.sum$cov.unscaled))
  
  beta.star <- beta + rv %*% rnorm(ncol(rv))
  
  # draw imputations
  p <- 1/(1 + exp(-(x[wy, , drop = FALSE] %*% beta.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  return(vec)
}

mice.impute.logreg.boot <- function(y, ry, x, wy =  NULL, ...) {
  if (is.null(wy)) wy <- !ry
  
  # draw a bootstrap sample for yobs and xobs
  xobs <- x[ry, , drop = FALSE]
  yobs <- y[ry]
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  doty <- y
  doty[ry] <- yobs[s]
  dotx <- x
  dotx[ry, ] <- xobs[s, , drop = FALSE]
  
  x <- dotx
  y <- doty
  
  # fit model
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(x = x[ry, , drop = FALSE], 
                             y = y[ry], 
                             family = binomial(link = logit)))
  fit <- suppressWarnings(eval(expr))
  beta.star <- coef(fit)
  
  # draw imputations
  p <- 1/(1 + exp(-(x[wy, ] %*% beta.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  return(vec)
}

augment <- function(y, ry, x, wy, maxcat = 50) {
  icod <- sort(unique(unclass(y)))
  k <- length(icod)
  if (k > maxcat) 
    stop("Maximum number of categories (", maxcat, ") exceeded")
  p <- ncol(x)
  
  # skip augmentation if there are no predictors
  if (p == 0) 
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, length(y))))
  
  ## skip augmentation if there is only 1 missing value 12jul2012 
  ## this need to be fixed 12jul2011
  if (sum(!ry) == 1) 
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, length(y))))
  
  # calculate values to augment
  mean <- apply(x, 2, mean, na.rm = TRUE)
  sd <- sqrt(apply(x, 2, var, na.rm = TRUE))
  minx <- apply(x, 2, min, na.rm = TRUE)
  maxx <- apply(x, 2, max, na.rm = TRUE)
  nr <- 2 * p * k
  a <- matrix(mean, nrow = nr, ncol = p, byrow = TRUE)
  b <- matrix(rep(c(rep.int(c(0.5, -0.5), k), rep.int(0, nr)), length = nr * p), nrow = nr, ncol = p, byrow = FALSE)
  c <- matrix(sd, nrow = nr, ncol = p, byrow = TRUE)
  d <- a + b * c
  d <- pmax(matrix(minx, nrow = nr, ncol = p, byrow = TRUE), d, na.rm = TRUE)
  d <- pmin(matrix(maxx, nrow = nr, ncol = p, byrow = TRUE), d, na.rm = TRUE)
  e <- rep(rep(icod, each = 2), p)
  
  dimnames(d) <- list(paste0("AUG", seq_len(nrow(d))), dimnames(x)[[2]])
  xa <- rbind.data.frame(x, d)
  # beware, concatenation of factors
  ya <- if (is.factor(y)) as.factor(levels(y)[c(y, e)]) else c(y, e)
  rya <- c(ry, rep.int(TRUE, nr))
  wya <- c(wy, rep.int(FALSE, nr))
  wa <- c(rep.int(1, length(y)), rep.int((p + 1)/nr, nr))
  
  return(list(y = ya, ry = rya, x = xa, w = wa, wy = wya))
}

plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,4)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
}
#############################################



#########HGSOC patient data analysis#########
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

#NA omit data
data.omit<-na.omit(data)
set.seed(1)
smp_size <- floor(0.7 * nrow(data.omit))
train_ind <- sample(seq_len(nrow(data.omit)), size = smp_size)
train.omit <- data.omit[train_ind, ]
test.omit <- data.omit[-train_ind, ]

#Multiple imputation data
imp=mice(data,seed=1,defaultMethod = c("pmm", "logreg_2", "polyreg")) #total of 5 imputated datasets
MIdata<-complete(imp,"long")[,c(-1,-2)]

smp_size <- floor(0.7 * nrow(MIdata))
set.seed(1)
train_ind <- sample(seq_len(nrow(MIdata)), size = smp_size)
train.MI <- MIdata[train_ind, ]
test.MI <- MIdata[-train_ind, ]

#need to factorize response variable and set label zero(0), one(1) 
#(required to run prediction model codes)

train.MI.factor<-train.MI%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI.factor<-test.MI%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
train.omit.factor<-train.omit%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.omit.factor<-test.omit%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

######A. NA omitted data analysis(stepwise selection)######
set.seed(1)
fit<-SuperLearner(Y = train.omit[,1], X = train.omit[,c(2:27,29:47)], family = binomial(),
                  SL.library = "SL.step",cvControl=list(V=5), method="method.AUC")
fit$fitLibrary$SL.step_All$object$call
#glm(formula = Y ~ Hypertension + Diabetes + Dyslipidemia + No_of_family_member_with_breast_cancer_upto_2nd_degree + 
#      Familial_history_of_gynecologic_cancer + CA125_initial + 
#      No_of_harvested_LNs + NAC + Upper_abdominal_surgery + Bladder_or_Rectal_mucosa + 
#      pleural_effusion + Lung + Residual_tumor_size_1st_debulking, 
#    family = family, data = X)

formula.step.Naomit<-"Platinum_sensitivity_in_platinum_users1 ~ Hypertension + Diabetes + Dyslipidemia + No_of_family_member_with_breast_cancer_upto_2nd_degree + Familial_history_of_gynecologic_cancer + CA125_initial + No_of_harvested_LNs + NAC + Upper_abdominal_surgery + Bladder_or_Rectal_mucosa + pleural_effusion + Lung + Residual_tumor_size_1st_debulking"
formula.step.Naomit<-as.formula(formula.step.Naomit)

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.step.Naomit,data=train.omit,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.omit[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.omit))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.omit[,1])
mean(glm.train.pred==train.omit[,1]) #training Accuracy = 0.81

glm.test.probs <- predict(glm.fits,test.omit[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.omit))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.omit[,1])
mean(glm.test.pred==test.omit[,1]) #test Accuracy = 0.75

#LDA
lda.fit <- lda(formula.step.Naomit, data=train.omit)

lda.train.pred <- predict(lda.fit, train.omit)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.omit[,1])
mean(lda.train.class==train.omit[,1]) #training Accuracy = 0.81

lda.test.pred <- predict(lda.fit, test.omit)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.omit[,1])
mean(lda.test.class==test.omit[,1]) #test Accuracy = 0.75

#train ROC curves
glm.train.roc<-roc(train.omit[,1],glm.train.probs)
lda.train.roc<-roc(train.omit[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.85(logistic)
lda.train.roc$auc #train AUC 0.84(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.omit[,1],glm.test.probs)
lda.test.roc<-roc(test.omit[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.66(logistic)
lda.test.roc$auc #test AUC 0.68(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:13) #Grid for tuning parameter to be cross-validated.
#For random forest, tuning parameter = mtry,
#which is the number of variables available for splitting at each tree node
#(각 트리의 노드 결정 시 설명변수의 후보 수)
#mtry <= total number of explanatory variables in formula.

step.Naomit.rf.cv <- train(formula.step.Naomit, data = train.omit.factor, method = "rf", 
                           trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#warning occurs because we set mtry over the total number of variables in formula
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

step.Naomit.rf.cv #we see mtry=2 is best parameter

plot(step.Naomit.rf.cv)  		# Plot the performance of the training models

#confusion matrix
step.Naomit.rf.train.pred <- predict(step.Naomit.rf.cv,train.omit.factor[,-1])
confusionMatrix(step.Naomit.rf.train.pred,train.omit.factor[,1]) #training accuracy 0.85

step.Naomit.rf.test.pred <- predict(step.Naomit.rf.cv,test.omit.factor[,-1])
confusionMatrix(step.Naomit.rf.test.pred,test.omit.factor[,1]) #test accuracy 0.79

#train ROC, AUC
MLeval::evalm(step.Naomit.rf.cv)$roc #train AUC = 0.71

#test ROC, AUC
step.Naomit.rf.test.probs <- predict(step.Naomit.rf.cv,test.omit.factor[,-1],type="prob")

step.Naomit.rf.test.ROC <- roc(predictor=step.Naomit.rf.test.probs$zero,
                               response=test.omit.factor[,1],
                               levels=rev(levels(test.omit.factor[,1])))
step.Naomit.rf.test.ROC$auc #test AUC 0.72
plot(step.Naomit.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)

#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

step.Naomit.svm.cv <- train(formula.step.Naomit, data = train.omit.factor, method = "svmRadial", 
                            trControl = control, verbose = F, tuneLength=50, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)
#tuneLength = 50 indicates that randomly set 50 different combinations of (C, sigma) and compare each training AUC.

step.Naomit.svm.cv #we see sigma=0.098 and C=2 is best.

#confusion matrix
step.Naomit.svm.train.pred <- predict(step.Naomit.svm.cv,train.omit.factor[,-1])
confusionMatrix(step.Naomit.svm.train.pred,train.omit.factor[,1]) #training accuracy 0.84

step.Naomit.svm.test.pred <- predict(step.Naomit.svm.cv,test.omit.factor[,-1])
confusionMatrix(step.Naomit.svm.test.pred,test.omit.factor[,1]) #test accuracy 0.80

#train ROC, AUC
MLeval::evalm(step.Naomit.svm.cv)$roc #train AUC = 0.67

#test ROC, AUC
step.Naomit.svm.test.probs <- predict(step.Naomit.svm.cv,test.omit.factor[,-1],type="prob")

step.Naomit.svm.test.ROC <- roc(predictor=step.Naomit.svm.test.probs$zero,
                                response=test.omit.factor[,1],
                                levels=rev(levels(test.omit.factor[,1])))
step.Naomit.svm.test.ROC$auc #test AUC 0.62






######B. NA omitted data analysis(lasso selected)######
x1=model.matrix(data.omit$Platinum_sensitivity_in_platinum_users1~.,data.omit)[,-1]
y1=data.omit$Platinum_sensitivity_in_platinum_users1

smp_size <- floor(0.7 * nrow(data.omit))
set.seed(1)
train_ind <- sample(seq_len(nrow(data.omit)), size = smp_size)

set.seed(1)
cv.fit=cv.glmnet(x1[train_ind,],y1[train_ind], 
                 alpha=1, nfolds = 5, family = "binomial")
cv.fit$lambda.min #lambda = 0.046
predict(cv.fit,type="coefficients",s=cv.fit$lambda.min)

# (Intercept)                                            -2.27847703
# Hypertension1                                           0.08626541
# CA125_initial                                           0.06987296
# PLN_status1                                             0.19224735
# No_of_harvested_LNs                                    -0.00289198
# NAC1                                                    0.88006517
# Liver_surface3                                          0.07716859
# Residual_tumor_size_1st_debulking1                      0.39915469

formula.lasso.Naomit<-"Platinum_sensitivity_in_platinum_users1 ~ Hypertension + CA125_initial + PLN_status + No_of_harvested_LNs + NAC + Liver_surface + Residual_tumor_size_1st_debulking"
formula.lasso.Naomit<-as.formula(formula.lasso.Naomit)

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.lasso.Naomit,data=train.omit,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.omit[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.omit))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.omit[,1])
mean(glm.train.pred==train.omit[,1]) #training Accuracy = 0.82

glm.test.probs <- predict(glm.fits,test.omit[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.omit))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.omit[,1])
mean(glm.test.pred==test.omit[,1]) #test Accuracy = 0.7375

#LDA
lda.fit <- lda(formula.lasso.Naomit, data=train.omit)

lda.train.pred <- predict(lda.fit, train.omit)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.omit[,1])
mean(lda.train.class==train.omit[,1]) #training Accuracy = 0.82

lda.test.pred <- predict(lda.fit, test.omit)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.omit[,1])
mean(lda.test.class==test.omit[,1]) #test Accuracy = 0.75

#train ROC curves
glm.train.roc<-roc(train.omit[,1],glm.train.probs)
lda.train.roc<-roc(train.omit[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.80(logistic)
lda.train.roc$auc #train AUC 0.80(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.omit[,1],glm.test.probs)
lda.test.roc<-roc(test.omit[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.74(logistic)
lda.test.roc$auc #test AUC 0.71(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:7) #Grid for tuning parameter to be cross-validated.
#For random forest, tuning parameter = mtry,
#which is the number of variables available for splitting at each tree node
#(각 트리의 노드 결정 시 설명변수의 후보 수)
#mtry <= total number of explanatory variables in formula.

lasso.Naomit.rf.cv <- train(formula.lasso.Naomit, data = train.omit.factor, method = "rf", 
                           trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#warning occurs because we set mtry over the total number of variables in formula
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

lasso.Naomit.rf.cv #we see mtry=2 is best parameter

plot(lasso.Naomit.rf.cv)  		# Plot the performance of the training models

#confusion matrix
lasso.Naomit.rf.train.pred <- predict(lasso.Naomit.rf.cv,train.omit.factor[,-1])
confusionMatrix(lasso.Naomit.rf.train.pred,train.omit.factor[,1]) #training accuracy 0.88

lasso.Naomit.rf.test.pred <- predict(lasso.Naomit.rf.cv,test.omit.factor[,-1])
confusionMatrix(lasso.Naomit.rf.test.pred,test.omit.factor[,1]) #test accuracy 0.8

#train ROC, AUC
MLeval::evalm(lasso.Naomit.rf.cv)$roc #train AUC = 0.71

#test ROC, AUC
lasso.Naomit.rf.test.probs <- predict(lasso.Naomit.rf.cv,test.omit.factor[,-1],type="prob")

lasso.Naomit.rf.test.ROC <- roc(predictor=lasso.Naomit.rf.test.probs$zero,
               response=test.omit.factor[,1],
               levels=rev(levels(test.omit.factor[,1])))
lasso.Naomit.rf.test.ROC$auc #test AUC 0.71
plot(lasso.Naomit.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE, search="random")

#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#search = "random" means we search grid randomly

lasso.Naomit.svm.cv <- train(formula.lasso.Naomit, data = train.omit.factor, method = "svmRadial", 
                            trControl = control, verbose = F, tuneLength=50, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)
#tuneLength = 50 indicates that randomly set 50 different combinations of (C, sigma) and compare each training AUC.

lasso.Naomit.svm.cv #we see sigma=0.015 and C=0.20 is best.

#confusion matrix
lasso.Naomit.svm.train.pred <- predict(lasso.Naomit.svm.cv,train.omit.factor[,-1])
confusionMatrix(lasso.Naomit.svm.train.pred,train.omit.factor[,1]) #training accuracy 0.80

lasso.Naomit.svm.test.pred <- predict(lasso.Naomit.svm.cv,test.omit.factor[,-1])
confusionMatrix(lasso.Naomit.svm.test.pred,test.omit.factor[,1]) #test accuracy 0.75

#train ROC, AUC
MLeval::evalm(lasso.Naomit.svm.cv)$roc #train AUC = 0.69

#test ROC, AUC
lasso.Naomit.svm.test.probs <- predict(lasso.Naomit.svm.cv,test.omit.factor[,-1],type="prob")

lasso.Naomit.svm.test.ROC <- roc(predictor=lasso.Naomit.svm.test.probs$zero,
                                response=test.omit.factor[,1],
                                levels=rev(levels(test.omit.factor[,1])))
lasso.Naomit.svm.test.ROC$auc #test AUC 0.66
plot(lasso.Naomit.svm.test.ROC)

######C. MI data analysis(stepwise selection)######
MI1<-complete(imp,1)
MI2<-complete(imp,2)
MI3<-complete(imp,3)
MI4<-complete(imp,4)
MI5<-complete(imp,5)

smp_size <- floor(0.7 * nrow(MI1))
set.seed(1)
train_ind <- sample(seq_len(nrow(MI1)), size = smp_size)
train.MI1 <- MI1[train_ind, ]
test.MI1 <- MI1[-train_ind, ]

train.MI2 <- MI2[train_ind, ]
test.MI2 <- MI2[-train_ind, ]

train.MI3 <- MI3[train_ind, ]
test.MI3 <- MI3[-train_ind, ]

train.MI4 <- MI4[train_ind, ]
test.MI4 <- MI4[-train_ind, ]

train.MI5 <- MI5[train_ind, ]
test.MI5 <- MI5[-train_ind, ]

set.seed(1)
MI1.step.fit<-SuperLearner(Y = train.MI1[,1], X = train.MI1[,c(2:47)], family = binomial(),
                           SL.library = "SL.step",cvControl=list(V=5), method="method.AUC")
MI1.step.fit$fitLibrary$SL.step_All$object$call

#glm(formula = Y ~ Menopausal_state + Hypertension + Dyslipidemia + 
#      CA125_initial + FIGO2014 + PLN_status + No_of_harvested_LNs + 
#      No_of_positive_LNs + NAC + Op_type + Small_bowel_and_mesentery + 
#      Liver_parenchyme + Residual_tumor_size_1st_debulking, family = family, 
#    data = X)


set.seed(1)
MI2.step.fit<-SuperLearner(Y = train.MI2[,1], X = train.MI2[,c(2:47)], family = binomial(),
                           SL.library = "SL.step",cvControl=list(V=5), method="method.AUC")
MI2.step.fit$fitLibrary$SL.step_All$object$call

#glm(formula = Y ~ Menopausal_state + Dyslipidemia + CA125_initial + 
#      PLN_status + No_of_harvested_LNs + NAC + Op_type + LN_dissection + 
#      Small_bowel_and_mesentery + Lung + Liver_parenchyme + Residual_tumor_size_1st_debulking + 
#      Total_cycles_of_1st_regimen, family = family, data = X)

set.seed(1)
MI3.step.fit<-SuperLearner(Y = train.MI3[,1], X = train.MI3[,c(2:47)], family = binomial(),
                           SL.library = "SL.step",cvControl=list(V=5), method="method.AUC")
MI3.step.fit$fitLibrary$SL.step_All$object$call
#glm(formula = Y ~ Menopausal_state + Familial_history_of_breast_cancer + 
#      Familial_history_of_gynecologic_cancer + CA125_initial + 
#      PLN_status + PALN_status + No_of_harvested_LNs + No_of_positive_LNs + 
#      NAC + Op_type + LN_dissection + Small_bowel_and_mesentery + 
#      Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen, 
#    family = family, data = X)

set.seed(1)
MI4.step.fit<-SuperLearner(Y = train.MI4[,1], X = train.MI4[,c(2:47)], family = binomial(),
                           SL.library = "SL.step",cvControl=list(V=5), method="method.AUC")
MI4.step.fit$fitLibrary$SL.step_All$object$call
#glm(formula = Y ~ Menopausal_state + Familial_history_of_breast_cancer + 
#      CA125_initial + PLN_status + No_of_harvested_LNs + No_of_positive_LNs + 
#      NAC + Op_type + Bladder_or_Rectal_mucosa + Small_bowel_and_mesentery + 
#      Spleen + Liver_parenchyme + Residual_tumor_size_1st_debulking, 
#    family = family, data = X)

set.seed(1)
MI5.step.fit<-SuperLearner(Y = train.MI5[,1], X = train.MI5[,c(2:47)], family = binomial(),
                           SL.library = "SL.step",cvControl=list(V=5), method="method.AUC")
MI5.step.fit$fitLibrary$SL.step_All$object$call

#glm(formula = Y ~ Menopausal_state + Familial_history_of_breast_cancer + 
#      Familial_history_of_gynecologic_cancer + No_of_family_member_with_gynecologic + 
#      CA125_initial + PLN_status + No_of_harvested_LNs + No_of_positive_LNs + 
#      NAC + Op_type + Large_bowel_resection + Small_bowel_and_mesentery + 
#      Spleen + Lung + Liver_parenchyme + Residual_tumor_size_1st_debulking + 
#      X1st_Regimen, family = family, data = X)

formula.MI1<-"Y ~ Menopausal_state + Hypertension + Dyslipidemia + CA125_initial + FIGO2014 + PLN_status + No_of_harvested_LNs + No_of_positive_LNs + NAC + Op_type + Small_bowel_and_mesentery + Liver_parenchyme + Residual_tumor_size_1st_debulking"
formula.MI2<-"Y ~ Menopausal_state + Dyslipidemia + CA125_initial + PLN_status + No_of_harvested_LNs + NAC + Op_type + LN_dissection + Small_bowel_and_mesentery + Lung + Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen"
formula.MI3<-"Y ~ Menopausal_state + Familial_history_of_breast_cancer + Familial_history_of_gynecologic_cancer + CA125_initial + PLN_status + PALN_status + No_of_harvested_LNs + No_of_positive_LNs + NAC + Op_type + LN_dissection + Small_bowel_and_mesentery + Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen"
formula.MI4<-"Y ~ Menopausal_state + Familial_history_of_breast_cancer + CA125_initial + PLN_status + No_of_harvested_LNs + No_of_positive_LNs + NAC + Op_type + Bladder_or_Rectal_mucosa + Small_bowel_and_mesentery + Spleen + Liver_parenchyme + Residual_tumor_size_1st_debulking"
formula.MI5<-"Y ~ Menopausal_state + Familial_history_of_breast_cancer + Familial_history_of_gynecologic_cancer + No_of_family_member_with_gynecologic + CA125_initial + PLN_status + No_of_harvested_LNs + No_of_positive_LNs + NAC + Op_type + Large_bowel_resection + Small_bowel_and_mesentery + Spleen + Lung + Liver_parenchyme + Residual_tumor_size_1st_debulking + X1st_Regimen"
formula.MI1<-as.formula(formula.MI1)
formula.MI2<-as.formula(formula.MI2)
formula.MI3<-as.formula(formula.MI3)
formula.MI4<-as.formula(formula.MI4)
formula.MI5<-as.formula(formula.MI5)

formulas<-c(formula.MI1,formula.MI2,formula.MI3,formula.MI4,formula.MI5)  
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes)

#vote=5
#CA125_initial 
#Liver_parenchyme
#Menopausal_state
#NAC
#No_of_harvested_LNs
#Op_type
#PLN_status
#Residual_tumor_size_1st_debulking
#Small_bowel_and_mesentery

formula_vote5<-"Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + Liver_parenchyme + Menopausal_state + NAC + No_of_harvested_LNs + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery"
formula_vote5<-as.formula(formula_vote5)

#vote=3, 4 (majority 50% ~ 100%)
#No_of_positive_LNs 
#Familial_history_of_breast_cancer
#test if we should add those 2 variables
#test No_of_positive_LNs
fit.without <- with(imp, lm(Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + Liver_parenchyme + Menopausal_state + NAC + No_of_harvested_LNs + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery))
fit.with <- with(imp, lm(Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + Liver_parenchyme + Menopausal_state + NAC + No_of_harvested_LNs + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery + No_of_positive_LNs))
mice::D1(fit.with, fit.without)
#The p-value is equal to 0.176, so No_of_positive_LNs is not needed in the model.

#test Familial_history_of_breast_cancer
fit.with <- with(imp, lm(Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + Liver_parenchyme + Menopausal_state + NAC + No_of_harvested_LNs + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery + Familial_history_of_breast_cancer))
mice::D1(fit.with, fit.without)
#The p-value is equal to 0.475, so Familial_history_of_breast_cancer is not needed

#select model only for vote 5 variables
formula.step.MI<- formula_vote5

#analyze MI1 data
train.MI1.factor<-train.MI1%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI1.factor<-test.MI1%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.step.MI,data=train.MI1,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI1[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI1))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI1[,1])
mean(glm.train.pred==train.MI1[,1]) #training Accuracy = 0.78

glm.test.probs <- predict(glm.fits,test.MI1[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI1))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI1[,1])
mean(glm.test.pred==test.MI1[,1]) #test Accuracy = 0.79

#LDA
lda.fit <- lda(formula.step.MI, data=train.MI1)

lda.train.pred <- predict(lda.fit, train.MI1)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI1[,1])
mean(lda.train.class==train.MI1[,1]) #training Accuracy = 0.78

lda.test.pred <- predict(lda.fit, test.MI1)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI1[,1])
mean(lda.test.class==test.MI1[,1]) #test Accuracy = 0.78

#train ROC curves
glm.train.roc<-roc(train.MI1[,1],glm.train.probs)
lda.train.roc<-roc(train.MI1[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.76(logistic)
lda.train.roc$auc #train AUC 0.76(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI1[,1],glm.test.probs)
lda.test.roc<-roc(test.MI1[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.68(logistic)
lda.test.roc$auc #test AUC 0.68(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

step.MI.rf.cv <- train(formula.step.MI, data = train.MI1.factor, method = "rf", 
                       trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

step.MI.rf.cv #we see mtry=1 is best parameter

plot(step.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
step.MI.rf.train.pred <- predict(step.MI.rf.cv,train.MI1.factor[,-1])
confusionMatrix(step.MI.rf.train.pred,train.MI1.factor[,1]) 
#0.77
step.MI.rf.test.pred <- predict(step.MI.rf.cv,test.MI1.factor[,-1])
confusionMatrix(step.MI.rf.test.pred,test.MI1.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(step.MI.rf.cv)$roc #train AUC = 0.69

#test ROC, AUC
step.MI.rf.test.probs <- predict(step.MI.rf.cv,test.MI1.factor[,-1],type="prob")

step.MI.rf.test.ROC <- roc(predictor=step.MI.rf.test.probs$zero,
                           response=test.MI1.factor[,1],
                           levels=rev(levels(test.MI1.factor[,1])))
step.MI.rf.test.ROC$auc #test AUC 0.65
plot(step.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

step.MI.svm.cv <- train(formula.step.MI, data = train.MI1.factor, method = "svmRadial", 
                        trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

step.MI.svm.cv #we see sigma=0.094 and C=0.25 is best.

#confusion matrix
step.MI.svm.train.pred <- predict(step.MI.svm.cv,train.MI1.factor[,-1])
confusionMatrix(step.MI.svm.train.pred,train.MI1.factor[,1]) 
#training accuracy 0.80

step.MI.svm.test.pred <- predict(step.MI.svm.cv,test.MI1.factor[,-1])
confusionMatrix(step.MI.svm.test.pred,test.MI1.factor[,1]) 
#test accuracy 0.78

#train ROC, AUC
MLeval::evalm(step.MI.svm.cv)$roc #train AUC = 0.67

#test ROC, AUC
step.MI.svm.test.probs <- predict(step.MI.svm.cv,test.MI1.factor[,-1],type="prob")

step.MI.svm.test.ROC <- roc(predictor=step.MI.svm.test.probs$zero,
                            response=test.MI1.factor[,1],
                            levels=rev(levels(test.MI1.factor[,1])))
step.MI.svm.test.ROC$auc #test AUC 0.59

#analyze MI2 data
train.MI2.factor<-train.MI2%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI2.factor<-test.MI2%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.step.MI,data=train.MI2,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI2[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI2))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI2[,1])
mean(glm.train.pred==train.MI2[,1]) #training Accuracy = 0.80

glm.test.probs <- predict(glm.fits,test.MI2[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI2))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI2[,1])
mean(glm.test.pred==test.MI2[,1]) #test Accuracy = 0.77

#LDA
lda.fit <- lda(formula.step.MI, data=train.MI2)

lda.train.pred <- predict(lda.fit, train.MI2)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI2[,1])
mean(lda.train.class==train.MI2[,1]) #training Accuracy = 0.80

lda.test.pred <- predict(lda.fit, test.MI2)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI2[,1])
mean(lda.test.class==test.MI2[,1]) #test Accuracy = 0.77

#train ROC curves
glm.train.roc<-roc(train.MI2[,1],glm.train.probs)
lda.train.roc<-roc(train.MI2[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.77(logistic)
lda.train.roc$auc #train AUC 0.77(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI2[,1],glm.test.probs)
lda.test.roc<-roc(test.MI2[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.65(logistic)
lda.test.roc$auc #test AUC 0.65(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

step.MI.rf.cv <- train(formula.step.MI, data = train.MI2.factor, method = "rf", 
                       trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

step.MI.rf.cv #we see mtry=1 is best parameter

plot(step.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
step.MI.rf.train.pred <- predict(step.MI.rf.cv,train.MI2.factor[,-1])
confusionMatrix(step.MI.rf.train.pred,train.MI2.factor[,1]) 
#0.76
step.MI.rf.test.pred <- predict(step.MI.rf.cv,test.MI2.factor[,-1])
confusionMatrix(step.MI.rf.test.pred,test.MI2.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(step.MI.rf.cv)$roc #train AUC = 0.69

#test ROC, AUC
step.MI.rf.test.probs <- predict(step.MI.rf.cv,test.MI2.factor[,-1],type="prob")

step.MI.rf.test.ROC <- roc(predictor=step.MI.rf.test.probs$zero,
                           response=test.MI2.factor[,1],
                           levels=rev(levels(test.MI2.factor[,1])))
step.MI.rf.test.ROC$auc #test AUC 0.62
plot(step.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

step.MI.svm.cv <- train(formula.step.MI, data = train.MI2.factor, method = "svmRadial", 
                        trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

step.MI.svm.cv #we see sigma=0.092 and C=2 is best.

#confusion matrix
step.MI.svm.train.pred <- predict(step.MI.svm.cv,train.MI2.factor[,-1])
confusionMatrix(step.MI.svm.train.pred,train.MI2.factor[,1]) 
#training accuracy 0.81

step.MI.svm.test.pred <- predict(step.MI.svm.cv,test.MI2.factor[,-1])
confusionMatrix(step.MI.svm.test.pred,test.MI2.factor[,1]) 
#test accuracy 0.78

#train ROC, AUC
MLeval::evalm(step.MI.svm.cv)$roc #train AUC = 0.68

#test ROC, AUC
step.MI.svm.test.probs <- predict(step.MI.svm.cv,test.MI2.factor[,-1],type="prob")

step.MI.svm.test.ROC <- roc(predictor=step.MI.svm.test.probs$zero,
                            response=test.MI2.factor[,1],
                            levels=rev(levels(test.MI2.factor[,1])))
step.MI.svm.test.ROC$auc #test AUC 0.58

#analyze MI3 data
train.MI3.factor<-train.MI3%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI3.factor<-test.MI3%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.step.MI,data=train.MI3,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI3[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI3))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI3[,1])
mean(glm.train.pred==train.MI3[,1]) #training Accuracy = 0.79

glm.test.probs <- predict(glm.fits,test.MI3[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI3))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI3[,1])
mean(glm.test.pred==test.MI3[,1]) #test Accuracy = 0.79

#LDA
lda.fit <- lda(formula.step.MI, data=train.MI3)

lda.train.pred <- predict(lda.fit, train.MI3)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI3[,1])
mean(lda.train.class==train.MI3[,1]) #training Accuracy = 0.79

lda.test.pred <- predict(lda.fit, test.MI3)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI3[,1])
mean(lda.test.class==test.MI3[,1]) #test Accuracy = 0.79

#train ROC curves
glm.train.roc<-roc(train.MI3[,1],glm.train.probs)
lda.train.roc<-roc(train.MI3[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.85(logistic)
lda.train.roc$auc #train AUC 0.84(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI3[,1],glm.test.probs)
lda.test.roc<-roc(test.MI3[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.68(logistic)
lda.test.roc$auc #test AUC 0.68(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

step.MI.rf.cv <- train(formula.step.MI, data = train.MI3.factor, method = "rf", 
                       trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

step.MI.rf.cv #we see mtry=1

plot(step.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
step.MI.rf.train.pred <- predict(step.MI.rf.cv,train.MI3.factor[,-1])
confusionMatrix(step.MI.rf.train.pred,train.MI3.factor[,1]) 
#0.77
step.MI.rf.test.pred <- predict(step.MI.rf.cv,test.MI3.factor[,-1])
confusionMatrix(step.MI.rf.test.pred,test.MI3.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(step.MI.rf.cv)$roc #train AUC = 0.71

#test ROC, AUC
step.MI.rf.test.probs <- predict(step.MI.rf.cv,test.MI3.factor[,-1],type="prob")

step.MI.rf.test.ROC <- roc(predictor=step.MI.rf.test.probs$zero,
                           response=test.MI3.factor[,1],
                           levels=rev(levels(test.MI3.factor[,1])))
step.MI.rf.test.ROC$auc #test AUC 0.64
plot(step.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

step.MI.svm.cv <- train(formula.step.MI, data = train.MI3.factor, method = "svmRadial", 
                        trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

step.MI.svm.cv #we see sigma=0.09 and C=16 is best.

#confusion matrix
step.MI.svm.train.pred <- predict(step.MI.svm.cv,train.MI3.factor[,-1])
confusionMatrix(step.MI.svm.train.pred,train.MI3.factor[,1]) 
#training accuracy 0.83

step.MI.svm.test.pred <- predict(step.MI.svm.cv,test.MI3.factor[,-1])
confusionMatrix(step.MI.svm.test.pred,test.MI3.factor[,1]) 
#test accuracy 0.80

#train ROC, AUC
MLeval::evalm(step.MI.svm.cv)$roc #train AUC = 0.66

#test ROC, AUC
step.MI.svm.test.probs <- predict(step.MI.svm.cv,test.MI3.factor[,-1],type="prob")

step.MI.svm.test.ROC <- roc(predictor=step.MI.svm.test.probs$zero,
                            response=test.MI3.factor[,1],
                            levels=rev(levels(test.MI3.factor[,1])))
step.MI.svm.test.ROC$auc #test AUC 0.57

#analyze MI4 data
train.MI4.factor<-train.MI4%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI4.factor<-test.MI4%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.step.MI,data=train.MI4,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI4[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI4))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI4[,1])
mean(glm.train.pred==train.MI4[,1]) #training Accuracy = 0.79

glm.test.probs <- predict(glm.fits,test.MI4[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI4))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI4[,1])
mean(glm.test.pred==test.MI4[,1]) #test Accuracy = 0.78

#LDA
lda.fit <- lda(formula.step.MI, data=train.MI4)

lda.train.pred <- predict(lda.fit, train.MI4)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI4[,1])
mean(lda.train.class==train.MI4[,1]) #training Accuracy = 0.79

lda.test.pred <- predict(lda.fit, test.MI4)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI4[,1])
mean(lda.test.class==test.MI4[,1]) #test Accuracy = 0.79

#train ROC curves
glm.train.roc<-roc(train.MI4[,1],glm.train.probs)
lda.train.roc<-roc(train.MI4[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.76(logistic)
lda.train.roc$auc #train AUC 0.76(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI4[,1],glm.test.probs)
lda.test.roc<-roc(test.MI4[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.67(logistic)
lda.test.roc$auc #test AUC 0.67(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

step.MI.rf.cv <- train(formula.step.MI, data = train.MI4.factor, method = "rf", 
                       trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

step.MI.rf.cv #we see mtry=1

plot(step.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
step.MI.rf.train.pred <- predict(step.MI.rf.cv,train.MI4.factor[,-1])
confusionMatrix(step.MI.rf.train.pred,train.MI4.factor[,1]) 
#0.77
step.MI.rf.test.pred <- predict(step.MI.rf.cv,test.MI4.factor[,-1])
confusionMatrix(step.MI.rf.test.pred,test.MI4.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(step.MI.rf.cv)$roc #train AUC = 0.69

#test ROC, AUC
step.MI.rf.test.probs <- predict(step.MI.rf.cv,test.MI4.factor[,-1],type="prob")

step.MI.rf.test.ROC <- roc(predictor=step.MI.rf.test.probs$zero,
                           response=test.MI4.factor[,1],
                           levels=rev(levels(test.MI4.factor[,1])))
step.MI.rf.test.ROC$auc #test AUC 0.63
plot(step.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

step.MI.svm.cv <- train(formula.step.MI, data = train.MI4.factor, method = "svmRadial", 
                        trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

step.MI.svm.cv #we see sigma=0.09 and C=0.5 is best.

#confusion matrix
step.MI.svm.train.pred <- predict(step.MI.svm.cv,train.MI4.factor[,-1])
confusionMatrix(step.MI.svm.train.pred,train.MI4.factor[,1]) 
#training accuracy 0.79

step.MI.svm.test.pred <- predict(step.MI.svm.cv,test.MI4.factor[,-1])
confusionMatrix(step.MI.svm.test.pred,test.MI4.factor[,1]) 
#test accuracy 0.79

#train ROC, AUC
MLeval::evalm(step.MI.svm.cv)$roc #train AUC = 0.64

#test ROC, AUC
step.MI.svm.test.probs <- predict(step.MI.svm.cv,test.MI4.factor[,-1],type="prob")

step.MI.svm.test.ROC <- roc(predictor=step.MI.svm.test.probs$zero,
                            response=test.MI4.factor[,1],
                            levels=rev(levels(test.MI4.factor[,1])))
step.MI.svm.test.ROC$auc #test AUC 0.59

#analyze MI5 data
train.MI5.factor<-train.MI5%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI5.factor<-test.MI5%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.step.MI,data=train.MI5,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI5[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI5))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI5[,1])
mean(glm.train.pred==train.MI5[,1]) #training Accuracy = 0.79

glm.test.probs <- predict(glm.fits,test.MI5[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI5))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI5[,1])
mean(glm.test.pred==test.MI5[,1]) #test Accuracy = 0.77

#LDA
lda.fit <- lda(formula.step.MI, data=train.MI5)

lda.train.pred <- predict(lda.fit, train.MI5)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI5[,1])
mean(lda.train.class==train.MI5[,1]) #training Accuracy = 0.78

lda.test.pred <- predict(lda.fit, test.MI5)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI5[,1])
mean(lda.test.class==test.MI5[,1]) #test Accuracy = 0.77

#train ROC curves
glm.train.roc<-roc(train.MI5[,1],glm.train.probs)
lda.train.roc<-roc(train.MI5[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.77(logistic)
lda.train.roc$auc #train AUC 0.76(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI5[,1],glm.test.probs)
lda.test.roc<-roc(test.MI5[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.66(logistic)
lda.test.roc$auc #test AUC 0.65(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

step.MI.rf.cv <- train(formula.step.MI, data = train.MI5.factor, method = "rf", 
                       trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

step.MI.rf.cv #we see mtry=1

plot(step.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
step.MI.rf.train.pred <- predict(step.MI.rf.cv,train.MI5.factor[,-1])
confusionMatrix(step.MI.rf.train.pred,train.MI5.factor[,1]) 
#0.77
step.MI.rf.test.pred <- predict(step.MI.rf.cv,test.MI5.factor[,-1])
confusionMatrix(step.MI.rf.test.pred,test.MI5.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(step.MI.rf.cv)$roc #train AUC = 0.7

#test ROC, AUC
step.MI.rf.test.probs <- predict(step.MI.rf.cv,test.MI5.factor[,-1],type="prob")

step.MI.rf.test.ROC <- roc(predictor=step.MI.rf.test.probs$zero,
                           response=test.MI5.factor[,1],
                           levels=rev(levels(test.MI5.factor[,1])))
step.MI.rf.test.ROC$auc #test AUC 0.60
plot(step.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

step.MI.svm.cv <- train(formula.step.MI, data = train.MI5.factor, method = "svmRadial", 
                        trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

step.MI.svm.cv #we see sigma=0.09 and C=0.25 is best.

#confusion matrix
step.MI.svm.train.pred <- predict(step.MI.svm.cv,train.MI5.factor[,-1])
confusionMatrix(step.MI.svm.train.pred,train.MI5.factor[,1]) 
#training accuracy 0.79

step.MI.svm.test.pred <- predict(step.MI.svm.cv,test.MI5.factor[,-1])
confusionMatrix(step.MI.svm.test.pred,test.MI5.factor[,1]) 
#test accuracy 0.79

#train ROC, AUC
MLeval::evalm(step.MI.svm.cv)$roc #train AUC = 0.67

#test ROC, AUC
step.MI.svm.test.probs <- predict(step.MI.svm.cv,test.MI5.factor[,-1],type="prob")

step.MI.svm.test.ROC <- roc(predictor=step.MI.svm.test.probs$zero,
                            response=test.MI5.factor[,1],
                            levels=rev(levels(test.MI5.factor[,1])))
step.MI.svm.test.ROC$auc #test AUC 0.59



######D. MI data analysis(lasso selection)######
x1=model.matrix(MI1$Platinum_sensitivity_in_platinum_users1~.,MI1)[,-1]
y1=MI1$Platinum_sensitivity_in_platinum_users1

set.seed(1)
cv.fit=cv.glmnet(x1[train_ind,],y1[train_ind],alpha=1, nfolds = 5, family = "binomial")
predict(cv.fit,type="coefficients",s=cv.fit$lambda.min)
#results : coeff > 0
#Menopausal_state
#CA125_initial
#FIGO2014
#PLN_status
#No_of_positive_LNs 
#NAC
#Op_type
#Small_bowel_and_mesentery
#Diaphragm
#Other_abdominal_tissue_outside_pelvis
#Liver_parenchyme
#Residual_tumor_size_1st_debulking
#Total_cycles_of_1st_regimen
formula.MI1.lasso<-as.formula("Platinum_sensitivity_in_platinum_users1 ~ Menopausal_state + CA125_initial + FIGO2014 + PLN_status + No_of_positive_LNs + NAC + Op_type + Small_bowel_and_mesentery + Diaphragm + Other_abdominal_tissue_outside_pelvis + Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen")

#MI2
x1=model.matrix(MI2$Platinum_sensitivity_in_platinum_users1~.,MI2)[,-1]
y1=MI2$Platinum_sensitivity_in_platinum_users1

set.seed(1)
cv.fit=cv.glmnet(x1[train_ind,],y1[train_ind],alpha=1, nfolds = 5, family = "binomial")
predict(cv.fit,type="coefficients",s=cv.fit$lambda.min)
#Menopausal_state
#CA125_initial 
#FIGO2014
#PLN_status
#NAC
#Op_type
#Small_bowel_and_mesentery
#Diaphragm
#Lung
#Liver_parenchyme
#Residual_tumor_size_1st_debulking
#Total_cycles_of_1st_regimen

formula.MI2.lasso<-as.formula("Platinum_sensitivity_in_platinum_users1 ~ Menopausal_state + CA125_initial + FIGO2014 + PLN_status + NAC + Op_type + Small_bowel_and_mesentery + Diaphragm + Lung + Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen")

#MI3
x1=model.matrix(MI3$Platinum_sensitivity_in_platinum_users1~.,MI3)[,-1]
y1=MI3$Platinum_sensitivity_in_platinum_users1

set.seed(1)
cv.fit=cv.glmnet(x1[train_ind,],y1[train_ind],alpha=1, nfolds = 5, family = "binomial")
predict(cv.fit,type="coefficients",s=cv.fit$lambda.min)

#Menopausal_state
#Diabetes
#Familial_history_of_gynecologic_cancer
#CA125_initial 
#FIGO2014
#PLN_status
#NAC
#Op_type
#Small_bowel_and_mesentery
#Diaphragm
#Other_abdominal_tissue_outside_pelvis
#Liver_parenchyme
#Residual_tumor_size_1st_debulking
#Total_cycles_of_1st_regimen

formula.MI3.lasso<-as.formula("Platinum_sensitivity_in_platinum_users1 ~ Menopausal_state + Diabetes + Familial_history_of_gynecologic_cancer + CA125_initial + FIGO2014 + PLN_status + NAC + Op_type + Small_bowel_and_mesentery + Diaphragm + Other_abdominal_tissue_outside_pelvis + Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen")

#MI4
x1=model.matrix(MI4$Platinum_sensitivity_in_platinum_users1~.,MI4)[,-1]
y1=MI4$Platinum_sensitivity_in_platinum_users1

set.seed(1)
cv.fit=cv.glmnet(x1[train_ind,],y1[train_ind],alpha=1, nfolds = 5, family = "binomial")
predict(cv.fit,type="coefficients",s=cv.fit$lambda.min)

#Menopausal_state
#CA125_initial
#FIGO2014
#PLN_status
#No_of_positive_LNs 
#NAC
#Op_type
#Bladder_or_Rectal_mucosa
#Small_bowel_and_mesentery
#Diaphragm
#Spleen
#Other_abdominal_tissue_outside_pelvis
#pleural_effusion
#Liver_parenchyme
#Residual_tumor_size_1st_debulking
#Total_cycles_of_1st_regimen

formula.MI4.lasso<-as.formula("Platinum_sensitivity_in_platinum_users1 ~ Menopausal_state + CA125_initial + FIGO2014 + PLN_status + No_of_positive_LNs + NAC + Op_type + Bladder_or_Rectal_mucosa + Small_bowel_and_mesentery + Diaphragm + Spleen + Other_abdominal_tissue_outside_pelvis + pleural_effusion + Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen")

x1=model.matrix(MI5$Platinum_sensitivity_in_platinum_users1~.,MI5)[,-1]
y1=MI5$Platinum_sensitivity_in_platinum_users1

set.seed(1)
cv.fit=cv.glmnet(x1[train_ind,],y1[train_ind],alpha=1, nfolds = 5, family = "binomial")
predict(cv.fit,type="coefficients",s=cv.fit$lambda.min)

#Menopausal_state
#CA125_initial
#FIGO2014
#PLN_status
#No_of_positive_LNs
#NAC
#Op_type
#Small_bowel_and_mesentery
#Spleen
#pleural_effusion
#Liver_parenchyme
#Residual_tumor_size_1st_debulking
#Total_cycles_of_1st_regimen

formula.MI5.lasso<-as.formula("Platinum_sensitivity_in_platinum_users1 ~ Menopausal_state + CA125_initial + FIGO2014 + PLN_status + No_of_positive_LNs + NAC + Op_type + Small_bowel_and_mesentery + Spleen + pleural_effusion + Liver_parenchyme + Residual_tumor_size_1st_debulking + Total_cycles_of_1st_regimen")

formulas<-c(formula.MI1.lasso,formula.MI2.lasso,formula.MI3.lasso,formula.MI4.lasso,formula.MI5.lasso)  
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes)

#vote=5
#CA125_initial 
#FIGO2014 
#Liver_parenchyme
#Menopausal_state
#NAC
#Op_type
#PLN_status 
#Residual_tumor_size_1st_debulking
#Small_bowel_and_mesentery
#Total_cycles_of_1st_regimen

formula_vote5_lasso<-as.formula("Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + FIGO2014 + Liver_parenchyme + Menopausal_state + NAC + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery + Total_cycles_of_1st_regimen")

#vote=3,4 (over 50%)
#Diaphragm 
#Other_abdominal_tissue_outside_pelvis
#No_of_positive_LNs

#test if we should add those 3 variables
#test Diaphragm 
fit.without <- with(imp, lm(Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + FIGO2014 + Liver_parenchyme + Menopausal_state + NAC + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery + Total_cycles_of_1st_regimen))
fit.with <- with(imp, lm(Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + FIGO2014 + Liver_parenchyme + Menopausal_state + NAC + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery + Total_cycles_of_1st_regimen + Diaphragm))
mice::D1(fit.with, fit.without)
#The p-value is equal to 0.75, so Diaphragm is not needed in the model.

#test Other_abdominal_tissue_outside_pelvis
fit.with <- with(imp, lm(Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + FIGO2014 + Liver_parenchyme + Menopausal_state + NAC + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery + Total_cycles_of_1st_regimen + Diaphragm + Other_abdominal_tissue_outside_pelvis))
mice::D1(fit.with, fit.without)
#The p-value is equal to 0.56, so Other_abdominal_tissue_outside_pelvis is not needed

#test No_of_positive_LNs
fit.with <- with(imp, lm(Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + FIGO2014 + Liver_parenchyme + Menopausal_state + NAC + Op_type + PLN_status + Residual_tumor_size_1st_debulking + Small_bowel_and_mesentery + Total_cycles_of_1st_regimen + Diaphragm + No_of_positive_LNs))
mice::D1(fit.with, fit.without)
#The p-value is equal to 0.80, so No_of_positive_LNs is not needed

formula.lasso.MI<-formula_vote5_lasso



#analyze MI1 data
train.MI1.factor<-train.MI1%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI1.factor<-test.MI1%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.lasso.MI,data=train.MI1,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI1[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI1))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI1[,1])
mean(glm.train.pred==train.MI1[,1]) #training Accuracy = 0.78

glm.test.probs <- predict(glm.fits,test.MI1[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI1))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI1[,1])
mean(glm.test.pred==test.MI1[,1]) #test Accuracy = 0.78

#LDA
lda.fit <- lda(formula.lasso.MI, data=train.MI1)

lda.train.pred <- predict(lda.fit, train.MI1)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI1[,1])
mean(lda.train.class==train.MI1[,1]) #training Accuracy = 0.78

lda.test.pred <- predict(lda.fit, test.MI1)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI1[,1])
mean(lda.test.class==test.MI1[,1]) #test Accuracy = 0.78

#train ROC curves
glm.train.roc<-roc(train.MI1[,1],glm.train.probs)
lda.train.roc<-roc(train.MI1[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.85(logistic)
lda.train.roc$auc #train AUC 0.84(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI1[,1],glm.test.probs)
lda.test.roc<-roc(test.MI1[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.66(logistic)
lda.test.roc$auc #test AUC 0.66(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

lasso.MI.rf.cv <- train(formula.lasso.MI, data = train.MI1.factor, method = "rf", 
                        trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

lasso.MI.rf.cv #we see mtry=2 is best parameter

plot(lasso.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
lasso.MI.rf.train.pred <- predict(lasso.MI.rf.cv,train.MI1.factor[,-1])
confusionMatrix(lasso.MI.rf.train.pred,train.MI1.factor[,1]) 
#0.80
lasso.MI.rf.test.pred <- predict(lasso.MI.rf.cv,test.MI1.factor[,-1])
confusionMatrix(lasso.MI.rf.test.pred,test.MI1.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(lasso.MI.rf.cv)$roc #train AUC = 0.7

#test ROC, AUC
lasso.MI.rf.test.probs <- predict(lasso.MI.rf.cv,test.MI1.factor[,-1],type="prob")

lasso.MI.rf.test.ROC <- roc(predictor=lasso.MI.rf.test.probs$zero,
                            response=test.MI1.factor[,1],
                            levels=rev(levels(test.MI1.factor[,1])))
lasso.MI.rf.test.ROC$auc #test AUC 0.63
plot(lasso.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

lasso.MI.svm.cv <- train(formula.lasso.MI, data = train.MI1.factor, method = "svmRadial", 
                         trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

lasso.MI.svm.cv #we see sigma=0.068 and C=0.5 is best.

#confusion matrix
lasso.MI.svm.train.pred <- predict(lasso.MI.svm.cv,train.MI1.factor[,-1])
confusionMatrix(lasso.MI.svm.train.pred,train.MI1.factor[,1]) 
#training accuracy 0.78

lasso.MI.svm.test.pred <- predict(lasso.MI.svm.cv,test.MI1.factor[,-1])
confusionMatrix(lasso.MI.svm.test.pred,test.MI1.factor[,1]) 
#test accuracy 0.78

#train ROC, AUC
MLeval::evalm(lasso.MI.svm.cv)$roc #train AUC = 0.63

#test ROC, AUC
lasso.MI.svm.test.probs <- predict(lasso.MI.svm.cv,test.MI1.factor[,-1],type="prob")

lasso.MI.svm.test.ROC <- roc(predictor=lasso.MI.svm.test.probs$zero,
                             response=test.MI1.factor[,1],
                             levels=rev(levels(test.MI1.factor[,1])))
lasso.MI.svm.test.ROC$auc #test AUC 0.58

#analyze MI2 data
train.MI2.factor<-train.MI2%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI2.factor<-test.MI2%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.lasso.MI,data=train.MI2,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI2[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI2))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI2[,1])
mean(glm.train.pred==train.MI2[,1]) #training Accuracy = 0.79

glm.test.probs <- predict(glm.fits,test.MI2[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI2))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI2[,1])
mean(glm.test.pred==test.MI2[,1]) #test Accuracy = 0.78

#LDA
lda.fit <- lda(formula.lasso.MI, data=train.MI2)

lda.train.pred <- predict(lda.fit, train.MI2)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI2[,1])
mean(lda.train.class==train.MI2[,1]) #training Accuracy = 0.79

lda.test.pred <- predict(lda.fit, test.MI2)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI2[,1])
mean(lda.test.class==test.MI2[,1]) #test Accuracy = 0.75

#train ROC curves
glm.train.roc<-roc(train.MI2[,1],glm.train.probs)
lda.train.roc<-roc(train.MI2[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.85(logistic)
lda.train.roc$auc #train AUC 0.84(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI2[,1],glm.test.probs)
lda.test.roc<-roc(test.MI2[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.64(logistic)
lda.test.roc$auc #test AUC 0.63(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

lasso.MI.rf.cv <- train(formula.lasso.MI, data = train.MI2.factor, method = "rf", 
                        trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

lasso.MI.rf.cv #we see mtry=2 is best parameter

plot(lasso.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
lasso.MI.rf.train.pred <- predict(lasso.MI.rf.cv,train.MI2.factor[,-1])
confusionMatrix(lasso.MI.rf.train.pred,train.MI2.factor[,1]) 
#0.76
lasso.MI.rf.test.pred <- predict(lasso.MI.rf.cv,test.MI2.factor[,-1])
confusionMatrix(lasso.MI.rf.test.pred,test.MI2.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(lasso.MI.rf.cv)$roc #train AUC = 0.7

#test ROC, AUC
lasso.MI.rf.test.probs <- predict(lasso.MI.rf.cv,test.MI2.factor[,-1],type="prob")

lasso.MI.rf.test.ROC <- roc(predictor=lasso.MI.rf.test.probs$zero,
                            response=test.MI2.factor[,1],
                            levels=rev(levels(test.MI2.factor[,1])))
lasso.MI.rf.test.ROC$auc #test AUC 0.60
plot(lasso.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

lasso.MI.svm.cv <- train(formula.lasso.MI, data = train.MI2.factor, method = "svmRadial", 
                         trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

lasso.MI.svm.cv #we see sigma=0.086 and C=0.5 is best.

#confusion matrix
lasso.MI.svm.train.pred <- predict(lasso.MI.svm.cv,train.MI2.factor[,-1])
confusionMatrix(lasso.MI.svm.train.pred,train.MI2.factor[,1]) 
#training accuracy 0.86

lasso.MI.svm.test.pred <- predict(lasso.MI.svm.cv,test.MI2.factor[,-1])
confusionMatrix(lasso.MI.svm.test.pred,test.MI2.factor[,1]) 
#test accuracy 0.78

#train ROC, AUC
MLeval::evalm(lasso.MI.svm.cv)$roc #train AUC = 0.64

#test ROC, AUC
lasso.MI.svm.test.probs <- predict(lasso.MI.svm.cv,test.MI2.factor[,-1],type="prob")

lasso.MI.svm.test.ROC <- roc(predictor=lasso.MI.svm.test.probs$zero,
                             response=test.MI2.factor[,1],
                             levels=rev(levels(test.MI2.factor[,1])))
lasso.MI.svm.test.ROC$auc #test AUC 0.53

#analyze MI3 data
train.MI3.factor<-train.MI3%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI3.factor<-test.MI3%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.lasso.MI,data=train.MI3,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI3[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI3))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI3[,1])
mean(glm.train.pred==train.MI3[,1]) #training Accuracy = 0.78

glm.test.probs <- predict(glm.fits,test.MI3[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI3))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI3[,1])
mean(glm.test.pred==test.MI3[,1]) #test Accuracy = 0.78

#LDA
lda.fit <- lda(formula.lasso.MI, data=train.MI3)

lda.train.pred <- predict(lda.fit, train.MI3)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI3[,1])
mean(lda.train.class==train.MI3[,1]) #training Accuracy = 0.78

lda.test.pred <- predict(lda.fit, test.MI3)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI3[,1])
mean(lda.test.class==test.MI3[,1]) #test Accuracy = 0.78

#train ROC curves
glm.train.roc<-roc(train.MI3[,1],glm.train.probs)
lda.train.roc<-roc(train.MI3[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.85(logistic)
lda.train.roc$auc #train AUC 0.84(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI3[,1],glm.test.probs)
lda.test.roc<-roc(test.MI3[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.64(logistic)
lda.test.roc$auc #test AUC 0.63(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

lasso.MI.rf.cv <- train(formula.lasso.MI, data = train.MI3.factor, method = "rf", 
                        trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

lasso.MI.rf.cv #we see mtry=2

plot(lasso.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
lasso.MI.rf.train.pred <- predict(lasso.MI.rf.cv,train.MI3.factor[,-1])
confusionMatrix(lasso.MI.rf.train.pred,train.MI3.factor[,1]) 
#0.81
lasso.MI.rf.test.pred <- predict(lasso.MI.rf.cv,test.MI3.factor[,-1])
confusionMatrix(lasso.MI.rf.test.pred,test.MI3.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(lasso.MI.rf.cv)$roc #train AUC = 0.7

#test ROC, AUC
lasso.MI.rf.test.probs <- predict(lasso.MI.rf.cv,test.MI3.factor[,-1],type="prob")

lasso.MI.rf.test.ROC <- roc(predictor=lasso.MI.rf.test.probs$zero,
                            response=test.MI3.factor[,1],
                            levels=rev(levels(test.MI3.factor[,1])))
lasso.MI.rf.test.ROC$auc #test AUC 0.62
plot(lasso.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

lasso.MI.svm.cv <- train(formula.lasso.MI, data = train.MI3.factor, method = "svmRadial", 
                         trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

lasso.MI.svm.cv #we see sigma=0.08 and C=0.25 is best.

#confusion matrix
lasso.MI.svm.train.pred <- predict(lasso.MI.svm.cv,train.MI3.factor[,-1])
confusionMatrix(lasso.MI.svm.train.pred,train.MI3.factor[,1]) 
#training accuracy 0.80

lasso.MI.svm.test.pred <- predict(lasso.MI.svm.cv,test.MI3.factor[,-1])
confusionMatrix(lasso.MI.svm.test.pred,test.MI3.factor[,1]) 
#test accuracy 0.78

#train ROC, AUC
MLeval::evalm(lasso.MI.svm.cv)$roc #train AUC = 0.65

#test ROC, AUC
lasso.MI.svm.test.probs <- predict(lasso.MI.svm.cv,test.MI3.factor[,-1],type="prob")

lasso.MI.svm.test.ROC <- roc(predictor=lasso.MI.svm.test.probs$zero,
                             response=test.MI3.factor[,1],
                             levels=rev(levels(test.MI3.factor[,1])))
lasso.MI.svm.test.ROC$auc #test AUC 0.57

#analyze MI4 data
train.MI4.factor<-train.MI4%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI4.factor<-test.MI4%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.lasso.MI,data=train.MI4,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI4[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI4))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI4[,1])
mean(glm.train.pred==train.MI4[,1]) #training Accuracy = 0.78

glm.test.probs <- predict(glm.fits,test.MI4[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI4))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI4[,1])
mean(glm.test.pred==test.MI4[,1]) #test Accuracy = 0.78

#LDA
lda.fit <- lda(formula.lasso.MI, data=train.MI4)

lda.train.pred <- predict(lda.fit, train.MI4)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI4[,1])
mean(lda.train.class==train.MI4[,1]) #training Accuracy = 0.78

lda.test.pred <- predict(lda.fit, test.MI4)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI4[,1])
mean(lda.test.class==test.MI4[,1]) #test Accuracy = 0.76

#train ROC curves
glm.train.roc<-roc(train.MI4[,1],glm.train.probs)
lda.train.roc<-roc(train.MI4[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.76(logistic)
lda.train.roc$auc #train AUC 0.76(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI4[,1],glm.test.probs)
lda.test.roc<-roc(test.MI4[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.65(logistic)
lda.test.roc$auc #test AUC 0.65(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

lasso.MI.rf.cv <- train(formula.lasso.MI, data = train.MI4.factor, method = "rf", 
                        trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

lasso.MI.rf.cv #we see mtry=1

plot(lasso.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
lasso.MI.rf.train.pred <- predict(lasso.MI.rf.cv,train.MI4.factor[,-1])
confusionMatrix(lasso.MI.rf.train.pred,train.MI4.factor[,1]) 
#0.76
lasso.MI.rf.test.pred <- predict(lasso.MI.rf.cv,test.MI4.factor[,-1])
confusionMatrix(lasso.MI.rf.test.pred,test.MI4.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(lasso.MI.rf.cv)$roc #train AUC = 0.68

#test ROC, AUC
lasso.MI.rf.test.probs <- predict(lasso.MI.rf.cv,test.MI4.factor[,-1],type="prob")

lasso.MI.rf.test.ROC <- roc(predictor=lasso.MI.rf.test.probs$zero,
                            response=test.MI4.factor[,1],
                            levels=rev(levels(test.MI4.factor[,1])))
lasso.MI.rf.test.ROC$auc #test AUC 0.65
plot(lasso.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

lasso.MI.svm.cv <- train(formula.lasso.MI, data = train.MI4.factor, method = "svmRadial", 
                         trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

lasso.MI.svm.cv #we see sigma=0.08 and C=0.25 is best.

#confusion matrix
lasso.MI.svm.train.pred <- predict(lasso.MI.svm.cv,train.MI4.factor[,-1])
confusionMatrix(lasso.MI.svm.train.pred,train.MI4.factor[,1]) 
#training accuracy 0.79

lasso.MI.svm.test.pred <- predict(lasso.MI.svm.cv,test.MI4.factor[,-1])
confusionMatrix(lasso.MI.svm.test.pred,test.MI4.factor[,1]) 
#test accuracy 0.78

#train ROC, AUC
MLeval::evalm(lasso.MI.svm.cv)$roc #train AUC = 0.63

#test ROC, AUC
lasso.MI.svm.test.probs <- predict(lasso.MI.svm.cv,test.MI4.factor[,-1],type="prob")

lasso.MI.svm.test.ROC <- roc(predictor=lasso.MI.svm.test.probs$zero,
                             response=test.MI4.factor[,1],
                             levels=rev(levels(test.MI4.factor[,1])))
lasso.MI.svm.test.ROC$auc #test AUC 0.58

#analyze MI5 data
train.MI5.factor<-train.MI5%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))
test.MI5.factor<-test.MI5%>% 
  dplyr::mutate(Platinum_sensitivity_in_platinum_users1 = factor(Platinum_sensitivity_in_platinum_users1, 
                                                                 labels = c("zero","one")))

#####1. logistic and LDA#####
#logistic
glm.fits <- glm(formula.lasso.MI,data=train.MI5,family=binomial)
summary(glm.fits)

glm.train.probs <- predict(glm.fits,train.MI5[,-1],type="response")
glm.train.pred <- rep(0,nrow(train.MI5))
glm.train.pred[glm.train.probs>.5] <- 1
#train set confusion matrix
table(glm.train.pred,train.MI5[,1])
mean(glm.train.pred==train.MI5[,1]) #training Accuracy = 0.79

glm.test.probs <- predict(glm.fits,test.MI5[,-1],type="response")
glm.test.pred <- rep(0,nrow(test.MI5))
glm.test.pred[glm.test.probs>.5] <- 1
#test set confusion matrix
table(glm.test.pred,test.MI5[,1])
mean(glm.test.pred==test.MI5[,1]) #test Accuracy = 0.78

#LDA
lda.fit <- lda(formula.lasso.MI, data=train.MI5)

lda.train.pred <- predict(lda.fit, train.MI5)
lda.train.class <- lda.train.pred$class
table(lda.train.class,train.MI5[,1])
mean(lda.train.class==train.MI5[,1]) #training Accuracy = 0.78

lda.test.pred <- predict(lda.fit, test.MI5)
lda.test.class <- lda.test.pred$class
table(lda.test.class,test.MI5[,1])
mean(lda.test.class==test.MI5[,1]) #test Accuracy = 0.75

#train ROC curves
glm.train.roc<-roc(train.MI5[,1],glm.train.probs)
lda.train.roc<-roc(train.MI5[,1],lda.train.pred$posterior[,2])
glm.train.roc <-glm.train.roc[c("specificities","sensitivities","thresholds","auc")]
lda.train.roc <-lda.train.roc[c("specificities","sensitivities","thresholds","auc")]

glm.train.roc$auc #train AUC 0.76(logistic)
lda.train.roc$auc #train AUC 0.76(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.train.roc$specificities),y=glm.train.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.train.roc$sensitivities,x=(1-lda.train.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#test ROC curves
glm.test.roc<-roc(test.MI5[,1],glm.test.probs)
lda.test.roc<-roc(test.MI5[,1],lda.test.pred$posterior[,2])
glm.test.roc <-glm.test.roc[c("specificities","sensitivities","thresholds","auc")]
lda.test.roc <-lda.test.roc[c("specificities","sensitivities","thresholds","auc")]

glm.test.roc$auc #test AUC 0.65(logistic)
lda.test.roc$auc #test AUC 0.64(LDA)

par(mfrow=c(1,1))
plot(x=(1-glm.test.roc$specificities),y=glm.test.roc$sensitivities,
     xlim=c(0,1),ylim=c(0,1),type="l",
     xlab="",ylab="")
par(new=TRUE) 
plot(y=lda.test.roc$sensitivities,x=(1-lda.test.roc$specificities),
     col = "red",type="l",
     xlab="1-Specificity",ylab = "Sensitivity",main = "ROC curve")

#####2. Random forest#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.

customGrid <- expand.grid(mtry = 1:10) #maximum = apprix. half of total number of selected variables

lasso.MI.rf.cv <- train(formula.lasso.MI, data = train.MI5.factor, method = "rf", 
                        trControl = control, verbose = F, tuneGrid=customGrid, metric="ROC")
#metric="ROC" indicates that we use AUC to pick the best model
#method="rf" indicates that we use Random forest.
#tuneGrid=customGrid indicates that we use grid = customGrid for searching best parameter.

lasso.MI.rf.cv #we see mtry=1

plot(lasso.MI.rf.cv)  		# Plot the performance of the training models

#confusion matrix
lasso.MI.rf.train.pred <- predict(lasso.MI.rf.cv,train.MI5.factor[,-1])
confusionMatrix(lasso.MI.rf.train.pred,train.MI5.factor[,1]) 
#0.76
lasso.MI.rf.test.pred <- predict(lasso.MI.rf.cv,test.MI5.factor[,-1])
confusionMatrix(lasso.MI.rf.test.pred,test.MI5.factor[,1]) 
#0.80

#train ROC, AUC
MLeval::evalm(lasso.MI.rf.cv)$roc #train AUC = 0.69

#test ROC, AUC
lasso.MI.rf.test.probs <- predict(lasso.MI.rf.cv,test.MI5.factor[,-1],type="prob")

lasso.MI.rf.test.ROC <- roc(predictor=lasso.MI.rf.test.probs$zero,
                            response=test.MI5.factor[,1],
                            levels=rev(levels(test.MI5.factor[,1])))
lasso.MI.rf.test.ROC$auc #test AUC 0.63
plot(lasso.MI.rf.test.ROC)
#####3. SWM#####
set.seed(1)
control <- trainControl(method = "cv", number = 10, classProbs=TRUE, 
                        summaryFunction = twoClassSummary, savePredictions = TRUE)
#For SVM, tuning parameter is sigma and C(cost).
#C = how much you care about misclassified points.
#large C value means you care more about classifying all of the training points correctly 
#than leaving wiggle room for future data.
#sigma = the scale parameter for radial basis functions.
#in this case, we do not search randomly(it gives better model after training)

lasso.MI.svm.cv <- train(formula.lasso.MI, data = train.MI5.factor, method = "svmRadial", 
                         trControl = control, verbose = F, tuneLength=15, metric="ROC")
#method="svmRadial" indicates that we use SVM kernel K(x, y) = exp (−γ||x−y||2)

lasso.MI.svm.cv #we see sigma=0.08 and C=1 is best.

#confusion matrix
lasso.MI.svm.train.pred <- predict(lasso.MI.svm.cv,train.MI5.factor[,-1])
confusionMatrix(lasso.MI.svm.train.pred,train.MI5.factor[,1]) 
#training accuracy 0.80

lasso.MI.svm.test.pred <- predict(lasso.MI.svm.cv,test.MI5.factor[,-1])
confusionMatrix(lasso.MI.svm.test.pred,test.MI5.factor[,1]) 
#test accuracy 0.79

#train ROC, AUC
MLeval::evalm(lasso.MI.svm.cv)$roc #train AUC = 0.63

#test ROC, AUC
lasso.MI.svm.test.probs <- predict(lasso.MI.svm.cv,test.MI5.factor[,-1],type="prob")

lasso.MI.svm.test.ROC <- roc(predictor=lasso.MI.svm.test.probs$zero,
                             response=test.MI5.factor[,1],
                             levels=rev(levels(test.MI5.factor[,1])))
lasso.MI.svm.test.ROC$auc #test AUC 0.57

