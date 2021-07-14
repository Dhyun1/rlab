library(caret)
###########################################################
control <- trainControl(method = "cv", number = 10)
rfe.svm.cv <- train(formula.rfe, data = train, method = "svmRadial", 
                   trControl = fitControl, verbose = F, tuneLength=5, preProc = c("center","scale"))
rfe.svm.cv$bestTune

rfe.svm.fit<-svm(formula.RFE, data=train, 
                  kernel="radial",gamma=0.0809, cost=4, decision.values=T)

rfe.svm.train.pred <- predict(rfe.svm.fit, train[,-1], decision.value=T)
rfe.svm.test.pred <- predict(rfe.svm.fit, test[,-1], decision.value=T)
confusionMatrix(data=rfe.svm.train.pred, reference=train[,1])
confusionMatrix(data=rfe.svm.test.pred, reference=test[,1])

rfe.svm.train.values <- attributes(rfe.svm.train.pred)$decision.values
rfe.svm.train.pred <- prediction(rfe.svm.train.values, train[,1])
rfe.svm.test.values <- attributes(rfe.svm.test.pred)$decision.values
rfe.svm.test.pred <- prediction(rfe.svm.test.values, test[,1])

par(mfrow=c(1,2))
plot.roc.curve(rfe.svm.train.pred, title.text = "SVM ROC Curve")
plot.roc.curve(rfe.svm.test.pred, title.text = "SVM ROC Curve")
