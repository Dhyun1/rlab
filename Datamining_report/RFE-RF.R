library(randomForest)

customGrid <- expand.grid(mtry = 1:10)
control <- trainControl(method = "cv", number = 10)
rfe.rf.cv <- train(formula.rfe, data = train, method = "rf", 
                  trControl = fitControl, verbose = F, tuneGrid = customGrid)
rfe.rf.cv$finalModel$mtry #mtry=4

rfe.rf.fit<-randomForest(formula.rfe, data=train, mtry=4,importance=TRUE)
importance(rf.best.fit)
#varImpPlot(rf.best.fit)

rfe.rf.train.pred.class <- predict(rfe.rf.fit, train[,-1], type="class")
confusionMatrix(rfe.rf.train.pred.class, reference = train[,1])

rfe.rf.test.pred.class <- predict(rfe.rf.fit, test[,-1], type="class")
confusionMatrix(rfe.rf.test.pred.class, reference = test[,1])

rfe.rf.train.pred.prob <- predict(rfe.rf.fit, test[,-1], type="prob")
rfe.rf.train.pred.values <- rfe.rf.train.pred.prob[,2]
rfe.rf.train.pred <- prediction(rfe.rf.train.pred.values, test[,1])

rfe.rf.test.pred.prob <- predict(rfe.rf.fit, test[,-1], type="prob")
rfe.rf.test.pred.values <- rfe.rf.test.pred.prob[,2]
rfe.rf.test.pred <- prediction(rfe.rf.test.pred.values, test[,1])

par(mfrow=c(1,2))
plot.roc.curve(rfe.rf.train.pred, title.text = "RF ROC Curve")
plot.roc.curve(rfe.rf.test.pred, title.text = "RF ROC Curve")
