library(caret)
set.seed(1)
control <- trainControl(method = "cv", number = 10)
#summaryFunction= twoClassSummary indicates that we use AUC to pick the best model.
seq(0.1,1.0,by=0.02)
customGrid <- expand.grid(span = seq(0.1,1.0,by=0.02))
formula<-as.formula("비수도권.신규확진자 ~ Days_after_First_Cases")

loess.cv <- train(formula, data = train, method = "gamLoess", 
                           trControl = control, verbose = F, tuneGrid=customGrid)