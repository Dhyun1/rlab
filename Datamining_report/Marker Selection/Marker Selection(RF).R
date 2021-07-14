rf_grid <- expand.grid(mtry = 1:10)
fitControl <- trainControl(method = "cv", number = 10)
rf_fit <- train(formula.init, data = train, method = "rf", trControl = fitControl, 
                verbose = FALSE, tuneGrid = rf_grid)
importance <- varImp(rf_fit, scale=FALSE)
plot(importance, cex.lab=0.5,top=10)

#Variable Selection with RF
formula.RF<-"Platinum_sensitivity_in_platinum_users1 ~ CA125_initial + Age + Weight + Hb + Height + No_of_harvested_LNs + No_of_positive_LNs + NAC + Small_bowel_and_mesentery"
formula.RF<-as.formula(formula.RF)