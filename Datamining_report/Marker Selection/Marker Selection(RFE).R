library(caret)
library(randomForest)
#7:3 split
smp_size <- floor(0.7 * nrow(MI))
set.seed(1)
train_ind <- sample(seq_len(nrow(MI)), size = smp_size)
train <- MI[train_ind, ]
test <- MI[-train_ind, ]
str(train)
str(test)
names(train)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(train[,c(-1,-13,-14,-15)], train[,1], sizes=c(5:15), rfeControl=control)

predictors(results)
print(results)
plot(results, type=c("g", "o"))

formula.rfe <- "Platinum_sensitivity_in_platinum_users1 ~ No_of_harvested_LNs + BMI + Hb + CA125_initial + Age + Height + Weight + No_of_harvested_LNs + Other_abdominal_tissue_outside_pelvis"
formula.rfe <- as.formula(formula.rfe)
