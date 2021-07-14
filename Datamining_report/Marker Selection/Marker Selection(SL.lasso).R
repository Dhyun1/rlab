library(SuperLearner)

###na.omit data
set.seed(1)
lasso.omit.fit<-SuperLearner(Y = train.omit[,1],
                   X = train.omit[,c(2:47)], family = binomial(),
                   cvControl=list(V=5),
                   SL.library = "SL.glmnet")
lasso.omit.obj = lasso.omit.fit$fitLibrary$SL.glmnet_All$object
coef(lasso.omit.obj, s = "lambda.min")
coef(lasso.omit.obj, s = "lambda.1se")
# 결과 (0인 항 제외)
#(Intercept)                                            -2.476952156
#Hypertension1                                           0.183791864
#CA125_initial                                           0.093052129
#PLN_status1                                             0.282917033
#No_of_harvested_LNs                                    -0.006032203
#NAC1                                                    0.955220663
#Liver_surface3                                          0.219917003
#pleural_effusion1                                       0.043515322
#Residual_tumor_size_1st_debulking1                      0.454240344

###MICE data
set.seed(1)
lasso.MI.fit<-SuperLearner(Y = train[,1],
                   X = train[,c(2:47)], family = binomial(),
                   cvControl=list(V=5),
                   SL.library = "SL.glmnet")
lasso.MI.obj = lasso.MI.fit$fitLibrary$SL.glmnet_All$object
coef(lasso.MI.obj, s = "lambda.min")  # 51개
coef(lasso.MI.obj, s = "lambda.1se")  # 31개

#(Intercept)                                            -3.972073947
#Age                                                     0.005747400
#Menopausal_state1                                       0.431174869
#Hypertension1                                           0.182081383
#Dyslipidemia1                                          -0.425089755
#No_of_family_member_with_breast_cancer_upto_2nd_degree -0.060159230
#Familial_history_of_gynecologic_cancer1                -0.330185988
#CA125_initial                                           0.113059236
#Hb                                                     -0.005110118
#FIGO20141                                               0.545191485
#PLN_status1                                             0.551474425
#PALN_status1                                            0.041853542
#No_of_harvested_LNs                                    -0.005314792
#No_of_positive_LNs                                      0.012997524
#NAC1                                                    0.744205516
#Op_type2                                               -0.357490818
#Other_pelvic_tissue_except_Ut_tube_and_LN1              0.103029880
#Bladder_or_Rectal_mucosa1                               0.124207153
#Small_bowel_and_mesentery1                              0.413903770
#Liver_surface2                                         -0.170569481
#Diaphragm2                                              0.201012732
#Diaphragm3                                             -0.009520503
#Spleen1                                                 0.130265491
#Other_abdominal_tissue_outside_pelvis1                  0.053533515
#Other_abdominal_tissue_outside_pelvis2                 -0.093098777
#pleural_effusion1                                       0.167559231
#Lung1                                                  -0.363093305
#Liver_parenchyme1                                       0.238758669
#Residual_tumor_size_1st_debulking1                      0.219402112
#X1st_Regimen1                                           0.013793564
#Cycle_1st_regimen1                                      0.127487335
#Total_cycles_of_1st_regimen2                            0.243603029