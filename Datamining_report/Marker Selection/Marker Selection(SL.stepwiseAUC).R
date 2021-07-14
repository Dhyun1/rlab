library(SuperLearner)
names(train.omit)
###na.omit data
set.seed(1)
step.omit.fit<-SuperLearner(Y = train.omit[,1],
                   X = train.omit[,c(2:27,29:47)], family = binomial(),
                   cvControl=list(V=5),
                   SL.library = "SL.step",method="method.AUC")
step.omit.obj = step.omit.fit$fitLibrary$SL.step_All$object
coef(step.omit.obj, s = "lambda.min")
coef(step.omit.obj, s = "lambda.1se")

##1se result
#(Intercept)                                          Hypertension1 
#-4.68783022                                             1.31057481 
#Diabetes1                                          Dyslipidemia1 
#-1.37994414                                            -1.62856567 
#No_of_family_member_with_breast_cancer_upto_2nd_degree                Familial_history_of_gynecologic_cancer1 
#1.02926836                                           -16.39200815 
#CA125_initial                                    No_of_harvested_LNs 
#0.34080905                                            -0.02963001 
#NAC1                               Upper_abdominal_surgery1 
#1.70583464                                             0.92459505 
#Bladder_or_Rectal_mucosa1                                      pleural_effusion1 
#1.13990853                                             0.92040430 
#Lung1                     Residual_tumor_size_1st_debulking1 
#-19.17682215                                             1.05604911 


###MICE data
set.seed(1)
step.MI.fit<-SuperLearner(Y = train[,1],
                   X = train[,c(2:27,29:47)], family = binomial(),
                   cvControl=list(V=5),
                   SL.library = "SL.step",method="method.AUC")
step.MI.obj = step.MI.fit$fitLibrary$SL.step_All$object
coef(step.MI.obj, s = "lambda.min")  
coef(step.MI.obj, s = "lambda.1se")  

##1se result
#Age 
#0.01119818 
#Menopausal_state1 
#0.51145949 
#Hypertension1 
#0.26723013 
#Diabetes1 
#0.27815157 
#Dyslipidemia1 
#-0.66594800 
#No_of_family_member_with_breast_cancer_upto_2nd_degree 
#-0.30233668 
#Familial_history_of_gynecologic_cancer1 
#-1.65799881 
#No_of_family_member_with_gynecologic 
#0.50412746 
#Height 
#0.01437452 
#CA125_initial 
#0.15388404 
#FIGO20141 
#1.47317623 
#PLN_status1 
#0.62323714 
#No_of_harvested_LNs 
#-0.01224908 
#No_of_positive_LNs 
#0.02691226 
#NAC1 
#0.95847561 
#Cycle_NAC1 
#-0.48860622 
#Op_type2 
#-1.05123990 
#Bladder_or_Rectal_mucosa1 
#0.29717028 
#Small_bowel_and_mesentery1 
#0.58675976 
#Small_bowel_and_mesentery2 
#0.27160184 
#Liver_surface1 
#-0.40724924 
#Liver_surface2 
#-0.49103111 
#Liver_surface3 
#0.02194810 
#Diaphragm1 
#-0.23618331 
#Diaphragm2 
#0.24011096 
#Diaphragm3 
#-0.17286352 
#Spleen1 
#0.36200718 
#Other_abdominal_tissue_outside_pelvis1 
#0.72319718 
#Other_abdominal_tissue_outside_pelvis2 
#-0.50378847 
#Other_abdominal_tissue_outside_pelvis3 
#-0.41150565 
#pleural_effusion1 
#0.21986598 
#Lung1 
#-1.24698299 
#Liver_parenchyme1 
#0.93427095 
#Residual_tumor_size_1st_debulking1 
#0.30046640 
#X1st_Regimen1 
#0.31127585 
#Total_cycles_of_1st_regimen1 
#0.09005205 
#Total_cycles_of_1st_regimen2 
#0.74507357 