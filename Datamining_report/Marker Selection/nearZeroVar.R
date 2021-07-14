#Filter Method : 데이터 1,000 개가 있는 데 이 중 990개에서 변수 A의 값이 0이며
#나머지 10개에서 변수  A의 값이 1이라고 하자.
#그러면 변수 A는 서로 다른 관찰을 구분하는 데 큰 의미가 없습니다. 
#따라서 데이터 모델링에서도 크게 유용하지 않으니 제거하게 됩니다. 
#이런 변수들은 분산이 0에 가까우며
#caret 패키지에서 nearZeroVar() 함수를 사용해 
#이런 변수를 쉽게 제거할 수 있다.

MI<-complete(imp,"long")[,c(-1,-2)]
nearZeroVar(MI, saveMetrics = TRUE)
colnames(MI)[nearZeroVar(MI)]

MI.zerovar <- MI[, -nearZeroVar(MI)]

#removed variables
#[1] "No_of_family_member_with_breast_cancer_upto_2nd_degree"
#[2] "Familial_history_of_gynecologic_cancer"                
#[3] "Op_type"                                               
#[4] "Lung"                                                  
#[5] "Liver_parenchyme"    


