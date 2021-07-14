#상관 계수가 큰 예측 변수들이 있을 경우 성능이 떨어지거나 모델이 불안정해집니다.
#상관 계수가 높은 변수가 여럿 존재하면 파라미터 수가 불필요하게 증가
library(caret)

#only for continuous variables
MI_conti<-MI%>%
  select(conti_ind)
findCorrelation(cor(MI_conti),cutoff=0.8) # 6 
names(MI_conti)[6] #6 : BMI

#상관 계수 행렬을 입력으로 받아 상관 계수가 cutoff를 넘는 변수 쌍 A, B를 찾습니다. 
#A과 B 중 둘을 제외한 다른 변수와의 상관계수가 큰 변수 하나를 삭제한다. 
#①~②을 반복하면서 상관 계수가 높은 변수들을 제거해나간다.
