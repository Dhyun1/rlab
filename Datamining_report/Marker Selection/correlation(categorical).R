var_categ<-names(MI)[-c(conti_ind,1)] #36 categorical variables

l<-list()
for (i in 2:36){
  for (j in 1:(i-1)){
    chi2 = chisq.test(MI[,var_categ[i]],MI[,var_categ[j]], correct=F)
    if (chi2$p.value>0.05){
      l<-append(l,list(c(var_categ[i],var_categ[j])))
    }
  }
}
l

#####GoodmanKruskal statistics#####
install.packages("GoodmanKruskal")
library(GoodmanKruskal)
GKmatrix<-GKtauDataframe(MI[,var_categ])

setdiff(which(GKmatrix>0.3),c(1:36)+36*c(0:35))
