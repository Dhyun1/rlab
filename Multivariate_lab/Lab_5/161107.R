# Ex 9.9 ------------------------------------------------------------------

## Data import
upper_element <- c(.02, .96, .42, .01,
                   .13, .71, .85,
                   .50, .11,
                   .79)
consumer_pref_cov <- diag(1, 5)
consumer_pref_cov[t(upper.tri(consumer_pref_cov))] <- upper_element
consumer_pref_cov[upper.tri(consumer_pref_cov)] <- t(consumer_pref_cov)[upper.tri(consumer_pref_cov)]
rownames(consumer_pref_cov) <- c("Taste", "Good buy for money", "Flavor", "Suitable for snack", "Provides lots of energy")

## Factor analysis
factor_consumer <- factanal( ~ . , factors = 2, covmat = consumer_pref_cov, rotation="none")
factor_consumer_rot <- factanal( ~ ., factors = 2, covmat = consumer_pref_cov)

plot(factor_consumer$loadings, type = "n" ,xlim=c(0,1),ylim=c(0,1))
text(factor_consumer$loadings, labels = rownames(consumer_pref_cov))
title(main = "ML method - loadings")

plot(factor_consumer_rot$loadings, type = "n")
text(factor_consumer_rot$loadings, labels = rownames(consumer_pref_cov))
title(main = "ML method - loadings_rot")


## Table 8-4

# Table 8-4 ---------------------------------------------------------------

data <-read.table("C:/Users/STD/Desktop/Lab_5/T8-4.dat",header=FALSE,sep="");
n=nrow(data);p=ncol(data);
colnames(data)<-c("JP Morgan","Citibank","Wells Fargo","Royal Dutch Shell","Exxon Mobil")
cov=cov(data); ## sample covariance

fa.res<-factanal(data,factors=2,rotation="none")
fa.res.rot<-factanal(data,factors=2)

plot(fa.res$loadings,xlim=c(0,1),ylim=c(0,1), type = "n")
text(fa.res$loadings,labels=rownames(cov))

plot(fa.res.rot$loadings,xlim=c(0,1),ylim=c(0,1), type = "n")
text(fa.res.rot$loadings,labels=rownames(cov))

fa.res.score<-factanal(data,factors=2,scores="Bartlett")
fa.res.score$scores

plot(fa.res.score$scores[,1])
plot(fa.res.score$scores[,2])
plot(fa.res.score$scores[,1],fa.res.score$scores[,2])
