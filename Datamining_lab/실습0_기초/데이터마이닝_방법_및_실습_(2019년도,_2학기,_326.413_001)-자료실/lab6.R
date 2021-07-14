

#############################################################################################

# Relaxed Lasso


install.packages("relaxnet")
library(relaxnet)

## generate predictor matrix
nobs <- 100 ; nvars <- 200
set.seed(23)
x <- matrix(rnorm(nobs * nvars), nobs, nvars)

## make sure it has unique colnames
colnames(x) <- paste("x", 1:ncol(x), sep = "")

## let y depend on first 5 columns plus noise
y <- x[,1]+2*x[,2]+3*x[,3]+4*x[,4]+5*x[,5] + rnorm(nrow(x))

## run cv.relaxnet
set.seed(1)
cv.result <- cv.relaxnet(x, y)
predict(cv.result, type = "nonzero") # print nonzero coefficients

## very few false positives compared to glmnet alone #
predict(cv.result$relaxnet.fit$main.glmnet.fit, type = "nonzero",
        s = cv.result$main.lambda.min) # glmnet min rule
predict(cv.result$relaxnet.fit$main.glmnet.fit, type = "nonzero",
        s = cv.result$main.lambda.1se) # glmnet 1se rule

# get values of the coefs for cv.relaxnet's chosen fit
coefs <- drop(predict(cv.result, type = "coef")) 
coefs[coefs != 0]
set.seed(1)
lasso.cv = cv.glmnet(x,y, alpha=1)
lambda.min = lasso.cv$lambda.min
lasso.beta = coef(lasso.cv, s = 'lambda.min')[,1]
(nonzero.lasso.beta = lasso.beta[lasso.beta !=0])

#############################################################################################

# Adaptive LASSO

# LASSO
lasso_cv <- cv.glmnet(x = x, y = y, type.measure = "mse", nfold = 10, alpha = 1)
plot(lasso_cv)
lasso_cv$lambda.min
coef(lasso_cv, s = lasso_cv$lambda.min)
best_lasso_coef <- coef(lasso_cv, s = lasso_cv$lambda.min)
rownames(best_lasso_coef)[as.numeric(best_lasso_coef)!=0] # Coef of lasso

# For adaptive lasso
ridge_cv <- cv.glmnet(x = x, y = y, type.measure = "mse", nfold = 10, alpha = 0)
plot(ridge_cv)
ridge_cv$lambda.min
coef(ridge_cv, s = ridge_cv$lambda.min)
best_ridge_coef <- as.numeric(coef(ridge_cv, s = ridge_cv$lambda.min))[-1]

adap_lasso <- glmnet(x = x, y = y, alpha = 1, penalty.factor = 1 / abs(best_ridge_coef))
plot(adap_lasso , xvar = "lambda")

adap_lasso_cv <- cv.glmnet(x = x, y = y, type.measure = "mse", nfold = 10, alpha = 1, 
                           penalty.factor = 1 / abs(best_ridge_coef), keep = TRUE)
plot(adap_lasso_cv)
adap_lasso_cv$lambda.min
best_adap_lasso_coef <- coef(adap_lasso_cv, s = adap_lasso_cv$lambda.min)
rownames(best_adap_lasso_coef)[as.numeric(best_adap_lasso_coef)!=0] # Coef of adaptive lasso


###################NonConVex Lasso

install.packages("ncvreg")
library(ncvreg)

start = Sys.time()
mcp = cv.ncvreg(x,y, penalty="MCP")
end = Sys.time()
end - start

coeff.mcp = coef(mcp, s = 'lambda.min')
coeff.mcp[coeff.mcp !=0]

scad = cv.ncvreg(x,y, penalty="SCAD")
coeff.scad = coef(scad, s = 'lambda.min')
coeff.scad[coeff.scad !=0]

sc = cv.ncvreg(x,y, penalty="lasso")
coeff.sc = coef(sc, s = 'lambda.min')
coeff.sc[coeff.sc !=0]



#########group

install.packages("gglasso")
library(gglasso)

create_factor = function(nb_lvl, n= 100 ){
  factor(sample(letters[1:nb_lvl],n, replace = TRUE))}

df = data.frame(var1 = create_factor(5), 
                var2 = create_factor(5), 
                var3 = create_factor(5), 
                var4 = create_factor(5),
                var5 = rnorm(100),
                y = rnorm(100))

x = dplyr::select(df, -y)
y = df$y


x = model.matrix( ~ ., dplyr::select(df, -y))[, -1]
groups = c(rep(1:4, each = 4), 5)
fit.glasso = cv.gglasso(x = x, y = y, group = groups, lambda = 1)
coef(fit.glasso, s="lambda.min")
