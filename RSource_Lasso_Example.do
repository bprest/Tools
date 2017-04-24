
clear 
set more off
capture ssc install rsource

* prep data for reading by R
sysuse auto, clear
describe, full
drop make foreign // glmnet can't handle non-numeric variables
drop if missing(rep78) // glmnet can't handle missing data
outsheet using myauto.csv, comma replace

* Run R code from within Stata
* note: R must be installed, and you must have installed the glmnet package using
* the command install.packages("glmnet") in R.
* in addition, you must change the rpath below to point to your R executable.
*rsource, terminator(END_OF_R) roptions(`" --vanilla --args "`tf1'" "`tf2'" "')
rsource, terminator(END_OF_R) roptions("--vanilla") ///
	rpath("C:\Program Files\R\R-3.3.1\bin\x64\R.exe")

rauto <- read.csv("myauto.csv")
rauto
y = rauto[,2] // second var is mpg. use that as dependent var.
X = as.matrix(rauto[,-2]) // all cols except col 2 (mpg)

library(glmnet)
set.seed(1)
lasso.cv = cv.glmnet(x=X,y=y, family="gaussian")
opt.lambda = which(lasso.cv$lambda==lasso.cv$lambda.1se) // 1 std error rule lambda. could also use lambda.min
// opt.lambda = which(lasso.cv$lambda==lasso.cv$lambda.min) // alternative rule

lasso.cv$glmnet.fit$beta[,opt.lambda] // beta with Lasso penalty

summary(lm(y~X)) // compare to OLS

END_OF_R

reg mpg weight length // post-selection OLS
