rauto <- read.csv("C:/Users/Brianprest/Documents/GitHub/Tools/myauto.csv")
rauto
y = rauto[,2] # second var is mpg. use that as dependent var.
X = as.matrix(rauto[,-2]) # all cols except col 2 (mpg)

library(glmnet)
set.seed(1)
lasso.cv = cv.glmnet(x=X,y=y, family="gaussian")
opt.lambda = which(lasso.cv$lambda==lasso.cv$lambda.1se) # 1 std error rule lambda. could also use lambda.min
# opt.lambda = which(lasso.cv$lambda==lasso.cv$lambda.min) # alternative rule

lasso.cv$glmnet.fit$beta[,opt.lambda] # beta with Lasso penalty


lars.fit = lars(x=X, y=y, type='lar')
lars.fit = lars(x=X, y=y, type='lasso')
summary(lars.fit)
lars.fit$beta[5,]
802.62/mean(residuals(lm(y~X))^2) - nrow(X) + 2*sum(lars.fit$beta[5,]!=0)
802.62/mean(residuals(lm(y~X))^2) + 2*sum(lars.fit$beta[5,]!=0)
str(lars.fit)
plot(lars.fit)
plot(lars.fit$Cp)


plot(lars.fit, plottype='Cp')

lars.fit$beta[which.min(lars.fit$Cp),]

lars.cv = cv.lars(x=X, y=y)
lars.cv
plotCVLars(lars.cv)
which.min(lars.cv$cv)
