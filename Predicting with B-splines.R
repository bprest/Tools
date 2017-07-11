
rm(list=ls())
library(data.table)
library(splines)

## Generate random data
set.seed(1)
N=30
X = rnorm(N, mean=0, sd=2)
y = 5 + 2*X - 1*X^2 + 0.1*X^3 + rnorm(N)

dt = data.table(y,X)

# Estimate coefficients
K=7; D=1
lm.fit = lm( y ~ bs(X, df=K, degree=D), data=dt)
summary(lm.fit)

## Now predict & plot predicted values using 2 different methods:
# 1) Using predict() function.
# 2) Obtaining bs() matrix and multiplying by estimated coefficients.

# Create x-grid to predict on
xgrid = seq(from = min(X), to = max(X), length.out=100)

# 1) Predict using predict() command
lm.pred = predict(lm.fit, newdata=data.table(X=xgrid))

# 2) Predict by multiplying spline basis by coefficients
# 2a) First create spline basis using the same knots (etc.) used in the regression
spline.basis.in.reg = lm.fit$model$`bs(X, df = K, degree = D)`
spline.basis.in.reg
knots.in.regression = attr(spline.basis.in.reg,'knots')
transformed.x = bs(xgrid, knots=knots.in.regression, degree=D)

# 2b) Multiply basis of xgrid by coefficients. Don't forget to account for the constant in the regression.
lm.pred.matrix = cbind(1,transformed.x) %*% lm.fit$coefficients


## Compare results from two methods
results = cbind(lm.pred, lm.pred.matrix)
results[,1] - results[,2] # they're the same

# Plot them: they're the same
plot(lm.pred ~ xgrid, type='l', col='blue', lwd=3)
lines(lm.pred.matrix ~ xgrid, col='red', lwd=2)
