# Kernel Regression & Density Estimation in R


require(graphics)
with(cars, {
        plot(speed, dist)
        lines(ksmooth(speed, dist, 'normal',bandwidth=2), col='red')
        lines(ksmooth(speed, dist, 'normal',bandwidth=5), col='blue')
})


Datos = faithful
names(Datos)
library(MASS)
dd = kde2d(Datos$eruptions,Datos$waiting) # estimate the bivariate kernel estimator
dd
par(mfrow=c(1,2))
contour(dd) # plot level sets of the density estimator
persp(dd) # plot density estimator in 3d



library(np)

# Example 1 - Italy GDP (region-year panel)
data('Italy')
attach(Italy)

dev.off()
plot(ordered(year), gdp, xlab="Year (ordered factor)",
     ylab="GDP (millions of Lire, 1990=base)")

plot(gdp ~ year)


bw = npregbw(gdp~ordered(year)) # determines optimal bandwidth through CV

model = npreg(bws=bw, gradients = TRUE)
summary(model)
plot(model, plot.errors.method='bootstrap')
points(ordered(year), gdp, cex=.2, col='red')

detach(Italy)

Sys.sleep(5)

# Example 2 - Canadian cross section of earnings of high school graduates: wage & age
data('cps71')
attach(cps71)
names(cps71)
plot(logwage ~ age, data=cps71)

bw = npregbw(logwage ~ age, regtype='ll', bwmethod='cv.aic') # use AIC to choose bandwidth

# plot(bw, plot.errors.method='bootstrap')
plot(bw, plot.errors.method='asymptotic')
points(logwage ~ age, data=cps71, cex=0.2, col='red')

Sys.sleep(5)

# plot derivative with gradient=TRUE
plot(bw, plot.errors.method='asymptotic', gradient=TRUE)

summary(lm(logwage ~ age))

Sys.sleep(5)

detach(cps71)

# Example 3 - OECD GDP growth panel (country-year)
data('oecdpanel')
attach(oecdpanel)
?oecdpanel
# detach(oecdpanel)

summary(lm((formula=growth~
                    factor(oecd)+
                    factor(year)+
                    poly(initgdp, 4, raw=TRUE)+
                    popgro+
                    inv+
                    poly(humancap, 3, raw=TRUE)), data=oecdpanel))

bw <- npregbw(formula=growth~
                           factor(oecd)+
                           factor(year)+
                           initgdp+
                           popgro+
                           inv+
                           humancap, data=oecdpanel)

plot(bw, plot.errors.method='asymptotic', ylim=c(-0.1,0.1))
# plot(bw, plot.errors.method='asymptotic', ylim=c(-0.1,0.1), gradient=TRUE)

bw.oecd <- npregbw(formula=growth~
                      factor(oecd)+
                      factor(year)+
                      initgdp+
                      popgro+
                      inv+
                      humancap, data=oecdpanel[oecdpanel$oecd==1,])

plot(bw.oecd, plot.errors.method='asymptotic', ylim=c(-0.1,0.1))

bw.nonoecd <- npregbw(formula=growth~
                           factor(oecd)+
                           factor(year)+
                           initgdp+
                           popgro+
                           inv+
                           humancap, data=oecdpanel[oecdpanel$oecd==0,])

plot(bw.nonoecd, plot.errors.method='asymptotic', ylim=c(-0.1,0.1))

detach(oecdpanel)

# Example 4 - Guinea pig tooth length vs vitamin C dosage.

library(datasets)
attach(ToothGrowth)

bw.tooth = npregbw(len~factor(supp)+ordered(dose))

plot(bw.tooth, plot.errors.method='bootstrap', plot.errors.type='quantile')
plot(bw.tooth, plot.errors.method='bootstrap')

detach(ToothGrowth)
# EXAMPLE 5 (INTERFACE=FORMULA): a pretty 2-d smoothing example adapted
# from the R mgcv library which was written by Simon N. Wood.

set.seed(12345)

# This function generates a smooth nonlinear DGP

dgp.func <- function(x, z, sx=0.3, sz=0.4)
{ (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
                       0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
}

# Generate 500 observations, compute the true DGP (i.e., no noise), 
# then a noisy sample

n <- 500

x <- runif(n)
z <- runif(n)

xs <- seq(0, 1, length=30)
zs <- seq(0, 1, length=30)

X.eval <- data.frame(x=rep(xs, 30), z=rep(zs, rep(30, 30)))

dgp <- matrix(dgp.func(X.eval$x, X.eval$z), 30, 30)

y <- dgp.func(x, z)+rnorm(n)*0.1

# Prepare the screen for output... first, plot the true DGP
dev.off()
# split.screen(c(2, 1))
# 
# screen(1)
par(mfrow=c(1,2))
persp(xs, zs, dgp, xlab="x1", ylab="x2", zlab="y", main="True DGP")

# Next, compute a local linear fit and plot that

bw <- npregbw(formula=y~x+z, regtype="ll", bwmethod="cv.aic")
fit <- fitted(npreg(bws=bw, newdata=X.eval))
fit.mat <- matrix(fit, 30, 30)

# screen(2)

persp(xs, zs, fit.mat, xlab="x1", ylab="x2", zlab="y",
      main="Local linear estimate")


