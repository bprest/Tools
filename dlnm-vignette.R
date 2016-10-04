library(dlnm)
chicagoNMMAPS = as.data.table(chicagoNMMAPS)
cb1.pm <- crossbasis(chicagoNMMAPS$pm10, lag=15, argvar=list(fun="lin"),
                     arglag=list(fun="poly",degree=4))

cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag=3, argvar=list(df=5),
                       arglag=list(fun="strata",breaks=1))

as.data.table(cb1.pm)[is.na(v1.l1), .N]


summary(cb1.pm)

library(splines)
model1 = glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow, family=gaussian(), data=chicagoNMMAPS)
summary(model1)


pred1.pm <- crosspred(cb1.pm, model1, at=0:20, bylag=0.2, cumul=TRUE)
# pred1.pm <- crosspred(cb1.pm, model1, at=0:20, bylag=2, cumul=TRUE)

par(mfrow=c(1,2))
plot(pred1.pm, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Association with a 10-unit increase in PM10")
plot(pred1.pm, "slices", var=10, col=2, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association with a 10-unit increase in PM10")
