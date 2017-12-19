library(MatchIt)
data(lalonde)
head(lalonde)
dim(lalonde)

demo(exact)


demo(subclass)


demo(nearest)


# left off here
demo(optimal)


demo(full)

demo(cem)

# Left off on page 14 of /Dropbox/reading/matchit.pdf
names(lalonde)
trt = lalonde[, 'treat']
plot(quantile(lalonde[trt==1, 're74'], probs=seq(0,1,by=0.01)) 
     ~ quantile(lalonde[trt==0, 're74'], probs=seq(0,1,by=0.01)),
     ylim=c(0,35000), xlim=c(0,35000))
abline(a=0,b=1)



plot(quantile(lalonde[trt==1, 'black'], probs=seq(0,1,by=0.01)) 
     ~ quantile(lalonde[trt==0, 'black'], probs=seq(0,1,by=0.01)),
     ylim=c(0,35000), xlim=c(0,35000))


m.out <- matchit(treat ~ age + educ + black + hispan + married + nodegree 
                 +                  + re74 + re75, data = lalonde, method = "cem")

m.out <- matchit(treat ~ black , data = lalonde, method = "cem")

print(m.out)
summary(m.out)
plot(m.out)




s.out <- summary(m.out, covariates = T, standardize = T)

print(s.out)

undebug(plot)

plot(s.out)
