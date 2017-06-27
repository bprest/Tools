# Caret in R
#install.packages('mlbench')
library(caret)
library(mlbench)
data(Sonar)
# str(Sonar[,1:10])
dim(Sonar)
set.seed(998)

# Can create data partitions by hand using this function:
inTraining = createDataPartition(Sonar$Class, p=0.75, list=FALSE)
training = Sonar[inTraining,]
testing = Sonar[-inTraining,]

# rm(list=c(','testing','inTraining'))

# Estimate Class (metal or rock) based on 60 variables using boosting.
# We will use 10-fold CV, repeated 10 times, to train the model
fitControl = trainControl(method='repeatedcv', number=10, repeats=10) # control object to feed to train()
# other methods include 'boot', 'cv','LOOCV','LGOCV','timeslice' (for time series), 'none', and 'oob' (for non-boosted models)

library(gbm)
set.seed(825)
gbmFit1 = train(Class ~ ., data=training, method='gbm', trControl=fitControl,
                # this last one is actually a gbm option, without which we will get output from each iteration
                verbose=FALSE)

# other options include selectionFunction, which is one of 'best','oneSE','tolerance'

gbmFit1
str(gbmFit1)
# Since we are using boosted trees, the tuning parameters are:
# (1) tree depth, (2) # of trees, (3) shrinkage (aka learning rate), and (4) min obs per node
# According to https://topepo.github.io/caret/model-training-and-tuning.html,
# "shrinkage and n.minobsinnode are not shown beause the grid set of candidate models all 
# use a single value for these tuning parameters" (huh?)
# Best model here turned out to be a depth of 3 and 150 trees 


# By default, train() generates a grid of length 3 for each tuning parameter, for a total grid size of 
# 3^(# tuning parameters). tuneLength option can change this 3 to something else

# tuneGrid can take a data frame with p columns, and each row is an element of the (hyper-) grid

gbmGrid = expand.grid(interaction.depth=c(1,5,9),
                      n.trees=(1:15)*100,
                      shrinkage = c(0.1,0.2),
                      n.minobsinnode = 20)

gbmGrid

nrow(gbmGrid)

fitControl2 = trainControl(method='repeatedcv', number=10, repeats=1) # control object to feed to train()

set.seed(825)
gbmFit2 = train(Class ~ ., data=training, method='gbm', trControl=fitControl2,
                verbose=FALSE, tuneGrid = gbmGrid)
# With large grids, this gets computationally burdensome. Can speed things up with
# a random search across the grid by using search='random' as in option in trainControl()

gbmFit2
which.best = best(gbmFit2$results, 'Accuracy', maximize=TRUE)
which.1se = oneSE(gbmFit2$results, 'Accuracy', maximize=TRUE, num=10) # num=10 is number of resamples to compute SE of Accuracy
which.tol = tolerance(gbmFit2$results, 'Accuracy', maximize=TRUE, tol=2) # tol=2 is a 2% tolerance for accuracy. i.e., simplest model within 2 pct of best

gbmFit2$results[which.best,]
gbmFit2$results[which.1se,]
gbmFit2$results[which.tol,]

# Note that "simplicity" is somewhat arbitrary in cases with more than 1 tuning parameter.
# E.g., is a model with 100 trees with interaction depth 5 simpler than one with 500 trees of depth 1?
# Hard to say. The package makes built-in assumptions about the relative importance of different tuning parameters.


trellis.par.set(caretTheme())
plot(gbmFit2)  

plot(gbmFit2, metric='Kappa') 

plot(gbmFit2, metric = "Kappa", plotType = "level") #scales = list(x = list(rot = 90)))

