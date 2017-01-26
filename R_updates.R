install.packages(c('lfe','dummies','xlsx','stargazer','xtable','randomForest','tree', 'data.table'))
)
library(data.table)

library(lfe)

library(stargazer)

library(xlsx)
library(randomForest)



install.packages("devtools")
library(devtools) 
install.packages('stringr')
install_github("susanathey/causalTree", force=TRUE)

library(causalTree)



# This script, when entered in the terminal of Mac OSX, will remove R from the machine:
  # rm -rf /Library/Frameworks/R.framework /Applications/R.app \    
  # /usr/bin/R /usr/bin/Rscript