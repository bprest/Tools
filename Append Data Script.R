# Reads in all csv files in a working directory, appends data, outputs full data 
# to a subdirectory called "/Appended Data/" (which must already exist in folder) 

library(data.table) # using fast-reading data table package

setwd('/Users/bcp17/Dropbox/shared with me/leases') # set WD
data.files = list.files() # find all files in working folder
data.files = data.files[grep('.CSV', data.files)] # only keep CSV files

dt = fread(data.files[1]) # read in first file
for (i in 2:length(data.files)) { # for each CSV file
  dt.temp = fread(data.files[i]) # read in file i into temporary data table
  dt = rbindlist(list(dt, dt.temp)) # append to working data table
  
  rm(dt.temp) # delete temp data table
}
gc() # garbage collect to clean up memory

# Sanity check that append got everything
dt[, uniqueN(`State/Province`)] # 15 unique states in data

uniqueN(substring(data.files, first=1, last=2)) # 15 unique states in file names

setwd('/Users/bcp17/Dropbox/shared with me/leases/Appended Data/')
fwrite(dt, 'Leases_All.csv') # write full data to csv
