### Simple Webscraper.
# Title: Webscraper.R
# Author: Brian Prest
# Date Created: August 1, 2016
# Description:
#  This code takes as input a url (base.url) and a local destination folder (download.folder).
#  It downloads the given url, along with any files hyperref'ed by that url. It stores all files
#  locally in a directory structure mirroring the structure of the files linked.
#  Note: this code has not been stress-tested yet. 
rm(list=ls()); gc()

## Inputs ----
# Base URL. Must be HTML, XML, or a string containing XML/HTML content content.
base.url = 'http://www2.stat.duke.edu/~rcs46/bayes.html'

# Destination Directory
myroot = '/Users/Brianprest/'
download.folder = paste0(myroot,'Downloads/webscrape/')

# Size Limit. It will not download any single file exceeding this size.
size.limit = 2^30 # 2^10 = 1 KB, 2^20 = 1 MB, 2^30 = 1 GB


## Processing ----
library(XML) # HTML processing
library(RCurl) # for checking the size of files before downloading

# Functions to extract roots. derooter removes the root from a string. rooter retrieves it
derooter = function(link) substring(link, max(unlist(gregexpr('/', link)))+1)
rooter = function(link) substring(link, 1, max(unlist(gregexpr('/', link))))

# Find root of url
super.root = rooter(base.url)

# If destination folder doesn't exist, create it.
if (!dir.exists(download.folder)) dir.create(file.path(download.folder))
setwd(download.folder)

# Download base url
file.out = paste0(download.folder, '/', derooter(base.url))
download.file(base.url, file.out)

# Read base url and find files to download.
doc.html <- htmlParse(base.url)
doc.links <- xpathSApply(doc.html, "//a/@href")
doc.links <- doc.links[grep('\\.', doc.links)] # only links containing a file (e.g .pdf, and "....com/anotherpage/")
# pdf.url <- as.character(doc.links[grep('\\.pdf', doc.links)]) # this line will download only pdf documents
for (i in 1:length(doc.links)) {
        url = paste0(rooter(base.url), '/', doc.links[i])
        
        # Find & create subdirectories, if necessary
        subdir = rooter(gsub(super.root,'',url)) 
        subdir = substring(subdir, 2, nchar(subdir)-1 )
        subdir = unlist(strsplit(subdir,'/'))
        if(length(subdir)>0) {
                for (j in 1:length(subdir)) {
                        subdir.temp = paste0(subdir[1:j], collapse='/')
                        subdir.temp = paste0(download.folder, subdir.temp, collapse='/')
                        if (!dir.exists(subdir.temp)) dir.create(file.path(subdir.temp))
                }
        } else subdir.temp = download.folder # if no subdirectories, just return root folder
        
        # Output name
        file.out = paste0(subdir.temp, '/', derooter(url))
        
        # Check the file size
        res = url.exists(url, .header=TRUE)
        size = as.numeric(res['Content-Length']) 
        if(is.na(size)) size = 0 # if we can't find the file size, download anyway (e.g. html files)
        if(size<size.limit)  try(download.file(url, file.out, mode="wb"))
}
