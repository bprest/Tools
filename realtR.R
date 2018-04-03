# realtR

# devtools::install_github("abresler/realtR")
# http://asbcllc.com/r_packages/realtR/2018/introduction/

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(realtR)
library(highcharter)
library(formattable)
library(skimr)
library(glue)
library(leaflet)
library(viridis)
library(forcats)

alex_growing_up_school <- "Landon School"
df_geocoded_school <- realtR::geocode(locations = alex_growing_up_school, return_message = F)

df_geocoded_school %>% glimpse()
