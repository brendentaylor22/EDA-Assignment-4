# Helper file to download the latest coronavirus data using the ncovr R package 
# see https://github.com/pzhaonet/ncovr for more details.

### ONLY DO THIS ONCE
# you need the remotes packages for this, install if you don't have
# install.packages("remotes")
# get the ncovr package
remotes::install_github('pzhaonet/ncovr')
###

### Get the latest data

# load required packages
library(ncovr)

# get the latest data
ncov <- get_ncov() 

# extract the area data frame, which contains everything we need
ncov <- ncov$area

# a function for converting Julian time to ymd_
conv_time <- function(x){
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}

# convert updateTime from Julian date to ymd-hms
ncov$updateTime <- conv_time(ncov$updateTime)

save(ncov, file = "ncov-latest.Rdata")
