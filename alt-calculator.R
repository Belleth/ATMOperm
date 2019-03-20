# Project ATMOperm
# Calculate Active Layer Thickness out of Observations
# Gernot Resch 06022019
####################################################################################
rm(list=ls()) ; gc()                                                           # initial cleanup
setwd("~/work/uni graz/projects/atmoperm/")                                    # switch to directory if script runs local
####################################################################################

source("02_scripts/work_files/f_alt.calculator.R")                             # load alt-calculating-function

load("01_data/bohrloch/bh3_postprocessed-hourly-interpolated.RData")           # load borehole-data

alt.calculator(temp.obs.int)                                                   # calculate active layer thickness

save(alt.daily, file="01_data/bohrloch/alt.daily.RData")                       # write daily and yearly data to harddisk
save(alt.year, file="01_data/bohrloch/alt.year.RData")
