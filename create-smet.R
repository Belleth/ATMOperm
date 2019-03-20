# Project ATMOperm
# prepare final long smet-file, including spinup, for data 2002 - 2017
# Gernot Resch 06022019
####################################################################################
rm(list=ls()) ; gc()                                                           # initial cleanup
setwd("~/work/uni graz/projects/atmoperm/01_data")                                
####################################################################################
spinup.time <- 6                                                               # define length of spinup-time in years (5 + 1 = 5 full years, 1 until 1.7.)

met.data <- read.table("snowpack/sonnblick_pr_south-20190209.smet",            # import met-data
                       skip=10, header=F, sep=" ")
    colnames(met.data) <- c("timestamp", "TA", "RH", "VW", "ISWR", "ILWR", "PSUM")
met.data$timestamp <- as.character(met.data$timestamp)

sel <- which(met.data$PSUM == "-999")
met.data$PSUM[sel] <- 0

rm(sel)

# create date-string
years <- substr(met.data[,1], 1, 4)
years.unique <- unique(years)
year.start <- as.numeric(years.unique[1]) - spinup.time

start <- paste0(year.start, "-01-01")
end <- paste0(year.start + spinup.time, "-12-31")

days <- rep(seq(as.Date(start), as.Date(end), by="1 day"), each=24)
hours <- paste0("T", c(paste0("0", seq(0, 9)), seq(10, 23)),
                ":00:00")
timestamp <- paste0(days, hours)

# create dataset for 2010 for being copied for thewhole spinup-period
s <- which(met.data$timestamp == "2010-01-01T00:00:00")
e <- which(met.data$timestamp == "2010-12-31T23:00:00")
yearly.data <- met.data[s:e,]                                                  # yearly data from 2010, ready for being copied 5 times

# create husk for artificial period
met.art <- as.data.frame(array(NA, c(length(timestamp), 7)))                   # husk for data
    colnames(met.art) <- colnames(met.data)

met.art$timestamp <- timestamp                                                 # copy timestamp

# add data to husk
for (i in 1:nrow(yearly.data)){
    sel <- which(substr(timestamp, 6, 19) == substr(yearly.data$timestamp[i], 6, 19))
    met.art[sel,-1] <- yearly.data[i, -1]                                      # copy selected year
}

# deal with leap years
s.leap <- which(substr(met.art$timestamp, 6, 19) == "02-29T00:00:00")          # select 02-29 in new dataset
e.leap <- which(substr(met.art$timestamp, 6, 19) == "02-29T23:00:00")

s <- which(substr(yearly.data$timestamp, 6, 19) == "02-28T00:00:00")           # select 02-28 in old dataset
e <- which(substr(yearly.data$timestamp, 6, 19) == "02-28T23:00:00")

for (i in 1:length(s.leap)){
    met.art[s.leap[i]:e.leap[i],-1] <- yearly.data[s:e,-1]                     # copy 02-28 over each iteration of new dataset
}

# shorten dataset to 2002-06-30, so it fits to measured data
sel <- which(met.art$timestamp == "2002-06-30T23:00:00")

met.art <- met.art[1:sel,]
##############################################################################
# +++++ create header, combine with data and export
# create header
header <- rep(NA, 10)                                                          # create object for header-files

header[1] <- "SMET 1.1 ASCII"
header[2] <- "[HEADER]"
header[3] <- "station_id = sonnblick"
header[4] <- "latitude = 47.05"
header[5] <- "longitude = 12.95"
header[6] <- "altitude = 3059"
header[7] <- "nodata = -999"
header[8] <- "tz = 0"
header[9] <- "fields = timestamp TA RH VW ISWR ILWR PSUM"
header[10] <- "[DATA]"

# combine header, artificial spinup-data and real data
art.start <- length(header) + 1
art.end <- art.start + nrow(met.art) - 1
obs.start <- art.end + 1
obs.end <- obs.start + nrow(met.data) - 1

smet <- as.data.frame(array(NA, c(nrow(met.art) + nrow(met.data) + 10,         # create export-object
                                  ncol(met.art))))               
smet[1:length(header),1] <- header                                             # add header-data
smet[art.start:art.end,] <- met.art                                            # add spinup-data
smet[obs.start:obs.end,] <- met.data                                           # add meteorological data

# export to binary and txt-file
write.table(smet, file="snowpack/sonnblick_09022019_spinup.smet",              # make .smet-file
            row.names=F, col.names=F, dec=".", sep=" ", quote=F, na="")

system("cp -rf snowpack/sonnblick_09022019_spinup.smet ../04_snowpack/input/") # copy to snowpack-directory
