# Project ATMOperm
# prepare final long smet-files for the scenarios, including spinup, for data 2002 - 2033
# Gernot Resch 06022019
####################################################################################
rm(list=ls()) ; gc()                                                               # initial cleanup
setwd("~/work/uni graz/projects/atmoperm/01_data")                                

####################################################################################
# +++++ spinup 
####################################################################################
# import observed data for spinup-time
met.data <- read.table("snowpack/sonnblick_pr_south-20190209.smet",                # import met-data
                       skip=10, header=F, sep=" ")
    colnames(met.data) <- c("timestamp", "TA", "RH", "VW", "ISWR", "ILWR", "PSUM")
met.data$timestamp <- as.character(met.data$timestamp)

# reduce dataset to spinup-period
s <- which(met.data$timestamp == "2010-01-01T00:00:00")
e <- which(met.data$timestamp == "2010-12-31T23:00:00")

met.data <- met.data[s:e,]

# create object containing spinup-metdata
start <- "1996-01-01"
end <- "2002-06-30"
days <- rep(seq(as.Date(start), as.Date(end), by="1 day"), each=24)
hours <- paste0("T", c(paste0("0", seq(0, 9)), seq(10, 23)),
                ":00:00")
timestamp <- paste0(days, hours)                                                   # create timestamp

met.spinup <- as.data.frame(array(NA, c(length(timestamp), ncol(met.data))))       # create husk
    colnames(met.spinup) <- colnames(met.data)
    
# add data to husk
met.spinup$timestamp <- timestamp                                                  # add timestamp

for (i in 1:nrow(met.data)){
    sel <- which(substr(timestamp, 6, 19) == substr(met.data$timestamp[i], 6, 19)) # select each day in met.spinup that matches the corresponding day in observed data
    met.spinup[sel,-1] <- met.data[i,-1]
}

# deal with leap years
s.leap <- which(substr(met.spinup$timestamp, 6, 19) == "02-29T00:00:00")           # select 02-29 in new dataset
e.leap <- which(substr(met.spinup$timestamp, 6, 19) == "02-29T23:00:00")

s <- which(substr(met.data$timestamp, 6, 19) == "02-28T00:00:00")                  # select 02-28 in old dataset as replacement
e <- which(substr(met.data$timestamp, 6, 19) == "02-28T23:00:00")

for (i in 1:length(s.leap)){
    met.spinup[s.leap[i]:e.leap[i],-1] <- met.data[s:e,-1]                         # copy 02-28 over each iteration of new dataset
}

rm(s, e, s.leap, e.leap, timestamp, days, hours, sel)

####################################################################################
# +++++ create header, load scenario-data and merge everything
####################################################################################
# +++++ create header
header <- rep(NA, 10)                                                              # create object for header-files

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
####################################################################################
# ++++++ load scenarios, merge and save
# get list of scenarios
sce.list <- list.files("snowpack/scenarios/", pattern="*.smet")
sce.names <- gsub(sce.list,
                  pattern = ".smet",
                  replacement = "")

# merge spinup and scenario-dataset
for (i in 1:length(sce.list)){
    met.sce <- read.table(paste0("snowpack/scenarios/", sce.list[i]),              # import scenario met-data
                           skip=10, header=F, sep=" ")
        colnames(met.sce) <- colnames(met.spinup)
        met.sce$timestamp <- as.character(met.sce$timestamp)
        
            # +++++ repair dataset (30.6. is missing except 00:00:00)
            # isolate single year of data and add 29.6. as 30.6.    
            met.sce.single <- met.sce[1:which(met.sce$timestamp == "2003-06-29T23:00:00"),]
            add <- which(substr(met.sce$timestamp, 1, 10) == "2003-06-29")    
            start <- nrow(met.sce.single) + 1
            end <- start + 23
            met.sce.single[start:end,] <- met.sce.single[add,]
            
            # change datestring from second 29 to 30
            met.sce.single$timestamp[start:end] <- gsub(met.sce.single$timestamp[start:end],
                                                        pattern = "-29T",
                                                        replacement = "-30T")
        
            # create longer series of dataset
            start <- substr(met.sce$timestamp[1], 1, 10)
            end <- substr(tail(met.sce$timestamp, 1), 1, 10)
            
            days <- rep(seq(as.Date(start), as.Date(end), by="1 day"), each=24)
            hours <- paste0("T", c(paste0("0", seq(0, 9)), seq(10, 23)),
                            ":00:00")
            timestamp <- paste0(days, hours)  
            
            # create husk for new data
            met.sce <- as.data.frame(array(NA, c(length(timestamp), ncol(met.sce))))
                colnames(met.sce) <- colnames(met.spinup)
            
            # add timestamp                
            met.sce$timestamp <- timestamp 
        
            # add meteorological data
            for (t in 1:nrow(met.sce.single)){
                # select each day in met.spinup that matches the corresponding day in observed data
                sel <- which(substr(timestamp, 6, 19) == substr(met.sce.single$timestamp[t], 6, 19)) 
                met.sce[sel,-1] <- met.sce.single[t,-1]
            }
    
            # deal with leap years
            s.leap <- which(substr(met.sce$timestamp, 6, 19) == "02-29T00:00:00")           # select 02-29 in new dataset
            e.leap <- which(substr(met.sce$timestamp, 6, 19) == "02-29T23:00:00")
            
            s <- which(substr(met.sce.single$timestamp, 6, 19) == "02-28T00:00:00")                  # select 02-28 in old dataset as replacement
            e <- which(substr(met.sce.single$timestamp, 6, 19) == "02-28T23:00:00")
            
            for (t in 1:length(s.leap)){
                met.sce[s.leap[t]:e.leap[t],-1] <- met.sce.single[s:e,-1]                         # copy 02-28 over each iteration of new dataset
            }
            
    met.full <- data.frame(array(NA, c(nrow(met.spinup) + nrow(met.sce),           # create husk
                                       ncol(met.spinup))))
        colnames(met.full) <- colnames(met.spinup)
    
    met.full$timestamp <- c(met.spinup$timestamp, met.sce$timestamp)                      # add timestamp
    
    # add spinup-data
    e <- which(met.full$timestamp == tail(met.spinup$timestamp, 1))                # select ending of spinup-data
    met.full[1:e,-1] <- met.spinup[,-1]
    
    # add scenario-data
    s <- e + 1                                                                     # start the next day after spinup ends
    e <- nrow(met.full)
    met.full[s:e,-1] <- met.sce[,-1]
    
    # short dataset to full year
    e <- which(met.full$timestamp == "2032-12-31T23:00:00")
    met.full <- met.full[1:e,]
    
    ####################################################################################    
    # +++++ combine header and data
    start.spinup <- length(header) + 1
    end.spinup <- start.spinup + nrow(met.spinup) - 1
    start.sce <- end.spinup + 1
    end.sce <- start.sce + nrow(met.sce) - 1
    
    smet <- as.data.frame(array(NA, c(nrow(met.spinup) + nrow(met.sce) + length(header),  # create export-object
                                      ncol(met.spinup))))               
    smet[1:length(header),1] <- header                                             # add header-data
    smet[start.spinup : end.spinup,] <- met.spinup                                 # add spinup-data
    smet[start.sce : end.sce,] <- met.sce                                          # add meteorological data
    
    # +++++ write to disk
    # +++++ export to disk and copy to snowpack-directory
    write.table(smet, file=paste0("snowpack/sonnblick_", sce.names[i], "_spinup.smet"), # make .smet-file
                row.names=F, col.names=F, dec=".", sep=" ", quote=F, na="")
    
    system(paste0("cp -rf snowpack/sonnblick_", sce.names[i], "_spinup.smet ../04_snowpack/input/scenarios/"))     # copy to snowpack-directory
}
