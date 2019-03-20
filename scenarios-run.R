# Project ATMOperm
# Gernot Resch 20.2.2019
# Run the atmospheric extreme scenarios with snowpack in a multicore environment
##############################################################################
rm(list=ls()) ; gc()                                                               # initial cleanup
Sys.setlocale("LC_TIME", "C")                                                      # change language to english for plots

# +++++ switch directories depending on environment
hostname <- system("uname", intern=T)

if (hostname == "Linux"){                                                          # switch to directory if script runs on icebear
    setwd("~/atmoperm/")
    cores <- 25 # 25                                                                    # set number of parallel snowpack runs. max: 32,
                                                                                   # better: 28. number of experiments / cores
                                                                                   # must be an integer!
}

if (hostname == "Darwin"){
    setwd("~/work/uni graz/projects/atmoperm/")                                    # switch to directory if script runs local
    cores <- 5                                                                     # set number of parallel snowpack runs
}

##############################################################################
# setup experiments
start <- "1996-08-01"
end <- "2032-08-30"
# end <- "2003-08-30"

days.original <- seq(as.Date(start), as.Date(end)-1, by="days")                    # create vector without last day because modelled data does not include it
parameters <- c("0501", "0503")                                                    # parameters that are extracted from pro-files

# load additional functions
source("02_scripts/f_pro.import.R")                                                # import extracted .pro-data
require(doParallel)                                                                # for parallel snowpack runs
    registerDoParallel(cores=cores)                                                # setup multicore environment

require(gtools)

# additional data
depth.obs <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1.0, 1.2, 1.5,                   # define depth of observations
               2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 7.0, 9.0, 10.0, 
               12.0, 14.0, 16.0, 18.0, 20.0)

##############################################################################
# +++++ prepare snowpack-run-terminal-commands
setwd("04_snowpack")

experiments <- gsub(                                                               # get list of all experiments
                    list.files("cfg/scenarios", pattern="*.ini"),
                    pattern=".ini", replacement="")

snowpack.run <- rep(NA, length(experiments))                                       # prepare terminal-commands
for (i in 1:length(snowpack.run)){
    snowpack.run[i] <- paste0("snowpack -c cfg/scenarios/", experiments[i], ".ini -b ", 
                           start, " -e ", end)
}

# +++++ calculate number of runs depending on number of found .ini-files
# creates a matrix "runs", where each line contains numbers of the parallelized runs.
n.ini <- length(experiments)

for (i in 1){
    if (n.ini >= cores){
        n.run <- ceiling(n.ini / cores)
        runs <- matrix(1:n.ini, n.run, cores, byrow=T)
    } 
    else {
        cores <- n.ini
        n.run <- ceiling(n.ini / cores)
        runs <- matrix(1:n.ini, n.run, cores, byrow=T)
    }
}    

rm(n.ini, n.run)

##############################################################################
# +++++ run experiments
for (y in 1:nrow(runs)){
# for (y in 1){
    foreach(c=runs[y,]) %dopar% {
        system(snowpack.run[c], ignore.stderr=T, ignore.stdout=T)                  # run snowpack, parallel simulations (25 per iteration)
    }
    
    for (x in 1:ncol(runs)){
        sel <- runs[y,x]
        
        # check if selected pro-file is > 50 MB to decide if run has some data to evaluate
        runs.completed <- system(paste0("ls output/*pro"), intern=T)
        runs.completed <- gsub(runs.completed, pattern="output/", replacement="")
        runs.completed <- which(runs.completed == paste0(experiments[sel], ".pro"))
        
        if (length(runs.completed) == 1){
            size <- system(paste0("du -sh output/", experiments[sel], ".pro"), intern=T)
            size <- as.numeric(gsub(
                unlist(strsplit(size, "\t"))[1], 
                pattern="M", replacement = ""))
            
            if (!is.na(size) &&
                size > 50){
                
                # depth
                system(paste0("./software/SnExtract.sh output/", experiments[sel],       # extract depth from .pro-file
                              ".pro ", parameters[1], " > output/extract/", 
                              experiments[sel], "_", parameters[1], ".txt"))
                
                # temperature
                system(paste0("./software/SnExtract.sh output/", experiments[sel],       # extract temperature from .pro-file
                              ".pro ", parameters[2], " > output/extract/", 
                              experiments[sel], "_", parameters[2], ".txt"))
                
                # cleanup
                system(paste0("rm -rf output/", experiments[sel], ".pro"))
                system(paste0("rm -rf output/", experiments[sel], ".smet"))
                
                # +++++ import data and prepare for analysis
                # import depth
                depth <- pro.import(parameters[1], experiments[sel])                     # import depth-data and isolate information about depth [cm]
                
                s <- which(!is.na(depth[,1]))                                            # get rid of na-values and reduce to a vector
                depth <- depth[s,1]
                
                s <- which(depth <= 0)                                                   # reduce to soil-layers (depth <= 0)
                depth <- depth[s]
                
                angle <- 20
                angle <- (90 - as.numeric(gsub(                                          # isolate angle and change from degree to radians
                    angle, pattern="slope_angle = ", replacement=""))) / (180/pi)
                
                depth <- round(sin(angle) * depth)
                
                # import temperature
                temperature <- pro.import(parameters[2], experiments[sel])               # import temperature data
                
                temp.modelled <-  temperature[s,]                                        # reduce to soil (depth <= 0) 
                row.names(temp.modelled) <- depth
                colnames(temp.modelled) <- substr(colnames(temp.modelled), 1, 10)
                
                # export full temperature data-set
                fill.me <- t(temp.modelled)
                colnames(fill.me) <- depth * -1
                
                temp.modelled.full <- as.data.frame(array(NA, dim(fill.me)))    
                row.names(temp.modelled.full) <- row.names(fill.me)
                colnames(temp.modelled.full) <- rev(colnames(fill.me))
                
                for (u in 1:ncol(fill.me)){
                    s <- which(colnames(fill.me) == colnames(temp.modelled.full[u]))
                    temp.modelled.full[,u] <- fill.me[,s]
                }    
                
                colnames(temp.modelled.full) <- round(as.numeric(colnames(temp.modelled.full)) / 100, 2)
                
                save(temp.modelled.full,                                                 # export data
                     file=paste0("../01_data/soil/scenarios/temp.modelled.full.",
                                 experiments[sel], ".RData"))
                
                # create new modelled dataset with only observed depths (temp.modelled.sel)
                days <- colnames(temp.modelled)
                
                start.m <- min(days)
                end.m <- max(days)
                
                depth.obs.negative <- (depth.obs * -1) * 100                             # change depths of sensors to negative values and from m to cm
                
                sel.depth <- rep(NA, length(depth.obs))                                  # select rows with selected sensors
                for (u in 1:length(sel.depth)){
                    sel.depth[u] <- max(which(depth <= depth.obs.negative[u]))             
                }
                
                temp.modelled.sel <- t(temp.modelled[sel.depth,])                        # create modelled dataset with observed depths and transpose
                row.names(temp.modelled.sel) <- substr(row.names(temp.modelled.sel), 1, 10)
                
                s <- which(row.names(temp.modelled.sel) == start.m)
                e <- which(row.names(temp.modelled.sel) == end.m)
                temp.modelled.sel <- temp.modelled.sel[s:e,]
                
                # prepare object for storing
                temp.modelled <- temp.modelled.sel
                colnames(temp.modelled) <- depth.obs
                
                save(temp.modelled,                                                      # export data
                     file=paste0("../01_data/soil/scenarios/temp.modelled.", 
                                 experiments[sel], ".RData"))
                
                ##############################################################################
                # +++++ calculate active layer thickness for each day
                days <- rownames(temp.modelled.full)
                
                alt.daily <- rep(NA, length(days.original))
                names(alt.daily) <- days.original
                
                select <- rep(NA, nrow(temp.modelled.full))                              # prepare vector for hourly alt-values
                
                for (a in 1:length(select)){
                    select[a] <- max(which(temp.modelled.full[a,] > 0), na.rm=T)         # calculate daily active layer thickness    
                }
                alt.daily[1:length(select)] <- as.numeric(colnames(temp.modelled.full)[select])    # change column to colname    
                
                save(alt.daily, file=paste0("../01_data/soil/scenarios/alt.",   # save to disk
                                            experiments[sel], ".RData"))
            }
        }
    }
}

# +++++ cleanup after model runs
system("rm -rf output/*.sno2*")                                            # delete unnecessary backup
system("rm -rf output/*.haz*")                                             # delete all .haz-files (not needed)
