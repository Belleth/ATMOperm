# Project ATMOperm
# Interprets the results of the 10000 simulations, run bei 06-sampling.R: 10.12.2018
# Gernot Resch: Adaption to daily modelled active layer thickness: 3.1.2019
# Gernot Resch: Adaption to new sampling done in February and some changes to active layer thickness-calculation: 14.2.2019
# Gernot Resch: Code cleanup and short adaption to fourth (hopefully final) sampling, started on 25.02.2019
###################################################################################################
rm(list=ls()) ; gc()                                                               # initial cleanup
Sys.setlocale("LC_TIME", "C")                                                      # change language to english for plots

library(Hmisc)

setwd("~/work/uni graz/projects/atmoperm/")                                    # switch to directory if script runs local
# setwd("~/atmoperm")
rmse.selection <- "yearly"
###################################################################################################
# snow disappearance day (aus beobachtungen)
sdd <- c("2008-07-10", "2050-01-01", "2011-07-08", "2012-07-05", "2013-07-31", "2014-08-05", "2050-01-01", "2016-06-15")

###################################################################################################
# +++++ load modeled daily alt
# setup experiments
start <- "1996-08-01"
end <- "2017-08-30"
days <- seq(as.Date(start), as.Date(end)-1, by="days")                                   # create vector without last day because modelled data does not include it

file.list <- list.files("01_data/soil/sampling/", pattern="alt*")
load(paste0("01_data/soil/sampling/", file.list[1]))                                 # load first file for preparing gathering object                           

experiments <- gsub(file.list, 
                    pattern="alt.sonnblick_sonnblick_", 
                    replacement="")
experiments <- gsub(experiments, 
                    pattern=".RData", 
                    replacement="")

alt.daily.modelled <- array(NA, c(length(experiments), length(alt.daily)))
rownames(alt.daily.modelled) <- experiments
colnames(alt.daily.modelled) <- names(alt.daily)

for (i in 1:nrow(alt.daily.modelled)){
    load(paste0("01_data/soil/sampling/", file.list[i]))
    alt.daily.modelled[i,1:length(alt.daily)] <- alt.daily
}

rm(file.list, alt.daily)    
###################################################################################################
#     load("01_data/soil/sampling/sampling-results/alt.daily.modelled.old.RData")            # daily modelled alt (computed with 07-daily-modelled-alt.R)

# +++++ calculate yearly alt
years <- unique(substr(colnames(alt.daily.modelled), 1, 4))

alt.yearly.modelled <- array(NA, c(nrow(alt.daily.modelled), length(years)))
colnames(alt.yearly.modelled) <- years
row.names(alt.yearly.modelled) <- row.names(alt.daily.modelled)

for (y in 1:nrow(alt.yearly.modelled)){
    for (x in 1:ncol(alt.yearly.modelled)){
        sel <- which(substr(colnames(alt.daily.modelled), 1, 4) == years[x])
        alt.yearly.modelled[y,x] <- max(alt.daily.modelled[y,sel], na.rm=T)    
    }    
}

sel <- which(alt.yearly.modelled == "-Inf")                                             # years with no values set to NA
alt.yearly.modelled[sel] <- NA

rm(x, y, i)
###################################################################################################
# +++++ cleanup data and prepare for further analysis
# use only stable models for further analysis (yearly)
for (i in 1:ncol(alt.yearly.modelled)){
    sel <- which(!is.na(alt.yearly.modelled[,i]))
    
    alt.yearly.modelled <- alt.yearly.modelled[sel,]
}

# also reduce daily
sel <- row.names(alt.yearly.modelled)                                                       # select remaining experiments
for (i in 1:length(sel)){
    sel[i] <- which(row.names(alt.daily.modelled) == sel[i])
}
alt.daily.modelled <- alt.daily.modelled[as.numeric(sel),]

rm(sel, i)

experiments <- row.names(alt.yearly.modelled)                                               # get list of experiments that fullfill the criteria above

# delete spinup-years
alt.daily.modelled.spinup <- alt.daily.modelled                                             # make backup of all days
sel <- which(colnames(alt.daily.modelled) == "2002-12-31")
alt.daily.modelled <- alt.daily.modelled[,-c(1:sel)]                                    # delete spinup-years, dataset starts with 2002-01-01

alt.yearly.modelled.spinup <- alt.yearly.modelled                                                  
sel <- which(colnames(alt.yearly.modelled) == "2002")
alt.yearly.modelled <- alt.yearly.modelled[,-c(1:sel)]

rm(sel)

###################################################################################################
# +++++ calculate observed active layer thickness
load("01_data/bohrloch/alt.daily.RData")
load("01_data/bohrloch/alt.year.RData")

# source("02_scripts/f_alt.calculator.R")                                            # load alt-calculating-function
# 
# # +++++ calculate active layer depth for interpolated observations
# load("01_data/bohrloch/bh3_postprocessed-hourly-interpolated.RData")               # load observed soil-temperature (interpolated)
# 
# alt.calculator(temp.obs.int)
# 
# # +++++ save alt-results
# save(alt.hour, file="01_data/bohrloch/alt.hour.RData")
# save(alt.daily, file="01_data/bohrloch/alt.daily.RData")
# save(alt.year, file="01_data/bohrloch/alt.year.RData")

# +++++ rename
alt.daily.observed <- alt.daily
alt.yearly.observed <- alt.year

# +++++
sel <- which(names(alt.daily.observed) == "2007-12-31")                            # get rid of uncomplete year 2007
alt.daily.observed <- alt.daily.observed[-c(1:sel)]    

sel <- which(names(alt.yearly.observed) == "2007")
alt.yearly.observed <- alt.yearly.observed[-c(1:sel)]

rm(sel, alt.daily, alt.year)                                       # cleanup
   

###################################################################################################
# +++++ calculate bias, rmse and rank experiments
###################################################################################################
source("02_scripts/f_rmse.R")                                                      # load rmse-function

# +++++ calculate yearly BIAS
s <- which(colnames(alt.yearly.modelled) == names(alt.yearly.observed)[1])
e <- which(colnames(alt.yearly.modelled) == names(alt.yearly.observed)[length(alt.yearly.observed)])

alt.yearly.bias <- alt.yearly.modelled[,s:e]                                       # create object for storing bias per day
alt.yearly.bias[,] <- NA
for (i in 1:nrow(alt.yearly.bias)){
    alt.yearly.bias[i,] <- alt.yearly.observed - alt.yearly.modelled[i,s:e]        # calculate bias per experiment, but only from 2008-01-01 - 2016-11-29 (same time period)
}

# +++++ calculate yearly RMSE
alt.yearly.rmse <- rep(NA, nrow(alt.yearly.bias))                                                # create object for storing rmse
    names(alt.yearly.rmse) <- row.names(alt.yearly.bias)

for (i in 1:nrow(alt.yearly.bias)){
    sel <- which(substr(colnames(alt.yearly.bias), 1, 4) == 2014)
    
    # sel <- which(substr(colnames(alt.yearly.bias), 1, 4) == 2008 |
    #   substr(colnames(alt.yearly.bias), 1, 4) == 2014)
    # 
    # sel <- which(substr(colnames(alt.yearly.bias), 1, 4) == 2008 |
    #              substr(colnames(alt.yearly.bias), 1, 4) == 2014 |
    #              substr(colnames(alt.yearly.bias), 1, 4) == 2015 |
    #              substr(colnames(alt.yearly.bias), 1, 4) == 2016)
    # 
    

    alt.yearly.rmse[i] <- rmse(alt.yearly.bias[i,-sel])                             # calculate rmse for each experiment
}

###################################################################################################
# +++++ calculate daily BIAS
s <- which(colnames(alt.daily.modelled) == names(alt.daily.observed)[1])
e <- which(colnames(alt.daily.modelled) == names(alt.daily.observed)[length(alt.daily.observed)])

alt.daily.bias <- alt.daily.modelled[,s:e]                                               # create object for storing bias per day
alt.daily.bias[,] <- NA
for (i in 1:nrow(alt.daily.bias)){
    alt.daily.bias[i,] <- alt.daily.observed - alt.daily.modelled[i,s:e]             # calculate bias per experiment, but only from 2008-01-01 - 2016-11-29 (same time period)
}

# +++++ calculate daily RMSE
# calculate rmse for all experiments
alt.daily.rmse <- rep(NA, nrow(alt.daily.bias))                                                # create object for storing rmse
    names(alt.daily.rmse) <- row.names(alt.daily.bias)

for (i in 1:nrow(alt.daily.bias)){
    # alt.rmse[i] <- rmse(alt.bias[i,])                                              # calculate rmse for each experiment, exclude nothing
    
    # sel <- which(substr(colnames(alt.daily.bias), 1, 4) == 2008 |
    #                  substr(colnames(alt.daily.bias), 1, 4) == 2014 | 
    #                  substr(colnames(alt.daily.bias), 1, 4) == 2015)
    
    sel <- which(substr(colnames(alt.daily.bias), 1, 4) == 2014)
    alt.daily.rmse[i] <- rmse(alt.daily.bias[i,-sel])
}


# +++++ calculate nash-sutcliff

###################################################################################################
# +++++ select rmse for continuing
# alt.rmse <- alt.daily.rmse
if (rmse.selection == "daily"){
    alt.rmse <- alt.daily.rmse
}
if (rmse.selection == "yearly"){
    alt.rmse <- alt.yearly.rmse
}
    

###################################################################################################
# exclude all models that are not within the 0 - 75% quantile range
exclude <- quantile(alt.rmse)[2]                                                   # select only models within a range of 0 - 25 % quantile range
sel <- which(alt.rmse <= exclude)
alt.rmse <- alt.rmse[sel]

sel <- rep(NA, length(alt.rmse))                                                   # reduce data to selected data range
for (i in 1:length(sel)){
    sel[i] <- which(row.names(alt.daily.modelled) == names(alt.rmse[i]))
}
    alt.daily.modelled <- alt.daily.modelled[sel,]
    alt.daily.modelled.spinup <- alt.daily.modelled.spinup[sel,]
    alt.yearly.modelled <- alt.yearly.modelled[sel,]
    alt.yearly.modelled.spinup <- alt.yearly.modelled.spinup[sel,]

# exclude models with strange results in single years
exclude <- rep(NA, nrow(alt.yearly.modelled))
    for (i in 1:nrow(alt.yearly.modelled)){
        exclude[i] <- max(alt.yearly.modelled[i,], na.rm=T)                        # calculate maximum yearly alt
    }
    sel <- which(exclude < 2)                                                      # select models with alt > 2 m, which is unrealistic
    
    alt.rmse <- alt.rmse[sel]
    alt.daily.modelled <- alt.daily.modelled[sel,]
    alt.daily.modelled.spinup <- alt.daily.modelled.spinup[sel,]
    alt.yearly.modelled <- alt.yearly.modelled[sel,]
    alt.yearly.modelled.spinup <- alt.yearly.modelled.spinup[sel,]
    
###################################################################################################
# +++++ rank models after their RMSE
    alt.rmse.ordered <- alt.rmse[order(alt.rmse)]                                      # rank models after their rmse

# rank models in objects
alt.daily.modelled.ordered <- alt.daily.modelled                                   # prepare object with experiments ranked after their rmse
    rownames(alt.daily.modelled.ordered) <- names(alt.rmse.ordered)
    alt.daily.modelled.ordered[,] <- NA
    
alt.daily.modelled.ordered.spinup <- alt.daily.modelled.spinup
    rownames(alt.daily.modelled.ordered.spinup) <- names(alt.rmse.ordered)
    alt.daily.modelled.ordered.spinup[,] <- NA

alt.yearly.modelled.ordered <- alt.yearly.modelled
    rownames(alt.yearly.modelled.ordered) <- names(alt.rmse.ordered)
    alt.yearly.modelled.ordered[,] <- NA
    
alt.yearly.modelled.ordered.spinup <- alt.yearly.modelled.spinup
    rownames(alt.yearly.modelled.ordered.spinup) <- names(alt.rmse.ordered)    
    alt.yearly.modelled.ordered.spinup[,] <- NA

# sort data into objects
for (i in 1:length(alt.rmse.ordered)){                                             # sort experiments after their ranking in rmse
    sel <- which(row.names(alt.daily.modelled) == names(alt.rmse.ordered)[i])
    
    alt.daily.modelled.ordered[i,] <- alt.daily.modelled[sel,]
    alt.daily.modelled.ordered.spinup[i,] <- alt.daily.modelled.spinup[sel,]
    alt.yearly.modelled.ordered[i,] <- alt.yearly.modelled[sel,]
    alt.yearly.modelled.ordered.spinup[i,] <- alt.yearly.modelled.spinup[sel,]
}

# save results to disk
save(alt.daily.modelled.ordered,
     file="01_data/soil/sampling/sampling-results/alt.daily.modelled.ordered.RData")
save(alt.daily.modelled.ordered.spinup,
     file="01_data/soil/sampling/sampling-results/alt.daily.modelled.ordered.spinup.RData")
save(alt.yearly.modelled.ordered,
     file="01_data/soil/sampling/sampling-results/alt.yearly.modelled.ordered.RData")
save(alt.yearly.modelled.ordered.spinup,
     file="01_data/soil/sampling/sampling-results/alt.yearly.modelled.ordered.spinup.RData")
save(alt.rmse.ordered,
     file="01_data/soil/sampling/sampling-results/alt.rmse.ordered.RData")

# cleanup
rm(e, i, s, sel, exclude)

###################################################################################################
# +++++ delete crap, cleanup and renaming for plotting
###################################################################################################
# +++++ copy objects
alt.daily.modelled <- alt.daily.modelled.ordered
alt.daily.modelled.spinup <- alt.daily.modelled.ordered.spinup
alt.yearly.modelled <- alt.yearly.modelled.ordered
alt.yearly.modelled.spinup <- alt.yearly.modelled.ordered.spinup


# ++++ replace NA with 0 for nicer plots
sel <- which(is.na(alt.daily.modelled))
    alt.daily.modelled[sel] <- 0
    
sel <- which(is.na(alt.daily.modelled.spinup))
    alt.daily.modelled.spinup[sel] <- 0


# +++++ observations
alt.daily.observed[is.na(alt.daily.observed)] <- 0                                 # replace NA with 0 for nicer plots


alt.yearly.observed.georg <- array(NA, c(length(alt.yearly.observed), 3))          # yearly observed alt (as told by georg)
    rownames(alt.yearly.observed.georg) <- names(alt.yearly.observed)
    colnames(alt.yearly.observed.georg) <- c("best", "min", "max")
    
    alt.yearly.observed.georg[,1] <- c(1.18, NA, 1.34, 1.54, 1.56, 1.38, NA, 1.22, 1.14)         
    alt.yearly.observed.georg[,2] <- c(1.1, NA, 1.26, 1.38, 1.46, 1.26, NA, 1.16, 1.06)
    alt.yearly.observed.georg[,3] <- c(1.3, NA, 1.46, 1.78, 1.88, 1.46, NA, 1.32, 1.24)
    
# +++++ prepare start and end for plots
s <- which(colnames(alt.daily.modelled) == names(alt.daily.observed)[1])                                          # start of observations
e <- which(colnames(alt.daily.modelled) == names(alt.daily.observed)[length(alt.daily.observed)])                 # end of observations

s.spinup <- which(colnames(alt.daily.modelled.spinup) == names(alt.daily.observed)[1])                            # start of spinup
e.spinup <- which(colnames(alt.daily.modelled.spinup) == names(alt.daily.observed)[length(alt.daily.observed)])   # end of spinup

rm(alt.daily.modelled.ordered, alt.daily.modelled.ordered.spinup,                  # cleanup
   alt.yearly.modelled.ordered, alt.yearly.modelled.ordered.spinup,
   sel, experiments)

###################################################################################################
# +++++ make plots
###################################################################################################
# prepare stuff for plots
# margins for plotting
par.mar.1 <- c(2, 4, 1, 0.2)
par.mar.2 <- c(3, 4, 1, 4)

date.spinup <- as.Date(colnames(alt.daily.modelled.spinup))
date <- as.Date(colnames(alt.daily.modelled))

years <- colnames(alt.yearly.modelled)
years.spinup <- colnames(alt.yearly.modelled.spinup)

ylim <- c(2, 0)
ylim25 <- c(2.5, 0)

# # calculate na in observed temperature data for NA-lines in plots
# load("01_data/bohrloch/bh3_postprocessed-hourly.RData")                            # load observed soil-temperature
# days <- unique(substr(rownames(temp.obs), 1, 10))
# na <- rep(NA, length(date.spinup))
#     names(na) <- date.spinup
# 
# start <- which(names(na) == head(substr(rownames(temp.obs), 1, 10), 1))
# end <- which(names(na) == tail(substr(rownames(temp.obs), 1, 10), 1))
# 
# na[1:start-1] <- 1                                                                 # set to 1 until beginning of measurements
# 
# for (i in 1:length(days)){
#     sel <- which(substr(rownames(temp.obs), 1, 10) == days[i])                     # look per day in the measurement-period if measurements are available or not
#     if (is.na(sum(temp.obs[sel,]))){                                               # set to 1 if no measurement has been found in this day
#         na[start:end][i] <- 1    
#     }
# }
# 
# na.spinup <- na                                                                    # na-for observations with spinup
# na <- na[which(names(na) == colnames(alt.daily.modelled)[1]) :                     # na for observations without spinup
#          which(names(na) == colnames(alt.daily.modelled)[ncol(alt.daily.modelled)])]

###################################################################################################
# +++++ POLYGONPLOT daily course of active layer thickness (without spinup, best 50)
# create object for polygon-plot: (column 1: lower range, column 2: upper range (and reversed))
sel.sdd <- rep(50000, length(sdd))
for (i in c(1, 3, 4, 5, 6, 8)){
    sel.sdd[i] <- which(date == sdd[i])                                    # snow disappearance day    
}

polygonplot <- array(NA, c(ncol(alt.daily.modelled), 2))
    rownames(polygonplot) <- date

    for (i in 1:nrow(polygonplot)){
        polygonplot[i,] <- range(alt.daily.modelled[1:50,i], na.rm=T)              # calculate data
        # polygonplot[i,] <- c(0, max(alt.daily.modelled[1:50,i], na.rm=T))              # calculate data
    }
    polygonplot[,2] <- rev(polygonplot[,2])


# make plot
png("03_plots/sampling/alt-daily-selected.png", height=12, width=20, res=450, units="cm")
par(mar = par.mar.1)
    plot(date, alt.daily.modelled[1,], type="l",
         ylim=ylim, xlab="", ylab="DEPTH [m]", col="white", xaxt="n")
    axis(1, labels=c(seq(2003, 2016, by=2), 2017), at=c(12350, 13030, 13760, 14530, 15250, 16000, 
                                               16700, 17400))
    # abline(v=date[sel.na], col="lightgrey")                                        # plot na´s
    
    grid()
    
    polygon(c(date, rev(date)), c(polygonplot), col="dodgerblue", border=F)

    lines(date[s:e], alt.daily.observed, col="black", lwd=1)                       # observed active layer thickness
    
    abline(v=date[sel.sdd], col="purple", lty=3, lwd=2)                                   # snow disappearance day
    
    abline(h=0, col="darkgrey", lwd=2)                                                  # soil surface
    
    # title("Daily course of Active Layer Thickness: Selected Models")
    legend("bottomleft", c("observed snow disappearance day", "observed", "modelled data range"), cex=.7,
           lty=c(3, 1, 1),
           col=c("purple", "black", "dodgerblue"),
           lwd=c(2, 2, 10))
dev.off()

###########################################################################################
png("03_plots/sampling/alt-yearly-lines.png", height=12, width=20, res=450, units="cm")
    par(mar = par.mar.1)    
    plot(years, alt.yearly.modelled[1,], type="l", col="white",
         ylim = c(2.5, 0), ylab = "DEPTH [m]", xlab = "")
    grid() ; abline(h=0, col="darkgrey", lwd=2)
    
    for (i in 1:nrow(alt.yearly.modelled)){
        lines(years, alt.yearly.modelled[i,], col="lightsteelblue")                # plot all models
    }    
    
    for (i in 1:50){
    # for (i in 1:10){
        lines(years, alt.yearly.modelled[i,], col="dodgerblue")
    }
    
    mean.alt.year <- rep(NA, ncol(alt.yearly.modelled))                            # calculate mean of best 5
        names(mean.alt.year) <- colnames(alt.yearly.modelled)
        for (i in 1:length(mean.alt.year)){
            mean.alt.year[i] <- median(alt.yearly.modelled[1:50,i])
        }
        points(years, mean.alt.year, col="purple", pch=15, type="o", lwd=2)        # plot mean of best 5
        
    sel <- c(1:6, 8:9)                                                             # select all but 2009 and 2014
    lines(names(alt.yearly.observed[sel]), alt.yearly.observed[sel],               # plot observed values
          col="black", type="p", pch=15, lwd=3)
    
    title("Yearly Active Layer Thickness")
    legend("bottomleft", c("models", "selected models", "median of selected models", "observation"), cex=.7,
           lty=1,
           col=c("lightsteelblue", "dodgerblue", "purple", "black"),
           lwd=c(2, 2, 2, 0), pch=c(1000, 1000, 1000, 15, 15))    
dev.off()





#####################################################################################################################
# +++++ POLYGONPLOT yearly course of active layer thickness (with spinup, best 50)
# create object for polygon-olot: (column 1: lower range, column 2: upper range (and reversed))
polygonplot <- array(NA, c(ncol(alt.yearly.modelled.spinup), 2))
    rownames(polygonplot) <- years.spinup

polygonplot.all <- array(NA, c(ncol(alt.yearly.modelled.spinup), 2))
    rownames(polygonplot.all) <- years.spinup
    
for (i in 1:nrow(polygonplot)){
    polygonplot[i,] <- range(alt.yearly.modelled.spinup[1:50,i], na.rm=T)           # calculate data
    polygonplot.all[i,] <- range(alt.yearly.modelled.spinup[,i], na.rm=T)           # calculate data
}

polygonplot[,2] <- rev(polygonplot[,2])
polygonplot.all[,2] <- rev(polygonplot.all[,2])

# calculate median for best 50
mean.alt.year <- rep(NA, ncol(alt.yearly.modelled.spinup))                                 # calculate mean of best 50
    names(mean.alt.year) <- colnames(alt.yearly.modelled.spinup)

    for (i in 1:length(mean.alt.year)){
        mean.alt.year[i] <- median(alt.yearly.modelled.spinup[1:50,i])
    }

alt.quantile <- array(NA, c(2, ncol(alt.yearly.modelled.spinup)))
    colnames(alt.quantile) <- years.spinup
    
    for (i in 1:ncol(alt.quantile)){
        alt.quantile[,i] <- quantile(alt.yearly.modelled.spinup[1:50,i], probs=c(0, 0.05, 0.95, 1))[2:3]
    }    


# make plot
png("03_plots/sampling/alt-yearly-spinup.png", height=12, width=20, res=450, units="cm")
par(mar = par.mar.1)
    plot(years.spinup, alt.yearly.modelled.spinup[1,], type="l",
         ylim=ylim25, xlab="", ylab="DEPTH [m]", col="white")
    
    abline(v=years.spinup[1:which(years.spinup == "2001")], col="lightgrey", lwd=40)     # spinup period
    box()
    
    polygon(c(years.spinup, rev(years.spinup)), c(polygonplot.all),                      # all models
            col="lightsteelblue1", border=F)
    
    polygon(c(years.spinup, rev(years.spinup)), c(polygonplot),                          # selected models
            col="dodgerblue", border=F)
    
    grid()
    
    lines(years.spinup, alt.quantile[1,], col="darkviolet", lwd=2, lty=3)   # plot 5% percentile of best 50
    lines(years.spinup, alt.quantile[2,], col="darkviolet", lwd=2, lty=3)   # plot 95% percentile of best 50
    points(years.spinup, mean.alt.year, col="darkviolet", pch=15, type="o", lwd=3, cex=1.2)    # plot median of best 50
    
    for (y in c(1, 3:6, 8:9)){
        plotyear <- as.numeric(rep(rownames(alt.yearly.observed.georg)[y], 2))
        lines(plotyear, alt.yearly.observed.georg[y,2:3])                           # vertical uncertainty line
        
        lines(c(plotyear[1] - .1, plotyear[2] + .1),                                # upper boundary
              rep(alt.yearly.observed.georg[y,2], 2))
        
        lines(c(plotyear[1] - .1, plotyear[2] + .1),                                # lower boundary
              rep(alt.yearly.observed.georg[y,3], 2))
    }
    
    points(years[c(6:11, 13:14)], alt.yearly.observed[c(1:6, 8:9)],                 # best estimate observed
           col="black", pch=19, cex=1.3) 
    
    abline(h=0, col="darkgrey", lwd=2)                                              # soil surface
    text(1998.5, 0.5, "SPINUP", col="white", cex=2)
    minor.tick(nx=5, ny=5)
    
    legend("bottomleft", c("observations (best estimate + uncertainty)", 
                           "all models (data range)", "selected models (data range)",
                           "selected models (median)",
                           "selected models (5. and 95. percentile)"), 
           cex=.7, lty=c(1, 1, 1, 1, 3), pch=c(19, 1000, 1000, 15, 1000),
           col=c("black", "lightsteelblue1", "dodgerblue", "darkorchid1", "darkorchid1"),
           lwd=c(2, 10, 10, 2, 2), pt.cex=c(1, 1.3, 1, 1, 1, 1))
    box()
dev.off()


########################
# +++++ POLYGONPLOT yearly course of active layer thickness (without spinup, best 50)
# create object for polygon-olot: (column 1: lower range, column 2: upper range (and reversed))
polygonplot <- array(NA, c(ncol(alt.yearly.modelled), 2))
    rownames(polygonplot) <- years

polygonplot.all <- array(NA, c(ncol(alt.yearly.modelled), 2))
    rownames(polygonplot.all) <- years

for (i in 1:nrow(polygonplot)){
    polygonplot[i,] <- range(alt.yearly.modelled[1:50,i], na.rm=T)                      # calculate data
    polygonplot.all[i,] <- range(alt.yearly.modelled[,i], na.rm=T)                      # calculate data
}

polygonplot[,2] <- rev(polygonplot[,2])
polygonplot.all[,2] <- rev(polygonplot.all[,2])

# make plot
png("03_plots/sampling/alt-yearly.png", height=12, width=20, res=450, units="cm")
    par(mar = par.mar.1)
    plot(years, alt.yearly.modelled[1,], type="l",
         ylim=ylim25, xlab="", ylab="DEPTH [m]", col="white")
    
    polygon(c(years, rev(years)), c(polygonplot.all), col="lightsteelblue1", border=F)
    
    polygon(c(years, rev(years)), c(polygonplot), col="dodgerblue", border=F)
    
    grid()
    
    for (y in c(1, 3:6, 8:9)){
        plotseq <- seq(alt.yearly.observed.georg[y,2],
                       alt.yearly.observed.georg[y,3],
                       length=400)
        for (x in 1:length(plotseq)){
            points(rownames(alt.yearly.observed.georg)[y], 
                   plotseq[x], col="darkgrey", pch=18)
        }
    }
    
    points(years[c(6:11, 13:14)], alt.yearly.observed[c(1:6, 8:9)], col="black", pch=15, cex=1.3) # best estimate observed
    
    # points(years[c(6:11, 13:14)], alt.yearly.observed[c(1:6, 8:9)],                # observed active layer thickness
    #       col="black", lwd=2, pch=15, type="p")    
    
    abline(h=0, col="darkgrey", lwd=2)                                                  # soil surface
    
    # title("Yearly maximum ALT, Validated with: 2008, 2010, 2011, 2012, 2013, 2015, 2016")
    
    legend("bottomleft", c("surface", "observations (uncertainty range)", "observations (best estimate)", "all models (data range)", "selected models (data range)"), cex=.7,
           lty=2, pch=c(1000, 15, 1000, 1000),
           col=c("darkgrey", "black", "lightsteelblue1", "dodgerblue"),
           lwd=c(5, 0, 10, 10), pt.cex=c(1, 1, 1.3, 1, 1))
    box()
    
dev.off()

################
# +++++ POLYGONPLOT daily course of active layer thickness (including spinup)
# create object for polygon-olot: (column 1: lower range, column 2: upper range (and reversed))
polygonplot <- array(NA, c(ncol(alt.daily.modelled.spinup), 2))
    rownames(polygonplot) <- date.spinup

    for (i in 1:nrow(polygonplot)){
        polygonplot[i,] <- range(alt.daily.modelled.spinup[,i], na.rm=T)                      # calculate data
    }
polygonplot[,2] <- rev(polygonplot[,2])

# make plot
png("03_plots/sampling/alt-daily-spinup.png", height=12, width=20, res=450, units="cm")
par(mar = par.mar.1)
    plot(date.spinup, alt.daily.modelled.spinup[1,], type="l",
         ylim=c(3, 0), xlab="", ylab="DEPTH [m]", col="white")
    abline(v=date.spinup[1:which(date.spinup == "2002-06-30")], col="lightgrey", lwd=2) # end of spinup period
    box() ; grid()
    
    polygon(c(date.spinup, rev(date.spinup)), c(polygonplot),
            col="dodgerblue", border=F)
    
    lines(date.spinup[s.spinup:e.spinup], alt.daily.observed, col="black", lwd=1)  # observed active layer thickness
    
    abline(h=0, col="darkgrey", lwd=2)                                                  # soil surface
    
    # title("Daily course of Active Layer Thickness")
    legend("bottomright", c("spinup period", "observed", "modelled data range"), cex=.7,
           lty=1,
           col=c("lightgrey", "black", "dodgerblue"),
           lwd=c(10, 2, 10))
dev.off()




# +++++ POLYGONPLOT daily course of active layer thickness (including spinup, best 50, each year one plot)
# create object for polygon-olot: (column 1: lower range, column 2: upper range (and reversed))
year <- c(2008, 2010:2016)

# make plot
png("03_plots/sampling/alt-daily-selected-multiple.png", height=20, width=30, res=450, units="cm")
par(mfrow=c(3, 3),
    mar = par.mar.1)

    # +++++ first year, with legend
        # calculate
    y <- 1
        sel <- which(substr(colnames(alt.daily.modelled.spinup), 1, 4) == year[y])
        
        polygonplot <- array(NA, c(length(sel), 2))
            rownames(polygonplot) <- as.character(date.spinup[sel])
        
        for (i in 1:nrow(polygonplot)){
            polygonplot[i,] <- range(alt.daily.modelled.spinup[1:50,sel[i]], na.rm=T)                      # calculate data
        }
        polygonplot[,2] <- rev(polygonplot[,2])
        
        # plot
        plot(date.spinup[sel], alt.daily.modelled.spinup[1,sel], type="l",
             ylim=ylim, xlab="", ylab="DEPTH [m]", col="white")
        grid()
        
        polygon(c(date.spinup[sel], rev(date.spinup[sel])), c(polygonplot),
                col="dodgerblue", border=F)
        
        lines(date.spinup[s.spinup:e.spinup], alt.daily.observed, col="black", lwd=1)  # observed active layer thickness
        
        sel.sdd <- which(date.spinup == sdd[y])                                    # snow disappearance day
        abline(v=date.spinup[sel.sdd], col="purple", lty=2)    
        
        abline(h=0, col="darkgrey", lwd=2)                                              # soil surface
        title(year[y])
        legend("bottomleft", c("observed snow disappearance day", "observed", "modelled data range"), cex=.7,
               lty=c(2, 1, 1),
               col=c("purple", "black", "dodgerblue"),
               lwd=c(1, 2, 10))
    
    # +++++ other years
    for (y in 2:length(year)){
        # calculate
        sel <- which(substr(colnames(alt.daily.modelled.spinup), 1, 4) == year[y])
        
        polygonplot <- array(NA, c(length(sel), 2))
            rownames(polygonplot) <- as.character(date.spinup[sel])
        
        for (i in 1:nrow(polygonplot)){
            polygonplot[i,] <- range(alt.daily.modelled.spinup[1:50,sel[i]], na.rm=T)                      # calculate data
        }
        polygonplot[,2] <- rev(polygonplot[,2])
    
        # plot
        plot(date.spinup[sel], alt.daily.modelled.spinup[1,sel], type="l",
             ylim=ylim, xlab="", ylab="DEPTH [m]", col="white")
        grid()
        
        polygon(c(date.spinup[sel], rev(date.spinup[sel])), c(polygonplot),
                col="dodgerblue", border=F)
        
        lines(date.spinup[s.spinup:e.spinup], alt.daily.observed,
              col="black", lwd=1)                                                  # observed active layer thickness
        
        sel.sdd <- which(date.spinup == sdd[y])                                    # snow disappearance day
        abline(v=date.spinup[sel.sdd], col="purple", lty=2)    
        abline(h=0, col="darkgrey", lwd=2)                                                  # soil surface
        title(year[y])
    }
dev.off()



# +++++ POLYGONPLOT daily course of active layer thickness (including spinup, best 50, each year one plot)
# create object for polygon-olot: (column 1: lower range, column 2: upper range (and reversed))
year <- c(2008, 2010:2016)

# make plot
for (y in 1:length(year)){
    png(paste0("03_plots/sampling/alt-daily-selected-", year[y], ".png"), height=12, width=20, res=450, units="cm")    
        par(mar=par.mar.1)
        
        sel <- which(substr(colnames(alt.daily.modelled.spinup), 1, 4) == year[y])
        
        polygonplot <- array(NA, c(length(sel), 2))
            rownames(polygonplot) <- as.character(date.spinup[sel])
        
        for (i in 1:nrow(polygonplot)){
            polygonplot[i,] <- range(alt.daily.modelled.spinup[1:50,sel[i]], na.rm=T)                      # calculate data
        }
        polygonplot[,2] <- rev(polygonplot[,2])
        
        # plot
        plot(date.spinup[sel], alt.daily.modelled.spinup[1,sel], type="l",
             ylim=ylim, xlab="", ylab="DEPTH [m]", col="white")
        grid()
        
        polygon(c(date.spinup[sel], rev(date.spinup[sel])), c(polygonplot),
                col="dodgerblue", border=F)
        
        lines(date.spinup[s.spinup:e.spinup], alt.daily.observed, col="black", lwd=2)  # observed active layer thickness
        
        sel.sdd <- which(date.spinup == sdd[y])                                    # snow disappearance day
        abline(v=date.spinup[sel.sdd], col="purple", lty=3, lwd=2)    
        
        abline(h=0, col="darkgrey", lwd=2)                                              # soil surface
        title(year[y])
        legend("bottomleft", c("observed snow disappearance day", "observed", "modelled data range"), cex=.7,
               lty=c(3, 1, 1),
               col=c("purple", "black", "dodgerblue"),
               lwd=c(2, 2, 10))
    dev.off()
    
}


# show range
round(range(alt.rmse.ordered[1:50], na.rm=T), 2)
