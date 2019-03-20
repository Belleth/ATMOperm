# Project ATMOperm
# Interprets the results of the 10000 simulations and makes temperature profile-graphics
# Gernot Resch: 26.02.2019
###################################################################################################
rm(list=ls()) ; gc()                                                                     # initial cleanup
Sys.setlocale("LC_TIME", "C")                                                            # change language to english for plots

setwd("~/work/uni graz/projects/atmoperm/")                                              # switch to directory
# setwd("~/atmoperm")
###################################################################################################
# +++++ prepare datasets
###################################################################################################
source("02_scripts/work_files/f_rmse.R")                                                      # load rmse-function

###################################################################################################
# +++++ prepare datasets
# load observed temperature data
load("01_data/bohrloch/bh3_postprocessed-hourly-interpolated.RData")
temp.observed <- temp.obs.int ; rm(temp.obs.int)
    colnames(temp.observed) <- as.numeric(colnames(temp.observed))

# load first modelled file
load("01_data/soil/sampling/sampling-results/alt.rmse.ordered.RData")

files <- paste0("temp.modelled.full.sonnblick_", names(alt.rmse.ordered), ".RData")      # create list of modelled datasets
    load(paste0("01_data/soil/sampling/", files[1]))
    
experiments <- gsub(files, pattern="temp.modelled.full.sonnblick_", replacement="")
experiments <- gsub(experiments, pattern=".RData", replacement="")

# reduce observations
sel <- colnames(temp.observed) %in% colnames(temp.modelled.full)                          # reduce observed depths to modelled depths
temp.observed <- temp.observed[,sel]

sel <- which(substr(rownames(temp.observed), 12, 19) == "23:59:59")                       # reduce observations to 00:00 (model output time)
temp.observed <- temp.observed[sel,]
    rownames(temp.observed) <- substr(rownames(temp.observed), 1, 10)

# prepare rmse-objects
soil.rmse <- array(NA, c(length(experiments), ncol(temp.observed)))
    rownames(soil.rmse) <- experiments
    colnames(soil.rmse) <- colnames(temp.observed)

soil.rmse.monthly <- soil.rmse  

###################################################################################################
# +++++ iterate through all experiments and calculate modelling score      
for (f in 1:length(files)){
    load(paste0("01_data/soil/sampling/", files[f]))                                     # load modelled data
    
    ###################################################################################################    
    # reduce modelled data to observations
    days.obs <- rownames(temp.observed)
    days.mod <- rownames(temp.modelled.full)

    include <- days.mod %in% days.obs                                                        # which days are both in days.obs and days.mod? (längeres vor kürzerem)
    sel <- days.mod[include]                                                                 # select days in modelled days
    temp.modelled.full <- temp.modelled.full[sel,]                                           # reduce modelled dataset to selected days
    
    ###################################################################################################    
    # prepare date
    days <- days.obs
        rm(days.obs, days.mod)
    
    months <- substr(days, 6, 7)
        months.unique <- unique(months)
    
    # calculate monthly mean temperatures
    temp.observed.monthly <- array(NA, c(12, ncol(temp.observed)))
        colnames(temp.observed.monthly) <- colnames(temp.observed)
        rownames(temp.observed.monthly) <- 1:12
        
    temp.modelled.monthly <- temp.observed.monthly
    
    for (i in 1:ncol(temp.observed.monthly)){
        temp.observed.monthly[,i] <- unlist(tapply(temp.observed[,i], months, mean, na.rm=T))
        temp.modelled.monthly[,i] <- unlist(tapply(temp.modelled.full[,i], months, mean, na.rm=T))
    }    
    
    # calculate bias
    temp.bias <- temp.observed - temp.modelled.full
    temp.bias.monthly <- temp.observed.monthly - temp.modelled.monthly
    
    # calculate rmse per sensor-depth
    for (i in 1:ncol(soil.rmse)){
        soil.rmse[f,i] <- rmse(temp.bias[,i])
        soil.rmse.monthly[f,i] <- rmse(temp.bias.monthly[,i])
    }
}
###################################################################################################
# +++++ sort rmse-objects after their rmse-value
soil.rmse.mean <- rep(NA, rep(nrow(soil.rmse)))                                          # mean rmse for each sensor
    names(soil.rmse.mean) <- rownames(soil.rmse)
soil.rmse.monthly.mean <- soil.rmse.mean                                                 # mean rmse for each sensor (monthly mean)
soil.rmse.mean.5 <- soil.rmse.mean                                                       # mean rmse per sensor but only sensors between 0 - 5 m depth

sel <- which(colnames(soil.rmse) == 5)                                                  # select only sensors between 0 - 5 m depth

for (i in 1:length(soil.rmse.mean)){
    soil.rmse.mean[i] <- mean(soil.rmse[i,])                                             # mean rmse for each sensor
    soil.rmse.monthly.mean[i] <- mean(soil.rmse.monthly[i,])                             # mean rmse for each sensor, monthly mean for calculations
    soil.rmse.mean.5[i] <- mean(soil.rmse[i,1:sel])
}

# rank models after their rmse
soil.rmse.mean <- soil.rmse.mean[order(soil.rmse.mean)]                                      
soil.rmse.monthly.mean <- soil.rmse.monthly.mean[order(soil.rmse.monthly.mean)]
soil.rmse.mean.5 <- soil.rmse.mean.5[order(soil.rmse.mean.5)]

###################################################################################################
# compare alt and profile
load("01_data/soil/sampling/sampling-results/alt.rmse.ordered.RData")

# +++++ compare alt and profiles
head(alt.rmse.ordered)
head(soil.rmse.mean)

# +++++ normalize for making °C and m comparable
# (x - min) / (max - min)
norm.alt <- rep(NA, length(alt.rmse.ordered))
    names(norm.alt) <- names(alt.rmse.ordered)
norm.profile <- rep(NA, length(soil.rmse.mean))
    names(norm.profile) <- names(soil.rmse.mean)

for (i in 1:length(norm.alt)){
    norm.alt[i] <- round((alt.rmse.ordered[i] - min(alt.rmse.ordered)) / 
                             (max(alt.rmse.ordered) - min(alt.rmse.ordered)), 4)
    
    norm.profile[i] <- round((soil.rmse.mean[i] - min(soil.rmse.mean)) /
                                 (max(soil.rmse.mean) - min(soil.rmse.mean)), 4)
}

# summe bilden und neu ordnen
norm.sum <- rep(NA, length(norm.alt))
    names(norm.sum) <- names(norm.alt)
for (i in 1:length(norm.sum)){
    a <- which(names(norm.alt) == names(norm.sum)[i])                 # where is alt
    p <- which(names(norm.profile) == names(norm.sum)[i])             # where is soil profile
    norm.sum[i] <- norm.alt[a] + norm.profile[p]                      # make sum
}
norm.sum <- norm.sum[order(norm.sum)]                               # new ranking

# comparison-array
comparison <- array(NA, c(3, length(norm.sum)))
    rownames(comparison) <- c("alt", "soil", "combined")

comparison[1,] <- names(alt.rmse.ordered)
comparison[2,] <- names(soil.rmse.mean)
comparison[3,] <- names(norm.sum)
save(comparison, file="01_data/soil/sampling/sampling-results/comparison-results.RData")


# +++++ export experiments which are used for scenarios
# sampling.results <- names(soil.rmse.mean)
sampling.results <- names(norm.sum)
save(sampling.results, file="01_data/soil/sampling/sampling-results/sampling.results.RData")

###################################################################################################
# +++++ profiles of best 10 combined results
for (f in 1:10){
    load(paste0("01_data/soil/sampling/temp.modelled.full.sonnblick_",                   # load modelled data 
                sampling.results[f], ".RData"))
    
    ###################################################################################################    
    # reduce modelled data to observations
    days.obs <- rownames(temp.observed)
    days.mod <- rownames(temp.modelled.full)
    
    include <- days.mod %in% days.obs                                                        # which days are both in days.obs and days.mod? (längeres vor kürzerem)
    sel <- days.mod[include]                                                                 # select days in modelled days
    temp.modelled.full <- temp.modelled.full[sel,]                                           # reduce modelled dataset to selected days
    
    ###################################################################################################    
    # prepare date
    days <- days.obs
    rm(days.obs, days.mod)
    
    months <- substr(days, 6, 7)
    months.unique <- unique(months)
    
    # calculate monthly mean temperatures
    temp.observed.monthly <- array(NA, c(12, ncol(temp.observed)))
        colnames(temp.observed.monthly) <- colnames(temp.observed)
        rownames(temp.observed.monthly) <- 1:12
    
    temp.modelled.monthly <- temp.observed.monthly
    
    for (i in 1:ncol(temp.observed.monthly)){
        temp.observed.monthly[,i] <- unlist(tapply(temp.observed[,i], months, mean, na.rm=T))
        temp.modelled.monthly[,i] <- unlist(tapply(temp.modelled.full[,i], months, mean, na.rm=T))
    }    
    
    # calculate bias
    temp.bias <- temp.observed - temp.modelled.full
    temp.bias.monthly <- temp.observed.monthly - temp.modelled.monthly
    
    # calculate rmse per sensor-depth
    for (i in 1:ncol(soil.rmse)){
        soil.rmse[f,i] <- rmse(temp.bias[,i])
        soil.rmse.monthly[f,i] <- rmse(temp.bias.monthly[,i])
    }
    
    ###################################################################################################    
    # prepare plot-stuff
    depth <- colnames(temp.observed)
    ylim <- c(20, 0)
    xlim <- c(-5, 5)
    
    lty <- rep(1:3, 4)
    col <- rep(c("dodgerblue", "blue", "orange", "purple"), 3)
    
    alt.rank <- which(comparison[1,] == comparison[3,f])
    profile.rank <- which(comparison[2,] == comparison[3,f])
    ###################################################################################################
    
    png(paste0("03_plots/profiles/combined-monthly-mean-",  sampling.results[f], ".png"), height=20, width=30, res=450, units="cm")
    plot(rep(0, ncol(temp.observed)), depth, ylim = ylim, xlim = xlim, col="white", xlab=" soil temperature [°C]", ylab="depth [m]")
    grid() ; abline(h = 0, col="red", lwd=2)
    
    for (i in 1:nrow(temp.observed.monthly)){
        lines(temp.observed.monthly[i,], depth, lty=lty[i], col=col[i], lwd=2)
    }
    
    for (i in 1:nrow(temp.modelled.monthly)){
        lines(temp.modelled.monthly[i,], depth, lty=lty[i], lwd=2) # col=col[i], lwd=2)
    }
    
    abline(v=0, col="black", lty=2, lwd=2)
    #title(paste0(comparison[3,f], "\nRanking: combined: ", f, " alt: ", alt.rank, " profile: ", profile.rank, 
     #            "\n RMSE ALT: ", ))
    # legend("bottomleft", rownames(temp.bias.monthly), lty=lty, col=col, lwd=2, cex=.7)
    
    dev.off()
    ###################################################################################################
}

# +++++ make plots for best 10 alt
for (f in 1:10){
    load(paste0("01_data/soil/sampling/temp.modelled.full.sonnblick_",                   # load modelled data 
                names(alt.rmse.ordered)[f], ".RData"))
    
    ###################################################################################################    
    # reduce modelled data to observations
    days.obs <- rownames(temp.observed)
    days.mod <- rownames(temp.modelled.full)
    
    include <- days.mod %in% days.obs                                                        # which days are both in days.obs and days.mod? (längeres vor kürzerem)
    sel <- days.mod[include]                                                                 # select days in modelled days
    temp.modelled.full <- temp.modelled.full[sel,]                                           # reduce modelled dataset to selected days
    
    ###################################################################################################    
    # prepare date
    days <- days.obs
    rm(days.obs, days.mod)
    
    months <- substr(days, 6, 7)
    months.unique <- unique(months)
    
    # calculate monthly mean temperatures
    temp.observed.monthly <- array(NA, c(12, ncol(temp.observed)))
    colnames(temp.observed.monthly) <- colnames(temp.observed)
    rownames(temp.observed.monthly) <- 1:12
    
    temp.modelled.monthly <- temp.observed.monthly
    
    for (i in 1:ncol(temp.observed.monthly)){
        temp.observed.monthly[,i] <- unlist(tapply(temp.observed[,i], months, mean, na.rm=T))
        temp.modelled.monthly[,i] <- unlist(tapply(temp.modelled.full[,i], months, mean, na.rm=T))
    }    
    
    # calculate bias
    temp.bias <- temp.observed - temp.modelled.full
    temp.bias.monthly <- temp.observed.monthly - temp.modelled.monthly
    
    # calculate rmse per sensor-depth
    for (i in 1:ncol(soil.rmse)){
        soil.rmse[f,i] <- rmse(temp.bias[,i])
        soil.rmse.monthly[f,i] <- rmse(temp.bias.monthly[,i])
    }
    
    ###################################################################################################    
    # prepare plot-stuff
    depth <- colnames(temp.observed)
    ylim <- c(20, 0)
    xlim <- c(-5, 5)
    
    lty <- rep(1:3, 4)
    col <- rep(c("dodgerblue", "blue", "orange", "purple"), 3)
    ###################################################################################################
    
    png(paste0("03_plots/profiles/alt-monthly-mean-",  names(alt.rmse.ordered)[f], ".png"), height=20, width=30, res=450, units="cm")
    plot(rep(0, ncol(temp.observed)), depth, ylim = ylim, xlim = xlim, col="white", xlab=" soil temperature [°C]", ylab="depth [m]")
    grid() ; abline(h = 0, col="red", lwd=2)
    
    for (i in 1:nrow(temp.observed.monthly)){
        lines(temp.observed.monthly[i,], depth, lty=lty[i], col=col[i], lwd=2)
    }
    
    for (i in 1:nrow(temp.modelled.monthly)){
        lines(temp.modelled.monthly[i,], depth, lty=lty[i], lwd=2) # col=col[i], lwd=2)
    }
    
    abline(v=0, col="black", lty=2, lwd=2)
    
    # legend("bottomleft", rownames(temp.bias.monthly), lty=lty, col=col, lwd=2, cex=.7)
    
    dev.off()
    ###################################################################################################
}

# +++++ make plots for best 10 profiles
for (f in 1:10){
    load(paste0("01_data/soil/sampling/temp.modelled.full.sonnblick_",                   # load modelled data 
                names(soil.rmse.mean)[f], ".RData"))
    
    ###################################################################################################    
    # reduce modelled data to observations
    days.obs <- rownames(temp.observed)
    days.mod <- rownames(temp.modelled.full)
    
    include <- days.mod %in% days.obs                                                        # which days are both in days.obs and days.mod? (längeres vor kürzerem)
    sel <- days.mod[include]                                                                 # select days in modelled days
    temp.modelled.full <- temp.modelled.full[sel,]                                           # reduce modelled dataset to selected days
    
    ###################################################################################################    
    # prepare date
    days <- days.obs
    rm(days.obs, days.mod)
    
    months <- substr(days, 6, 7)
    months.unique <- unique(months)
    
    # calculate monthly mean temperatures
    temp.observed.monthly <- array(NA, c(12, ncol(temp.observed)))
    colnames(temp.observed.monthly) <- colnames(temp.observed)
    rownames(temp.observed.monthly) <- 1:12
    
    temp.modelled.monthly <- temp.observed.monthly
    
    for (i in 1:ncol(temp.observed.monthly)){
        temp.observed.monthly[,i] <- unlist(tapply(temp.observed[,i], months, mean, na.rm=T))
        temp.modelled.monthly[,i] <- unlist(tapply(temp.modelled.full[,i], months, mean, na.rm=T))
    }    
    
    # calculate bias
    temp.bias <- temp.observed - temp.modelled.full
    temp.bias.monthly <- temp.observed.monthly - temp.modelled.monthly
    
    # calculate rmse per sensor-depth
    for (i in 1:ncol(soil.rmse)){
        soil.rmse[f,i] <- rmse(temp.bias[,i])
        soil.rmse.monthly[f,i] <- rmse(temp.bias.monthly[,i])
    }
    
    ###################################################################################################    
    # prepare plot-stuff
    depth <- colnames(temp.observed)
    ylim <- c(20, 0)
    xlim <- c(-5, 5)
    
    lty <- rep(1:3, 4)
    col <- rep(c("dodgerblue", "blue", "orange", "purple"), 3)
    ###################################################################################################
    
    png(paste0("03_plots/profiles/profile-monthly-mean-",  names(soil.rmse.mean)[f], ".png"), height=20, width=30, res=450, units="cm")
    plot(rep(0, ncol(temp.observed)), depth, ylim = ylim, xlim = xlim, col="white", xlab=" soil temperature [°C]", ylab="depth [m]")
    grid() ; abline(h = 0, col="red", lwd=2)
    
    for (i in 1:nrow(temp.observed.monthly)){
        lines(temp.observed.monthly[i,], depth, lty=lty[i], col=col[i], lwd=2)
    }
    
    for (i in 1:nrow(temp.modelled.monthly)){
        lines(temp.modelled.monthly[i,], depth, lty=lty[i], lwd=2)
    }
    
    abline(v=0, col="black", lty=2, lwd=2)
    
    dev.off()
}
