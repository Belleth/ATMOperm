# Project ATMOperm
# Interpret results of ATMOperm-scenarios
# Gernot Resch 21.02.2019
###################################################################################################
rm(list=ls()) ; gc()                                                               # initial cleanup
Sys.setlocale("LC_TIME", "C")                                                      # change language to english for plots

setwd("~/work/uni graz/projects/atmoperm/")                                        # switch to directory
# setwd("~/atmoperm")
library(Hmisc)
###################################################################################################
# +++++ load modeled daily alt
file.list <- list.files("01_data/soil/scenarios/",
                    pattern="alt.sonnblick*")

load(paste0("01_data/soil/scenarios/", file.list[1]))                               # load first file for preparations

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
    load(paste0("01_data/soil/scenarios/", file.list[i]))
    alt.daily.modelled[i,1:length(alt.daily)] <- alt.daily
}

rm(file.list, alt.daily, i)   
    
###################################################################################################
# +++++ get rid of last uncomplete year
years <- unique(substr(colnames(alt.daily.modelled), 1, 4))
    sel <- which(substr(colnames(alt.daily.modelled), 1, 4) == years[length(years)])
alt.daily.modelled <- alt.daily.modelled[,-sel]

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

sel <- which(alt.yearly.modelled == "-Inf")                                     # years with no values set to NA
alt.yearly.modelled[sel] <- NA

# ++++ create two objects: one with and one without spinup-years
alt.yearly.modelled.spinup <- alt.yearly.modelled
    # sel <- which(colnames(alt.yearly.modelled) == "2002")
    sel <- which(colnames(alt.yearly.modelled) == "2001")
    alt.yearly.modelled <- alt.yearly.modelled[,-c(1:sel)]

years.spinup <- years
years <- colnames(alt.yearly.modelled)                                                  # also delete spinup-years here

rm(x, y, sel)

###################################################################################################
# +++++ cleanup and renaming for plotting
###################################################################################################
# +++++ create list of remaining scenarios and sort
backup.yearly <- alt.yearly.modelled
    alt.yearly.modelled[,] <- NA
backup.yearly.spinup <- alt.yearly.modelled.spinup
    alt.yearly.modelled.spinup[,] <- NA
backup.daily <- alt.daily.modelled
    alt.daily.modelled[,] <- NA
experiments.backup <- experiments

scenarios.list <- rownames(alt.yearly.modelled)
    scenarios.list <- substr(scenarios.list, 1, 5)
    scenarios.list <- gsub(scenarios.list,
                      pattern="_",
                      replacement="")
    scenarios.list.backup <- scenarios.list
    
o <- as.numeric(gsub(scenarios.list, pattern="sce", replacement=""))
o.order <- o[order(o)]
o.unique <- unique(o.order)

for (i in 1:length(o.unique)){
    sel <- which(o == o.unique[i])
    put <- which(o.order == o.unique[i])
    
    alt.yearly.modelled[put,] <- backup.yearly[sel,]
        rownames(alt.yearly.modelled)[put] <- rownames(backup.yearly)[sel]
        
    alt.yearly.modelled.spinup[put,] <- backup.yearly.spinup[sel,]
        rownames(alt.yearly.modelled.spinup)[put] <- rownames(backup.yearly.spinup)[sel]    
        
    alt.daily.modelled[put,] <- backup.daily[sel,]
        rownames(alt.daily.modelled)[put] <- rownames(backup.daily)[sel]
    
    experiments[put] <- experiments.backup[sel]
    
    scenarios.list[put] <- scenarios.list.backup[sel]
}
    
scenarios <- unique(scenarios.list)

scenarios.number <- gsub(scenarios,
                         pattern="sce",
                         replacement="")

rm(o, o.order, o.unique, sel, put, scenarios.list.backup, backup.yearly, 
   backup.daily, i, experiments.backup, backup.yearly.spinup)

###################################################################################################
# +++++ calculate additional values for plotting
###################################################################################################
alt.yearly.max <- array(NA, c(length(scenarios), length(years)))
    colnames(alt.yearly.max) <- years
    rownames(alt.yearly.max) <- scenarios

alt.yearly.max.spinup <- array(NA, c(length(scenarios), length(years.spinup)))
    colnames(alt.yearly.max.spinup) <- years.spinup
    rownames(alt.yearly.max.spinup) <- scenarios
    
for (s in 1:length(scenarios)){
    sel <- which(scenarios.list == scenarios[s])                           # select rows with corresponding scenario
    
    for (i in 1:ncol(alt.yearly.max)){
        # alt.yearly.max[s,i] <- max(median(alt.yearly.modelled[sel,i], na.rm=T), na.rm=T)
        alt.yearly.max[s,i] <- max(mean(alt.yearly.modelled[sel,i], na.rm=T), na.rm=T)
    }
    
    for (i in 1:ncol(alt.yearly.max.spinup)){
        # alt.yearly.max.spinup[s,i] <- max(median(alt.yearly.modelled.spinup[sel,i], na.rm=T), na.rm=T)
        alt.yearly.max.spinup[s,i] <- max(mean(alt.yearly.modelled.spinup[sel,i], na.rm=T), na.rm=T)
    }
}    

mean.spinup <- rep(NA, ncol(alt.yearly.modelled.spinup))
    for (i in 1:length(mean.spinup)){
        mean.spinup[i] <- mean(alt.yearly.modelled.spinup[,i], na.rm=T)
    }    

rm(i, s, sel)
###################################################################################################
# +++++ make plots
###################################################################################################
# prepare stuff for plots
par.mar.1 <- c(4, 4, 1, 0.2)
ylim <- c(4, 0)

load("01_data/snowpack/scenarios/sce.color.RData")
col <- c(col[1:7], col[9]) # which scenarios to plot
# col2 <- c("cornsilk", "bisque", "lightcoral", "mediumseagreen", "mediumseagreen", 
#           "mediumorchid1")# , "")

year.list <- 1:ncol(alt.yearly.max)
year.list.spinup <- c(rep(1, 6), year.list)

spinup.1 <- rep(5000, 4)
spinup.2 <- rep(-5000, 4)

###################################################################################################
# +++++ LINEPLOT yearly maximum extent of active layer thickness in scenarios (without spinup)
png("03_plots/scenarios/maximum-yearly-alt.png", height=12, width=20, res=450, units="cm")
    par(mar = par.mar.1)
    plot(year.list, alt.yearly.max[1,], type="l",
         ylim=ylim, xlab="YEAR", ylab="DEPTH [m]", col="white")
    
    # for (s in 1:length(scenarios)){
    #     sel <- which(scenarios.list == scenarios[s])
    #     for (i in 1:length(sel)){
    #         # lines(years, alt.yearly[sel[i],], col=col[s])
    #         lines(year.list, alt.yearly.modelled[sel[i],], col="lightgrey")    
    #     }
    # }
    
    for (i in 1:nrow(alt.yearly.max)){
        lines(year.list, alt.yearly.max[i,], col = col[i], lwd=2, lty=1)
    }
    
    grid()
    abline(h=0, col="red", lwd=2, lty=3)                                                  # soil surface
    minor.tick(nx=5, ny=5)
    # title("Yearly maximum Active Layer Thickness in modelled scenarios")
    legend("bottomleft", scenarios, cex=.7,
           lty=1,
           col=col,
           lwd=2, ncol=2)
dev.off()


# +++++ LINEPLOT yearly maximum extent of active layer thickness in scenarios (with spinup)
png("03_plots/scenarios/maximum-yearly-alt-spinup.png", height=12, width=20, res=450, units="cm")
par(mar = par.mar.1)
    plot(1:36, alt.yearly.max.spinup[1,], type="l",
         ylim=ylim, xlab="YEAR", ylab="DEPTH [m]", col="white",
         xaxt="n")
    
        axis(1, at=seq(6, 36, length=7), labels=seq(0, 30, 5))                      # add timeline for scenarios
        
        # plot grid
        gridseq <- seq(6, 36, length=7)
        for (i in 1:length(gridseq)){
            abline(v=gridseq[i], lty=3, col="snow3")
        }
        
        gridseq <- seq(2, 8, length=4)
        for (i in 1:length(gridseq)){
            abline(h=gridseq[i], lty=3, col="snow3")
        }
        
        lines(2:5, spinup.1, col="lightgrey", type="h", lwd=35)                           # add spinup-block
        lines(2:5, spinup.2, col="lightgrey", type="h", lwd=35)
        box()
        # minor.tick(nx=5, ny=5)
        
        # for (s in 1:length(scenarios)){
        #     sel <- which(scenarios.list == scenarios[s])
        #     for (i in 1:length(sel)){
        #         lines(1:36, alt.yearly.modelled.spinup[sel[i],], col="lightgrey")
        #     }
        # }
        
        text(3.5, 2, "SPINUP", col="white", srt=90, cex=1.5)
        
        lines(1:6, mean.spinup[1:6], lwd=2)
        for (i in 1:nrow(alt.yearly.max.spinup)){
            lines(6:36, alt.yearly.max.spinup[i,6:36], col = col[i], lwd=2, lty=1)
        }
            
        abline(h=0, col="darkgrey", lwd=2)                                                  # soil surface
        
        # title("Yearly maximum Active Layer Thickness in modelled scenarios")
        legend("bottomleft", c("spinup", scenarios), cex=.7,
               lty=1,
               col=c("black", col),
               lwd=2, ncol=2)
dev.off()

########## plot for single scenarios
for (s in 1:length(scenarios)){
    sel <- which(scenarios.list == scenarios[s])
    
    polygonplot <- array(NA, c(31, 2))
        rownames(polygonplot) <- 6:36
        
    for (i in 1:nrow(polygonplot)){
        polygonplot[i,] <- quantile(alt.yearly.modelled.spinup[sel,i+5],               # calculate data
                                    probs=c(0, 0.05, 0.95, 1), na.rm=T)[2:3]
    }
    polygonplot[,2] <- rev(polygonplot[,2])
    
    ############################
    
    png(paste0("03_plots/scenarios/maximum-yearly-alt-spinup-", scenarios[s], 
               ".png"), height=12, width=20, res=450, units="cm")    
    par(mar=par.mar.1)
    
    plot(1:36, alt.yearly.max.spinup[1,], type="l",
         ylim=ylim, xlab="YEAR", ylab="DEPTH [m]", col="white",
         xaxt="n")
    
    axis(1, at=seq(6, 36, length=7), labels=seq(0, 30, 5))                      # add timeline for scenarios
    
    # plot grid
    gridseq <- seq(6, 36, length=7)
    for (i in 1:length(gridseq)){
        abline(v=gridseq[i], lty=3, col="snow3")
    }
    
    gridseq <- seq(2, 8, length=4)
    for (i in 1:length(gridseq)){
        abline(h=gridseq[i], lty=3, col="snow3")
    }
    
    lines(2:5, spinup.1, col="lightgrey", type="h", lwd=35)                           # add spinup-block
    lines(2:5, spinup.2, col="lightgrey", type="h", lwd=35)
    box()
    
    text(3.5, 2, "SPINUP", col="white", srt=90, cex=1.5)
    
    lines(1:6, mean.spinup[1:6], lwd=3)
    polygon(c(6:36, 36:6), c(polygonplot),
            col="lightsteelblue1", border=F)
    lines(6:36, alt.yearly.max.spinup[s,6:36], col = col[s], lwd=3, lty=1)
    
    abline(h=0, col="darkgrey", lwd=2)                                                  # soil surface
    
    # title("Yearly maximum Active Layer Thickness in modelled scenarios")
    legend("bottomleft", c("spinup", paste0(scenarios[s], " range"), paste0(scenarios[s], " mean")), cex=.7,
           lty=1,
           col=c("black", "lightsteelblue1", col[s]),
           lwd=c(3, 10, 3), ncol=1)
    dev.off()
}



#############################################################################################
########## plot for multiple scenarios
png("03_plots/scenarios/maximum-yearly-alt-spinup-all.png", height=30, width=30, res=450, units="cm")    
par(mar=par.mar.1,
    mfrow=c(4, 2))
for (s in 1:length(scenarios)){
    
    sel <- which(scenarios.list == scenarios[s])
    
    polygonplot <- array(NA, c(31, 2))
    rownames(polygonplot) <- 6:36
    
    for (i in 1:nrow(polygonplot)){
        polygonplot[i,] <- quantile(alt.yearly.modelled.spinup[sel,i+5],               # calculate data
                                    probs=c(0, 0.05, 0.95, 1), na.rm=T)[2:3]
    }
    polygonplot[,2] <- rev(polygonplot[,2])
    
    ############################
    
    
    # par(mar=par.mar.1)
    
    plot(1:36, alt.yearly.max.spinup[1,], type="l",
         ylim=ylim, xlab="YEAR", ylab="DEPTH [m]", col="white",
         xaxt="n")
    
    axis(1, at=seq(6, 36, length=7), labels=seq(0, 30, 5))                      # add timeline for scenarios
    
    # plot grid
    gridseq <- seq(6, 36, length=7)
    for (i in 1:length(gridseq)){
        abline(v=gridseq[i], lty=3, col="snow3")
    }
    
    gridseq <- seq(2, 8, length=4)
    for (i in 1:length(gridseq)){
        abline(h=gridseq[i], lty=3, col="snow3")
    }
    
    lines(2:5, spinup.1, col="lightgrey", type="h", lwd=35)                           # add spinup-block
    lines(2:5, spinup.2, col="lightgrey", type="h", lwd=35)
    box()
    
    text(3.5, 2, "SPINUP", col="white", srt=90, cex=1.5)
    
    lines(1:6, mean.spinup[1:6], lwd=3)
    polygon(c(6:36, 36:6), c(polygonplot),
            col="lightsteelblue1", border=F)
    lines(6:36, alt.yearly.max.spinup[s,6:36], col = col[s], lwd=3, lty=1)
    
    abline(h=0, col="darkgrey", lwd=2)                                                  # soil surface
    
    # title("Yearly maximum Active Layer Thickness in modelled scenarios")
    legend("bottomleft", c("spinup", paste0(scenarios[s], " range"), paste0(scenarios[s], " mean")), cex=.7,
           lty=1,
           col=c("black", "lightsteelblue1", col[s]),
           lwd=c(3, 10, 3), ncol=1)
}
dev.off()
