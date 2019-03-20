rm(list=ls())
setwd("~/work/uni graz/projects/atmoperm/")                                

data.old <- read.table("04_snowpack/input/sonnblick_06122018.smet", skip=10, header=F)
    colnames(data.old) <- c("timestamp", "TA", "RH", "VW", "ISWR", "ILWR", "PSUM")

# data.new <- read.table("04_snowpack/input/sonnblick_pr_south-20192801.smet", skip=10, header=F)    
data.new <- read.table("04_snowpack/input/sonnblick_pr_south-20190802.smet", skip=10, header=F)
    colnames(data.new) <- c("timestamp", "TA", "RH", "VW", "ISWR", "ILWR", "PSUM")

# reduce to same time period
start <- "2010-01-01T00:00:00"
end <- "2017-08-30T23:00:00"

s <- which(data.old$timestamp == start)
e <- which(data.old$timestamp == end)
data.old <- data.old[s:e,]

s <- which(data.new$timestamp == start)
e <- which(data.new$timestamp == end)
data.new <- data.new[s:e,]

# calculate differences
data.diff <- data.old - data.new
data.diff$timestamp <- as.character(data.old$timestamp)

# calculate daily mean/sum
days <- unique(substr(data.diff$timestamp, 1, 10))

data.old.daily <- as.data.frame(array(NA, c(length(days), ncol(data.diff))))      # create object for daily data
    colnames(data.old.daily) <- colnames(data.diff)
    data.old.daily$timestamp <- days
    
data.new.daily <- data.old.daily

for (y in 1:length(days)){
    sel <- which(substr(data.old$timestamp, 1, 10) == days[y])
    
    for (x in 2:6){                                                                # mean for TA, RH, VW, ISWR, ILWR
        data.old.daily[y,x] <- round(mean(data.old[sel,x], na.rm=T), 3)
        data.new.daily[y,x] <- round(mean(data.new[sel,x], na.rm=T), 3)
    }

    data.old.daily[y,7] <- sum(data.old[sel,7], na.rm=T)                           # sum for PSUM
    data.new.daily[y,7] <- sum(data.new[sel,7], na.rm=T)
}

data.old.daily[,2] <- round(data.old.daily[,2], 2)
data.new.daily[,2] <- round(data.new.daily[,2], 2)

data.diff.daily <- data.old.daily
data.diff.daily[,-1] <- NA
data.diff.daily[,2:7] <- data.old.daily[,-1] - data.new.daily[,-1]

# plot differences
# hourly values
png("03_plots/smet-comparison/smet-comparison-hourly.png", height=20, width=30, res=450, units="cm")
par(mfrow=c(3, 2))

    plot(as.Date(data.diff$timestamp), data.diff$TA, type="l", col="blue", xlab="", ylab="[K]")
    title("TA")
    grid()
    
    plot(as.Date(data.diff$timestamp), data.diff$RH, type="l", col="blue", xlab="", ylab="[%]")
    title("RH")
    grid()
    
    plot(as.Date(data.diff$timestamp), data.diff$VW, type="l", col="blue", xlab="", ylab="[m/s]")
    title("VW")
    grid()
    
    plot(as.Date(data.diff$timestamp), data.diff$ISWR, type="l", col="orange", xlab="", ylab="[W/m2]")
    title("ISWR")
    grid()
    
    plot(as.Date(data.diff$timestamp), data.diff$ILWR, type="l", col="orange", xlab="", ylab="[W/m2]")
    title("ILWR")
    grid()
    
    plot(as.Date(data.diff$timestamp), data.diff$PSUM, type="l", col="blue", xlab="", ylab="[mm]")
    title("PSUM")
    grid()

dev.off()

# daily mean/sum values
png("03_plots/smet-comparison/smet-comparison-daily.png", height=20, width=30, res=450, units="cm")
par(mfrow=c(3, 2))

    plot(as.Date(data.diff.daily$timestamp), data.diff.daily$TA, type="l", col="blue", xlab="", ylab="[K]")
    title("TA")
    grid()
    
    plot(as.Date(data.diff.daily$timestamp), data.diff.daily$RH, type="l", col="blue", xlab="", ylab="[%]")
    title("RH")
    grid()
    
    plot(as.Date(data.diff.daily$timestamp), data.diff.daily$VW, type="l", col="blue", xlab="", ylab="[m/s]")
    title("VW")
    grid()
    
    plot(as.Date(data.diff.daily$timestamp), data.diff.daily$ISWR, type="l", col="orange", xlab="", ylab="[W/m2]")
    title("ISWR")
    grid()
    
    plot(as.Date(data.diff.daily$timestamp), data.diff.daily$ILWR, type="l", col="orange", xlab="", ylab="[W/m2]")
    title("ILWR")
    grid()
    
    plot(as.Date(data.diff.daily$timestamp), data.diff.daily$PSUM, type="l", col="blue", xlab="", ylab="[mm]")
    title("PSUM")
    grid()

dev.off()
