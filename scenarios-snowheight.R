# Project ATMOperm
# calculate snowheight of different scenarios
# Gernot Resch 01.03.2019
###################################################################################################
rm(list=ls()) ; gc()                                                                # initial cleanup
Sys.setlocale("LC_TIME", "C")                                                       # change language to english for plots

setwd("~/work/uni graz/projects/atmoperm/")    # switch to directory

file.path <- "04_snowpack/output/extract"
###################################################################################################
# +++++ 
source("02_scripts/f_pro.import.R")

start <- "2007-08-01"
end <- "2008-07-31"
days <- seq(as.Date(start), as.Date(end), by="day")

# files <- list.files(file.path, pattern="snowheight*")
files <- c("snowheight_sce1_0501.txt", "snowheight_sce2_0501.txt", "snowheight_sce3_0501.txt",
           "snowheight_sce4_0501.txt", "snowheight_sce5_0501.txt", "snowheight_sce12_0501.txt",
           "snowheight_sce13_0501.txt", "snowheight_sce14_0501.txt", "snowheight_sce15_0501.txt",
           "snowheight_sce16_0501.txt")


s <- gsub(files, pattern="snowheight_sce", replacement="")
s <- gsub(s, pattern="_0501.txt", replacement="")


experiments <- gsub(gsub(files, pattern=".txt", replacement=""), 
                    pattern="_0501", replacement="")

snowheight.scenarios <- as.data.frame(array(NA, c(length(experiments), length(days))))
    rownames(snowheight.scenarios) <- gsub(experiments, pattern="snowheight_", replacement="")
    colnames(snowheight.scenarios) <- days

for (i in 1:length(files)){
    pro.import.snowheight("0501", experiments[i])                                   # import snowheight
    snowheight.scenarios[i,] <- snowheight[1:ncol(snowheight.scenarios)]            # put in right place and cut to 2007-08-01 - 2008-07-31
}

save(snowheight.scenarios, file="01_data/snowpack/scenarios/snowheight.scenarios.RData")



#############
par.mar.1 <- c(2, 4, 1, 0.2)
ylim <- range(snowheight.scenarios)
# col <- brewer.pal(10, "Spectral")
col <- c("yellow", "orange", "red", "green", "darkgreen", "purple", "cyan3", "dodgerblue", "blue", "darkblue")
    names(col) <- rownames(snowheight.scenarios)
    save(col, file="01_data/snowpack/scenarios/sce.color.RData")

# col <- rep(c("dodgerblue", "cyan3", "orange", "red", "purple"), 2)
lty <- rep(1, 10)
# lty <- rep(c(1, 3), each=5)
lwd <- 2
scenarios <- gsub(experiments, pattern="snowheight_", replacement="")

png("03_plots/scenarios/scenarios-snowheight.png", height=12, width=20, res=450, units="cm")
par(mar = par.mar.1)
plot(days, snowheight.scenarios[1,], col="white", ylim=ylim, ylab="snowheight [cm]")
    grid()
    
    for (i in 1:nrow(snowheight.scenarios)){
        lines(days, snowheight.scenarios[i,], col=col[i], lty=lty[i], lwd=lwd)
    }

    legend("topleft", c(scenarios), col=col, lty=lty, cex=.7, ncol=2, lwd=lwd)
dev.off()
