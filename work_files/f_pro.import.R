# Project ATMOperm
# import extracted soil parameters from existing .txt-file
# Gernot Resch 09052018
####################################################################################
# example parameters
# parameter <- "0501"             # parameters to be imported
# experiment <- "sonnblick_test"  # name of experiment, .pro is being added

####################################################################################
pro.import <- function(parameter, experiment){
    {
        ncols <- 5000 # 5000 mögliche schichten
        file.name <- paste0("output/extract/", experiment, "_", parameter, ".txt")
        m <- read.table(file.name, fill=T, col.names=rep("", ncols), skip=2)
        m <- m[-1,]     # da erste zeile um 00:00 startet
        # m <- m[1:nrow(m)-1,] # da modell einen tag vorher endet (statt 1.9. 31.8.)
        
        # array auf spalten mit werten verkleinern
        na.list <- rep(NA, ncols)
        for (z in 1:ncols){
            na.list[z] <- length(which(!is.na(m[,z])))
        }
        m <- m[,which(na.list > 0)]
        
        # isolate timesteps
        timesteps <- m[,1]
        m <- m[,-1]
        
        # transpose data
        model <- t(m)
        colnames(model) <- timesteps
        rownames(model) <- as.character(seq(1, nrow(model)))
    }
    model
}

pro.import.snowheight <- function(parameter, experiment){
    {
        ncols <- 5000 # 5000 mögliche schichten
        file.name <- paste0("04_snowpack/output/extract/", experiment, "_", parameter, ".txt")
        m <- read.table(file.name, fill=T, col.names=rep("", ncols), skip=2)
        # m <- m[-1,]     # da erste zeile um 00:00 startet
        m <- m[1:nrow(m)-1,] # da modell einen tag vorher endet (statt 1.9. 31.8.)
        
        # array auf spalten mit werten verkleinern
        na.list <- rep(NA, ncols)
        for (z in 1:ncols){
            na.list[z] <- length(which(!is.na(m[,z])))
        }
        m <- m[,which(na.list > 0)]
        
        # isolate timesteps
        timesteps <- m[,1]
        m <- m[,-1]
        
        # transpose data
        model <- t(m)
            colnames(model) <- timesteps
            rownames(model) <- as.character(seq(1, nrow(model)))
            
        subsurface <- which(model[,] <= 0)                                          # reduce to rows above surface
        model <- model[-subsurface,]
            
        snowheight <- rep(NA, ncol(model))
            names(snowheight) <- substr(colnames(model), 1, 10)
        for (z in 1:ncol(model)){
            snowheight[z] <- max(model[,z], na.rm=T)
        }
        snowheight[snowheight == "-Inf"] <- 0
            snowheight <- snowheight[-1]
        
    }
    snowheight <<- snowheight
}
