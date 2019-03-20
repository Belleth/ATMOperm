alt.calculator <- function(data){
    {
        # calculate active layer thickness for each row and store in one big vector
        # calculate hourly alt as base for further calculations
        # alt.hour <- rep(NA, nrow(temp.obs))                                        # prepare vector for hourly alt-values
        alt.hour <- rep(NA, nrow(data))                                        # prepare vector for hourly alt-values
        
        for (i in 1:nrow(data)){
            alt.hour[i] <- as.numeric(max(which(data[i,] > 0), na.rm=T))       # calculate daily active layer thickness
        }
        alt.hour <- colnames(data)[alt.hour]                                   # change column to colname
        names(alt.hour) <- substr(row.names(data), 1, 16)                      # add date string
        
        # calculate daily alt
        days <- substr(names(alt.hour), 1, 10)
        days.unique <- unique(days)
        
        alt.daily <- rep(NA, length(days.unique))
        names(alt.daily) <- days.unique    
        
        for (i in 1:length(days.unique)){
            sel <- which(days == days.unique[i])
            alt.daily[i] <- as.numeric(max(alt.hour[sel], na.rm=T))
        }
        
        # calculate yearly alt
        year <- substr(row.names(data), 1, 4)
        year.unique <- unique(year)     
        
        alt.year <- rep(NA, length(year.unique))
        names(alt.year) <- year.unique
        
        for (i in 1:length(year.unique)){
            sel <- which(year == year.unique[i])
            alt.year[i] <- as.numeric(max(alt.hour[sel], na.rm=T))
        }
        
        alt.hour <<- alt.hour
        alt.daily <<- alt.daily
        alt.year <<- alt.year
        }
}
