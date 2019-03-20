# Project ATMOperm
# Automatic detection of data gaps and linear interpolation in time series
# usage: object <- mean.fill(object, column)
# Gernot Resch 7.2.2018
##############################################################################

lin.fill <- function(object, column) {
    # if last timesteps are NA, repeat last measurement
    non_na_index <- which(!is.na(object[,column]))
    na_index <- which(is.na(object[,column]))
    
    if (max(na_index) == nrow(object)){
        object[max(na_index),column] <- object[max(non_na_index),column]
    }
    
    na_korrektur <- which(is.na(object[,column]))     # find empty values in data-vector
    na_vector <- NA                                   # calculate positions of empty value-windows (na_window_start and na_window_end)
    
    for (i in 1:length(na_korrektur)-1){
        na_vector[i] <- na_korrektur[i+1] - na_korrektur[i]
    }
    
    na_window <- which(na_vector > 1)
    na_window_start <- c(1, na_window +1)
    na_window_end <- c(na_window, length(na_vector)+1)
    
    # +++++ correction of empty data-windows
    for (a in 1:length(na_window_start)){
        
        # step 1: define start and end of empty windows
        start <- na_korrektur[na_window_start[a]]
        end <- na_korrektur[na_window_end[a]]
        
        # step 2: define datavalues before and after this NA-window
        start_snow <- round(object[start-1,column], 1)
        end_snow <- round(object[end+1,column], 1)    
        
        # step 3: calculate number of time-steps
        na_length <- length(seq(start, end))
        
        # step 4: calculate change of snowheight for each time-step
        na_fill_step_1 <- end_snow - start_snow
        for (e in 1){
            if (na_fill_step_1 == 0){
                na_fill_step_2 <- 0
            }    
            else {
                na_fill_step_2 <- round(na_fill_step_1 / (na_length + 1), 1)
            }    
        }
        
        # step 5: calculate new snowheight
        na_fill <- rep(NA, na_length)
        
        if (na_length == 1){
            na_fill <- start_snow + na_fill_step_2
        }
        
        if (na_length > 1){
            na_fill[1] <- object[(start-1),column] + na_fill_step_2
            
            for (i in 2:na_length){
                na_fill[i] <- na_fill[i-1] + na_fill_step_2
            }    
        }
        
        # fill empty window with new data
        object[start:end,column] <- na_fill
    }
    # write data to object
    object
}
