# calculate root mean square error (rmse) and mean absolute error (mae) between observations and model
# error-calculation should be: observation - model
# https://heuristically.wordpress.com/2013/07/12/calculate-rmse-and-mae-in-r-and-sas/
# Gernot Resch 6.2.2018

rmse <- function(error){
    sqrt(mean(error^2, na.rm=T))
}

mae <- function(error){
    mean(abs(error), na.rm=T)
}
