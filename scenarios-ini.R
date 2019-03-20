# Project ATMOperm
# prepare ini-files for scenarios
# Gernot Resch 20022019
####################################################################################
rm(list=ls()) ; gc()                                                                # initial cleanup
setwd("~/work/uni graz/projects/atmoperm/")                                
# setwd("~/atmoperm")

sce.list <- list.files("01_data/snowpack/scenarios/", pattern="*.smet")
scenarios <- gsub(sce.list,                                                         # get list of scenarios
                  pattern = ".smet",
                  replacement = "")

load("01_data/soil/sampling/sampling-results/sampling.results.RData")               # load results of sampling- and profile-analysing
experiments <- sampling.results[1:50]

rm(sampling.results, sce.list)

####################################################################################
######################### +++++ create ini-file  ###################################
####################################################################################
setwd("04_snowpack/cfg")
system("rm -rf scenarios/*.ini")                                                         # cleanup

for (e in 1:length(experiments)){
    for (s in 1:length(scenarios)){
        file <- paste0("scenarios/sonnblick_", scenarios[s], "_", experiments[e], ".ini")
        
        system(paste0("cp -f sampling/sonnblick_", experiments[e],                 # copy ini-file
                      ".ini ", file))
        
        # replace STATION1
        search <- paste0("STATION1\t=\tsonnblick_09022019_spinup.smet ; sonnblick_pr_south-20182511-gst.smet")
        replace <- paste0("STATION1\t=\tsonnblick_", scenarios[s], "_spinup.smet")
        system(paste0("sed -i '' 's/", search, "/", replace, "/g' ", file))
        
        # replace EXPERIMENT
        search <- paste0("EXPERIMENT\t=\t", experiments[e])
        replace <- paste0("EXPERIMENT\t=\t", scenarios[s], "_",experiments[e])
        system(paste0("sed -i '' 's/", search, "/", replace, "/g' ", file))
    }
}
