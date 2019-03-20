# Project ATMOperm
# prepare sno-files for sampling, containing ground- and snowlayerdata for initialization
# fill it with latin hypercube-sampling-data for different parameters as well as combinatoric
# Gernot Resch 21112018
####################################################################################
rm(list=ls()) ; gc()                                                               # initial cleanup

# +++++ switch directories depending on environment
hostname <- system("uname", intern=T)                                              # get hostname
    
if (hostname == "Linux"){                                                          # if script runs on icebear
    setwd("~/atmoperm/")
}

if (hostname == "Darwin"){                                                         # if script runs local
    setwd("~/work/uni graz/projects/atmoperm/")                                
}

# +++++ load libraries
library("DiceDesign")

# +++++ select start of model-run for temperature initialization data
start <- "2010-08-26"   # "2010-08-26 , temperature from observations
start.spinup <- "2010-08-26" # "2005-08-26" # temperature for sno-file
####################################################################################
# +++++ prepare sampling data via lhs
load("04_snowpack/sampling/sampling_0612018.RData")

# matrix <- lhsDesign(10000, 38)
# sampling <- as.data.frame(matrix$design)
# 
# colnames(sampling) <- c(rep("density", 6), rep("conductivity", 6), rep("capacity", 6), rep("soil", 6),
#                         rep("density", 2), rep("conductivity", 2), rep("capacity", 2), rep("soil", 2),
#                         rep("density", 1), rep("conductivity", 1), rep("capacity", 1), rep("soil", 1),
#                         "geo_heat", "albedo"
#                         )
# 
# # +++++ fill matrix with sampling-data, using the normalized data via the formula below
# # designmatrix$design[,par_col] * (par_max - par_min) + par_min
# # +++++ debris cover
# for (i in 1:6){
#     sampling[,i] <- round(sampling[,i] * (2700 - 1600) + 1600)                     # density
# }
# 
# for (i in 7:12){
#     sampling[,i] <- round(sampling[,i] * (2.5 - 0.2) + 0.2, 1)                     # heat conductivity
# }
# 
# for (i in 13:18){
#     sampling[,i] <- round(sampling[,i] * (890 - 830) + 830)                        # heat capacity
# }
# 
# for (i in 19:24){
#     sampling[,i] <- round(sampling[,i] * (1 - 0.8) + 0.8, 3)                       # soil fraction
#     # sampling[,i] <- round(sampling[,i] * (1 - 0.6) + 0.6, 3)                       # soil fraction
# }
# 
# # +++++ jointed rock
# for (i in 25:26){
#     sampling[,i] <- round(sampling[,i] * (2700 - 1600) + 1600)                     # density
# }
# 
# for (i in 27:28){
#     sampling[,i] <- round(sampling[,i] * (4 - 1.9) + 1.9, 1)                       # heat conductivity
# }
# 
# for (i in 29:30){
#     sampling[,i] <- round(sampling[,i] * (1110 - 775) + 775)                       # heat capacity
# }
# 
# for (i in 31:32){
#     sampling[,i] <- round(sampling[,i] * (1 - 0.8) + 0.8, 3)                     # soil fraction
#     # sampling[,i] <- round(sampling[,i] * (1 - 0.75) + 0.75, 3)                     # soil fraction
# }
# 
# # +++++ solid rock
# sampling[,33] <- round(sampling[,33] * (2700 - 1600) + 1600, 1)                    # density
# sampling[,34] <- round(sampling[,34] * (4 - 1.9) + 1.9, 1)                         # heat conductivity
# sampling[,35] <- round(sampling[,35] * (1110 - 775) + 775)                         # heat capacity
# sampling[,36] <- round(sampling[,36] * (1 - 0.8) + 0.8, 3)                       # soil fraction
# # sampling[,36] <- round(sampling[,36] * (1 - 0.75) + 0.75, 3)                       # soil fraction
# 
# # +++++ geo-heat and albedo
# sampling[,37] <- round(sampling[,37] * (0.05 - 0.00001) + 0.00001, 3) * -1         # geo heat transfer
# sampling[,38] <- round(sampling[,38] * (0.4 - 0.2) + 0.2, 3)                       # albedo
# 
# # rownames
# row.names(sampling) <- seq(1, nrow(sampling))
# 
# # ++++ cleanup
# rm(matrix, i)


# +++++ write sampling-file
# save(file="04_snowpack/sampling/sampling_0612018.RData", sampling)


####################################################################################
######################### +++++ calculate values ###################################
####################################################################################
# +++++ layer thickness
layer.thickness <- as.character(c(rep(0.5, 6), 2, "4", "11"))

# +++++ soil temperature
depth.obs <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1.0, 1.2, 1.5,
               2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 7.0, 9.0, 10.0, 
               12.0, 14.0, 16.0, 18.0, 20.0)

temp.obs <- read.csv("01_data/bohrloch/bh3_postprocessed-hourly.csv", sep=",",     # import observed soil data
                     skip=1, header=F)
    colnames(temp.obs) <- c("timestamp", paste0("D", depth.obs))
    
sel <- which(temp.obs[,1] == paste0(start, " 01:00:00"))
soil.temperature <- rev(temp.obs[sel,-1] + 273.15)                                 # get measured soil temperature for initialization
soil.temperature <- unlist(soil.temperature[c(4, 8, 11, 13, 14, 15, 16, 18, 22)])  # reduce to modelled layers

rm(depth.obs, temp.obs, sel)    

# +++++ calculate ice/water/void/soil-fractions
sel <- which(colnames(sampling) == "soil")
frac.soil <- sampling[,sel]

frac.ice <- round((1 - frac.soil) * 0.8, 3)                                        # calculation of ice and water. if ice or water are present will be determined in next step

frac.water <- frac.ice

frac.void <- round(1 - frac.soil - frac.ice, 3)                                    # calculate voids as residuum
    
# +++++ density
sel <- which(colnames(sampling) == "density")
soil.density <- round(sampling[,sel])
    
# +++++ conductivity
sel <- which(colnames(sampling) == "conductivity")
soil.conductivity <- sampling[,sel]

# +++++ heat capacity
sel <- which(colnames(sampling) == "capacity")
soil.capacity <- sampling[,sel]

# +++++ geothermal heat transfer
sel <- which(colnames(sampling) == "geo_heat")
soil.geoheat <- sampling[,sel]

# +++++ albedo
sel <- which(colnames(sampling) == "albedo")
soil.albedo <- sampling[,sel]

# +++++ number of elements per layer (virtual splitting of thicker layers)
soil.ne <- c(25, 25, 25, 25, 25, 25,
             20, 20,
             22)

####################################################################################
######################### +++++ create sno-file  ###################################
####################################################################################
for (i in 1:nrow(sampling)){

    # +++++ create header
    header <- rep(NA, 27)
    
    header[1] <- "SMET 1.1 ASCII"
    header[2] <- "[HEADER]"
    header[3] <- "station_id = sonnblick"
    header[4] <- "station_name = Sonnblick"
    header[5] <- "latitude = 47.05"
    header[6] <- "longitude = 12.95"
    header[7] <- "altitude = 3059"
    header[8] <- "epsg = 21781"
    header[9] <- "nodata = -999"
    header[10] <- "tz = 0"
    header[11] <- "source = Gernot Resch"
    header[12] <- paste0("ProfileDate = ", start.spinup, "T00:00")
    header[13] <- "HS_Last = 0.0000"
    header[14] <- "SlopeAngle = 20"
    header[15] <- "SlopeAzi = 170"
    header[16] <- paste0("nSoilLayerData = ", 9)
    header[17] <- "nSnowLayerData = 0"
    header[18] <- paste0("SoilAlbedo = ", soil.albedo[i])
    header[19] <- "BareSoil_z0 = 0.200"
    header[20] <- "CanopyHeight = 0.00"
    header[21] <- "CanopyLeafAreaIndex = 0.00"
    header[22] <- "CanopyDirectThroughfall = 1.00"
    header[23] <- "WindScalingFactor = 1.00"
    header[24] <- "ErosionLevel = 0"
    header[25] <- "TimeCountDeltaHS = 0.000000"
    header[26] <- "fields = timestamp Layer_Thick T Vol_Frac_I Vol_Frac_W Vol_Frac_V Vol_Frac_S Rho_S Conduc_S HeatCapac_S rg rb dd sp mk mass_hoar ne CDot metamo"
    header[27] <- "[DATA]"
        
    # +++++ create sno-object containing data
    data.raw <- as.data.frame(array(NA, c(9, 19)))
        colnames(data.raw) <- c("timestamp","Layer_Thick","T","Vol_Frac_I",
                                "Vol_Frac_W","Vol_Frac_V","Vol_Frac_S",
                                "Rho_S","Conduc_S","HeatCapac_S","rg","rb",
                                "dd","sp","mk","mass_hoar","ne","CDot","metamo")
        
    data <- data.raw
    
    data[,1] <- paste0(start.spinup, "T00:00:00")                    # date (not the real, but the spinup-date)
    data[,2] <- rev(layer.thickness)                                 # layer thickness
    data[,3] <- soil.temperature                                     # temperature
    
    data[,4] <- unlist(rev(frac.ice[i,]))                            # fractional ice volume [0-1]                                              
        sel <- which(data[,3] > 273.15)                              # selecte layers < 0 °C
        data[sel,4] <- 0                                             # change to water
        
    data[,5] <- unlist(rev(frac.water[i,]))                          # fractional water volume [0-1]
        sel <- which(data[,3] <= 273.15)
        data[sel,5] <- 0                                                    
        
    data[,6] <- unlist(rev(frac.void[i,]))                           # fractional voids volume [0-1]
    data[,7] <- unlist(rev(frac.soil[i,]))                           # fractional soil volume [0-1]
    data[,8] <- unlist(rev(soil.density[i,]))                        # soil density [kg/m3]
    data[,9] <- unlist(rev(soil.conductivity[i,]))                   # mineral phase soil thermal conductivity [w/(mK)]
    data[,10] <- unlist(rev(soil.capacity[i,]))                      # mineral phase soil heat capacity [J/K]
    data[,11] <- 12.5 # rev(soil.rg)                                        # grain radius [mm]
    data[,12] <- 0                                                   # bond radius [mm]
    data[,13] <- 0                                                   # snow dendricity (0 <- old snow, 1 <- new snow)
    data[,14] <- 0                                                   # snow sphericity (0 <- facettiert, 1 <- gerundet)
    data[,15] <- 0                                                   # marker (0 <- nothing, 7 <- glacier ice)
    data[,16] <- 0                                                   # mass_hoar (mass of surface hoar)
    data[,17] <- rev(soil.ne)                                        # ne number of elements
    data[,18] <- 0                                                   # CDot
    data[,19] <- 0                                                   # metamo
    
    # +++++ combine header and data    
    sno <- as.data.frame(array(NA, c(c(length(header) + 9), 19)))
    sno[1:length(header),1] <- header
    sno[c(length(header)+1):nrow(sno),] <- data 
    
    write.table(sno, file=paste0("04_snowpack/input/sno/sonnblick_sampling_", i, ".sno"),          # write sno-object to file system
                row.names=F, col.names=F, dec=".", sep=" ", quote=F, na="")
}

rm(soil.density, soil.conductivity, soil.capacity, frac.water, frac.void,          # cleanup
   frac.soil, frac.ice, data.raw, data, sno, header, layer.thickness, sel,
   soil.ne, soil.temperature)

####################################################################################
######################### +++++ create ini-file  ###################################
####################################################################################
setwd(paste0(getwd(), "/04_snowpack/cfg/sampling"))

# +++++ switch directories depending on environment
if (hostname == "Linux"){                                                      # if script runs on icebear
    for (i in 2:10000){
        system(paste0("cp -f sonnblick_sampling_1.ini sonnblick_sampling_", i, ".ini"))
        
        # replace sno-file
        system(paste0("sed -i 's/sonnblick_sampling_1.sno/sonnblick_sampling_", i, ".sno",
                      "/g' sonnblick_sampling_", i, ".ini"))
        
        # replace experiment
        system(paste0("sed -i 's/EXPERIMENT	=	sampling_1/EXPERIMENT	=	sampling_", i,
                      "/g' sonnblick_sampling_", i, ".ini"))
        
        # replace geothermal heat
        system(paste0("sed -i 's/GEO_HEAT	=	-0.035/GEO_HEAT	=	", soil.geoheat[i],
                      "/g' sonnblick_sampling_", i, ".ini"))
    }
}

if (hostname == "Darwin"){                                                     # if script runs locally (mac os)
    for (i in 2:10000) {                                                        
        # create new file
        system(paste0("cp -f sonnblick_sampling_1.ini sonnblick_sampling_", i, ".ini"))
        
        # replace sno-file
        system(paste0("sed -i '' 's/sonnblick_sampling_1.sno/sonnblick_sampling_", i, ".sno",
                      "/g' sonnblick_sampling_", i, ".ini"))
        
        # replace experiment
        system(paste0("sed -i '' 's/EXPERIMENT	=	sampling_1/EXPERIMENT	=	sampling_", i,
                      "/g' sonnblick_sampling_", i, ".ini"))
        
        # replace geothermal heat
        system(paste0("sed -i '' 's/GEO_HEAT	=	-0.035/GEO_HEAT	=	", soil.geoheat[i],
                      "/g' sonnblick_sampling_", i, ".ini"))
    }
}


####################################################################################
################## +++++ create smet-file which includes spinup  ####################
####################################################################################
setwd("../../../01_data/snowpack/")

spinup.time <- 5                                                                   # define length of spinup-time

met.data <- read.table("sonnblick_pr_south-20182511-gst.smet",                     # import met-data
                     skip=10, header=F, sep=" ")
    colnames(met.data) <- c("timestamp", "TA", "RH", "VW", "ISWR", "ILWR", "PSUM")
    met.data$timestamp <- as.character(met.data$timestamp)

    sel <- which(met.data$PSUM == "-999")
    met.data$PSUM[sel] <- 0
    
# create date-string
years <- substr(met.data[,1], 1, 4)
years.unique <- unique(years)
year.start <- as.numeric(years.unique[1]) - spinup.time

start <- paste0(year.start, "-01-01")
end <- paste0(year.start + 4, "-12-31")

days <- rep(seq(as.Date(start), as.Date(end), by="1 day"), each=24)
hours <- paste0("T", c(paste0("0", seq(0, 9)), seq(10, 23)),
                ":00:00")
timestamp <- paste0(days, hours)

s <- 1
e <- which(met.data$timestamp == paste0(years.unique[1], "-12-31T00:00:00"))
yearly.data <- met.data[1:e,]

# create husk for artificial period
met.art <- as.data.frame(array(NA, c(length(timestamp), 7)))                       # husk for data
    colnames(met.art) <- colnames(met.data)

met.art$timestamp <- timestamp                                                     # copy timestamp

hours <- unique(substr(timestamp, 6, 19))                                          # isolate days of a year

# add data to husk
for (i in 1:length(hours)){                                                        # copy data
    sel <- which(substr(timestamp, 6, 19) == hours[i])                             # select all rows of the day/time
    met.art[sel,-1] <- met.data[i,-1]                                              # copy data to selected rows
}

sel.leap <- which(substr(met.art$timestamp, 1, 10) == "2008-02-29")                # deal with leap year (add data from 02-28)
sel <- which(substr(met.data$timestamp, 1, 10) == paste0(years.unique[1], "-02-28"))
met.art[sel.leap,-1] <- met.data[sel,-1]

##############################################################################
# +++++ create header, combine with data and export
# create header
header <- rep(NA, 10)                                                              # create object for header-files

header[1] <- "SMET 1.1 ASCII"
header[2] <- "[HEADER]"
header[3] <- "station_id = sonnblick"
header[4] <- "latitude = 47.05"
header[5] <- "longitude = 12.95"
header[6] <- "altitude = 3059"
header[7] <- "nodata = -999"
header[8] <- "tz = 0"
header[9] <- "fields = timestamp TA RH VW ISWR ILWR PSUM"
header[10] <- "[DATA]"

# combine header, artificial spinup-data and real data
art.start <- length(header) + 1
art.end <- art.start + nrow(met.art) - 1
obs.start <- art.end + 1
obs.end <- obs.start + nrow(met.data) - 1

smet <- as.data.frame(array(NA, c(nrow(met.art) + nrow(met.data) + 10,             # create export-object
                                  ncol(met.art))))               
smet[1:length(header),1] <- header                                                 # add header-data
smet[art.start:art.end,] <- met.art                                                # add spinup-data
smet[obs.start:obs.end,] <- met.data                                               # add meteorological data

# export to binary and txt-file
write.table(smet, file="sonnblick_06122018.smet",                                  # make .smet-file
            row.names=F, col.names=F, dec=".", sep=" ", quote=F, na="")

save(met.art, file="sonnblick_06122018.smet.RData")                                # make backup

system("cp -rf sonnblick_06122018.smet ../../04_snowpack/input/")                  # copy to snowpack-directory
