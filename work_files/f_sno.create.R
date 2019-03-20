# Project ATMOperm
# prepare sno-file, contains ground- and snowlayerdata for initialization
# Gernot Resch 08052018
####################################################################################
# example parameters
# start <- "2010-09-01"
# soil <- "soil.temp"
# depths <- "depths.obs"

####################################################################################
sno.create <- function(start, soil, depths){
    {
    depths <- get(depths)                                            # import measurement-depths
        layers <- length(depths) -1                                  # get amount of layers
        
    layer.thickness <- rep(NA, layers)
        for (i in 1:length(layer.thickness)){                        # calculate thickness of layers
            layer.thickness[i] <- depths[i+1] - depths[i]
        }
        layer.thickness[layers] <- layer.thickness[layers]           # set thickness of last layers to the of the second-last 
        layer.thickness <- rev(layer.thickness)                      # switch to correct order
        
    soil.temperature <- get(soil)                                    # load soil data
        sel <- which(soil.temperature[,1] == 
                         paste0(start, " 01:00:00"))                 # select starting-date
        soil.temperature <- soil.temperature[sel,-1] + 273.15        # convert to kelvin
        soil.temperature <- rev(soil.temperature[1:layers])          # reduce to amount of layers
            
    ice.volume <- c(rep(0.001, 4),                                   # fractional ice volume [0-1]
                    rep(0.005, layers-4))
    
    water.volume <- 0                                                # fractional water volume [0-1]
    void.volume <- 0                                                 # fractional voids volume [0-1]
    
    soil.volume <- c(rep(0.999, 4),                                  # fractional soil volume [0-1]
                     rep(0.995, layers-4))
    
    soil.density <- c(rep(2800, 2),                                  # soil density [kg/m3]
                      rep(2700, 2),
                      rep(2600, layers-4))
    
    soil.conductivity <- 2.6                                         # mineral phase soil thermal conductivity [w/(mK)]
    soil.capacity <- 1100                                            # mineral phase soil heat capacity [J/K]
    soil.rg <-  c(rep(10000, 13),                                    # grain radius, könnte auch bis zu 10000 sein (massiver fels)
                   rep(3.5, 11))
    #                 
    soil.rb <- 0                                                     # bond radius [mm]
    
    # calculation of elements (splitting in virtual 25 cm-layers)
    soil.ne <- c(rep(8, 5),
                 4,
                 rep(8, 2),
                 rep(2, 8),
                 rep(4, 8)
                 )
    ####################################################################################
    # +++++ create sno-object containing data for sampling
    data <- as.data.frame(array(NA, c(layers, 19)))
    colnames(data) <- c("timestamp","Layer_Thick","T","Vol_Frac_I","Vol_Frac_W","Vol_Frac_V","Vol_Frac_S",
                              "Rho_S","Conduc_S","HeatCapac_S","rg","rb","dd","sp","mk","mass_hoar","ne","CDot","metamo")
    
    data[,1] <- paste0(start, "T00:00:00")                           # date
    data[,2] <- layer.thickness                                      # layer thickness
    data[,3] <- unlist(soil.temperature)                             # temperature
    data[,4] <- ice.volume                                           # fractional ice volume [0-1]
    data[,5] <- water.volume                                         # fractional water volume [0-1]
    data[,6] <- void.volume                                          # fractional voids volume [0-1]
    data[,7] <- soil.volume                                          # fractional soil volume [0-1]
    data[,8] <- soil.density                                         # soil density [kg/m3]
    data[,9] <- soil.conductivity                                    # mineral phase soil thermal conductivity [w/(mK)]
    data[,10] <- soil.capacity                                       # mineral phase soil heat capacity [J/K]
    data[,11] <- soil.rg                                             # grain radius [mm]
    data[,12] <- soil.rb                                             # bond radius [mm]
    data[,13] <- 0                                                   # snow dendricity (0 <- old snow, 1 <- new snow)
    data[,14] <- 0                                                   # snow sphericity (0 <- facettiert, 1 <- gerundet)
    data[,15] <- 0                                                   # marker (0 <- nothing, 7 <- glacier ice)
    data[,16] <- 0                                                   # mass_hoar (mass of surface hoar)
    data[,17] <- soil.ne                                             # ne number of elements
    data[,18] <- 0                                                   # CDot
    data[,19] <- 0                                                   # metamo

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
    header[12] <- paste0("ProfileDate = ", start, "T00:00")
    header[13] <- "HS_Last = 0.0000"
    header[14] <- "SlopeAngle = 20"
    header[15] <- "SlopeAzi = 170"
    header[16] <- paste0("nSoilLayerData = ", layers)
    header[17] <- "nSnowLayerData = 0"
    header[18] <- "SoilAlbedo = 0.15"
    header[19] <- "BareSoil_z0 = 0.200"
    header[20] <- "CanopyHeight = 0.00"
    header[21] <- "CanopyLeafAreaIndex = 0.00"
    header[22] <- "CanopyDirectThroughfall = 1.00"
    header[23] <- "WindScalingFactor = 1.00"
    header[24] <- "ErosionLevel = 0"
    header[25] <- "TimeCountDeltaHS = 0.000000"
    header[26] <- "fields = timestamp Layer_Thick T Vol_Frac_I Vol_Frac_W Vol_Frac_V Vol_Frac_S Rho_S Conduc_S HeatCapac_S rg rb dd sp mk mass_hoar ne CDot metamo"
    header[27] <- "[DATA]"

    # +++++ combine header and data    
    sno <- as.data.frame(array(NA, c(c(length(header) + layers), 19)))
    sno[1:length(header),1] <- header
    sno[c(length(header)+1):nrow(sno),] <- data 
    }
    
    sno <<- sno
}
