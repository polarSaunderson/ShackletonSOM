## ############# ##
## su11_set_up.R ##
## ############# ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Prepares for the analysis scripts
#
# Comments: - You shouldn't need to do anything here
#           - But you can overwrite user settings (u_variable) instead of having
#             to change them individually in each script
#
# Updates:
# 2022/05/09  v1.0  Created a tidier version of the script

# RUN SET UP SCRIPTS ###########################################################
source("R/setUp/su01_set_up.R")

# OVERWRITE USER SETTINGS ######################################################
# Optional; comment out to use script settings
# u_shelf   <- "Shackleton"
# u_version <- "v01"
# u_sensor  <- "amsrB"
# u_somCol  <- 3
# u_somRow  <- 3

# PREPARE FOR ANALYSIS SCRIPTS #################################################
# Create a new environment to keep the script output / variables tidy
ee <- new.env()

# Which sensors have some data? We only want to try plotting if there's data!
dd$inputSensors <- names(dd$meltGrid)

# Create list of all perPixel datasets - used in an01 & an02
dd$pixelData <- list(dd$pixelOnset,  dd$pixelFreeze, 
                     dd$pixelLength, dd$pixelDuration, 
                     dd$pixelFraction) 

# Set names of pixel metrics
gg$pixelMetrics <- c("Onset", "Freeze", "Length", 
                  "Duration", "Fraction", "RACMO Flux")
names(dd$pixelData) <- gg$pixelMetrics[-6]

# Which sensors should be plotted? Used in an12
if ("amsre" %in% dd$inputSensors & "amsr2" %in% dd$inputSensors) {
  dd$plotSensors <- "amsrB"
  if ("ssmis" %in% dd$inputSensors) {
    dd$plotSensors <- c("amsrB", "ssmis")
  }
} else {
  dd$plotSensors <- dd$inputSensors
}

# Load interim SOM data if it exists
ff$interimData <- list.dirs(ff$interimPath, full.names = FALSE)
ff$somData  <- paste0(ff$versionInfo, "_som")

if (ff$somData %in% ff$interimData) {
  # Load interim som data
  loadFullFolder(paste0(ff$interimPath, ff$versionInfo, "_som"), dd)
}
