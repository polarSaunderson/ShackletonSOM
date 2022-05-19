## ######################### ##
## su04_set_global_options.R ##
## ######################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Define global options relating to the sensors
#
# Comments: - You shouldn't need to do anything here
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# SET GLOBAL OPTIONS ###########################################################
# Create an environment for global options
gg <- new.env()

# Default Colours for Plots
gg$kulaD <- colour("sunset")(20)   # default colour scheme - divergent
gg$kulaQ <- colour("bright")(7)    # default colour scheme - qualitative
gg$kulaS <- colour("YlOrBr")(10)   # default colour scheme - sequential

# Specifics of the sensors - the order matters here!
gg$sensors    <- c("amsre", "amsr2", "ssmis")
gg$rawResol   <- c(12500, 12500, 25000) # pixel width in metres
gg$startYear  <- c(2002, 2012, 1979)    # Nov. of summers w/ full data
gg$endYear    <- c(2010, 2020, 2019)    # Also November dates 
gg$startDate  <- "-04-01"

# Which summers have complete data?
gg$dataPeriod <- c(min(gg$startYear):max(gg$endYear))                    
if (exists("u_sensors")) {
  gg$dataPeriod <- c()
  for (ii in 1:length(u_sensors)) {
    gg$dataPeriod    <- c(gg$dataPeriod, 
                          gg$startYear[ii]:gg$endYear[ii]) %>% 
      unique() %>% sort()
  }
  # In the scripts we'll use a summer index to help with the austral summer
  # We define these against the start of the satellite period
  # A summer index of 1 is summer 1979/1980
  gg$satellitePeriod <- c(1979:max(gg$dataPeriod))
  gg$summerIndices   <- gg$satellitePeriod - 1978
  rm(ii)
}

