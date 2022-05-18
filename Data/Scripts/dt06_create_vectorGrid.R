## ######################## ##
## dt06_create_vectorGrid.R ##
## ######################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Create vectorGrids for each sensor, which are ready for direct 
#             input to the SOM functions in later scripts
#
# Comments: - A very simple rearrangement that prepares the data for SOM input
#             - Creating it here keeps this separate 
#             - It also eases / speeds up sensitivity testing
#           - In the vectorGrid, each row is a day, and each column a specific 
#             pixel of the mapped shelf
#
# Updates:
# 2022/05/09  v1.0  Created a tidier version of the script
#

# User Options #################################################################
fresh("") # reset everything

# Which shelf are you interested in?
u_shelf <- "Shackleton"

# Add a version number to keep track
u_version  <- "v01"

# Which sensors should be included?
u_sensors  <- c("amsre", "amsr2", "ssmis")

# Set-Up #######################################################################
source("R/setUp/su01_set_up.R") # Global variables, filepaths, functions, etc.

# If both amsr datasets are being used, also use the combined AMSR-B 
if ("amsre" %in% u_sensors & "amsr2" %in% u_sensors) {
  u_sensors <- c(u_sensors, "amsrB")
}

# Create a folder to store the vectorGrid
ee$subFolder <- createSubFolders(ff$interimPath, 
                                    paste0(ff$versionInfo, "_som"))

# Begin Timer
ee$startTime <- proc.time()

# _____CODE_____ ###############################################################
# Chunk 1: Prepare Data ========================================================
# We only want to include melt season days
for (ii in 1:length(u_sensors)) {
  ee$meltList   <- dd$newMeltList[[ii]]
  ee$meltData   <- dd$meltGrid[[ii]]
  
  ee$meltDays   <- ee$meltList[!is.na(ee$meltData$mSeason_c10)]
  ee$meltDate   <- ee$meltData$date_c1[!is.na(ee$meltData$mSeason_c10)]
  
  # How many days have some melt?
  ee$somDays    <- length(ee$meltDays)
  
  # Preallocate matrix that is the input for SOM algorithm
  # Only get the ID numbers of shelf pixels
  ee$shelfIds   <- dd$commonIds[!is.na(dd$commonIds)] 
  ee$vectorGrid <- matrix(NA, 
                             nrow = ee$somDays,
                             ncol = length(ee$shelfIds)) %>% 
    `colnames<-`(ee$shelfIds) %>%
    `rownames<-`(ee$meltDate)
  
  # Populate vector storage with binary melt/no-melt for each pixel for each day
  for (jj in 1:ee$somDays) {
    ee$vectorGrid[jj, ] <- ee$shelfIds %in% ee$meltDays[[jj]] * 1
  }
  
  # Rename objects for tracking the sensor used when loaded elsewhere
  ee$gridName <- paste0("vectorGrid_", u_sensors[ii])
  assign(ee$gridName, ee$vectorGrid, envir = ee)
  
  # Save the vectorGrid
  do.call(save, list(ee$gridName, 
                     file = paste0(ee$subFolder, "/",
                                             ff$versionInfo, "_vectorGrid_",
                                             u_sensors[ii], ".rData")), 
          envir = ee)
  
  cat(" Saved vectorGrid for", u_sensors[ii], "\n")
}

# Finished
rm(ii, jj)
printLine()
cat("\n Script dt06 complete for", ff$versionInfo, "\n\n")
beep("ping")
