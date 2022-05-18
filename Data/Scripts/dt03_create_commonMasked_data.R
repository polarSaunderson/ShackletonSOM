## ############################### ##
## dt03_create_commonMasked_data.R ##
## ############################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Create a common mask for the AMSR and resampled SSMIS datasets
#
# Comments: - The AMSR and SSMIS are different resolutions; this resamples the 
#             SSMIS data to the AMSR resolution and crops the datasets together
#           - This script is still necessary even if only using the AMSR data, 
#             because it prepares the data & creates the correct names / paths;
#             it only takes seconds to run if just using AMSR data
#           - There are likely much quicker ways of doing this overall - now, it 
#             takes around 4 minutes to resample all of the SSMIS data (for all 
#             years); with just AMSR data, it is only seconds
#           - Alternative masks could be included in ee$resampledRasters, or 
#             just created as a new raster in ee$commonMask. However, be aware 
#             that the data was already clipped to the shelf in script dt02
#           ! Make sure that the interim data has been created for all the 
#             sensors you are trying to include!   
#
# Updates:
# 2022/05/09  v1.0  Created a tidier version of the script
#

# User Options #################################################################
fresh("") # reset everything

# Which shelf are you interested in?
u_shelf    <- "Shackleton"

# Add a version number to keep track
u_version  <- "v01"

# Which sensors should be included?
u_sensors  <- c("amsre", "amsr2", "ssmis")

# Set-Up #######################################################################
source("R/setUp/su01_set_up.R") # Global variables, filepaths, functions, etc.

# Begin Timer
ee$startTime <- proc.time()

# _____CODE_____ ###############################################################
# Chunk 1: Preallocation Preparation ===========================================
## Load Data -------------------------------------------------------------------
# Pre-allocate lists for inputs from each sensor
ee$ii_meltLists    <- list() # the melting pixel ID numbers for each day
ee$ii_shelfIds     <- list() # the pixel ID numbers
ee$ii_shelfRasters <- list() # the binary raster of the shelf 
ee$ii_shelfAreas   <- list() # the pixel shelf areas
ee$ii_shelfMasks   <- list() # binary matrix of the shelf areas
ee$ii_resolution   <- list() # the pixel resolution

# Load & Store data for each sensor
for (ii in 1:length(u_sensors)) {
  # Which sensor is this?
  iiSensor <- u_sensors[ii]
  
  # Create a sub-environment to hold the data in for the sensor
  ee$subEnv <- new.env()
  loadFullFolder(paste0(ff$interimPath, ff$versionInfo, "_", iiSensor), 
                 ee$subEnv)
  
  # Store each datasets data in lists
  ee$ii_meltLists[[ii]]    <- ee$subEnv$meltList
  ee$ii_shelfIds[[ii]]     <- ee$subEnv$shelfIds
  ee$ii_shelfRasters[[ii]] <- ee$subEnv$shelfRaster
  ee$ii_shelfAreas[[ii]]   <- ee$subEnv$shelfAreas
  ee$ii_shelfMasks[[ii]]   <- ee$subEnv$shelfMask
  ee$ii_resolution[[ii]]   <- res(ee$subEnv$shelfRaster)[1]
  
  # Print Progress
  cat("___Loaded", ff$interimPath, iiSensor, "data into memory\n")
}
printLine()

# Name list items
names(ee$ii_meltLists)    <- u_sensors
names(ee$ii_shelfIds)     <- u_sensors
names(ee$ii_shelfRasters) <- u_sensors
names(ee$ii_shelfAreas)   <- u_sensors
names(ee$ii_resolution)   <- u_sensors
names(ee$ii_shelfMasks)   <- u_sensors

## Create a common mask from all input rasters ---------------------------------
# Set commonMask resolution
ee$maskRes   <- 12500 # matches amsr

# Which dataset does this match? (Later we resample to match against this)
ee$maskIndex  <- which(gg$rawResol == ee$maskRes)[1] 
ee$maskRaster <- ee$ii_shelfRasters[[ee$maskIndex]]

# Create empty container to hold resampled datasets
ee$resampledRasters <- list()

# Resample or just add to new container as necessary
for (ii in 1:length(ee$ii_shelfRasters)) {
  # Select shelf raster for current dataset
  ee$ii_raster <- ee$ii_shelfRasters[[ii]]
  
  # Get resolution of ii_raster
  ee$ii_Res <- as.integer(res(ee$ii_raster)[1])
  
  # Resample if neccesary (i.e. resolution doesn't match)
  if (ee$ii_Res != ee$maskRes) {
    cat("___Needs Resampling ::", 
        names(ee$ii_resolution)[ii], "\n")
    
    # Resample with nearest neighbour approach
    ee$resampledRasters[[ii]] <- resample(ee$ii_shelfRasters[[ii]], # from
                                          ee$maskRaster,            # to
                                          "ngb")                    # how? 
  } else {
    cat("___Needs Masking ::", 
        names(ee$ii_resolution)[ii], "\n")
    ee$resampledRasters[[ii]] <- ee$ii_shelfRasters[[ii]]
  }
  
  # Convert to binary masks of shelf / non-shelf
  ee$resampledRasters[[ii]][is.na(ee$resampledRasters[[ii]])] <- 0 # non-shelf
  ee$resampledRasters[[ii]][ee$resampledRasters[[ii]] != 0]   <- 1 # shelf
}
printLine()

# Reduce all input rasters to their common overlap
ee$commonMask <- Reduce("*", ee$resampledRasters)
cat("\n___Successfully created commonMask! \n")
printLine()

# Display masks to verify
par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

# Input masks
for (ii in 1:length(ee$resampledRasters)) {
  plotShelf(x = ee$resampledRasters[[ii]], 
            col = gg$kulaQ[c(7, 1)],
            main = names(ee$ii_resolution[ii]))
}

# Display common mask output
plotShelf(x = ee$commonMask, 
          col = gg$kulaQ[c(7, 1)],
          main = "commonMask")

# Chunk 2: Resample & Store Melting Pixel ID Numbers ===========================
## Prep for resampling ---------------------------------------------------------
# Get the correct pixel ids of the output raster grid
ee$commonIds <- ee$ii_shelfIds[[ee$maskIndex]]
ee$commonIds[as.matrix(ee$commonMask) == 0] <- NA

# Get the correct pixel areas of the output raster grid
ee$commonAreas <- ee$ii_shelfAreas[[ee$maskIndex]]
ee$commonAreas[as.matrix(ee$commonMask) == 0] <- NA

# Preallocate storage for the resampled melting pixel IDs lists
ee$meltListResampled <- rep(list(list()), length(u_sensors))

## Resample and/or mask data as necessary --------------------------------------
# Loop through all datasets
for (ii in 1:length(u_sensors)) {                                               
  
  # Preallocate a list to store the melting pixel IDs
  # How many days of data?
  ee$ii_sensorDays           <- length(ee$ii_meltLists[[ii]])
  
  # Create a list for each day to store the melting pixel ID numbers in
  ee$meltListResampled[[ii]] <- vector(mode = "list",  
                                          length = ee$ii_sensorDays) 
  
  # Which dataset is being used? Create a null copy of it 
  ee$ii_raster <- ee$ii_shelfRasters[[ii]] * 0
  
  # What is the resolution of this dataset? Determines masking or resampling
  ee$ii_rasterRes <- res(ee$ii_raster)[1] %>% as.integer()
  
  # Mask if resolutions mask, resample & mask if they disagree
  if (ee$ii_rasterRes == ee$maskRes) {
    #### ii_Raster is at the correct resolution: data just needs commonMasking
    cat("___Masking the", u_sensors[ii], "dataset \n")
    
    # Loop through all days and extract ID numbers of non-masked melting pixels
    for (jj in (1:ee$ii_sensorDays)) {                                             
      # Reform daily list of melting pixel IDs as a grid
      ee$meltGrid <- idList2meltGrid(ee$ii_meltLists[[ii]], 
                                     ee$ii_shelfIds[[ii]], 
                                     jj)
      
      # Mask out pixels beyond the commonMask
      ee$meltGrid[as.matrix(ee$commonMask) == 0] <- NA
      
      # Find & store only the pixel ID numbers where melt is observed
      ee$ids <- ee$commonIds[ee$meltGrid == 1]
      ee$meltListResampled[[ii]][[jj]] <- ee$ids[!is.na(ee$ids)]
    }
    
  } else {
    #### ii_Raster resolution doesn't match commonMask: needs resampling (slow!)
    cat("___Resampling the", u_sensors[ii], "dataset ... ",
        "This could take a few minutes ! \n")
    
    # Loop through all days and extract IDs of non-masked melting pixels
    for (jj in (1:ee$ii_sensorDays)) {
      # Reform daily list of melting pixel IDs as a grid
      ee$meltGrid <- idList2meltGrid(ee$ii_meltLists[[ii]], 
                                        ee$ii_shelfIds[[ii]], 
                                        jj)
      
      # Assign melting values to ii_raster, resample and apply commonMask
      values(ee$ii_raster)  <- ee$meltGrid
      ee$ii_rasterResampled <- resample(ee$ii_raster,  # from
                                        ee$commonMask, # to
                                        "ngb")         # how?
      ee$ii_rasterResampled[ee$commonMask == 0] <- NA
      
      # Find & store only the pixel ID numbers where melt is observed
      ee$ids <- ee$commonIds[as.matrix(ee$ii_rasterResampled) == 1]
      ee$meltListResampled[[ii]][[jj]] <- ee$ids[!is.na(ee$ids)]
    }
  }
  # Name each list (i.e. which version, sensor and Picard day)
  names(ee$meltListResampled[[ii]]) <- names(ee$ii_meltLists[[ii]])[1:ee$ii_sensorDays]
}

# Rename sublists with the sensor name to help understand what they show!
names(ee$meltListResampled) <- u_sensors
printLine()

# Chunk 3: Save Data ===========================================================
# Create a subfolder to store the output in at the end
ee$subFolder <- createSubFolders(ff$interimPath, 
                                    paste(ff$versionInfo,"commonMask", sep = "_"))

# File path & name
ee$fileNaming <- paste0(ee$subFolder, "/", ff$versionInfo, "_commonMask")

# Save
save(meltListResampled, envir = ee, 
     file = paste0(ee$fileNaming, "_meltList.rData"))
save(commonMask,  envir = ee, 
     file = paste0(ee$fileNaming, "_commonMask.rData"))
save(commonAreas, envir = ee, 
     file = paste0(ee$fileNaming, "_commonAreas.rData"))
save(commonIds,   envir = ee, 
     file = paste0(ee$fileNaming, "_commonRaster.rData"))

# Finished
rm(ii, iiSensor, jj)
ee$runningTime <- proc.time() - ee$startTime
print(ee$runningTime)
printLine()
cat("\n Script sc03 complete for", ff$versionInfo, "\n\n")
beep("ping") 
