## ############################## ##
## dt05_create_perPixel_metrics.R ##
## ############################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Create melt metrics on a per-pixel basis for each sensor
#
# Comments: - Metrics include: 
#               - melt season onset
#               - melt season freeze-up 
#               - melt season length
#               - melt season duration
#               - melt season fraction
#           - Calculates the metrics for each summer individually, and as 
#             an average across each respective dataset  
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
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

# If both amsr datasets are being used, also use the combined AMSR-B 
if ("amsre" %in% u_sensors & "amsr2" %in% u_sensors) {
  u_sensors <- c(u_sensors, "amsrB")
}

# Begin Timer
ee$startTime <- proc.time()

# _____CODE_____ ###############################################################
# Chunk 1: Preallocate to hold perPixel Data ===================================
# Only apply to data that exists!
ee$sensors      <- names(dd$meltGrid)

# Create a list of all pixels with shelf area
ee$shelfPixels  <- dd$commonIds[!is.na(dd$commonIds)]

# Preallocate storage container for pixels each summer (col = summer, row = ID)
ee$pixelDuration <- rep(list(matrix(NA, 
                                    ncol = length(gg$satellitePeriod) + 1,
                                    nrow = length(ee$shelfPixels)) %>%
                               `rownames<-`(ee$shelfPixels) %>%
                               `colnames<-`(c(paste0("summer",
                                                     gg$satellitePeriod + 1),
                                              "meanValue"))), # extra col = mean
                        length(u_sensors)) # Repeat the holder for each sensor
names(ee$pixelDuration) <- u_sensors

# Replicate for each metric
ee$pixelOnset    <- ee$pixelDuration
ee$pixelFreeze   <- ee$pixelDuration
ee$pixelLength   <- ee$pixelDuration
ee$pixelFraction <- ee$pixelDuration

# Chunk 2: Calculate Melt Season Duration ======================================
# Loop through sensors
for (ii in 1:length(u_sensors)) {
  # Rename for easier handling
  ee$mList <- dd$newMeltList[[ii]]
  ee$mData <- dd$meltGrid[[ii]]
  
  # Calculate annual occurrence (How often do pixel ID's appear each summer?)
  for (jj in gg$summerIndices) {
    # List all ID numbers for melting pixel this summer
    ee$summerIds <- ee$mList[ee$mData$summerIndex_c6 == jj] %>% 
      unname() %>% unlist()
    
    # Count occurrence of the pixel in above list
    for (kk in ee$shelfPixels) {
      ee$idCount <- length(ee$summerIds[ee$summerIds == kk])
      
      # Store
      ee$pixelDuration[[ii]][which(ee$shelfPixels == kk), 
                                which(gg$summerIndices == jj)] <- ee$idCount
    }
  }
}
cat(" Calculated Melt Season Duration \n")

# Chunk 3: Calculate Melt Season Onset & Freeze-Up =============================
# Loop through sensors
for (ii in 1:length(u_sensors)) {
  # Rename for easier handling
  ee$mList <- dd$newMeltList[[ii]]
  ee$mData <- dd$meltGrid[[ii]]
  
  # NA's complicate indexing by matching, so ignore them with a -1
  ee$mData$summerIndex_c6[is.na(ee$mData$summerIndex_c6)] <- -1
  
  # Identify first & last melting days each summer
  for (jj in gg$summerIndices) {
    # ID numbers of all melt pixels for summer jj; keep each day a separate list 
    ee$summerIds <- ee$mList[ee$mData$summerIndex_c6 == jj]
    
    
    # Only proceed if there is melt data for summer jj
    if (length(ee$summerIds)) {
      # For each pixel ID...
      for (kk in ee$shelfPixels) {
        
        # Check every day for melt onset by...
        for (mm in 1:length(ee$summerIds)) {
          
          # Check when the ID is first found (store day & break loop when it is)
          if (kk %in% ee$summerIds[[mm]]) {
            ee$pixelOnset[[ii]][which(ee$shelfPixels == kk), 
                                   which(gg$summerIndices     == jj)] <- mm
            # print(which(ee$shelfPixels == kk))
            # cat("found first day of pixel ", kk, ": day ", mm, "\n")
            break
          }
        }
        
        # Check every day for melt freeze-up (do the same check, but in reverse)
        for (mm in length(ee$summerIds):1) {
          
          # Check day the ID is found first (reverse = last); store & break loop
          if (kk %in% ee$summerIds[[mm]]) {
            ee$pixelFreeze[[ii]][which(ee$shelfPixels == kk), 
                                    which(gg$summerIndices     == jj)] <- mm
            # cat("found last day of pixel ", kk, ": day ", mm, "\n---\n")
            break
          }
        }
      }
    }
  }
}
cat(" Calculated Melt Season Onset & Freeze-Up Dates \n")

# Chunk 4: Pixel Length & Fraction =============================================
for (ii in 1:length(u_sensors)) {
  ee$pixelLength[[ii]]   <- ee$pixelFreeze[[ii]] - 
    ee$pixelOnset[[ii]] + 1
  ee$pixelFraction[[ii]] <- (ee$pixelDuration[[ii]] / 
                                  ee$pixelLength[[ii]]) %>% round(3)
}
cat(" Calculated Melt Season Length & Fraction \n")

# Chunk 5: NA Years Without Melt Observations ==================================
for (ii in 1:length(u_sensors)) {
  # Identify years without any melt anywhere - indicates no observations
  ee$naColumns <- which(apply(ee$pixelDuration[[ii]], 2, sum) == 0) %>% 
    unname()
  
  # Remove SSMIS data for 1987/88 as incomplete observations 
  if (u_sensors[ii] == "ssmis") {
    ee$naColumns <- c(ee$naColumns, 
                         which(colnames(ee$pixelDuration[[1]]) == "summer1988"))
  }
  
  # NA all pixels in the above years
  ee$pixelDuration[[ii]][ , ee$naColumns] <- NA
  ee$pixelOnset[[ii]][ , ee$naColumns]    <- NA
  ee$pixelFreeze[[ii]][ , ee$naColumns]   <- NA
  ee$pixelLength[[ii]][ , ee$naColumns]   <- NA
  ee$pixelFraction[[ii]][ , ee$naColumns] <- NA
}

cat(" Removed any years with missing data \n")

# Chunk 6: Calculate Mean Values ===============================================
for (ii in 1:length(u_sensors)) {
  ee$endCol <- length(gg$satellitePeriod) + 1
  ee$pixelDuration[[ii]][, ee$endCol] <- apply(ee$pixelDuration[[ii]], 
                                                     1, na.rm = TRUE, 
                                                     mean) %>% round(2)
  ee$pixelOnset[[ii]][, ee$endCol]    <- apply(ee$pixelOnset[[ii]], 
                                                     1, na.rm = TRUE, 
                                                     mean) %>% round(2)
  ee$pixelFreeze[[ii]][, ee$endCol]   <- apply(ee$pixelFreeze[[ii]], 
                                                     1, na.rm = TRUE, 
                                                     mean) %>% round(2)
  ee$pixelLength[[ii]][, ee$endCol]   <- apply(ee$pixelLength[[ii]], 
                                                     1,na.rm = TRUE, 
                                                     mean) %>% round(2)
  ee$pixelFraction[[ii]][, ee$endCol] <- apply(ee$pixelFraction[[ii]], 
                                                     1, na.rm = TRUE, 
                                                     mean) %>% round(2)
}
cat(" Calculated Average Values for Each Sensor \n")
printLine()

# Chunk 6: Saving Data =========================================================
ff$fileNaming <- paste0(ff$analysePath, ff$versionInfo, "_pixel")

save(pixelDuration, envir = ee, file = paste0(ff$fileNaming, "Duration.rData"))
save(pixelOnset,    envir = ee, file = paste0(ff$fileNaming, "Onset.rData"))
save(pixelFreeze,   envir = ee, file = paste0(ff$fileNaming, "Freeze.rData"))
save(pixelLength,   envir = ee, file = paste0(ff$fileNaming, "Length.rData"))
save(pixelFraction, envir = ee, file = paste0(ff$fileNaming, "Fraction.rData"))

# Finished
rm(mm, kk, ii, jj)
cat(" Saved perPixel metrics\n")
printLine()
cat(" Script dt05 complete for", ff$versionInfo, "\n\n")
beep("ping")
