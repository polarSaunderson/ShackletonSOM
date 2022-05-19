## ################################## ##
## an08_compare_sensors_vs_patterns.R ##
## ################################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Compare the AMSR and SSMIS melt observations on a day, and also 
#             compare against their assigned SOM patterns (Fig. S)
#
# Comments: - Plots the observations and assigned patterns from each sensor 
#             side-by-side to allow visual comparison
#           ! Make sure that scripts an01 & an07 have already been run
#           - Errors likely mean that the combination doesn't exist
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# User Options #################################################################
fresh("") # reset everything

# Which shelf are you interested in?
u_shelf   <- "Shackleton"

# What is the dataset version number?
u_version <- "v01"

# Which patterns to compare? -1 for any pattern; NA for the supplement plots
u_pattern1 <- NA   # AMSR
u_pattern2 <- NA   # SSMIS

# How many examples do you want to see?
u_sample   <- 3

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# Chunk 1: Evaluate if the SOM patterns fit with the SSMIS data ================
# SSMIS SOM Data
ee$sData  <- dd$sPatterns
ee$sClass <- ee$sData$unit.classif
ee$sDists <- ee$sData$distances

# Print so we know what's going on
cat("\n Fit of SSMIS against SOM \n")

# Calculate how well each pattern matches the SSMIS observations
for (ii in 1:9) {
  ee$tt <- ee$sDists[ee$sClass == ii] %>% mean() %>% round(3)
  cat("Pattern", ii, ":::", ee$tt, "n =", 
      length(ee$sDists[ee$sClass == ii]), "\n")
}
# Display
cat("All sData :::", mean(ee$sDists) %>% round(3), "n =", 
    length(ee$sDists), "\n")
printLine()

# Chunk 2: Visual examples to compare ========================================== 
## Prep the data ---------------------------------------------------------------
# Rename the assigned patterns for the days according to each sensor
ee$patterns <- dd$somCompare

# SOM Output data for plotting the SOM patterns
ee$sData <- dd$somOutput_amsrB

# ssmis melt vectors
ee$ssmVectors  <- dd$vectorGrid_ssmis

# amsr melt vectors
ee$amsrVectors <- dd$vectorGrid_amsrB

# We are only viewing a sample of the data
cat("\n SAMPLING DATA \n\n")

# Recreate supplement figure or choose random days to compare
if (!is.na(u_pattern1) | !is.na(u_pattern2)) {
  if ( u_pattern1 == -1 ) {
    u_pattern1 <- unique(ee$sClass)
    cat("\n Displaying random AMSR patterns\n")
  }
  if ( u_pattern2 == -1 ) {
    u_pattern2 <- unique(ee$sClass)
    cat("\n Displaying random SSMIS patterns\n")
  } 
  
  # Choose a sample!    
  ee$patternSample <- (ee$patterns[, 1][ee$patterns[, 2] %in% u_pattern1 & 
                                          ee$patterns[, 3] %in% u_pattern2]) %>% 
    sample(u_sample) %>%
    sort() # in chronological order
  
} else {
  # These are the dates used in the Supplement Figures (Fig. S7 & S8)
  ee$patternSample <- c(as.Date("2004-02-14") - as.Date("1970-01-01"),
                        as.Date("2006-12-23") - as.Date("1970-01-01"), 
                        as.Date("2007-12-04") - as.Date("1970-01-01"), 
                        as.Date("2008-01-01") - as.Date("1970-01-01"), 
                        as.Date("2008-01-20") - as.Date("1970-01-01"), 
                        as.Date("2009-12-15") - as.Date("1970-01-01"), 
                        as.Date("2010-02-08") - as.Date("1970-01-01"), 
                        as.Date("2014-12-07") - as.Date("1970-01-01"), 
                        as.Date("2012-12-14") - as.Date("1970-01-01"))
  
  # Create new figure window with defined size
  dev.new(width = 12,
          height = 10,
          unit = "cm",
          noRStudioGD = TRUE)
}

# # These are alternative dates that are interesting
# ee$patternSample <- c(as.Date("2006-01-14") - as.Date("1970-01-01"), 
#                    as.Date("2011-01-06") - as.Date("1970-01-01"),
#                    as.Date("2018-12-01") - as.Date("1970-01-01"), 
#                    as.Date("2015-12-28") - as.Date("1970-01-01"), 
#                    as.Date("2006-01-20") - as.Date("1970-01-01"), 
#                    as.Date("2004-01-23") - as.Date("1970-01-01"), 
#                    as.Date("2012-12-16") - as.Date("1970-01-01"))

## Plotting --------------------------------------------------------------------
# Set up plotting area
par(bg = "#F5F5F5",
    mfrow = c(3, 4),
    mar = c(1,1,2,1))

# For each sampled day...
for (ii in ee$patternSample) {
  
  # For supplement, create three separate plot windows
  if ((is.na(u_pattern1[1]) | is.na(u_pattern2[1])) & 
      which(ee$patternSample == ii) %in% c(4, 7)) {
    dev.new(width = 12,
            height = 10,
            unit = "cm",
            noRStudioGD = TRUE)
    par(bg = "#F5F5F5",
        mfrow = c(3, 4),
        mar = c(1,1,2,1))
  }
  
  # Get the matching data for a day
  ee$sampleIndex <- which(ee$patterns[, 1] == ii)
  ee$sampleData  <- (ee$patterns[ee$sampleIndex, ])
  cat(paste0("Sample Date: ", as.Date(ii, "1970-01-01")), "\n")
  print(ee$sampleData)
  printLine()
  
  ## AMSR SOM Pattern ----------------------------------------------------------
  # Which SOM pattern was assigned to the AMSR data on this day?
  ee$amsrSOM     <- ee$sData$codes[[1]][ee$sampleData[2], ]
  ee$amsrSOMPlot <- pixelValues2Raster(ee$amsrSOM, dd$commonMask)
  # here()
  
  # Add a title to the top 
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    par(mar = c(1,1,1.4,1))
  } else {
    par(mar = c(1,1,1,1))
  }
  
  # Add a blank background
  plotShelf(x = dd$commonMask,
            col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
            axes = FALSE, main = "",
            shelfOutline = NA)
  
  # Add the raster info
  plotShelf(ee$amsrSOMPlot,
            col  = gg$kulaS,
            add = TRUE,
            zlim = c(0, 1))
  
  # Add a title if on the first row
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    title(main = "AMSR SOM Pattern", line = 0.5)
  }
  
  # Add labelling details
  annotateShelfPlot(dd$commonMask, cex = 2.25, pos = "topleft", 
                    nudgeH = 0.01, 
                    ee$sampleData[2])
  annotateShelfPlot(dd$commonMask, cex = 2, pos = "bottomright", 
                    nudgeH = 3, nudgeV = -3,
                    paste0("(", letters[which(ee$patternSample == ii)], ")"))
  
  ## AMSR Melt Observation -----------------------------------------------------
  # Get the melt observation
  ee$amsrObs     <- ee$amsrVectors[which(rownames(ee$amsrVectors) == ii), ]
  ee$amsrObsPlot <- pixelValues2Raster(ee$amsrObs, dd$commonMask)
  
  # Add space for a title to the top row
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    par(mar = c(1,1,1.4,1))
  } else {
    par(mar = c(1,1,1,1))
  }
  
  # Create a blank background
  plotShelf(x = dd$commonMask,
            col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
            axes = FALSE, main = "",
            shelfOutline = NA)
  # Add the raster info
  plotShelf(ee$amsrObsPlot, 
            col = gg$kulaS,
            add = TRUE, main = "",
            zlim = c(0, 1))
  
  
  # Add a title if on the first row
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    title(main = "AMSR Observation", line = 0.5)
  }
  
  # Add labelling details
  annotateShelfPlot(dd$commonMask, cex = 1, pos = "bottomright", 
                    nudgeH = 0.75, nudgeV = -5,
                    main = as.Date(ii, "1970-01-01"))
  
  ## SSMIS Melt Observation ----------------------------------------------------
  # Get the data
  ee$ssmisObs     <- ee$ssmVectors[which(rownames(ee$ssmVectors) == ii), ]
  
  # Account for it is empty (i.e. not a melt season day according to SSMIS)
  if (sum(ee$ssmisObs) == 0) {
    ee$ssmisObsPlot <- ee$amsrObsPlot * 0
  } else {
    ee$ssmisObsPlot <- pixelValues2Raster(ee$ssmisObs, dd$commonMask)
  }
  
  # Add space for a title to the top row
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    par(mar = c(1,1,1.4,1))
  } else {
    par(mar = c(1,1,1,1))
  }
  
  # Create a blank background
  plotShelf(x = dd$commonMask,
            col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
            shelfOutline = NA,
            axes = FALSE)
  
  # Plot the raster
  plotShelf(ee$ssmisObsPlot, 
            col = gg$kulaS,
            zlim = c(0, 1), 
            add = TRUE)
  
  # Add a title to the top 
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    title(main = "SSMIS Observation", line = 0.5)
  }
  
  # Labelling details
  annotateShelfPlot(dd$commonMask, cex = 1, pos = "bottomright", 
                    nudgeH = 0.75, nudgeV = -5,
                    main = as.Date(ii, "1970-01-01"))
  
  ## SSMIS SOM Pattern ---------------------------------------------------------
  # Which SOM pattern is assigned to the SSMIS data?
  if (ee$sampleData[3] != 0) {
    ee$ssmisSOM     <- ee$sData$codes[[1]][ee$sampleData[3], ]
    ee$ssmisSOMPlot <- pixelValues2Raster(ee$ssmisSOM, dd$commonMask)
  } else {
    ee$ssmisSOMPlot <- ee$ssmisObsPlot * 0
  }
  
  # Add space for a title to the top row
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    par(mar = c(1,1,1.4,1))
  } else {
    par(mar = c(1,1,1,1))
  }
  
  # Create a blank background
  plotShelf(x = dd$commonMask,
            col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
            axes = FALSE,
            shelfOutline = NA)
  
  # Plot the raster
  plotShelf(ee$ssmisSOMPlot,
            col  = gg$kulaS,
            zlim = c(0, 1),
            add = TRUE)
  
  # Add labelling details
  annotateShelfPlot(dd$commonMask, cex = 2.25, pos = "topleft",
                    nudgeH = 0.01, 
                    ee$sampleData[3])
  addPlotScale(dd$commonMask, 25, placeH = 85)
  
  # Add a title to the top 
  if (which(ee$patternSample == ii) %in% c(1, 7)) {
    title(main = "SSMIS SOM Pattern", line = 0.5)
  }
}

# Finished
rm(ii)
printLine()
cat("\n Script an08_compare_sensors_vs_patterns.R complete!")
