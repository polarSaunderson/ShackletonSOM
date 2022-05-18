## ################################## ##
## an03_compare_SOM_vs_observations.R ##
## ################################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Plot examples of the days assigned to a specific pattern (Fig. S3)
#
# Comments: - Each SOM pattern is representative of many melt days (the pattern
#             is a model of them, not exact replicas). This script allows visual
#             comparison of how good the SOM pattern (i.e. the model) matches 
#             the observed melt
#           ! Make sure you have run script an01 already
#
# Updates:
# 2022/05/09  v1.0  Created a tidier version of the script
#

# User Options #################################################################
fresh("") # reset everything

# Which shelf are you interested in?
u_shelf   <- "Shackleton"

# What is the dataset version number?
u_version <- "v01"

# Which sensor were the SOM patterns created from? Only tested with amsrB
u_sensor  <- "amsrB"

# Which pattern do you want to look at?
u_pattern <- 2

# Which letter should be added to the subplot? (for the manuscript)
u_subplot <- "(a)"

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# Get the data for the correct sensor and rename for ease
ee$vectorData <- get(paste0("vectorGrid_", u_sensor), envir = dd)
ee$meltGrid   <- get(paste0("meltGridSom_", u_sensor), envir = dd)
ee$somData    <- get(paste0("somOutput_", u_sensor), envir = dd)

# _____CODE_____ ###############################################################
# Chunk 1: Plot the SOM Pattern ================================================
# Create new figure window with defined size
dev.new(width = 12,
        height = 14,
        unit = "cm",
        noRStudioGD = TRUE)

# Prep plot area
par(mfrow = c(5, 5),
    mar = c(1, 1, 1, 1))
layout(matrix(c(1, 1, 2, 3, 4,
                1, 1, 5, 6, 7,
                8, 9, 10, 11, 12,
                13, 14, 15, 16, 17,
                18, 19, 20, 21, 22), byrow = TRUE, ncol = 5))

# Add a background colour to help differentiate plot groupings in the supplement
par(bg = "#F5F5F5")

# Plot the SOM Pattern
ee$somRast <- pixelValues2Raster(ee$somData$codes[[1]][u_pattern, ], dd$commonMask)
plotShelf(x    = dd$commonMask,
          col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
          axes = FALSE,
          sub  = "kk",
          shelfOutline = NA)
plotShelf(x    = ee$somRast, 
          zlim = c(0, 1),
          col  = gg$kulaS,
          add  = TRUE,
          axes = FALSE)

# Prettify!
annotateShelfPlot(raster = dd$commonMask, 
                  main   = u_pattern,
                  pos    = "topleft", 
                  cex    = 3)
annotateShelfPlot(raster = dd$commonMask,  
                  main   = u_subplot,
                  pos    = "bottomright", 
                  nudgeH = 5, nudgeV = -4,
                  cex    = 2.75)

# Chunk 2: Plot the observed melt ==============================================
# Retrieve the assigned pattern on all days with melt
ee$meltGrid$mSeason_c10[!is.na(ee$meltGrid$mSeason_c10)] <- 1
ee$mPatterns <- filterData(ee$meltGrid, "mPattern_c11", "mSeason_c10", 1)
ee$mDates    <- filterData(ee$meltGrid, "date_c1", "mSeason_c10", 1)

# Get binary melt vector on days with pattern u_pattern 
ee$iiData <- ee$vectorData[ee$mPatterns == u_pattern, ]

# And retrieve the corresponding dates
ee$iiDates <- ee$mDates[ee$mPatterns == u_pattern]

# Randomly sample 21 days to plot
set.seed(123)
ee$kkRandoms <- sample(1:dim(ee$iiData)[1], 21) %>% sort()

## Plot ------------------------------------------------------------------------
# Plot them up!
for (kk in ee$kkRandoms) {
  # Create a raster of the correct data
  ee$kkRast <- pixelValues2Raster(ee$iiData[kk, ], dd$commonMask)
  
  # Grey background
  plotShelf(x    = dd$commonMask,
            col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
            axes = FALSE, withArrow = FALSE,
            shelfOutline = NA)
  
  # Plot melt pattern
  plotShelf(x    = ee$kkRast, 
            zlim = c(0, 1),
            col  = gg$kulaS,
            add  = TRUE, withArrow = FALSE,
            axes = FALSE)
  
  # Add the observation date
  annotateShelfPlot(raster = dd$commonMask, 
                    main = as.Date(ee$iiDates[kk], "1970-01-01"),
                    pos = "bottomright",
                    nudgeV = -10,
                    nudgeH = 0.5,
                    cex = 0.75)
}

# Finished
rm(kk)
printLine()
cat("\n Script an03_compare_SOM_vs_observations.R complete!")
