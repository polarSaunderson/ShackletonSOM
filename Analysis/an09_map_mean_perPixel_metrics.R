## ################################ ##
## an09_map_mean_perPixel_metrics.R ##
## ################################ ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Plot the mean metric values for each pixel and RACMO flux (Fig. 6)
#
# Comments: - Creates maps of the mean metric values for each pixel over the 
#             full data period studied
#           ! Make sure that the dt05 script has been run
#           ! If you don't have the RACMO data from script dt07, the code here 
#             will plot the melt metrics first and then fail
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

# Which sensor should the data be plotted for?
u_sensor  <- c("amsrB")  # "amsre", "amsr2", "amsrB", "ssmis"

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# Chunk 1: Prepare Data ========================================================
# Identify relevant data corresponding to u_sensor setting
ee$sensorIndex <- which(dd$inputSensors == u_sensor)
ee$pixelData   <- list(dd$pixelData$Onset[[ee$sensorIndex]],
                       dd$pixelData$Freeze[[ee$sensorIndex]],
                       dd$pixelData$Length[[ee$sensorIndex]],
                       dd$pixelData$Duration[[ee$sensorIndex]],
                       dd$pixelData$Fraction[[ee$sensorIndex]])

# Chunk 2: Define Plot Settings ================================================
# What units are being displayed by the colours? Add title to colour bar
ee$barNames <- c("Onset (days since 1st Nov)", 
                 "Freeze-Up (days since 1st Nov)", 
                 "Season Length (days)", 
                 "Season Duration (days)", 
                 "Season Fraction", 
                 expression(paste("Summer Flux (kg  ", m^-2 , ")")))

# What are the min and max values to plot? Need to define so each plot matches
ee$barZlims <- list(c(25, 55), c(75, 105),  c(28, 76), 
                    c(10, 70), c(0.67, 0.91), c(0.5, 300))

# What colour scheme should be used? Differs for each metric
ee$kulas <- list(colour("sunset")(22)[22:13],               # onset
                 colour("sunset")(22)[1:10][10:1],    # freeze-up
                 colour("nuuk")(12)[8:1],             # length
                 colour("YlOrBr")(13)[-c(1:3)],       # duration
                 colour("tokyo")(11)[c(9:2)],         # fraction
                 colour("YlOrBr")(13)[-c(1:3)])       # flux

# For values beyond the z-limits
ee$kulaE <- list(colour("sunset")(22)[c(12)],               # onset
                 colour("sunset")(22)[c(11)],         # freeze-up
                 colour("nuuk")(12)[c(9)],            # length 
                 colour("YlOrBr")(13)[c(3)],          # duration
                 colour("tokyo")(11)[c(10, 1)])       # fraction

ee$extraBar <- list("above",
                    "below", 
                    "below",
                    "below",
                    "both")

# Subplot lettering for the paper figure
ee$subPlotLetter <- c("a", "d", "b", "c", "e", "f")

# Create pop-out window
dev.new(width  = 12,
        height = 11,
        unit   = "cm",
        noRStudioGD = TRUE)

# Change plot order & include location for an adjacent colour bar
layout(matrix(c(1, 5, 7,
                1, 5, 7,
                1, 5, 7,
                1, 5, 7,
                2, 6, 8,
                3, 9, 11,
                3, 9, 11,
                3, 9, 11,
                3, 9, 11,
                4, 10, 12), byrow = TRUE, ncol = 3))

# We'll need to reset the plotting area often after adding a colour bar
ee$resetMar <- c(1, 1, 1, 1)
par(mar = ee$resetMar)

# Chunk 3: Plotting Average Metric Values ======================================
cat(" Displaying average metric values for", u_sensor, "\n")

# Plot each metric's average values
for (ii in 1:5) {
  # Get data for metric ii (last col holds the mean values)
  ee$meanData <- ee$pixelData[[ii]][, dim(ee$pixelData[[ii]])[2]] 
  
  # Uncommenting here plots another statistic: beware of colour bars & z-limits!
  # ee$meanData <- ee$pixelData[[ii]][, -1] %>% apply(1, na.rm= TRUE, sd)
  
  # Plot grey area for non-shelf area (and reset plotRaster for each summer)
  ee$plotRaster <- dd$commonMask       # commonMask to get the shelf pixels
  plotShelf(x    = ee$plotRaster,
            col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"), 
            axes = FALSE, withArrow = FALSE,shelfOutline = NA,
            cex.main = 2)
  
  # Add the mean values to the raster
  ee$plotRaster <- pixelValues2Raster(ee$meanData, dd$commonMask)
  
  # Points beyond zlimits -----------------------------------------------------!
  # Create and add a new raster to overlay and colour the extreme points
  ee$pp <- ee$plotRaster
  ee$pp[ee$pp < ee$barZlims[[ii]][2] & 
          ee$pp > ee$barZlims[[ii]][1]] <- mean(ee$barZlims[[ii]])
  
  # Add it
  plotShelf(x   = ee$pp,
            add = TRUE,
            col = ee$kulaE[[ii]])
  
  # Plot within z-limits data pixel metric map --------------------------------!
  plotShelf(x    = ee$plotRaster,
            zlim = ee$barZlims[[ii]],
            add  = TRUE, 
            col  = ee$kulas[[ii]])
  
  # Add a plot scale
  addPlotScale(ee$plotRaster, barLength = 25, placeH = 80, labelOffset = 4)
  
  # Annotate each subplot with a letter
  annotateShelfPlot(ee$plotRaster, 
                    "topleft", nudgeH = 0.03,
                    main = paste0("(", ee$subPlotLetter[ii], ")"),
                    cex  = 2.25)
  
  # Add a colour bar at the end
  addColourBar(col         = ee$kulas[[ii]], 
               zlim        = ee$barZlims[[ii]], 
               mar         = c(3, 2, 1.5, 2), # Change for plot window size
               aboveBelow  = ee$extraBar[[ii]], 
               extraCol    = ee$kulaE[[ii]],
               allLabels   = "odds",
               horiz       = TRUE,
               cex.labels  = 1.2,
               title       = ee$barNames[ii],
               cex.title   = 1.4,
               labelOffset = -3.5)
  
  # Reset margins for next map
  par(mar = ee$resetMar)
}
rm(ii)

# Display to console which years are included in the averages
cat(" Check the pixelData object for the years included in the average \n")

# Chunk 4: Plot Average RACMO Melt Flux Values =================================
par(mar = ee$resetMar)

# READ IN THE GEOTIFF WE MADE OF RACMO
ff$racmoFile <- paste0(paste("Data", ff$versionInfo, "racmo/", sep = "/"),
                       paste("racmo2_11_2_snowmelt.tif"))
ee$racmo <- terra::rast(ff$racmoFile)
crs(ee$racmo) <- paste("+proj=ob_tran +o_proj=longlat +o_lat_p=-180.0", 
                       "+lon_0=10.0 -m 57.295779506")
# Add the background of colours beyond the z
ee$plotTrick <- ee$racmo
ee$plotTrick[ee$plotTrick <= ee$barZlims[[6]][1]]    <- 1
ee$plotTrick[ee$plotTrick >= last(ee$barZlims[[6]])] <- 2

terra::plot(ee$plotTrick, 
            range = c(1, 2), 
            col   = paste0(gg$kulaQ[7], "99"),
            axes  = FALSE, legend = FALSE,
            mar   = ee$resetMar)

# Display racmo pixel values ( mean summer totals, except albedo & t2m )
terra::plot(ee$racmo,
            add   = TRUE,
            col   = gg$kulaS, 
            range = ee$barZlims[[6]],
            axes  = FALSE, legend = FALSE)

# Get the shelf outline in the RACMO projection
ee$shelves <- terra::vect(ff$rawShelf)
ee$shelf   <- ee$shelves[ee$shelves$NAME == u_shelf]
ee$shelf   <- terra::project(ee$shelf, ee$racmo)
lines(ee$shelf, lwd = 1.2)

# Add a north arrow
addNorthArrow(raster(ee$racmo), lwd = 1.2, 
              endLength = 0.06, placeV = 96,
              nOffset = 4, nSize = 0.9, col = "black")

# Add a scale bar
addPlotScale(raster(ee$racmo), 
             barLength = 0.00025, # we know these pixels are 27km; this fits!
             label = 27, 
             placeH = 79, 
             placeV = 2,labelOffset = 4)

# Annotate with letter for naming subplots
annotateShelfPlot(raster(ee$racmo),
                  "topleft", nudgeH = -0.01,
                  main = paste0("(", ee$subPlotLetter[6], ")"),
                  cex  = 2.25)

# Add a colour bar for the fluxes
addColourBar(col         = ee$kulas[[6]], 
             zlim        = ee$barZlims[[6]], 
             mar         = c(2.9, 2, 1.7, 2), # Change for plot window size
             allLabels   = "odds",
             horiz       = TRUE, tickSignif = 2,
             cex.labels  = 1.2,
             title       = ee$barNames[6],
             cex.title   = 1.4,
             labelOffset = -3.75)

# Finished
printLine()
cat(" Script an09_map_mean_perPixel_metrics.R complete!\n")
