## ################################## ##
## an10_plot_racmo_summer_variables.R ##
## ################################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Plot average cumulative summer racmo values (Fig. 7)
#
# Comments: - Plots the average cumulative summer values
#           ! Make sure you run script dt07 first, which preps the data:
#             - Run dt07 separately for each variable listed here in u_variable
#             - Ensure u_summerMonths matches the geotiffs dates
#             - u_meanRaster should be TRUE in dt07 for this data
#           - RACMO2.3p3 data comes from van Dalum et al. (2021; 2022); see dt07
#           - The colour bars - z-limits are set up for the manuscript:
#             - They may not be appropriate for other shelves / time periods
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

# Which summers are in the data? Make sure there are geotiffs for these!
u_summerMonths <- c(12, 1)

# Which sensor should the data be plotted for?
u_variable <- c("snowmelt",
                "t2m",
                "albedo", 
                "precip", 
                "senf",
                "swsd", 
                "subl",
                "latf",
                "lwsd")

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# Create a pop-out window
dev.new(width  = 12,
        height = 12,
        unit   = "cm",
        noRStudioGD = TRUE)

# Prep the layout order
layout(matrix(c(1, 3, 5, 
                1, 3, 5,
                2, 4, 6,
                7, 9, 11,
                7, 9, 11,
                8, 10, 12,
                13, 15, 17, 
                13, 15, 17, 
                14, 16, 18), 
              byrow = TRUE, ncol = 3))

# We need to often reset the margins after plotting colour bars
ee$resetMar <- c(0,0,0,0)
par(mar = ee$resetMar)

# How many months are in the data? This allows rudimentary scaling of z-limits
ee$monthCount <- length(u_summerMonths) 

# _____CODE_____ ###############################################################
# Chunk 1: Plot the data! ======================================================
# For each variable,
for (ii in u_variable) {
  ## Prep for different variables ----------------------------------------------
  # z-limits for the plots
  ee$zLim <- switch(ii, 
                    snowmelt = c(0, 125) * ee$monthCount,
                    smb      = c(0, 60) * ee$monthCount,
                    precip   = c(0, 80) * ee$monthCount,
                    subl     = c(-20, 0) * ee$monthCount,
                    t2m      = c(268.5, 271.5),
                    albedo   = c(0.46, 0.86),
                    # remember: fluxes are in millions
                    swsd     = c(800, 1000) * ee$monthCount,
                    lwsd     = c(550, 750) * ee$monthCount,
                    senf     = c(-60, 60) * ee$monthCount ,
                    latf     = c(-60, -20) * ee$monthCount)
  
  ee$zLim <- switch(ii, 
                    snowmelt = c(0.25, 135) * ee$monthCount,
                    smb      = c(0, 60) * ee$monthCount,
                    precip   = c(15, 60) * ee$monthCount,
                    precip   = c(17.5, 62.5) * ee$monthCount,
                    subl     = c(-20, -5) * ee$monthCount,
                    t2m      = c(268.5, 271.5),
                    albedo   = c(0.78, 0.88),
                    # remember: fluxes are in millions
                    swsd     = c(725, 1025) * ee$monthCount,
                    lwsd     = c(575, 725) * ee$monthCount,
                    senf     = c(-60, 60) * ee$monthCount ,
                    latf     = c(-55, -25) * ee$monthCount)
  
  # And what are the units? display on colour bar
  ee$zUnit <- switch(ii, 
                     snowmelt = bquote("Melt (kg"~ m^2~ ")"),
                     smb      = bquote("SMB (kg"~ m^2~ ")"),
                     precip   = bquote("Precipitation (kg"~ m^2~ ")"),
                     subl     = bquote("Sublimation (kg"~ m^2~ ")"),
                     t2m      = bquote("Temperature (K)"),
                     albedo   = "Albedo",
                     swsd     = bquote(~SW ["IN"] ~ "(x" ~ 10^6 ~"J"~ m^2~ ")"),
                     lwsd     = bquote(~LW ["IN"] ~ "(x" ~ 10^6 ~"J"~ m^2~ ")"),
                     senf     = bquote(~"SHF (x" ~ 10^6 ~"J"~ m^2~ ")"),
                     latf     = bquote(~"LHF (x" ~ 10^6 ~"J"~ m^2~ ")"))
  
  # Do any go above or below the z-limits?
  ee$zLabels <- switch(ii,
                       snowmelt = "neither",
                       smb      = "below",
                       precip   = "both",
                       precip   = "neither",
                       subl     = "above",
                       t2m      = "both",
                       albedo   = "below",
                       swsd     = "below",
                       lwsd     = "neither",
                       senf     = "neither",
                       latf     = "above")
  
  # What colours are we using?
  ee$kulaS <- switch(ii, 
                     snowmelt = colour("YlOrBr")(11)[2:10],
                     smb      = colour("BuRd")(20)[c(9, 11:20)],
                     precip   = colour("devon")(14)[13:2], # no white
                     subl     = colour("acton")(11),
                     t2m      = colour("nuuk")(14),
                     albedo   = colour("tokyo")(15)[-c(1, 2, 14, 15)], # 11
                     swsd     = colour("lajolla")(16)[-c(1:2, 14:16)],
                     lwsd     = colour("lajolla")(16)[-c(1:3, 14:16)],
                     senf     = colour("BuRd")(18)[-c(1, 8, 9, 10, 11, 18)],
                     senf     = colour("BuRd")(24)[-c(1, 8, 9, 10, 11, 18)],
                     latf     = colour("BuRd")(24)[c(1:11)])
  
  # Make adjustments if necessary to plot aboveBelow
  if (ee$zLabels == "below") {
    ee$kulaE <- ee$kulaS[1]
    ee$kulaS <- ee$kulaS[-1]
  } else if (ee$zLabels == "above") {
    ee$kulaE <- ee$kulaS[length(ee$kulaS)]
    ee$kulaS <- ee$kulaS[-length(ee$kulaS)]
  } else if (ee$zLabels == "both") {
    ee$kulaE <- ee$kulaS[c(1, length(ee$kulaS))]
    ee$kulaS <- ee$kulaS[-c(1, length(ee$kulaS))]
  } else {
    ee$kulaE   <- c(gg$kulaQ[c(7, 7)], last(ee$kulaS))
  }
  
  ## Get the data --------------------------------------------------------------
  # Read in the data from file
  ee$racmoFile <- paste0(paste("Data", ff$versionInfo, "racmo/", sep = "/"),
                         paste("racmo2", 
                               u_summerMonths[1], last(u_summerMonths), 
                               ii, 
                               sep = "_"), ".tif")
  ee$racmo <- terra::rast(ee$racmoFile)
  
  # terra struggles with the crs, so add it explicitly
  crs(ee$racmo) <- paste("+proj=ob_tran +o_proj=longlat +o_lat_p=-180.0", 
                         "+lon_0=10.0 -m 57.295779506")
  
  # We need the average value if using albedo or temperature
  if (ii %in% c("albedo", "t2m")) {
    ee$racmo <- ee$racmo / ee$monthCount
  }
  
  ## Plot the data! ------------------------------------------------------------
  # Add the background of colours beyond the z-limits
  ee$plotTrick <- ee$racmo
  ee$plotTrick[ee$plotTrick < ee$zLim[1]] <- 1
  ee$plotTrick[ee$plotTrick > ee$zLim[2]] <- 2
  terra::plot(ee$plotTrick, 
              range = c(1, 2), 
              col   = ee$kulaE,
              axes  = FALSE, legend = FALSE,
              mar   = ee$resetMar)
  
  # Display racmo pixel values ( mean summer totals, except albedo & t2m )
  terra::plot(ee$racmo,
              add = TRUE,
              range = ee$zLim, 
              col   = ee$kulaS,
              axes  = FALSE, legend = FALSE)
  
  # Get the ee$shelf outline in the racmo projection
  ee$shelves <- terra::vect(ff$rawShelf)
  ee$shelf   <- ee$shelves[ee$shelves$NAME == u_shelf]
  ee$shelf   <- terra::project(ee$shelf, ee$racmo)
  lines(ee$shelf, lwd = 1.1)
  
  # Mappify
  addNorthArrow(raster(ee$racmo), 
                lwd = 1.1, 
                endLength = 0.05, 
                placeV = 95, 
                nOffset = 5)
  addPlotScale(raster(ee$racmo), 
               barLength = 0.00023, 
               label = 27, 
               placeH = 79, 
               placeV = 1,
               labelOffset = 5)
  
  
  # Annotate with letter for naming subplots
  annotateShelfPlot(raster(ee$racmo),
                    "topleft", nudgeH = -0.2, nudgeV = -0.02, 
                    main = paste0("(", letters[which(u_variable == ii)], ")"),
                    cex  = 2)
  
  # More significant figures needed on the temperatures scale
  if (u_variable == "t2m") {
    ee$tickSignif  = 4
  } else {
    ee$tickSignif  = 3
  }
  
  # Add a colour bar!
  addColourBar(col         = ee$kulaS, 
               zlim        = ee$zLim, 
               mar         = c(3.2, 5.5, 2, 5.5), # Change for plot window size
               allLabels   = "minMax",
               aboveBelow  = ee$zLabels,
               extraCol    = ee$kulaE, 
               horiz       = TRUE, 
               tickSignif  = ee$tickSignif,
               cex.labels  = 1.1,
               title       = ee$zUnit,
               cex.title   = 1.35,
               labelOffset = -3.85)
  par(mar = ee$resetMar)
}

# Finished
rm(ii)
cat("Script an10_plot_racmo_summer_variables.R complete! \n\n")
