## ############################## ##
## an12_plot_annual_melt_extent.R ##
## ############################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Plot a time series of the melt extent for each summer
#
# Comments: - Plots AMSR and SSMIS daily melt extent throughout the summer, 
#             plotting the two datasets together on the same plot
#           - Can plot data for a single summer, all summers with data, or only 
#             summers with AMSR-E or AMSR-2 data
#
# Updates:
# 2022/08/30  v1.1  Created a tidier version of the script to share
#
# User Options #################################################################
# fresh("") # reset everything

# Which shelf are you interested in?
u_shelf   <- "Shackleton"

# What is the dataset version number?
u_version <- "v01"

# Plot a single summer (use Jan date), all summers (NA), or all "amsr" summers? 
u_summer <- 2018 # "amsr" # NA

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# Chunk 1: Plot Data ===========================================================
cat(" Plotting Daily Melt Extent for", ff$versionInfo, "data\n")

# Create new figure window with defined size
dev.new(width = 12,
        height = 6,
        unit = "cm",
        noRStudioGD = TRUE)

# Define plotting area
par(mfrow = c(3, 3),
    mar = c(3, 5, 2, 2))

# Which summers to plot?
if (is.na(u_summer)) {
  ee$plotDates <- gg$dataPeriod
} else if (u_summer == "amsr") {
  par(mfrow = c(3, 3))
  ee$plotDates <- gg$dataPeriod[c(24:32, 34:42)]
} else {
  par(mfrow = c(1, 1))
  ee$plotDates <- u_summer - 1
}

# Plot each summer separately
for (ii in ee$plotDates) {
  ## Set Up Plot Background ----------------------------------------------------
  # x-axis date baseline
  ee$xDates <- seq(as.Date(paste0(ii, "-11-01")), 
                   as.Date(paste0(ii + 1, "-03-15")), 
                   10)                    # Plot a line every how many days?
  
  # Create the blank background plot area
  plot(ee$xDates, y = c(1:length(ee$xDates)), "n", 
       ylim = c(0, 100),
       axes = FALSE,
       ylab = "",
       cex.main = 1.5,
       main = paste(ii, "/", ii + 1))
  
  # Add x-axis with easier to read dates
  axis(side   = 1, at = ee$xDates, 
       labels = format(ee$xDates, "%d-%b"))
  
  # Add y-axis back in
  axis(2, las = 2, cex.axis = 1.4)
  
  # Add custom gridlines
  abline(v   = ee$xDates, 
         h   = seq(0, 100, 20),
         col = paste0(gg$kulaQ[7], "55"))
  
  # Add a y-label - we don't want these everywhere
  if (ii == 2005 | ii == 2015 | ii == 2017) {
    title(ylab = "Melt Extent (%)", cex.lab = 1.7)
  }
  
  # Add a legend
  if (ii == 2007 | ii == 2017) {
    legend("right",
           cex = 1.4,
           legend = c("AMSR", "SSMIS"),
           col = gg$kulaQ[c(2, 1)],
           lwd = c(2, 1))
  }
  
  # Get correct data & plot it -------------------------------------------------
  ee$summerIndex <- ii - 1978
  
  # Reset the colour before the new summer's loop
  kulaK <- 2
  lwdK  <- 3
  
  # Loop through the sensors with data (should only ever be a max of 2)
  for (jj in c("ssmis", "amsrB")) {
    # Get index of the correct melt dataset
    ee$dataIndex <- which(names(dd$meltGrid) == jj)
    
    # Get melt dates
    ee$jjDates   <- filterData(dd$meltGrid[[ee$dataIndex]],
                               "date_c1",
                               "summerIndex_c6",
                               ee$summerIndex)
    
    # Get melt extent data
    ee$jjMelt    <- filterData(dd$meltGrid[[ee$dataIndex]],
                               "mExtent_c7",
                               "summerIndex_c6",
                               ee$summerIndex)
    
    # Add data to the plot
    lines(ee$jjDates, ee$jjMelt, 
          col = gg$kulaQ[kulaK], lwd = lwdK, lty = 1)
    
    # For the colour
    if (kulaK == 2) {
      kulaK = 1
      lwdK = 2
    } else {
      kulaK = 2
      lwdK = 3
    }
  }
}

rm(lwdK, kulaK, ii, jj)
