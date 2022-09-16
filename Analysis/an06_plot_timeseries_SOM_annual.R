## ################################# ##
## an06_plot_timeseries_SOM_annual.R ##
## ################################# ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Plot relative occurrence of the SOM patterns each summer (Fig. 5)
#
# Comments: - Also creates timeseries of annual SOM pattern occurrence (Fig. S8)
#           - Additionally, calculates interannual correlations with the CMS
#           - Fig. S8 code is predominantly set-up for a 3 x 3 grid, so plotting
#             with other configurations could good awry!
#           ! Make sure you have run script an01 already
#
# Updates:
# 2022/09/16  v1.1  Separated basic stats table into absolute & relative tables
# 2022/08/30        Added code for hatching; amended comments re: Fig. 5 & S8
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# User Options #################################################################
fresh("") # reset everything

# Which shelf are you interested in?
u_shelf   <- "Shackleton"

# What is the dataset version number?
u_version <- "v01"

# Which sensor were the SOM patterns created with? (needs running through an05)
# This does NOT plot SSMIS observations against the amsrB SOM patterns: use ex03
u_sensor  <- c("amsrB")

# What shape is the SOM output? (figures are configured for 3 x 3 but try!)
u_somCol  <- 3
u_somRow  <- 3

# Should the absolute pattern count be shown? Or proportional to season length?
u_abs     <- TRUE  # FALSE = proportional counts; "both" = overlapping plot

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# Chunk 1: Calculate Annual Occurrence =========================================
# Set Up
ee$mGrid       <- get(paste0("meltGridSom_", u_sensor), envir = dd)
ee$patterns    <- paste0("somOutput_", u_sensor) %>% get(envir = dd)
ee$patterns    <- unique(ee$patterns$unit.classif) %>% sort()
ee$summerIndex <- as.integer(rownames(dd$CMS)) - 1979

# Preallocate for annual occurrence
ee$annualSom   <- matrix(NA, 
                         ncol = length(ee$summerIndex),
                         nrow = length(ee$patterns)) %>%
  `colnames<-`(paste0("ms", ee$summerIndex)) %>%
  `rownames<-`(paste0("p", ee$patterns))

# Calculate annual occurrence for given summers
for (ii in ee$summerIndex) {
  # For each pattern
  for (jj in ee$patterns) {
    # Count when index is ii and pattern is jj
    ee$jjCount <- (ee$mGrid$mPattern_c11[ee$mGrid$mSeason_c10 == ii &
                                           ee$mGrid$mPattern_c11 == jj]) %>%
      na.exclude %>%
      length()
    
    # Add to container
    ee$annualSom[which(ee$patterns == jj), 
                 which(ee$summerIndex == ii)] <- ee$jjCount
  }
}
# NA years with no observations
ee$annualSom[, which(apply(ee$annualSom, 2, sum) == 0)] <- NA

# Get total number of melt days each summer
ee$msTotals <- apply(ee$annualSom, 2, na.rm = TRUE, sum)

# Calculate relative occurrence of patterns
ee$propSom <- ee$annualSom * 0
for (ii in 1:(u_somCol * u_somRow)) {
  ee$propSom[ii, ] <- (ee$annualSom[ii, ] / ee$msTotals * 100 )
}

# Chunk 2: Plot annual pattern time series =====================================
# New window
dev.new(width = 12,
        height = 10,
        unit = "cm",
        noRStudioGD = TRUE)

# Display for all of the patterns separately
par(mfrow = c(u_somRow, u_somCol),
    mar = c(6, 6, 2, 2))

# If for paper
if (u_shelf == "Shackleton") {
  # Different y-axes for 1 & 9
  ee$maxYs <- rep(35, 3 * 3)
  ee$maxYs[c(1, 9)] <- 70
  ee$titleY <- rep(32.5, 3 * 3)
  ee$titleY[c(1, 9)] <- 65
} else {
  # Set y-limits and title location for each plot
  ee$maxYs  <- rep(max(ee$annualSom, na.rm = TRUE), u_somCol * u_somRow)
  ee$titleY <- rep(max(ee$annualSom, na.rm = TRUE) * 0.9, 
                   u_somCol * u_somRow)
}

# Plot 
for (ii in ee$patterns) {
  # Plot in same order as the som output 
  jj <- matchKohoPlotOrder(ii, u_somRow, u_somCol)
  
  # Get either absolute counts or proportional (relative) data
  if (isFALSE(u_abs)) {
    ee$iiData <- ee$propSom[jj, ]
  } else {
    ee$iiData <- ee$annualSom[jj, ]
  }
  
  # y label
  if (ii == u_somCol + 1) {
    if (isTRUE(u_abs)) {
      ee$yLabels <- "# of Days"
    } else if (isFALSE(u_abs)) {
      ee$yLabels <- "Occurrence (% of melt season)"
    } else  {
      ee$yLabels <- "Occurrence % || Total Count"
    }
  } else {
    ee$yLabels <- ""
  }
  
  # Set up the margins differently depending on subplot position
  ee$plotMar <- switch(ii,
                       c(1, 4, 3, 0), # top left
                       c(1, 4, 3, 0), # top middle
                       c(1, 4, 3, 0), # top right
                       c(2, 4, 2, 0), # middle left
                       c(2, 4, 2, 0), # middle
                       c(2, 4, 2, 0), # middle right
                       c(3, 4, 1, 0), # bottom left
                       c(3, 4, 1, 0), # bottom middle
                       c(3, 4, 1, 0)) # bottom right
  par(mar = ee$plotMar)
  
  # Create background for the plot
  plot(ee$summerIndex + 1979, type = "n",
       ylim = c(0, ee$maxYs[jj]), 
       xlim = c(2003, 2021),
       axes = FALSE,              # we need more control on the axes (below)
       ylab = "",                 
       xlab = "",
       cex.lab = 1.8,
       mgp = c(2.6, 1, 0))
  grid(lty = 1, col = "#12121210")
  
  # Add y-axes back in, with rotated text
  mtext(side = 2, 
        ee$yLabels,             # only shows a label on the second row
        cex = 1.3, line = 2.5) 
  axis(2, las = 2, cex.axis = 1.25)
  
  # We only want x-axes labelling on the bottom row
  if (ii %in% c(u_somCol * u_somRow - c(1:u_somCol)  + 1)) {
    axis(1, cex.axis = 1.2)
  } else {
    axis(1, labels = FALSE)
  }
  
  # Plot the data
  lines(ee$summerIndex + 1979,
        ee$iiData,
        pch = 4, lwd = 4, col = "#54BDCA")
  points(ee$summerIndex + 1979, 
         ee$iiData,
         pch = 3, col = gg$kulaQ[1], cex = 0.75)
  
  if (u_abs == "both") {
    lines(ee$summerIndex + 1979,
          ee$annualSom[jj, ] / ee$msTotals * 100, 
          lwd = 3, col = gg$kulaQ[2])
    if (ii == 1) {
      legend("topright", c("absolute", "relative"), 
             col = c("#54BDCA", gg$kulaQ[2]),
             lwd = 3)
    }
  }
  
  # Add plot "title"
  text(jj, x = 2012, 
       y = ee$titleY[jj], 
       cex = 2.25, col = "black")
}  

# Chunk 3: Correlate with annual CMS ===========================================
# Get correct data (i.e. which sensor is being plotted?)
ee$cms <- dd$CMS[, which(dd$inputSensors == u_sensor)]

# Print to console
cat(" Correlation with the CMS \n\n")

# Calculate for each pattern
for (ii in 1:(u_somRow * u_somCol)) {
  # Correlate
  ee$kaw    <- cor.test(ee$cms, ee$annualSom[ii, ])
  ee$kawEst <- ee$kaw$estimate %>% round(2)
  ee$kawP   <- ee$kaw$p.value %>% round(2)
  
  # Print to console
  cat("Pattern", ii, ":", 
      sprintf("%+.2f", ee$kawEst), 
      "@ p = ", ee$kawP,  "\n")
}

# Chunk 4: Summer pattern proportion barplot ===================================
# New plot window
dev.new(width  = 12,
        height = 9.75,
        unit = "cm",
        noRStudioGD = TRUE)

# Set up the layout
layout(matrix(c(0, 1, 1, 1, 
                1, 1, 1, 1,
                1, 1, 1, 1, 
                1, 1, 2, 1), byrow = TRUE, nrow = 1))
par(mar = c(6, 1.5, 6, 0))

# We need specific colours here (they match those in Fig. 3)
gg$kulaT <- colour("light")(9)
gg$kulaT <- gg$kulaT[c(5, 8, 3, 6, 9, 7, 1, 4, 2)] # rearrange order

ee$incYears <- which(colnames(ee$propSom) %in% paste0("ms", c(24:42)))

# Add pointless columns to help align gg$kulaBar and axes
ee$plotData <- cbind(ee$propSom[, ee$incYears],
                     rep(NA, u_somCol * u_somRow),
                     rep(NA, u_somCol * u_somRow))

# Create the plot
ee$xAxisPlot <- barplot(ee$plotData,
                        col  = gg$kulaT,
                        ylim = c(0, 100),
                        ylab = "",
                        axisnames = FALSE,
                        axes = FALSE)

# Add hatching
gg$hatchDensity <- c(0, 0, 10, 0, 0,  20, 10, 0, 0)
gg$hatchAngles  <- c(0, 0, 45, 0, 0, -45, 45, 0, 0)
gg$kulaH        <- rep("#767676", 9)
gg$kulaH[7]     <- "#ffffff"

barplot(ee$plotData,
        add       = TRUE,
        col       = gg$kulaH,
        angle     = gg$hatchAngles,
        density   = gg$hatchDensity,
        ylim      = c(0, 100),
        ylab      = "",
        axisnames = FALSE,
        axes      = FALSE)

# Hide extra column bottoms - very hacky!
# Using density in above barplotting, shows the x-axis marks for the extra cols
# we used in ee$somXdata to give space for the colour scalebar.
par(xpd = TRUE)
polygon(x = c(0, 3, 3, 0, 0) + 23,
        y = c(0, 0, 2, 2, 0) - 1, 
        col = "white", border = NA)
par(xpd = FALSE)

## Labels ----------------------------------------------------------------------
# Add y-axis label text
mtext("Relative Occurrence (%)", 
      side = 2, 
      line = 3.25, 
      cex = 1.8)

# Add y-axis tick values
axis(side = 2,
     at = seq(0,
              100,
              length.out = 11),
     las = 2,
     line = -1,
     mgp = c(3, 1, 0),
     cex.axis = 2.25)

# Draw axis values outside the plot area frame
par(xpd = TRUE)

# Prep x-axis labels
ee$xLabs <-  substring(colnames(ee$propSom[, ee$incYears]), 3,4) %>% 
  as.integer() + 1979

# It will try to include our extra blank colums, which we don't want - remove!
ee$xLength   <- length(ee$xAxisPlot)
ee$xAxisPlot <- ee$xAxisPlot[-c(ee$xLength, ee$xLength - 1)]

# Add the labels
text(x = ee$xAxisPlot,
     y = -5,
     cex = 1.65,
     labels = ee$xLabs,
     srt = 45)

# Add little ticks in to help viewer align years and bars
axis(1, at = ee$xAxisPlot, labels = FALSE)

# Add a label to the upcoming colour bar (default in addColourBar fails)
# Add here, so that works within (i.e. beyond) the bar plot area
text(x = max(ee$xAxisPlot) + 3.55, 
     y = 50, 
     "SOM Pattern", 
     cex = 2.5, srt = 270)

# Reset plotting within area
par(xpd = FALSE)

# Add a colour scale bar
addColourBar(gg$kulaT[c(1:(u_somRow * u_somCol))],
             zlim = c(1, u_somRow * u_somCol),
             type = "Q",
             hatchColour = gg$kulaH,
             hatchDensity = gg$hatchDensity,
             hatchAngle = gg$hatchAngles,
             allLabels = "yes",
             mar = c(8, 3, 8, 0), 
             cex.labels = 2.25)

# Chunk 5: Calculate basic stats ===============================================
# Preallocate to store
ee$stats_abs <- matrix(NA, ncol = 6, 
                        nrow = u_somRow * u_somCol) %>% 
  `colnames<-`(c("Mean", "Median", "MAD",
                 "SD", "CV", "Variability"))

ee$stats_rel <- ee$stats_abs

# Select only the years with melt data for ease
ee$incYears <- ee$incYears[-(which(ee$incYears == 33))]
ee$pAbs <- ee$annualSom[, ee$incYears]
ee$pRel <- ee$propSom[, ee$incYears]

# Mean Occurrence
for (ii in 1:(u_somRow * u_somCol)) {
  ee$stats_abs[ii, 1]  <- mean(ee$pAbs[ii, ], na.rm = TRUE)
  ee$stats_rel[ii, 1]  <- mean(ee$pRel[ii, ], na.rm = TRUE)
}

# Median Occurrence
for (ii in 1:(u_somRow * u_somCol)) {
  ee$stats_abs[ii, 2]  <- median(ee$pAbs[ii, ], na.rm = TRUE)
  ee$stats_rel[ii, 2]  <- median(ee$pRel[ii, ], na.rm = TRUE)
}

# MAD of Occurrence
for (ii in 1:(u_somRow * u_somCol)) {
  ee$stats_abs[ii, 3]  <- mad(ee$pAbs[ii, ], constant = 1, na.rm = TRUE)
  ee$stats_rel[ii, 3]  <- mad(ee$pRel[ii, ], constant = 1, na.rm = TRUE)
}

# SD of Occurrence
for (ii in 1:(u_somRow * u_somCol)) {
  ee$stats_abs[ii, 4]  <- sd(ee$pAbs[ii, ], na.rm = TRUE)
  ee$stats_rel[ii, 4]  <- sd(ee$pRel[ii, ], na.rm = TRUE)
}

# CV values (MAD / median)
for (ii in 1:(u_somRow * u_somCol)) {
  ee$stats_abs[ii, 5]  <- ee$stats_abs[ii, 3] / ee$stats_abs[ii, 2]
  ee$stats_rel[ii, 5]  <- ee$stats_rel[ii, 3] / ee$stats_rel[ii, 2]
}

# Variability values (SD / mean)
for (ii in 1:(u_somRow * u_somCol)) {
  ee$stats_abs[ii, 6]  <- ee$stats_abs[ii, 4] / ee$stats_abs[ii, 1]
  ee$stats_rel[ii, 6]  <- ee$stats_rel[ii, 4] / ee$stats_rel[ii, 1]
}

# Print tables
printLine()
cat("Statistics for Absolute Annual Pattern Counts\n\n")
print(ee$stats_abs %>% round(2))

printLine()
cat("Statistics for Relative Annual Pattern Counts\n\n")
print(ee$stats_rel %>% round(2))

# Finished
rm(ii, jj)
printLine()
cat("\n Script an06_plot_timeseries_SOM_annual.R complete!\n")
