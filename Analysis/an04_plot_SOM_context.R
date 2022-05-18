## ####################### ##
## an04_plot_SOM_context.R ##
## ####################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Plots when the SOM patterns occur during the melt season (Fig. 3)
#
# Comments: - This script can plot when the patterns occur against the context
#             or calendar dates
#             - Context refers to how far through a melt season an observation 
#               occurs, given as a percentage of the full season length in days
#           - There are a few options for u_xAxis: 
#             - Plot all patterns at once, as in the manuscript, or
#             - Plot each pattern separately ("eachCon" or "eachDate")
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

# Which sensor were the SOM patterns created with? (needs running through an05)
u_sensor  <- c("amsrB")

# Plot against context, or absolute dates? All patterns, or eachPattern plots?
u_xAxis   <- "context"   # "context", "dates" "eachCon" "eachDate"

# What shape is the SOM output?
u_somCol  <- 3
u_somRow  <- 3

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# Chunk 1: Prepare Data ========================================================
# Set Up
ee$mGrid       <- get(paste0("meltGridSom_", u_sensor), envir = dd)
ee$patterns    <- paste0("somOutput_", u_sensor) %>% get(envir = dd)
ee$patterns    <- unique(ee$patterns$unit.classif) %>% sort()
ee$summerIndex <- as.integer(rownames(dd$CMS)) - 1979

# How should the data be plotted? Overall combined plot, or per pattern plots?
if (u_xAxis == "context") {
  ee$bins  <- seq(0, 110, 5)
} else if (u_xAxis == "dates") {
  ee$bins  <- seq(0, 130, 5)
} else if (u_xAxis == "eachCon" | u_xAxis == "eachDate" ) {
  # Or if individual plots for each pattern
  par(mfrow = c(u_somRow, u_somCol))
  
  # rename for ease
  ee$tt <- ee$mGrid
  
  # loop through patterns
  for (ii in ee$patterns) {
    
    # plot in same order
    jj <- matchKohoPlotOrder(ii, somRows = u_somRow, somCols = u_somCol)
    
    # only pattern jj days
    ee$tjj <- ee$tt[ee$tt$mPattern_c11 == jj, ]
    ee$tjj <- ee$tjj[!is.na(ee$tjj$mPattern_c11), ]
    
    # get context / date & plot as a histogram
    if (u_xAxis == "eachCon") { 
      ee$con <- ee$tjj$mContext_c9
      hist(ee$con, breaks = 20, xlim = c(0, 100), main = jj)
    } else if (u_xAxis == "eachDate") {
      ee$DoMS <- ee$tjj$DoMS_c5
      hist(ee$DoMS, breaks = 20, xlim = c(0, 120), main = jj)
    }
  }
  # Don't continue!
  rm(ii, jj)
  stop("Script complete - 
       the remaining code makes no make sense for this axis option")
}

# Colours are important here for distinguishing patterns 
ee$kulaT <- colour("light")(9)
ee$kulaT <- ee$kulaT[c(5, 8, 3, 6, 9, 7, 1, 4, 2)] # rearrange order

# Preallocate for storing context occurrence
ee$somXdata <- matrix(NA, 
                      ncol = length(ee$bins) - 1,
                      nrow = length(ee$patterns)) %>%
  `colnames<-`(ee$bins[-1]) %>%
  `rownames<-`(paste0("p", ee$patterns))

# Calculate occurrence in each bin
for (ii in 1:(length(ee$bins) - 1)) {
  # What are the bins bounds?
  ee$lowerBin <- ee$bins[ii]
  ee$upperBin <- ee$bins[ii + 1]
  
  # Which patterns fall within these bin bounds?
  if (u_xAxis == "context") {
    ee$binPatterns <- ee$mGrid$mPattern_c11[ee$mGrid$mContext_c9 > ee$lowerBin &
                                              ee$mGrid$mContext_c9 <= ee$upperBin]
  } else if (u_xAxis == "dates") {
    ee$binPatterns <- ee$mGrid$mPattern_c11[ee$mGrid$DoMS_c5 > ee$lowerBin &
                                              ee$mGrid$DoMS_c5 <= ee$upperBin]
  }
  
  # Remove NA's
  ee$binPatterns <- ee$binPatterns[!is.na(ee$binPatterns)]
  ee$binPatterns <- ee$binPatterns[ee$binPatterns != 0]
  
  # Temporary in-loop storage
  ee$patternCount <- ee$patterns * NA
  
  # How often does each pattern occur in this bin?
  for (jj in ee$patterns) {
    ee$patternCount[jj] <- length(ee$binPatterns[ee$binPatterns == jj])
  }
  
  # We want each pattern's occurrence as a percentage of the total in this bin
  ee$binPercent <- ee$patternCount / sum(ee$patternCount) * 100
  
  # Store
  ee$somXdata[, ii] <- ee$binPercent %>% round(4)
}

# Chunk 2: Display =============================================================
dev.new(width  = 12, # 70
        height = 9.75, # 30
        unit = "cm",
        noRStudioGD = TRUE)

# Set up the layout
layout(matrix(c(0, 1, 1, 1, 
                1, 1, 1, 1,
                1, 1, 1, 1, 
                1, 1, 2, 1), byrow = TRUE, nrow = 1))
par(mar = c(6, 1.5, 6, 0))

# Create the plot
ee$xAxisPlot <- barplot(ee$somXdata,
                        col  = ee$kulaT,
                        ylim = c(0, 100),
                        ylab = "",
                        axisnames = FALSE,
                        axes = FALSE)

# Add y-axis label text
mtext("Overall Occurrence (%)", 
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

## x-axis ----
if (u_xAxis == "context") {
  # Add x-axis label
  mtext("Melt Season Context (%)", 1, line = 4.25, cex = 1.65)
}

# Only label every second bar
ee$labelDoMS <- ee$bins[seq(length(ee$bins)) %% 2 == 1]
ee$xAxisPlot <- ee$xAxisPlot[seq(length(ee$xAxisPlot)) %% 2 == 1]

# The last one tries to loop around on the plot & writes over the first!
ee$labelDoMS <- ee$labelDoMS[-length(ee$labelDoMS)] 

# Draw axis values outside the plot area frame
par(xpd = TRUE)

# x-Axis depends on plotting against context or dates
if (u_xAxis == "dates") {
  text(x = ee$xAxisPlot - ((ee$xAxisPlot[2] - ee$xAxisPlot[1]) / 2) + 0.05,
       y = -5,
       cex = 1.65,
       labels = format(as.Date("2002-11-01") + ee$labelDoMS, "%d-%b"),
       srt = 45)
} else if (u_xAxis == "context") {
  text(x = ee$xAxisPlot - ((ee$xAxisPlot[2] - ee$xAxisPlot[1]) / 4) + 0,
       y = -3,
       cex = 2.25,
       labels = ee$labelDoMS,
       srt = 0)
}

# Add little ticks in to help align!
axis(1, 
     at = ee$xAxisPlot - ((ee$xAxisPlot[2] - ee$xAxisPlot[1]) / 4), 
     labels = FALSE)

# Add a label to the upcoming colour bar (default in addColourBar fails)
# Add here, so that works within (i.e. beyond) the bar plot area
text(x = max(ee$xAxisPlot) + 2.3, 
     y = 50, 
     "SOM Pattern", 
     cex = 2.5, srt = 270)

# Reset plotting within area
par(xpd = FALSE)

# Add a colour scale bar
addColourBar(ee$kulaT[c(1:(u_somCol * u_somRow))],
             zlim = c(1, u_somCol * u_somRow),
             type = "Q",
             # title = "SOM Pattern",
             allLabels = "yes",
             mar = c(8, 2.7, 8, 0.3), 
             cex.labels = 2.25)

# Finished
rm(ii, jj)
printLine()
cat("\n Script an04_plot_SOM_context.R complete!")
