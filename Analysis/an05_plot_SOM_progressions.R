## ############################ ##
## an05_plot_SOM_progressions.R ##
## ############################ ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Plots the frequency of day-to-day progressions between the SOM
#             patterns (Fig. 4)
#
# Comments: - Plots how often pattern X on day N changes to pattern Y on day N+1
#           ! Make sure you have run script an01 already
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# User Options #################################################################
# fresh("") # reset everything

# Which shelf are you interested in?
u_shelf   <- "Shackleton"

# What is the dataset version number?
u_version <- "v01"

# Which sensor were the SOM patterns created with? (needs running through an05)
u_sensor  <- c("amsrB")

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# Chunk 1: Calculate Annual Occurrence =========================================
# Set Up
ee$mGrid       <- get(paste0("meltGridSom_", u_sensor), envir = dd)
ee$patterns    <- paste0("somOutput_", u_sensor) %>% get(envir = dd)
ee$patterns    <- unique(ee$patterns$unit.classif) %>% sort()
ee$summerIndex <- as.integer(rownames(dd$CMS)) - 1979

# We don't want our default sequential colours here
ee$kulaS       <- colour("lapaz", reverse = TRUE)(10)

# Set NA as 0, allow us to check and ignore non-melt days easier
ee$mGrid$mPattern_c11[is.na(ee$mGrid$mPattern_c11)] <- 0

# Preallocate: column = pattern on day X, row = pattern on day X + 1
ee$somProgress <- matrix(0, 
                         nrow = length(ee$patterns),
                         ncol = length(ee$patterns)) %>%
  `colnames<-`(paste0(ee$patterns, "_is1st")) %>%
  `rownames<-`(paste0(ee$patterns, "_is2nd"))

# Chunk 2: Calculate relative frequency ========================================
# Firstly, calculate absolute count of progressions for each starting pattern
for (ii in ee$patterns) {
  # for each subsequent pattern
  for (jj in ee$patterns) {
    # Which progression are we looking for?
    ee$prog <- c(ii, jj)
    
    # Which rows (i.e. dates) can a progression start on? i.e. exclude last one
    ee$kk_starts <- seq.int(length = dim(ee$mGrid)[1] - 1)
    
    # Whittle down until only the matching pattern remains in our list
    for (kk in seq.int(length = length(ee$prog))) {
      ee$kk_starts <- ee$kk_starts[ee$prog[kk] == ee$mGrid$mPattern_c11[ee$kk_starts + kk - 1]]
    }
    
    # Store count
    ee$somProgress[jj, ii] <- length(na.exclude(ee$kk_starts))
  }
}
# But we want them to be relative changes
# i.e. if a day is pattern X, how often does it change to pattern y
ee$progFreq <- ee$somProgress

for (ii in ee$patterns) {
  ee$iiChanges      <- ee$somProgress[, ii] # all changes beginning on pattern ii
  ee$iiSum          <- sum(ee$iiChanges)
  ee$progFreq[, ii] <- (ee$iiChanges / ee$iiSum * 100) %>% 
    round(3) %>% unname()
}

# Chunk 3: Matrix-Like Plot ====================================================
# Rotate the data matrix so that it aligns how you expect
ee$progFreq   <- apply(ee$progFreq, 2, rev)
ee$plotMatrix <- rotate4Plot(ee$progFreq)

# New plot window
dev.new(width  = 12,
        height = 10,
        unit   = "cm",
        noRStudioGD = TRUE)

# for scaling figure axes
ee$cc <- 2.25

# Set up the layout
layout(matrix(c(0, 1, 1, 1, 
                1, 1, 1, 1,
                1, 1, 1, 1, 
                1, 1, 2, 2), byrow = TRUE, nrow = 1))
par(mar = c(6, 4, 6, 1))

# Plot data
image(ee$plotMatrix, 
      axes = FALSE,      # we need to add our own
      zlim = c(0, 100), 
      col = ee$kulaS)

# Blank out progression that never occur
ee$plotMatrix[ee$plotMatrix != 0] <- NA
image(ee$plotMatrix, add = TRUE, col = "white")

# Add Axes & Grid Line ---------------------------------------------------------
# Nudge the axes lines to undo the automatic, confusing R plotting behaviour
ee$nudgeV <- (1 / (length(ee$patterns) - 1) / 2) 
ee$nudgeH <- (1 / (length(ee$patterns) - 1) / 2) 

# Add vertical and horizontal grid lines
abline(v = seq(0 - ee$nudgeH,
               1 + ee$nudgeH,
               length.out = length(ee$patterns) + 1))
abline(h = seq(0 - ee$nudgeV,
               1 + ee$nudgeV,
               length.out = length(ee$patterns) + 1))

# Add x-axis label & ticks
mtext("Pattern X on Day N", side = 1, line = 4.5, cex = 1.65)
axis(side = 1, 
     at = seq(0 - ee$nudgeH,
              1 + ee$nudgeH,
              length.out = length(ee$patterns) + 1) + ee$nudgeH, 
     labels = c(1:(length(ee$patterns) + 1)),
     mgp = c(3, 1.5, 0),
     cex.axis = ee$cc)

# Add y-axis label & ticks
mtext("Pattern Y on Day N + 1", side = 2, line = 4.5, cex = 1.75)
axis(side = 2, 
     at = seq(0 - ee$nudgeV,
              1 + ee$nudgeV,
              length.out = length(ee$patterns) + 1) + ee$nudgeV, 
     cex.axis = ee$cc,
     las = 2,
     labels = c(1:(length(ee$patterns) + 1)))

# Add colourbar
addColourBar(col = ee$kulaS, zlim = c(0, 100), type = "S",
             title = "Progression Frequency (%)",
             mar = c(11.5, 0.2, 11.5, 6),
             cex.title = 2.5,
             cex.labels = 2,
             allLabels = "odds", 
             extraCol = "#FFFFFF", # fake colour to allow aboveBelow
             aboveBelow = "aboveMin",
             labelOffset = 4)

# Finished
rm(ii, jj, kk)
printLine()
cat("\n Script an05_plot_SOM_progressions.R complete!")
