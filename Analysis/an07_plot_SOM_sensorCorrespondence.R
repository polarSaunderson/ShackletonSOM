## #################################### ##
## an07_plot_SOM_sensorCorrespondence.R ##
## #################################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Compare how often the AMSR and SSMIS sensors agree on a day's SOM 
#             melt pattern (Fig. 6)
#
# Comments: - This script matches the SSMIS data to the nearest SOM pattern, and
#             then plots how well the SOM pattern is seen by both sensors on the 
#             same day
#           ! Make sure that data for AMSR and SSMIS exists:
#             - Run the dt01-dt06 scripts, with u_sensors including "amsre", 
#               "amsr2" & "ssmis"
#           ! Make sure that the AMSR data has been run through script an01
#           ! The coding approach used here only works as it is because SSMIS 
#             never observes melt when AMSR doesn't; this may not always be the 
#             case for other shelves, and those discrepancies would therefore be 
#             missed in this plot
#           - As a by product, we create ssmis_vAmsrSomPatterns.rData (saved to 
#             Data/version/interim/version_som):
#             - That data is used in script an08
#           - This has only been tested with a 3 x 3 som output configuration,
#             so be careful with others - the plot may not align correctly
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

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.
library(kohonen)

# We need a different colour scheme than the default option
ee$kulaT    <- colour("acton", reverse = TRUE)(10)
ee$maxZ     <- 100 # Maximum z value; a percentage, so usually 100 makes sense!

# _____CODE_____ ###############################################################
# Chunk 1: Prepare Data for Comparisons ========================================
# Rename for ease
ee$aVectors <- dd$vectorGrid_amsrB
ee$sVectors <- dd$vectorGrid_ssmis
ee$aGrid    <- dd$meltGridSom_amsrB

# Which dates have AMSR-B observations?
ee$aDays      <- rownames(ee$aVectors) %>% as.integer()
ee$aGridIndex <- which(ee$aGrid$date_c1 %in% ee$aDays)

# Preallocate a grid to store the comparison info in
ee$somCompare <- matrix(NA, 
                        nrow = length(ee$aDays), 
                        ncol = 3) %>%
  `colnames<-`(c("date", "amsrB", "ssmis"))

ee$somCompare[ , 1] <- ee$aDays                                # Add dates
ee$somCompare[ , 2] <- ee$aGrid$mPattern_c11[ee$aGridIndex] # amsr pattern

# Chunk 2: Map SSMIS Melt Data against AMSR SOM Patterns =======================
# Map each melt day to its closest pattern
set.seed(1609) # map needs a constant seed to reproduce
ee$sPatterns  <- kohonen::map(x = dd$somOutput_amsrB, 
                              newdata = ee$sVectors)

# Add date information to help use elsewhere
ee$sPatterns[[5]] <- rownames(ee$sVectors)
names(ee$sPatterns)[5] <- "date"

## Find when both datasets have data -------------------------------------------
# Which dates have SSMIS observations?
ee$sDays      <- rownames(ee$sVectors) %>% as.integer()

# Which of the SSMIS dates are also in the amsr data? Get their row indices
ee$sIndex     <- which(ee$sDays %in% ee$aDays)

# Populate somCompare holder with the SOM patterns assigned to the SSMIS data
ee$somCompare[which(ee$aDays %in% 
                      ee$sDays[ee$sIndex]), 3] <- ee$sPatterns[[1]][ee$sIndex]

# Days outside the SSMIS melt season have no pattern - set any NA as pattern 0
ee$somCompare[is.na(ee$somCompare)] <- 0

# But make sure not to compare when there is no SSMIS data processed at the end!
ee$somCompare <- ee$somCompare[(as.Date(ee$somCompare[, 1], 
                                        "1970-01-01") < "2020-06-01"), ]

# Save the SSMIS assignments for use elsewhere
save(somCompare, envir = ee, 
     file = paste0(ff$interimPath, ff$versionInfo, "_som/", 
                   ff$versionInfo, "_somCompare.rda"))
save(sPatterns, envir = ee, 
     file = paste0(ff$interimPath, ff$versionInfo, "_som/", 
                   ff$versionInfo, "_ssmis_vAmsrSomPatterns.rda"))

# Chunk 3: Comparisons =========================================================
# How many patterns are there overall?
ee$somT <- ee$somCompare[, c(2, 3)] %>% max()

# Preallocate storage for counting pattern matches
ee$somMatches <- matrix(NA, nrow = ee$somT + 1, ncol = ee$somT + 1) %>%
  `colnames<-`(paste0("AMSR", c(0:ee$somT))) %>%
  `rownames<-`(paste0("SSMIS", c(0:ee$somT)))

########### We have prior knowledge (i.e. I checked manually) that   ##########!
########### SSMIS doesn't observe melt if AMSR doesn´t. This code    ##########!
########### relies on this assumption to calculate the matches       ##########! 
########### If this isn't the case, more complex code is necessary   ##########!

# rename for code brevity
ee$tt <- ee$somCompare

# For each AMSR pattern
for (ii in 1:(ee$somT + 1)) {
  # Count how often the corresponding SSMIS pattern occurs
  for (jj in 1:(ee$somT + 1)) {
    ee$somMatches[jj, ii] <- length(ee$tt[, 1][ee$tt[, 2] == ii - 1 & 
                                                 ee$tt[, 3] == jj - 1])
  }
}

# As we have seen that the SSMIS never melts outside the AMSR melt season, so we
# can set the AMSR0-SSMIS0 correspondence to 100% - any non-AMSR melt season is 
# also non-SSMIS melt season.
ee$somMatches[1, 1] <- 1

########### End of the part which only works on our assumption above ##########!
########### End of the part which only works on our assumption above ##########!

## Pattern Correspondence ------------------------------------------------------
# We want the values as a percentage though
# How many times does each pattern occur in AMSR?
ee$amsrTotals <- apply(ee$somMatches, 2, sum)

# How often does the SSMIS pattern occur, as a % of each original AMSR pattern?
ee$sPatterns  <- sweep(ee$somMatches, 2, ee$amsrTotals, "/") %>% 
  `*`(100) %>% round(2)

# Catch NaN (i.e. if pattern never occurs)
ee$sPatterns[is.nan(ee$sPatterns)] <- 0 

# Chunk 4: Prep for Plotting ===================================================
# Add data to a table for easier plotting
ee$amsrCol  <- rep(c(0:ee$somT), each  = ee$somT + 1)
ee$ssmisCol <- rep(c(0:ee$somT), times = ee$somT + 1)
ee$plotCol  <- cbind(ee$amsrCol, ee$ssmisCol,
                     matrix(ee$sPatterns, ncol = 1), # How often does the match occur?
                     c(1:100) * NA)                  # For storing colour value

# Colour depends on % occurrence
for (ii in 1:((ee$somT + 1) * (ee$somT + 1))) {
  if (as.double(ee$plotCol[ii, 3]) != 0) {
    ee$plotCol[ii, 4] <- ee$kulaT[ceiling(as.double(ee$plotCol[ii, 3]) /
                                            (ee$maxZ / length(ee$kulaT)))]
  }
}

# Chunk 5: Plotting ============================================================
dev.new(width = 12,
        height = 9.75,
        unit = "cm",
        noRStudioGD = TRUE)

# Some spacing for the bar
layout(matrix(c(1, 1, 1, 1, 1, 2), ncol = 1))
par(mar = c(7, 6, 3, 3))

# Plot background
plot(ee$plotCol[, 1:2], type = "n",
     ylim = c(-0.25, ee$somT + .25),
     xlim = c(0, ee$somT),
     # axes = FALSE,
     yaxt = 'n',       # remove default numbers
     xaxt = 'n',
     xlab = "",
     ylab = "")

# Add axes
mtext("AMSR Pattern", side = 1, line = 4.5, cex = 1.5)
axis(1, at = c(0:ee$somT), c(0:ee$somT), cex.axis = 2.25, mgp = c(3, 2, 0))

mtext("SSMIS Pattern", side = 2, line = 3, cex = 1.75)
axis(2, at = c(0:ee$somT), c(0:ee$somT), las = 2, cex.axis = 2.25, mgp = c(3, 1, 0))

# Add faint grid lines
abline(h = c(0:ee$somT), lwd = 1, col = "#34343415")
abline(v = c(0:ee$somT), lwd = 1, col = "#34343415")

# Add "bubble" points - the data!!!
points(ee$plotCol[, 1:2], 
       col = ee$plotCol[, 4], 
       pch = 19, 
       cex = 9)

# Add colour scale
addColourBar(col = ee$kulaT, 
             zlim = c(0, ee$maxZ),
             type = "S", horiz = TRUE,
             title = "Correspondence (%)", 
             mar = c(7, 8, 0.5, 5),
             cex.title = 2.25,
             aboveBelow = "aboveMin",
             extraCol = "white",
             cex.labels = 2,
             labelOffset = 4)

# Finished
rm(ii, jj)
printLine()
cat("\n Script an07_plot_SOM_sensorCorrespondence.R complete!")
