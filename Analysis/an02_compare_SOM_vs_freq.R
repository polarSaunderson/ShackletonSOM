## ########################## ##
## an02_compare_SOM_vs_freq.R ##
## ########################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    What do the SOM values represent in terms of melt frequency?
#
# Comments: - Compares the occurrence of melt in a pixel for a given pattern 
#             against the SOM value for that pattern.
#           - The values for the occurrence and the SOM value are near-identical
#             (r > 0.99; rmse = 0.022) 
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

# What shape is the SOM output?
u_somCol  <- 3
u_somRow  <- 3

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# Rename for ease
ee$vectorData  <- dd$vectorGrid_amsrB
ee$somData     <- dd$somOutput_amsrB
ee$patternData <- ee$somData$unit.classif

# Set axis limits for plotting differences
ee$dMax <- 0.1
ee$dMin <- ee$dMax * -1

# Chunk 1: Examine SOM values & melt frequency =================================
## Show all the frequency plots ------------------------------------------------
par(mfrow = c(u_somCol, u_somRow), 
    mar = c(3, 3, 3, 3))

# For each pattern
for (ii in 1:(u_somRow * u_somCol)) {
  # Match koho order
  jj <- matchKohoPlotOrder(ii, u_somRow, u_somCol)
  
  # On which days does the current pattern occur?
  ee$patternDays <- ee$vectorData[ee$patternData == jj, ]
  
  # Mean melt occurrence of each pixel for pattern ii
  ee$meanOccur <- apply(ee$patternDays, 2, 
                        na.rm = TRUE, mean) # can use mean as is only binary
  
  # Prep to plot average occurrence
  ee$avOccur <- pixelValues2Raster(pixelData = ee$meanOccur,
                                   commonMask = dd$commonMask)
  
  # Add a grey background plot area
  plotShelf(x    = dd$commonMask,
            col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
            main = paste0("", ii, " Frequency"),
            axes = FALSE,
            shelfOutline = NA)
  
  # Plot the data!
  plotShelf(ee$avOccur, zlim = c(0, 1),
            col = gg$kulaS,
            add = TRUE,
            axes = FALSE)
  # Keep track of plots
  annotateShelfPlot(dd$commonMask, pos = "topleft", main = jj, cex = 2)
}

## Examine Each Pattern Individually -------------------------------------------
# Pre-allocate to store in-loop values for after-loop overall comparison
ee$allSoms <- NA
ee$allAverages <- NA

# For each pattern
for (ii in 1:(u_somRow * u_somCol)) {
  # Fresh layout
  par(mfrow = c(2, 3), mar = c(5, 5, 5, 5))
  
  # days for the current pattern
  ee$patternDays <- ee$vectorData[ee$patternData == ii, ]
  
  # Mean melt occurrence of each pixel for pattern ii
  ee$meanOccur <- apply(ee$patternDays, 2, 
                        na.rm = TRUE, mean) # can use mean as is only binary 
  
  # Plot average occurrence
  ee$avOccur <- pixelValues2Raster(pixelData  = ee$meanOccur,
                                   commonMask = dd$commonMask)
  
  # Plot SOM values
  ee$somValues <- pixelValues2Raster(ee$somData$codes[[1]][ii, ],
                                     commonMask = dd$commonMask)
  plotShelf(ee$somValues, zlim = c(0, 1), col = gg$kulaS, 
            main = paste0("p", ii, "SOM"), axes = FALSE)  
  
  # Add to storage for use ee$allSomside loop
  ee$allSoms <- c(ee$allSoms, as.vector(ee$somValues))
  ee$allAverages <- c(ee$allAverages, as.vector(ee$avOccur))
  
  # Plot Difference
  ee$somDiffAv <- ee$somValues - ee$avOccur
  plotShelf(ee$somDiffAv, zlim = c(ee$dMin, ee$dMax), col = gg$kulaD, 
            main = paste0("p", ii, "Diff"),
            axes = FALSE) 
  
  # Scatter plot
  plot(ee$somValues, ee$avOccur, 
       main = paste0(cellStats(ee$somDiffAv, "min") %>% round(3), " : : ",
                     cellStats(ee$somDiffAv, "max") %>% round(3)),
       xlab = "SOM", ylab = "Freq",
       xlim = c(0, 1),
       ylim = c(0, 1))
  
  # Plot histogram
  hist(ee$somDiffAv, xlim = c(ee$dMin, ee$dMax), main = "som - freq")
  
  # Print Correlation  
  ee$kaw <- cor.test(as.vector(ee$somValues), as.vector(ee$avOccur))
  cat("\n pattern", ii, "r =", ee$kaw$estimate %>% round(3),
      "@ p =", ee$kaw$p.value %>% round(3))
  
  # rmse
  ee$somRmseAv <- as.matrix(ee$somDiffAv) %>% 
    raise_to_power(2) %>% 
    mean(na.rm = TRUE) %>% 
    sqrt() 
  cat(" ... rmse =", ee$somRmseAv %>% 
        round(3))
  
  # Colour bar
  addColourBar(gg$kulaD, zlim = c(ee$dMin, ee$dMax), allLabels = "minMax")
}

## Comparison across all patterns ----------------------------------------------
# Simple Linear Comparisons
ee$kaw   <- cor.test(ee$allAverages, ee$allSoms)
ee$rmse  <- (ee$allAverages - ee$allSoms) %>% 
  raise_to_power(2) %>% 
  mean(na.rm = TRUE) %>% 
  sqrt()
ee$linearModel <- lm(ee$allAverages ~ ee$allSoms)

# Plot area
par(mfrow = c(1, 1),
    mar = c(5, 5, 5, 5))

# BoxPlot
boxplot(ee$allSoms - ee$allAverages,
        las = 2,
        ylim = c(-0.15, 0.15), 
        ylab = "SOM - Average Occurrence")
grid()

# Replot over grid - poor practice!
boxplot(ee$allSoms - ee$allAverages,
        ylim = c(-0.15, 0.15),
        col = gg$kulaQ[1],
        axes = FALSE,
        add = TRUE)

# Add label with stats info
text(bquote(r^2 == .(ee$kaw$estimate^2 %>% round(3))), 
     x =  1.40, y = -0.105, adj = 1)
text(bquote("n " == .(length(ee$allSoms[!is.na(ee$allSoms)]))), 
     x =  1.40, y = -0.12, adj = 1)
text(bquote("rmse" == .(round(ee$rmse, 3))),           
     x =  1.40, y = -0.135, adj = 1)

# Scatter Plot
plot(ee$allSoms, ee$allAverages, "n",
     ylab = "SOM Value",
     xlab = "Average Melt Occurrence within Pattern",
     ylim = c(0, 1),
     xlim = c(0, 1))
grid()

# Add fit line and then the points
abline(ee$linearModel, lwd = 4, col = paste0(gg$kulaQ[2], "55"))
points(ee$allSoms, ee$allAverages, col = gg$kulaQ[1], pch = 19, cex = 0.5)

# Add label with stats info
text(bquote(atop(r^2 == .(ee$kaw$estimate^2 %>% round(3)), 
                 "rmse" == .(round(ee$rmse, 3)))), 
     x = 0.9, 
     y = 0.21)

# Chunk 2: Create boxplot & example figures (for supplement) ===================
## Create Example Figures ------------------------------------------------------
# Create new figure window with defined size
dev.new(width = 8.3,
        height = 8,
        unit = "cm",
        noRStudioGD = TRUE)

# Add a background colour to help differentiate plot groupings in the supplement
par(bg = "#FFFFFF")

# horizontal
layout(matrix(c(1, 3, 5,
                1, 3, 5,
                1, 3, 5,
                1, 3, 5,
                7, 7, 7,
                2, 4, 6,
                2, 4, 6,
                2, 4, 6,
                2, 4, 6), byrow = TRUE, ncol = 3))

# Re-pre-allocate
ee$allSoms <- NA
ee$allAverages <- NA

# For each pattern
for (ii in c(1:9)) {
  # days for the current pattern
  ee$patternDays <- ee$vectorData[ee$patternData == ii, ]
  
  # Calculate mean melt occurrence of each pixel for pattern ii
  ee$meanOccur <- apply(ee$patternDays, 2, 
                        na.rm = TRUE, mean) # can use mean as is only binary 
  
  # Arrange as a raster of average pixel melt occurrence
  ee$avOccur <- pixelValues2Raster(pixelData = ee$meanOccur,
                                   commonMask = dd$commonMask)
  
  # Get SOM values
  ee$somValues <- pixelValues2Raster(ee$somData$codes[[1]][ii, ],
                                     commonMask = dd$commonMask)
  
  # Calculate the difference
  ee$somDiffAv <- ee$somValues - ee$avOccur
  
  # Add values to storage for use ee$allSomside loop
  ee$allSoms <- c(ee$allSoms, as.vector(ee$somValues))
  ee$allAverages <- c(ee$allAverages, as.vector(ee$avOccur))
  
  # Plot a selection as examples
  if (ii %in% c(2, 5, 9)) {  
    
    # Add SOM to allow comparison
    par(mar = c(2, 1, 1.5, 3))
    plotShelf(x = dd$commonMask,
              # main = "som",
              col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
              axes = FALSE,
              shelfOutline = NA)
    plotShelf(ee$somValues, zlim = c(0, 1),
              col = gg$kulaS,
              add = TRUE,
              axes = FALSE)
    annotateShelfPlot(dd$commonMask, pos = "topleft", main = ii, cex = 2.5)
    addPlotScale(dd$commonMask, 25)
    
    
    # Add average occurrence within the pattern
    plotShelf(x = dd$commonMask,
              # main = "average",
              col  = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
              axes = FALSE,
              shelfOutline = NA)
    plotShelf(ee$avOccur, zlim = c(0, 1), 
              col = gg$kulaS, 
              add = TRUE,
              axes = FALSE)  
    
    # Prettify the shelf plot
    annotateShelfPlot(dd$commonMask, pos = "topleft", main = ii, cex = 2.5)
    addPlotScale(dd$commonMask, 25)
  }
} 
rm(ii, jj)

# Add a colour bar
addColourBar(gg$kulaS, c(0, 1), 
             horiz = TRUE, 
             # title = "Melt Frequency",
             labelOffset = 1,
             cex.title = 2,
             allLabels = "minMax2",
             mar = c(2.5, 7, 2, 7))

# Add labels above and below the colour bar
par(xpd = TRUE)
text(x = 0.5, y = 2.25, "SOM Value", cex = 2)
text(x = 0.5, y = -2.5, "Melt Frequency", cex = 2)
text("(b)",
     x =  -0.13, y = -0.05, adj = 1, cex = 2.75)
par(xpd = FALSE)

## Create Boxplot Figure -------------------------------------------------------
# Simple Linear Comparisons
ee$kaw   <- cor.test(ee$allAverages, ee$allSoms)
ee$rmse  <- (ee$allAverages - ee$allSoms) %>% 
  raise_to_power(2) %>%
  mean(na.rm = TRUE) %>% 
  sqrt()
ee$linearModel <- lm(ee$allAverages ~ ee$allSoms)

# Create new figure window with defined size
dev.new(width = 8.3,
        height = 8,
        unit = "cm",
        noRStudioGD = TRUE)

# Plot set-up
par(mfrow = c(1, 1),
    mar = c(1, 9, 1, 3))

# Plot boxplot
boxplot(ee$allSoms - ee$allAverages,
        las = 2,
        cex.axis = 1.75,
        ylim = c(-0.15, 0.15))
title(ylab = "SOM Value - Average Occurrence", 
      mgp = c(5.5, 1, 0), cex.lab = 2)
grid()

# Add again to colour over the grid - very poor practice!
boxplot(ee$allSoms - ee$allAverages,
        ylim = c(-0.15, 0.15),
        col = paste0(gg$kulaQ[1], "44"),
        axes = FALSE,
        add = TRUE)

# Add label with stats info
text(bquote(r^2 == .(ee$kaw$estimate^2 %>% round(3))), 
     x =  1.40, y = -0.10, adj = 1, cex = 1.75)
text(bquote("n " == .(length(ee$allSoms[!is.na(ee$allSoms)]))), 
     x =  1.40, y = -0.125, adj = 1, cex = 1.75)
text(bquote("rmse" == .(round(ee$rmse, 3))),
     x =  1.40, y = -0.15, adj = 1, cex = 1.75)
text("(a)",
     x =  0.57, y = -0.15, adj = 1, cex = 2)

# Finished
printLine()
cat("\n Script an02_compare_SOM_vs_freq.R complete!\n")
