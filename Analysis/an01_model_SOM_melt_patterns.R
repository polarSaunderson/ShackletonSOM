## ############################## ##
## an01_model_SOM_melt_patterns.R ##
## ############################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Identify representative spatial melt patterns using a 
#             Self-Organising Map (SOM)
#
# Comments: - Based on the "kohonen" package (Wehrens & Kruisselbank, 2018); see 
#             their package and the TCD paper for more specific details
#           ! Running this script overwrites which pattern has been assigned to
#             each day in meltGridSom
#               - Subsequent scripts depend on this
#               - Run again with original settings to reset
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

# Which sensor should be used for the patterns?
u_sensor  <- c("amsrB")  # "amsre", "amsr2", "amsrB", "ssmis"

# Add a colour bar to the plot window? Purely for plotting purposes
u_addBar  <- TRUE

# SOM Settings -----------------------------------------------------------------
u_somCol     <- c(3)          # How many columns in the SOM output?          [3]
u_somRow     <- c(3)          # How many rows in the SOM output?             [3]
u_distance   <- c("tanimoto") # How to judge similarity? If binary, Tanimoto!

u_alpha1     <- c(0.05)       # Learning rate at start                    [0.05]
u_alpha2     <- c(0.01)       # Learning rate at end                      [0.01]
u_seed       <- c(1609)       # Initiates randomly; set seed to replicate [1609]
u_iterations <- c(100)        # How many iterations are necessary?         [100]

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.

# Load kohonen package
library(kohonen)

# Load correct vectorGrid
base::load(file = paste0(ff$interimPath, ff$versionInfo, "_som/", 
                         ff$versionInfo, "_vectorGrid_", u_sensor, ".rData"), 
           envir = dd)
ee$vectorGrid <- get(x = paste0("vectorGrid_", u_sensor), 
                     envir =  dd)

# _____CODE_____ ###############################################################
# Chunk 1: Perform SOM (w/ Sensitivity if required) ============================
# Initialisation
set.seed(u_seed)

# Set up SOM grid (i.e. required output)
ee$somGrid   <- somgrid(u_somCol, u_somRow, 
                        "rectangular", 
                        toroidal = FALSE)

# Run SOM
ee$somOutput <- som(ee$vectorGrid,
                    grid  = ee$somGrid,
                    rlen  = u_iterations,
                    alpha = c(u_alpha1, u_alpha2),
                    dist.fcts = u_distance)

# Chunk 2: Display SOM Output Patterns =========================================
# Print somOutput Information to Console
summary(ee$somOutput)
cat("u_seed: ", u_seed,
    "\nu_alpha: from", u_alpha1, "to", u_alpha2,
    "\nu_iterations: ", u_iterations, "\n")

# Display Default kohonen plots
plotKohoMetrics(ee$somOutput)

## Set-up for SOM plots --------------------------------------------------------
# This section is overly-complicated to ease replication of supplement figures
if (u_somCol == 3 & u_somRow == 3 & isTRUE(u_addBar)) {
  ee$plotHeight <- 18
} else {
  ee$plotHeight <- 14
}

# We want a faint background colour to tie plots together if no colour bar
if (isTRUE(u_addBar)) {
  ee$bgKula <- "#FFFFFF"
} else {
  ee$bgKula <- "#F5F5F5"
}

# Create new figure window with defined size
dev.new(width = 12 * 4,
        height = ee$plotHeight * 4,
        unit = "cm",
        noRStudioGD = TRUE)

# Add a background colour to help frame plot groupings in the supplement
par(bg = ee$bgKula)

# We want to display all output patterns, and add a colour bar scale 
# Set up tweaked for the final figures when using 3 x 3 grid; else automate
if (isTRUE(u_addBar)) {
  if (u_somCol == 3 & u_somRow == 3) {
    # 3 x 3
    layout(matrix(c(1, 2, 3,
                    1, 2, 3, 
                    1, 2, 3,
                    4, 5, 6,
                    4, 5, 6,
                    4, 5, 6,
                    7, 8, 9,
                    7, 8, 9,
                    7, 8, 9,
                    10, 10, 10),
                  ncol = 3, byrow = TRUE))
    ee$barMar   <- c(2.5, 6, 1.75, 6)
    ee$labelOff <- -3.75
  } else if (u_somCol == 4 & u_somRow == 4)  {
    # 4 x 4 
    layout(matrix(c(1, 2, 3, 4,
                    1, 2, 3, 4,
                    5, 6, 7, 8,
                    5, 6, 7, 8,
                    9, 10, 11, 12, 
                    9, 10, 11, 12, 
                    13, 14, 15, 16,
                    13, 14, 15, 16,
                    17, 17, 17, 17), 
                  ncol = 4, byrow = TRUE))
    ee$barMar   <- c(2.5, 6, 2.5, 6)
    ee$labelOff <- -3.75
  }  else if (u_somCol == 5 & u_somRow == 5)  {
    # 5 x 5 
    layout(matrix(c(1, 2, 3, 4, 5,
                    1, 2, 3, 4, 5,
                    6, 7, 8, 9, 10,
                    6, 7, 8, 9, 10,
                    11, 12, 13, 14, 15,
                    11, 12, 13, 14, 15,
                    16, 17, 18, 19, 20,
                    16, 17, 18, 19, 20,
                    21, 22, 23, 24, 25,
                    21, 22, 23, 24, 25,
                    26, 26, 26, 26, 26),
                  ncol = 5, byrow = TRUE))
    ee$barMar   <- c(2.5, 6, 1.25, 6)
    ee$labelOff <- -3.25
  } else {
    # This can lead to massive or tiny colour bars
    layout(setLayoutBar(nrows = u_somRow, 
                        ncols = u_somCol, 
                        horiz = TRUE))
  }
} else {
  par(mfrow = c(u_somRow, u_somCol))
}

# Don't add all the map detail if loads of plots!
if (u_somCol == 10 & u_somRow == 10) {
  # 10 x 10 
  ee$mapBits <- "counts"
  ee$plotMar <- c(1, 0, 0, 0)
} else {
  ee$mapBits <- TRUE
  ee$plotMar <- c(1, 1, 1, 1)
}

## Display SOM Output ----------------------------------------------------------
# Display SOM Output Melt Patterns
plotSomPatterns(somOutput = ee$somOutput, 
                somRow  = u_somRow, 
                somCol  = u_somCol,
                plotMar = ee$plotMar, 
                addBar  = FALSE,           # add below for more control
                addMapParts  = ee$mapBits,
                shelfOutline = dd$shelf3976,
                commonMask   = dd$commonMask)

# And add a colour bar besides it if necessary
if (isTRUE(u_addBar)) {
  # Add a colour scale
  addColourBar(col  = gg$kulaS, 
               zlim = c(0, 1), 
               cex.labels = 1.5, 
               horiz = TRUE,
               allLabels = "odds",
               title = "Melt Likelihood",
               labelOffset = ee$labelOff,
               mar = ee$barMar)
} 

# Chunk 3: Mapping Patterns ====================================================
# Map each melt day to its closest pattern
set.seed(u_seed)
ee$somPatterns <- kohonen::map(x = ee$somOutput, 
                               newdata = ee$vectorGrid)

# Prep meltGrid by appending new columns
ee$meltGridSom <- dd$meltGrid[[which(dd$inputSensors == u_sensor)]]
ee$meltGridSom <- cbind(ee$meltGridSom[, 1:10], 
                        ee$meltGridSom[, 1:2] * NA)
colnames(ee$meltGridSom)[11:12] <- c("mPattern_c11", 
                                     "mSomDistance_c12")

# Add patterns & distance to meltGrid (for use elsewhere)
ee$meltGridSom[, 11][!is.na(ee$meltGridSom$mSeason_c10)] <- ee$somPatterns$unit.classif
ee$meltGridSom[, 12][!is.na(ee$meltGridSom$mSeason_c10)] <- ee$somPatterns$distances

# Chunk 4: Quick Boxplots for Assessing Patterns ===============================
# Which days have melt?
ee$mDays <- ee$meltGridSom$date_c1[!is.na(ee$meltGridSom$mSeason_c10)]

# Preallocate a new container for ease & plotting SOM distance information
ee$somInfo <- matrix(NA, 
                     nrow = length(ee$mDays),
                     ncol = 3) %>% 
  `colnames<-`(c("date", 
                 "pattern", 
                 "distance"))

# Populate our new container
ee$somInfo[, 1] <- ee$mDays
ee$somInfo[, 2] <- ee$somPatterns$unit.classif
ee$somInfo[, 3] <- ee$somPatterns$distances

# Create new figure window with defined size
dev.new(width = 12,
        height = 14,
        unit = "cm",
        noRStudioGD = TRUE)

# Prep a plotting area for box plots
par(mfrow = c(u_somRow, u_somCol))

# Display box plots of pattern distance
for (ii in 1:(u_somCol * u_somRow)) {
  # Get index to align with the default kohonen output order
  jj     <- matchKohoPlotOrder(ii, somCols = u_somCol, somRows = u_somRow)
  
  # Get the tanimoto distances for these plots (saved in the somOutput)
  ee$taniDis <- filterData(ee$somInfo, "distance", "pattern", jj)
  
  # Set margins dependent on the plot's place within the grid - for 3 x 3
  if (ii %in% c(1, 4, 7)) {
    par(mar = c(2, 5, 2, 3))
  } else if (ii %in% c(3, 6, 9)) {
    par(mar = c(2, 3, 2, 5))
  } else {
    par(mar = c(2, 4, 2, 4))
  }
  
  # Add the plots
  boxplot(ee$taniDis, 
          ylim = c(0, 0.43),
          las = 2,
          cex.axis = 1.25,
          col = paste0(gg$kulaQ[1], "44"))
  grid()
  boxplot(ee$taniDis, 
          ylim = c(0, 0.43),
          add = TRUE,
          axes = FALSE,
          col = paste0(gg$kulaQ[1], "44"))
  
  # Add a title
  title(main = paste0("Pattern ", jj), mgp = c(3, 1, 0), cex.main = 1.4)
  
  # Add a single y-label - we don't want labels all over the place!
  if (ii == 4) {
    title(ylab = "Tanimoto Distance", mgp = c(3, 1, 0), cex.lab = 1.65)
  }
  
  # Add count as a label
  if (ii == 1) {
    yy <- 0.4
  } else {
    yy <- 0.365
  }
  text(paste0("n = ", length(ee$taniDis)), 
       x = 0.8, y = yy, adj = 0, cex = 1.25)
}
rm(ii, jj, yy)

# Chunk 5: Save & Tidy =========================================================
# Saving
ff$fileNaming <- paste0(ff$analysePath, ff$versionInfo, "_", u_sensor , "_")

# Rename objects for tracking the sensor used when loaded elsewhere
ee$gridName <- paste0("meltGridSom_", u_sensor)
assign(ee$gridName, ee$meltGridSom, envir = ee)

ee$somName <- paste0("somOutput_", u_sensor)
assign(ee$somName, ee$somOutput, envir = ee)

# And save
do.call(save, 
        list(ee$gridName, file = paste0(ff$fileNaming, "meltGridSom.rData")),
        envir = ee)

do.call(save, 
        list(ee$somName,  file = paste0(ff$fileNaming, "somOutput.rData")),
        envir = ee)

# Save complete
printLine()
cat(" Som output saved \n",
    "meltGrid melt days assigned a SOM pattern \n",
    "meltGrid overwritten \n")     

# Finished
printLine()
cat(" Script an01_model_SOM_melt_patterns.R complete! \n")
beep("ping")

