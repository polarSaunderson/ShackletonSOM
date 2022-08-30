## ############################# ##
## an11_tabulate_pattern_stats.R ##
## ############################# ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Calculate the statistics for each pattern (Table 1)
#
# Comments: - This script outputs different statistics to the console
#           ! The Racmo2.3p3 temperature correlation requires the correct data
#             from dt07:
#             - Run dt07 with u_variable as t2m and u_meanRaser as FALSE
#             - The manuscript uses DJ as the summer definition for correlations
#
# Updates:
# 2022/08/30  v1.1  Table now includes SD as a column
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# User Options #################################################################
fresh("") # reset everything

# Which shelf are you interested in?
u_shelf   <- "Shackleton"

# What is the dataset version number?
u_version <- "v01"

# Which sensor? (only tested with amsrB)
u_sensor  <- "amsrB"

# Set-Up #######################################################################
source("R/setUp/su11_set_up.R") # Global variables, filepaths, functions, etc.
library(Kendall)

# Get data
ee$summerIndex <- as.integer(rownames(dd$CMS)) - 1979
ee$era5_2      <- read.csv("../../Data/ERA5/ee-chart__8.csv")
ee$tableData   <- matrix(NA, nrow = 9, ncol = 10) %>% 
  `colnames<-`(c("Freq", "Ext. Mean", "Ext. SD", 
                 "Tani. Mean", "Median", "MAD", "CV", "Trend",
                 "CMS", "T2m"))

# Chunk 1: Calculate Annual Occurrence =========================================
# Set Up
ee$mGrid       <- get(paste0("meltGridSom_", u_sensor), envir = dd)
ee$somOutput   <- paste0("somOutput_", u_sensor) %>% get(envir = dd)
ee$patterns    <- unique(ee$somOutput$unit.classif) %>% sort()
ee$summerIndex <- as.integer(rownames(dd$CMS)) - 1979
ee$cms         <- dd$CMS[, which(colnames(dd$CMS) == u_sensor)]

# Preallocate for annual occurrence
ee$annualSom <- matrix(NA, 
                       ncol = length(ee$summerIndex),
                       nrow = length(ee$patterns)) %>%
  `colnames<-`(paste0("ms", ee$summerIndex)) %>%
  `rownames<-`(paste0("p", ee$patterns))

# Calculate annual occurrence
for (ii in ee$summerIndex) {
  # For each pattern
  for (jj in ee$patterns) {
    ee$jjCount <- (ee$mGrid$mPattern_c11[ee$mGrid$mSeason_c10 == ii &
                                           ee$mGrid$mPattern_c11 == jj]) %>%
      na.exclude %>%
      length()
    
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
for (ii in 1:9) {
  ee$propSom[ii, ] <- (ee$annualSom[ii, ] / ee$msTotals * 100 )
}

# Chunk 2: Add data to table ===================================================
## Calculate Overall Frequency -------------------------------------------------
ee$tableData[1:9, 1] <- apply(ee$annualSom, 1, na.rm = TRUE, sum) %>% 
  `/`(sum(ee$annualSom, na.rm = TRUE)) %>%
  `*`(100) %>%
  round(1)

## Calculate Mean Extent -------------------------------------------------------
for (ii in 1:9) {
  ee$iiData <- ee$mGrid$mExtent_c7[ee$mGrid$mPattern_c11 == ii]
  ee$iiData <- ee$iiData[!is.na(ee$iiData)]
  ee$tableData[ii, 2] <- mean(ee$iiData) %>% round(1)
  ee$tableData[ii, 3] <- sd(ee$iiData) %>% round(1)
}

## Calculate Mean Tanimoto Distance --------------------------------------------
ee$somPatts <- ee$somOutput$unit.classif
ee$somDists <- ee$somOutput$distances

for (ii in 1:9) {
  ee$iiData <- ee$somDists[ee$somPatts == ii]
  ee$iiData <- ee$iiData[!is.na(ee$iiData)]
  ee$tableData[ii, 4] <- mean(ee$iiData) %>% round(2)
}

# What type?
ee$data <- ee$annualSom

## Calculate Median Annual Occurrence ------------------------------------------
cat("Trend Significance \n\n")
for (ii in 1:9) {
  ee$tableData[ii, 5] <- median(ee$data[ii, ], na.rm = TRUE) %>% round(1)
  ee$tableData[ii, 6] <- mad(ee$data[ii, ], na.rm = TRUE, constant = 1) %>% round(1)
  ee$tableData[ii, 7] <- (ee$tableData[ii, 6] / ee$tableData[ii, 5] * 100) %>% round()
  
  # Trend
  ee$mkTrend <- MannKendall(ee$data[ii, ])
  ee$tableData[ii, 8] <- ee$mkTrend$tau %>% round(2)
  
  # Print to console
  cat("Pattern", ii, ":", 
      sprintf("%+.3f", ee$mkTrend$tau), 
      "@ p = ", ee$mkTrend$sl %>% round(2),  "\n")
}

## Correlate with annual CMS ---------------------------------------------------
printLine()
cat("CMS Correlations \n\n")
# Calculate for each pattern
for (ii in 1:9) {
  # Correlate
  ee$kaw  <- cor.test(ee$cms, ee$data[ii, ])
  
  # Get r and p values
  ee$kawEst <- ee$kaw$estimate %>% round(2)
  ee$kawP   <- ee$kaw$p.value %>% round(2)
  
  # Print to console
  cat("Pattern", ii, ":", 
      sprintf("%+.3f", ee$kawEst), 
      "@ p = ", ee$kawP,  "\n")
  
  ee$tableData[ii, 9] <- ee$kawEst
}

## Inter Pattern Correlations (not in table) -----------------------------------
printLine()
cat("Interpattern Correlations (p < 0.1) \n\n")

# Preallocate
ee$interee$kaw    <- matrix(NA, nrow = 9, ncol = 9)
ee$interee$pValue <- matrix(NA, nrow = 9, ncol = 9)

# For each pattern
for (ii in 1:9) {
  ee$iiData <- ee$data[ii, ]
  
  # for each subsequent pattern
  for (jj in ii:9) {
    if ( ii != jj) {
      ee$jjData <- ee$data[jj, ]
      
      # Calculate correlation
      ee$kaw <- cor.test(ee$iiData, ee$jjData)
      
      # Store & display if significant
      ee$interee$kaw[ii, jj] <- ee$kaw$estimate %>% round(2)
      ee$interee$pValue[ii, jj] <- ee$kaw$p.value %>% round(2)
      if (ee$kaw$p.value < 0.1) {
        cat("Pattern", ii, "_", jj, ":", 
            sprintf("%+.2f", ee$kaw$estimate), 
            "@ p = ", ee$kaw$p.value %>% round(3),  "\n")
      }
    }
  }
}

## RACMO temperature correlations ----------------------------------------------
printLine()
cat("RACMO2.3p3 DJ Correlations \n\n")

# Load the data
ee$racmoT2m <- terra::rast(paste0(paste("Data", 
                                        ff$versionInfo, "racmo/", sep = "/"),
                                  ("racmo2_12_1_t2m_each.tif")))

# Calculate mean across the shelf each summer
ee$racmoT <- c()
for (ii in 1:15) {
  ee$racmoT[ii] <- (subset(ee$racmoT2m, ii) / 2) %>% as.matrix() %>% mean()
}

# Correlate for each pattern
for (ii in 1:9) {
  ee$iiData <- ee$data[ii, c(24:32, 34:39)]
  ee$kaw    <- cor.test(ee$racmoT, ee$iiData)
  ee$kawEst <- ee$kaw$estimate %>% round(2)
  ee$kawP   <- ee$kaw$p.value %>% round(2)
  
  # Add to table
  ee$tableData[ii, 10] <- ee$kawEst
  
  # Print to console
  cat("Pattern", ii, ":", 
      sprintf("%+.3f", ee$kawEst), 
      "@ p = ", ee$kawP,  "\n")
}

# Also correlate with the cms
ee$iiData <- dd$CMS[c(24:32, 34:39), 4]
ee$kaw    <- cor.test(ee$racmoT, ee$iiData)
ee$kawEst <- ee$kaw$estimate %>% round(2)
ee$kawP   <- ee$kaw$p.value %>% round(2)

# Print to console
cat("CMS       :", 
    sprintf("%+.3f", ee$kawEst), 
    "@ p = ", ee$kawP,  "\n")

## Calculate extremes (in terms of normal variability) -------------------------
ee$amsr <- ee$data[, 24:42]

# Preallocate storage
ee$pp <- matrix(NA, nrow = 9, ncol = 8) %>%
  `colnames<-`(c("min", "median","mad", "max", 
                 "minDiff", "maxDiff",
                 "madScaledMinDiff", "madScaledMaxDiff"))

# Calculate the stats
ee$pp[, 1] <- apply(ee$amsr, 1, na.rm = TRUE, min)
ee$pp[, 2] <- apply(ee$amsr, 1, na.rm = TRUE, median)
ee$pp[, 3] <- apply(ee$amsr, 1, na.rm = TRUE, constant = 1, mad)
ee$pp[, 4] <- apply(ee$amsr, 1, na.rm = TRUE, max)
ee$pp[, 5] <- ee$pp[, 2] - ee$pp[, 1]
ee$pp[, 6] <- ee$pp[, 4] - ee$pp[, 2]
ee$pp[, 7] <- ee$pp[, 5] / ee$pp[, 3]
ee$pp[, 8] <- ee$pp[, 6] / ee$pp[, 3]

# Display
printLine()
cat("Extremes \n\n")
print(ee$pp %>% round(2)) # round for ease of reading, not 1.0000000000!

# Output table data at the end! ------------------------------------------------
printLine()
cat(" Table Data \n\n")
print(ee$tableData)

# Finished
rm(ii, jj)
printLine()
cat("\n Script an11_tabulate_pattern_stats.R complete! \n\n")
