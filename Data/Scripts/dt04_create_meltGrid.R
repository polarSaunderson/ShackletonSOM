## ###################### ##
## dt04_create_meltGrid.R ##
## ###################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Creates meltGrid, which is a list of dataframes, each storing all 
#             the melt data for a specific sensor
#
# Comments: - Within a dataframe, each row represents a day of information 
#             for that sensor, and each column holds a specific metric / result, 
#             such as: melt extent, context, change in extent, date.
#           - Also creates CMS along the way, which stores the Cumulative 
#             Melting Surface for each sensor for each summer
#           - This script takes ~ 40-60 seconds if including SSMIS data
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# User Options #################################################################
fresh("") # reset everything

# Which shelf are you interested in?
u_shelf    <- "Shackleton"

# Add a version number to keep track
u_version  <- "v01"

# Which sensors should be included?
u_sensors  <- c("amsre", "amsr2", "ssmis")

# Set-Up #######################################################################
source("R/setUp/su01_set_up.R") # Global variables, filepaths, functions, etc.

# Begin Timer
ee$startTime <- proc.time()

# Create a new version of meltListResampled for use
ee$newMeltList <- dd$meltListResampled

# _____CODE_____ ###############################################################
# Chunk 1: Create Basic meltGrid with Date-Related Information =================
# Preallocate a data container called meltGrid
ee$meltGrid        <- rep(list(list()), length(u_sensors))
names(ee$meltGrid) <- u_sensors

# Let user know that it has begun
cat(" Identifying correct dates... \n\n")

# Cycle through each sensor in separate meltGrid lists & populate with date info
for (ii in 1:length(u_sensors)) {
  ## Convert row names to real dates -------------------------------------------
  # We want to know how many days have data for the number of rows in meltGrid
  # Get rownames (Picard Dates, i.e. since start of dataset) for all list items
  ee$sensorDays <- names(ee$newMeltList[[ii]])
  
  # Extract "days since" part of the item name ("time" in Prof. Picard's NetCDF)
  ee$itemName   <- paste0(ff$versionInfo, "_pTime_")
  ee$picDays <- as.numeric(sapply(ee$sensorDays,
                                        FUN = substring,
                                        first = nchar(ee$itemName) + 1))
  
  # Convert above "days since" to human-readable real dates
  ee$realDates <- as.Date(ee$picDays, 
                             origin = paste0(gg$startYear[ii], gg$startDate))
  
  ## Populate meltGrid ---------------------------------------------------------
  # Now we can initiate meltGrid (needed to know number of picardDays first)
  ee$meltGrid[[ii]] <- matrix(NA, 
                                 ncol = 10, 
                                 nrow = length(ee$picDays)) %>%
    `rownames<-`(ee$realDates) %>%
    `colnames<-`(paste0("col_", c(1:10))) # temp names (need for later renaming)
  
  # Add date-related column names
  colnames(ee$meltGrid[[ii]])[1:6] <- c("date_c1", "year_c2", 
                                           "month_c3", "day_c4", 
                                           "DoMS_c5", "summerIndex_c6")
  
  # Input date info as columns of data to meltGrid
  for (jj in 1:length(ee$realDates)) {
    # Add the date (stored as days since 1970-01-01)
    ee$meltGrid[[ii]][jj, 1] <- ee$realDates[jj] 
    # Add the year, month & day information
    ee$meltGrid[[ii]][jj, 2] <- as.numeric(substring(ee$realDates[jj], 
                                                        1, 4))  # year
    ee$meltGrid[[ii]][jj, 3] <- as.numeric(substring(ee$realDates[jj], 
                                                        6, 7))  # month
    ee$meltGrid[[ii]][jj, 4] <- as.numeric(substring(ee$realDates[jj], 
                                                        9, 10)) # day
  }
  
  ## Add Day of Melt Season (days since 1st November) & Summer Index -----------
  # Identify the 1st November each summer
  ee$meltGrid[[ii]][, 5][ee$meltGrid[[ii]][, 3] == 11 & 
                              ee$meltGrid[[ii]][, 4] == 1] <- 1
  
  # Which rows are the above dates on?
  ee$nov1Rows <- which(ee$meltGrid[[ii]][, 5] == 1)
  
  # Use nov1Rows as anchor point to count off the remaining days of the summer
  for (kk in ee$nov1Rows) {
    ee$yrRows <- c(kk:(kk + 134)) # 135 days after 1st Nov is March 15th
    
    # Add a summer index which is easier than crossing years in austral summer
    # 1979/1980 is considered summer index 1
    ee$summerIndex <- which(gg$satellitePeriod == ee$meltGrid[[ii]][kk, 2])
    
    # Stop adding data when the end of meltGrid is reached
    if (max(ee$yrRows) <= dim(ee$meltGrid[[ii]])[1]) { 
      ee$meltGrid[[ii]][ee$yrRows, 5] <- c(1:135)
      ee$meltGrid[[ii]][ee$yrRows, 6] <- ee$summerIndex
    } else {
      ee$lastYrRows <- ee$yrRows[1:length(ee$yrRows[ee$yrRows <= 
                                                      dim(ee$meltGrid[[ii]])[1]])]
      ee$meltGrid[[ii]][ee$lastYrRows, 5] <- 1:length(ee$lastYrRows)
      ee$meltGrid[[ii]][ee$lastYrRows, 6] <- ee$summerIndex
    }
  }
}
cat(" Created meltGrid & added date information \n")

# Chunk 2: Calculate Daily Values ==============================================
## Calculate Daily Melt Extent (% of the full shelf) ---------------------------
# What is the areal size of each pixel? in km^2
ee$pixelArea <- (res(dd$commonMask)[1] / 1000) ^ 2 

# Melting area is stored as a % of the pixel in commonAreas, hence divide by 100
ee$meltArea <- ee$pixelArea * dd$commonAreas / 100

# What is the total area that could melt in a single day?
ee$shelfTotalArea <- sum(ee$meltArea, na.rm = TRUE)

# Calculate melt extent each day
for (ii in 1:length(u_sensors)) {
  # Identify correct dataset for this sensor
  ee$ii_meltList <- ee$newMeltList[[ii]]
  
  # Calculate melt extent (% of the full shelf) for each day & store in meltGrid
  for (jj in 1:length(ee$ii_meltList)) {
    ee$meltGrid[[ii]][jj, 7] <- dd$commonIds %in% 
      ee$ii_meltList[[jj]] %>%                 # Which pixels melt?
      `*`(ee$meltArea) %>%                     # Only use melting shelf area
      sum(na.rm = TRUE) %>%                       # Sum across the full shelf
      `/`(ee$shelfTotalArea) %>% `*`(100) %>%  # We want it as a percentage
      round(2)
  }
  
  # Rename column
  colnames(ee$meltGrid[[ii]])[7] <- "mExtent_c7"
}
cat(" Calculated daily melt extents \n")

## Calculate Daily Change in Melt Extent ---------------------------------------
# Loop through each sensor & day
for (ii in 1:length(u_sensors)) {
  # How many rows of data are there?
  ee$picDays <- dim(ee$meltGrid[[ii]])[1]
  
  # Calculate the difference in melt extent from the previous day
  ee$meltGrid[[ii]][2:ee$picDays, 8] <- ee$meltGrid[[ii]][c(2:ee$picDays), 7] %>%
    `-`(ee$meltGrid[[ii]][c(1:(ee$picDays - 1)), 7])
  
  # Rename column
  colnames(ee$meltGrid[[ii]])[8] <- "mExtChange_c8"
}
cat(" Calculated daily changes in melt extent \n")

# Chunk 3: Calculate Annual CMS ================================================
# Preallocate storage
ee$CMS <- matrix(NA, 
                    nrow = length(gg$dataPeriod), 
                    ncol = length(u_sensors)) %>%
  `colnames<-`(u_sensors) %>%
  `rownames<-`(gg$dataPeriod + 1) # +1 so rows show the January date of the summer

# Loop through each sensor
for (ii in 1:length(u_sensors)) {
  # Create temporary variable for easier handling than subsetting meltGrid
  ee$meltData <- ee$meltGrid[[ii]]
  
  # Which years does this dataset have data for?
  ee$years <- unique(ee$meltData[, 2]) %>% head(-1)
  
  # Calculate the CMS for each summer (inc. Nov & Dec of jj; JFM of jj + 1)
  for (jj in ee$years) {
    # Sum the daily melt extents
    ee$cum <- sum(ee$meltData[, 7]                        # sum melt extents
                     [(ee$meltData[, 3] %in% c(11, 12) &     # when month is N/D
                         ee$meltData[, 2] == jj) |           # in year JJ, OR
                         (ee$meltData[, 3] %in% c(1, 2, 3) & # month is J/F/M in
                            ee$meltData[, 2] == jj + 1)])    # year JJ + 1
    
    # But the above is a sum of the daily %; we want the true area
    ee$cum <- ee$cum %>%
      `/`(100) %>% `*`(ee$shelfTotalArea) %>% # convert from % to area
      `/`(10^6)                               # Store in millions for ease
    
    # Store CMS
    ee$rowIndex <- which(gg$dataPeriod == jj)   # Which row is this year?
    ee$CMS[ee$rowIndex, ii] <- ee$cum        # Store
  }
}

# Remove 1987/88 summer from the SSMIS dataset (incomplete observations)
ee$CMS[which(rownames(ee$CMS) == 1988), 
          which(colnames(ee$CMS) == "ssmis")] <- NA

cat(" Calculated summer CMS \n")

# Chunk 4: Establish Melt Season Dates =========================================
# Loop through each sensor
for (ii in 1:length(u_sensors)) {
  # Create temporary variable for easier handling than subsetting meltGrid
  ee$meltData <- ee$meltGrid[[ii]]
  
  # Which years does this dataset have data for?
  ee$years <- unique(ee$meltData[, 2]) %>% head(-1)
  
  # Identify melt season days for each summer
  for (jj in ee$years) {
    # Which summer is this?
    ee$summerIndex <- which(gg$satellitePeriod == jj)
    
    # Which rows have melt? min = onset; max = day before freeze-up
    ee$meltingRows <- (which(ee$meltGrid[[ii]][, 7] != 0 & 
                                  ee$meltGrid[[ii]][, 6] == ee$summerIndex))
    
    # Only try to store data if melt occurs
    if (length(ee$meltingRows) > 0) {
      ee$meltingRows <- range(ee$meltingRows)
      ee$meltGrid[[ii]][ee$meltingRows[1]:ee$meltingRows[2], 10] <- ee$summerIndex
    }
  }
  
  # Rename column
  colnames(ee$meltGrid[[ii]])[c(10)] <- c("mSeason_c10")
}

cat(" Established Melt Season Dates \n")

# Chunk 5: Calculate Melt Context ==============================================
for (ii in 1:length(u_sensors)) {
  # Create temporary variable for easier handling than subsetting meltGrid
  ee$meltData <- ee$meltGrid[[ii]]
  
  # Which years does this dataset have data for?
  ee$years <- unique(ee$meltData[, 2]) %>% head(-1)
  
  # Calculate melt context of each melt day each summer
  for (jj in ee$years) {
    ee$summerIndex <- which(gg$satellitePeriod == jj)
    
    # Retrieve Melt Season Days
    ee$mDates  <- ee$meltData[, 5][ee$meltData[, 10] == ee$summerIndex]
    ee$mDates  <- ee$mDates[!is.na(ee$mDates)] # remove NA from above
    
    # Last day of non-melt before season is context 0%; last day of melt is 100%
    if (length(ee$mDates) > 1) {
      ee$mOnset  <- ee$mDates[[1]] - 1 # -1 for last non-melt season date 
      ee$mLength <- length(ee$mDates) 
      
      # How many days since the melt season began?
      ee$mSince    <- ee$mDates - ee$mOnset
      
      # Context - how far through the melt season length is each day?
      ee$mCon    <- (ee$mSince / ee$mLength * 100 ) %>% round(4)
      
    } else {
      ee$mCon <- NA
    }
    
    # Store in meltGrid
    ee$meltGrid[[ii]][which(ee$meltData[, 10] %in% 
                                 ee$summerIndex), 9] <- ee$mCon
  }
  
  # Rename column
  colnames(ee$meltGrid[[ii]])[c(9)] <- c("mContext_c9")
}

# Print completion
cat(" Calculated daily melt context \n")
printLine()
cat(" Successfuly created and populated meltGrid \n")

# Chunk 6: Join the AMSR-E & AMSR-2 Datastes ===================================
# Only try to join if both of these datasets are here!
if ("amsre" %in% u_sensors & "amsr2" %in% u_sensors) {
  
  # Get indices to join correct datasets
  ee$abIndex <- length(u_sensors) + 1      # AMSR-B refers to AMSR-Both
  ee$aeIndex <- which(u_sensors == "amsre")
  ee$a2Index <- which(u_sensors == "amsr2")
  
  # Join in meltGrid
  ee$meltGrid[[ee$abIndex]] <- rbind(ee$meltGrid[[ee$aeIndex]], 
                                           ee$meltGrid[[ee$a2Index]])
  names(ee$meltGrid)[ee$abIndex] <- "amsrB"
  
  # Join meltLists (i.e. the melting pixel IDs for each day)
  ee$newMeltList[[ee$abIndex]] <- c(ee$newMeltList[[ee$aeIndex]], 
                                          ee$newMeltList[[ee$a2Index]])
  names(ee$newMeltList)[ee$abIndex] <- "amsrB"
  
  # Join CMS data by adding AMSR columns as there is no overlap years, only NA's
  ee$CMS <- cbind(ee$CMS, ee$CMS[, 1])            # Create an extra column
  ee$CMS[is.na(ee$CMS)] <- 0                      # NA's can't be added
  ee$CMS[, ee$abIndex] <- ee$CMS[, ee$aeIndex] + 
    ee$CMS[, ee$a2Index]   # Add the columns
  colnames(ee$CMS)[ee$abIndex] <- "amsrB"              # Rename
  ee$CMS[ee$CMS == 0] <- NA                            # Reset 0's to NA
  
  # Print completion
  cat("\n Successfuly created joint AMSR-B dataset (B for AMSR-Both) \n")
  printLine()
}

# Chunk 8: Save Output =========================================================
# Convert meltGrid matrices to dataframes for easier access later
for (ii in 1:length(ee$meltGrid)) {
  ee$meltGrid[[ii]] <- as.data.frame(ee$meltGrid[[ii]])
}

# File Names
ff$fileNaming <- paste0(ff$analysePath, ff$versionInfo)

# Save
save(meltGrid,    envir = ee, file = paste0(ff$fileNaming, "_meltGrid.rData"))
save(CMS,         envir = ee, file = paste0(ff$fileNaming, "_CMS.rData"))
save(newMeltList, envir = ee, file = paste0(ff$fileNaming, "_meltList.rData"))

# Finished!
rm(ii, jj, kk)
ee$runningTime <- proc.time() - ee$startTime
print(ee$runningTime)
cat("\n Script dt04 complete for", ff$versionInfo, "\n\n")
beep("ping")
