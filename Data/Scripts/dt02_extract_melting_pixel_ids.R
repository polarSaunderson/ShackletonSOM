## ################################ ##
## dt02_extract_melting_pixel_ids.R ##
## ################################ ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Create a list of ID numbers indicating which pixels melt each day
#
# Comments: - This script turns the "raw" (binary) melt data into usable data 
#             for the following scripts. 
#           - The code here is very verbose and a long way around - much easier,
#             and more efficient ways are definitely possible, but this works.
#           - Run this script for any shelf, providing script dt01 has been run 
#             for it; also make sure that the data has been downloaded and that 
#             the correct path is set in the su02 script
#           - As a quick overview, this script:
#             - 1) Creates ID numbers for the whole Antarctic region:
#                   - Each pixel in the dataset has a corresponding ID number. 
#                   - The ID numbers are created here & have no wider use
#                   - ID numbers count from the top-left (~ Chile) to the 
#                     top-right, then ascend row-by-row, finishing in the
#                     bottom-right (~ Wilkes).
#             - 2) Extracts raw (binary) melt data for the selected shelf:
#                   - Identifies the ID numbers of melting pixels for each day 
#                   - Each day's melting pixels are stored in a separate list
#           - Also stored are additional supplementary datasets:
#                 pixel latitude matrix     || pixel longitude matrix
#                 shelf pixel ID matrix     || pixel shelf area matrix
#                 binary shelf mask matrix  || shelf region raster
#           - Running time for this script is very inconsistent:
#             - Normally it has taken 1-2 minutes for AMSR-E or AMSR-2, and 4-5 
#               minutes for the longer SSMIS dataset
#             - Sometimes it only takes seconds for each
#           - u_sensors allows all three sensors to be looped through, but often
#             R can crash when trying this; it may be better to run it 
#             individually for each sensor instead
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

# Which sensors should be included? If it lags/crashes, try them separately!
u_sensors  <- c("amsre", "amsr2", "ssmis")

# Run only for year as a quick test?
# u_testScript <- FALSE

# Set-Up #######################################################################
source("R/setUp/su01_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# We need to extract the data for each sensor separately
for (ii in u_sensors) {
  # Reset the environment for each sensor
  ee <- new.env()
  
  # Begin Timer
  ee$startTime <- proc.time()
  
  # Dataset-Specific Settings for Binary Melt Data
  printLine(paste0("___", ii, "___"), 0.35)
  ee$sensorIndex     <- which(gg$sensors == ii)        # Identify the dataset
  ee$pixelResolution <- gg$rawResol[ee$sensorIndex]    # Get pixel width
  ee$gpStartDate <- paste(gg$startYear[ee$sensorIndex], 
                              gg$startDate)            # Get dataset start date
  ee$ncFileName      <- ff$rawPaths[ee$sensorIndex]    # Get correct dataset 
                                                       # to open/access later
  cat("File:'", ee$ncFileName, "'\n")
  
  # Chunk 1: Identify Pixels to Extract ==========================================
  ## _pseudo-code ----
  # __Create a grid of the pixel coordinates from the NetCDF (default EPSG:4326)
  # __Reproject to EPSG:3976 to match the NSIDC grid coordinates
  # __Create a new raster of pixels with IDs, w/ above coordinates & projection
  # __Crop ID raster to the selected shelf
  # __Identify which rows & columns of the NetCDF need extracting
  
  ## Create a spatial points object of pixel coordinates (EPSG:4326) -----------
  # Open NetCDF file for access
  ee$nc           <- open.nc(ee$ncFileName)
  
  # Display info on the netCDF 
  cat("\n__Summary of Binary Melt NetCDF File\n")
  print.nc(ee$nc)
  printLine()
  
  # Extract coordinates for all of Antarctica from the NetCDF
  ee$gpAaLong <- var.get.nc(ee$nc, "lon",
                                   c(1, 1, 1),
                                   c(NA, NA, 1))
  ee$gpAaLati <- var.get.nc(ee$nc, "lat",
                                   c(1, 1, 1),
                                   c(NA, NA, 1))
  
  # Extract a single day of melt data to use as a test
  ee$gpAaMelt <- var.get.nc(ee$nc, "melt",
                                   c(1, 1, 300),
                                   c(NA, NA, 1))
  
  # Shorten dataset if only testing
  if (exists("u_testScript")) {
    ee$gpDays <- var.get.nc(ee$nc, "time")[0:364]
  } else {
    ee$gpDays <- var.get.nc(ee$nc, "time")
  }
  
  # How many days are in Picard's dataset?
  ee$gpDataLength <- length(ee$gpDays)
  cat("\n", ii, "dataset length:", ee$gpDataLength, "days\n")
  
  # Close netcdf file for now
  close.nc(ee$nc)

  # Reshape extracted coordinate matrices to single columns
  ee$gpAaLongList <- matrix(ee$gpAaLong, ncol = 1)
  ee$gpAaLatiList <- matrix(ee$gpAaLati, ncol = 1)
  
  # Combine the coordinate columns and convert to a spatial points object
  ee$gpAaCoordList  <- cbind(ee$gpAaLongList, 
                                    ee$gpAaLatiList)
  ee$gpAaCoords4326 <- SpatialPoints(coords = ee$gpAaCoordList)
  
  ## Reproject the extracted coordinates from EPSG:4326 to ESPG:3976 -----------
  # The NetCDF has the coordinates in latitude & longitude (EPSG:4326)
  ee$crs4326 <- CRSargs(CRS("+init=epsg:4326"))            # Define 4326
  proj4string(ee$gpAaCoords4326) <- CRS(ee$crs4326) # Assign to coords
  
  # We need the coordinates in the NSIDC polar stereo projection (EPSG:3976)
  ee$crs3976 <- CRSargs(CRS("+init=epsg:3976"))            # Define 3976
  ee$gpAaCoords3976 <- spTransform(ee$gpAaCoords4326, 
                                          CRS(ee$crs3976)) # Convert points 
  
  # Create bounding boxes of coordinates & print to check the reprojection
  ee$gpAaBbox4326 <- ee$gpAaCoords4326@bbox
  cat("\n___EPSG:4326 Coordinates for Antarctic Bounding Region:\n")
  print(ee$gpAaBbox4326)
  
  ee$gpAaBbox3976 <- ee$gpAaCoords3976@bbox
  cat("\n___EPSG:3976 Coordinates for Antarctic Bounding Region:\n")
  print(ee$gpAaBbox3976)
  printLine()
  
  ## Create a new projected raster of pixels with Unique ID Numbers ------------
  # We'll need to add 1/2 a pixel width as the NetCDF points are grid centres
  ee$halfPixel <- as.integer(ee$pixelResolution) / 2
  
  # Define the raster's bounding box:
  ee$gpAaMinX3976 <- ee$gpAaBbox3976[1, 1] - ee$halfPixel
  ee$gpAaMaxX3976 <- ee$gpAaBbox3976[1, 2] + ee$halfPixel
  ee$gpAaMinY3976 <- ee$gpAaBbox3976[2, 1] - ee$halfPixel
  ee$gpAaMaxY3976 <- ee$gpAaBbox3976[2, 2] + ee$halfPixel
  
  # Create a raster grid of ID numbers, running from top left to bottom right
  ee$dsAaIdRaster <- raster(ncol = dim(ee$gpAaLati)[1], 
                                ymn = ee$gpAaMinY3976, 
                                ymx = ee$gpAaMaxY3976,
                                nrow = dim(ee$gpAaLati)[2], 
                                xmn = ee$gpAaMinX3976, 
                                xmx = ee$gpAaMaxX3976)
  
  # Add the correct projection to the new raster
  projection(ee$dsAaIdRaster) <- "+init=EPSG:3976"
  
  # Display progress
  cat("\n___Created a new raster of pixels with unique ID numbers: \n")
  cat("___Check the resolution is", ee$pixelResolution, 
      "and crs (3976) are as expected!\n")
  print(ee$dsAaIdRaster)
  printLine()
  
  # Display test of alignment: add example melt day values to the new raster 
  par(mfrow = c(1, 2))
  
  # R object is transposed compared to melt data
  values(ee$dsAaIdRaster) <- t(ee$gpAaMelt) # Transpose for alignment
  
  # Display binary melt day example 
  plotShelf(x = ee$dsAaIdRaster,
            main = paste("Potential", ii, "Melt Pixels"),
            breaks  = c(-10, -1, 0, 1), 
            col = gg$kulaQ[c(7, 1, 5)],
            withArrow = FALSE,
            sub = "Blue = locations checked for melt") # not ocean or high alt.
  
  # Display newly created pixel ID raster 
  values(ee$dsAaIdRaster) <- (1:ncell(ee$dsAaIdRaster))
  plotShelf(x = ee$dsAaIdRaster,
            main = "Pixel IDs across Antarctica",
            col = gg$kulaD,
            withArrow = FALSE,
            sub = paste(u_shelf, 
                        "shelf should be displayed - it may be small!"))
  
  ## Crop the new raster of IDs to the selected shelf bounds -------------------
  # Crop to shelf
  ee$dsShelfIdRaster <- crop(ee$dsAaIdRaster, 
                                 snap = "out", gg$shelf3976)
  
  # Display to check alignment
  par(mfrow = c(2, 2))
  
  # Display cropped ID Grid
  plotShelf(x = ee$dsShelfIdRaster,
            main = paste0("Check Shelf Cropping for ", u_shelf),
            sub = "Coloured according to pixel ID",
            col = gg$kulaD)
  
  ## Identify Rows & Columns for netCDF Extraction -----------------------------
  # Get the ID numbers of the pixels cropped to the shelf
  ee$dsShelfPixelId <- ee$dsShelfIdRaster@data@values
  
  # Create a matrix (not raster!) of all ID numbers for the full Antarctic grid
  ee$dsAaPixelId <- as.matrix(ee$dsAaIdRaster)
  
  # NA pixels in above matrix if the pixel isn't within the shelf bounds
  ee$dsAaPixelId[!ee$dsAaPixelId %in% ee$dsShelfPixelId] <- NA
  
  # Find, in the NetCDF file, which rows & columns contain the selected shelf
  # Firstly, find the corners
  ee$gpShelfTopLeft     <- which(ee$dsAaPixelId == min(ee$dsShelfPixelId), 
                                        arr.ind = TRUE) # matrix location
  ee$gpShelfBottomRight <- which(ee$dsAaPixelId == max(ee$dsShelfPixelId), 
                                        arr.ind = TRUE) # matrix location
  
  # Then calculate how many pixel rows and columns are needed to cover the shelf
  ee$gpShelfCols <- ee$gpShelfBottomRight[2] - ee$gpShelfTopLeft[2] + 1
  ee$gpShelfRows <- ee$gpShelfBottomRight[1] - ee$gpShelfTopLeft[1] + 1
  
  # Chunk 2: Pre-Data Extraction ===============================================
  ## _pseudo-code ----
  # __Create binary shelf/non-shelf mask from NetCDF
  # __Create bounding boxes of the shelf to extract the data
  # __Create matrices of pixel ID numbers and pixel shelf area
  # __Display as plots to confirm alignment
  
  ## Create shelf/non-shelf mask from the netCDF's binary melt data ------------
  # Open NetCDF melt dataset
  ee$nc <- open.nc(ee$ncFileName)
  
  # Extract a single day of example melt data for the shelf pixels 
  # The columns are input first (an R matrix is transposed vs Picard's dataset)
  ee$gpShelfPixels <- var.get.nc(ee$nc, "melt",
                                        c(ee$gpShelfTopLeft[2], 
                                          ee$gpShelfTopLeft[1], 
                                          300),
                                        c(ee$gpShelfCols, 
                                          ee$gpShelfRows, 
                                          1)) 
  
  # The NetCDF data doesn't check for melt over ocean or high elevation
  # Therefore, we mask out the unchecked pixels (-10) with NA's
  ee$gpShelfPixels[ee$gpShelfPixels == -10] <- NA 
  
  # All the remaining pixels were checked for melt - We are interested in them! 
  # We want a binary shelf/non-shelf grid here, so we turn 0's (non-melt) to 1's
  # We'll now have pixels checked for melt (1) and not checked (NA)
  ee$gpShelfPixels[ee$gpShelfPixels == 0] <- 1
  
  # Transpose binary mask to align back to our R matrices & rasters
  ee$gpShelfPixels <- t(ee$gpShelfPixels)
  
  # For later, also extract NetCDF pixel coordinates within the cropped shelf
  ee$gpShelfLati   <- var.get.nc(ee$nc, "lat",
                                        c(ee$gpShelfTopLeft[2], 
                                          ee$gpShelfTopLeft[1], 
                                          1),
                                        c(ee$gpShelfCols, 
                                          ee$gpShelfRows, 
                                          1))
  ee$gpShelfLong   <- var.get.nc(ee$nc, "lon",
                                        c(ee$gpShelfTopLeft[2], 
                                          ee$gpShelfTopLeft[1], 
                                          1),
                                        c(ee$gpShelfCols, 
                                          ee$gpShelfRows, 
                                          1))
  
  # Close netCDF file for now
  close.nc(ee$nc)
  
  ## Create a bounding box of the shelf for the melt data ----------------------
  # Reshape extracted shelf pixel coordinates to single columns
  ee$gpShelfLongList   <- matrix(ee$gpShelfLong, ncol = 1)
  ee$gpShelfLatiList   <- matrix(ee$gpShelfLati, ncol = 1)
  
  # Combine above coordinates as spatial points
  ee$gpShelfCoordsList <- cbind(ee$gpShelfLongList, 
                                       ee$gpShelfLatiList)
  ee$gpShelfCoords4326 <- SpatialPoints(coords = 
                                                 ee$gpShelfCoordsList)
  
  # Reproject the above spatial points from EPSG:4326 to EPSG:3976
  # NetCDF pixels have lat & long in EPSG:4326 (defined earlier)
  proj4string(ee$gpShelfCoords4326) <- CRS(ee$crs4326)
  
  # We need them in NSIDC polar stereographic (EPSG:3976; defined earlier)
  ee$gpShelfCoords3976 <- spTransform(ee$gpShelfCoords4326, 
                                             CRS(ee$crs3976))
  
  # Create bounding boxes & print to check cropped shelf reprojection is correct
  ee$gpShelfBbox4326 <- ee$gpShelfCoords4326@bbox
  cat("\n___EPSG:4326 Coordinates for", u_shelf, "Bounding Region:\n")
  print(ee$gpShelfBbox4326)
  
  ee$gpShelfBbox3976 <- ee$gpShelfCoords3976@bbox
  cat("\n___EPSG:3976 Coordinates for", u_shelf, "Bounding Region:\n")
  print(ee$gpShelfBbox3976)
  printLine()
  
  ## Create matrices of pixel ID numbers and shelf area ------------------------
  # Calculate each pixel's shelf area (i.e. pixel area within shelf boundary)
  ee$dsShelfPixelIdArea  <- raster::extract(ee$dsShelfIdRaster,
                                                gg$shelf3976,
                                                exact = TRUE, weights = TRUE, 
                                                normalizeWeights = FALSE) %>% 
                                    suppressWarnings()
  
  # Remove pixels with tiny shelf area (<1%)
  # Replicates result of extract function before the raster package was updated
  ee$dsShelfPixelIdArea[[1]][, c(1:2)][ee$dsShelfPixelIdArea[[1]][, 2] < 0.01] <- NA
  rr <- ee$dsShelfPixelIdArea[[1]][, 1][!is.na(ee$dsShelfPixelIdArea[[1]][, 1])]
  ss <- ee$dsShelfPixelIdArea[[1]][, 2][!is.na(ee$dsShelfPixelIdArea[[1]][, 2])]
  ee$dsShelfPixelIdArea <- list(cbind(rr, ss))
  rm(rr, ss)
  
  # Which pixel ID numbers contain shelf area?
  ee$dsShelfIds <- ee$dsShelfIdRaster %in% ee$dsShelfPixelIdArea[[1]][,1]
  
  # Create a new raster of pixel ID numbers to replace with shelf pixel area
  ee$dsShelfPixelAreaRaster <- ee$dsShelfIdRaster
  
  # NA any non-shelf pixels (i.e. when shelf area = 0)
  ee$dsShelfPixelAreaRaster[!ee$dsShelfIds] <- NA 
  
  # Add area of pixels covered by shelf as a percentage of the total pixel area
  ee$dsShelfPixelAreaRaster[ee$dsShelfIds] <- ee$dsShelfPixelIdArea[[1]][,2] * 100
  
  # Create matrix of the pixel shelf area raster (easier for NetCDF extraction) 
  ee$dsShelfPixelArea  <- as.matrix(ee$dsShelfPixelAreaRaster)
  
  # Create a binary mask (shelf/non-shelf) of the pixels (according to MEaSURES)
  ee$dsMeasuresShelfMask <- ee$dsShelfPixelArea
  ee$dsMeasuresShelfMask[!is.na(ee$dsMeasuresShelfMask)] <- 1
  
  # Create a mask that combines MEaSURES pixels & NetCDF meltable shelf pixels
  ee$dsShelfMask <- ee$dsMeasuresShelfMask * ee$gpShelfPixels
  
  # Create the above shelf mask as a matrix & apply to the pixel ID & pixel area
  ee$dsShelfPixelId   <- as.matrix(ee$dsShelfIdRaster)
  ee$dsShelfIdMask    <- ee$dsShelfPixelId   * ee$dsShelfMask
  ee$dsShelfPixelArea <- ee$dsShelfPixelArea * ee$dsShelfMask
  
  ## Display to confirm alignment ----------------------------------------------
  # Define bounding box: add 1/2 pixel width as Picard's points are grid centres
  # Define a bounding box for display purposes (add half pixel to align centre)
  ee$gpShelfMinX3976 <- ee$gpShelfBbox3976[1, 1] - ee$halfPixel
  ee$gpShelfMaxX3976 <- ee$gpShelfBbox3976[1, 2] + ee$halfPixel
  ee$gpShelfMinY3976 <- ee$gpShelfBbox3976[2, 1] - ee$halfPixel
  ee$gpShelfMaxY3976 <- ee$gpShelfBbox3976[2, 2] + ee$halfPixel
  
  # Create an empty raster to hold values for plotting purposes
  ee$dsShelfRaster <- raster(ncol = ee$gpShelfCols, 
                                 ymn  = ee$gpShelfMinY3976, 
                                 ymx  = ee$gpShelfMaxY3976,
                                 nrow = ee$gpShelfRows, 
                                 xmn  = ee$gpShelfMinX3976, 
                                 xmx  = ee$gpShelfMaxX3976)
  
  # Reproject the new empty raster
  projection(ee$dsShelfRaster) <- "+init=EPSG:3976"
  cat("\n___Newly Created Cropped Shelf Raster Information: \n")
  cat("___Check the resolution is", ee$pixelResolution, 
      "and crs (3976) are as expected!\n")
  print(ee$dsShelfRaster)
  
  # Plot pixels Prof. Picard's NetCDF checks for melt (non-ocean, low elevation)
  values(ee$dsShelfRaster) <- ee$gpShelfPixels
  plotShelf(ee$dsShelfRaster,
            main = paste0("Potential Melt Pixels for ", 
                          ff$versionInfo, " : ", ii),
            sub = "Locations assessed for melt",
            col = paste0(gg$kulaQ[1], "AA"), 
            zlim = c(0, 1))
  
  # Plot pixels which contain at least part of the MEaSURES-defined shelf area
  values(ee$dsShelfRaster) <- ee$dsMeasuresShelfMask
  plotShelf(ee$dsShelfRaster,
            main = paste0("MEaSURES Shelf Pixels for ", ff$versionInfo),
            sub = "Pixels containing MEaSURES-defined ice shelf area",
            col = gg$kulaQ[1], 
            zlim = c(0, 1))
  
  # Plot pixels checked for melt & that are within MEaSURES boundaries
  values(ee$dsShelfRaster) <- ee$dsShelfIdMask
  plotShelf(ee$dsShelfRaster,
            main = paste0("Final Shelf Pixels for ", 
                          ff$versionInfo, " : ", ii),
            sub = paste("Pixels containing MEaSURES shelf and melt-assessed"),
            col = gg$kulaD)
  printLine()
  
  # Chunk 3: Extract Binary Melt Data from NetCDF ==============================
  if (exists("u_testScript")) {
    cat("\n Only extracting the first year of data as a quick test")
  }
  cat("\n Extracting Melt Pixel ID Numbers for", 
      u_shelf, "(", ff$versionInfo, ii, ") \n",
      "This may take a few minutes... \n\n",
      "... \n ... \n ... \n\n")
  
  ## Extract melting pixel ids each day ----------------------------------------
  ee$nc     <- open.nc(ee$ncFileName)
  
  # Apply function to extract the ID numbers of the pixels that melt on each day
  # Returned as a list of lists, each sub-list representing a single day's melt
  ee$idList <- lapply(1:ee$gpDataLength,    # Day 0 is index 1 & so on
                         topLeft     = ee$gpShelfTopLeft, 
                         columnCount = ee$gpShelfCols, 
                         rowCount    = ee$gpShelfRows,
                         idMask      = ee$dsShelfIdMask,
                         ncData      = ee$nc,
                         FUN         = extractMeltingIds)
  close.nc(ee$nc)
  
  # Rename sub-lists as their "Picard Day" (# of days since dataset started)
  names(ee$idList) <- paste0(ff$versionInfo, "_pTime_", ee$gpDays)
  
  # Chunk 4: Tidying Up & Saving Extracted Data ================================
  # Rename variables for easier use outside this script
  ee$meltList    <- ee$idList           # list of daily lists of IDs for melt pixels
  ee$bboxLati    <- ee$gpShelfLati      # matrix of the shelf's pixel latitudes
  ee$bboxLong    <- ee$gpShelfLong      # matrix of the shelf's pixel longitudes
  ee$shelfIds    <- ee$dsShelfIdMask    # matrix of IDs for pixels with shelf area
  ee$shelfMask   <- ee$dsShelfMask      # binary matrix of pixels with shelf area
  ee$shelfAreas  <- ee$dsShelfPixelArea # matrix of the pixel area covered by shelf
  ee$shelfRaster <- ee$dsShelfRaster    # raster (EPSG:3976) for easy value display
  
  # Create a subfolder to store it
  ff$subFolder   <- createSubFolders(ff$interimPath, 
                                        paste(ff$versionInfo, 
                                              ii, sep = "_"))
  
  ff$fileName    <- paste0(ff$subFolder, "/", ff$versionInfo, "_", ii)   
  
  # Save files
  save(meltList,    envir = ee, file = paste0(ff$fileName, "_meltList.rData"))
  save(bboxLati,    envir = ee, file = paste0(ff$fileName, "_shelfLat.rData"))
  save(bboxLong,    envir = ee, file = paste0(ff$fileName, "_shelfLon.rData"))
  save(shelfIds,    envir = ee, file = paste0(ff$fileName, "_shelfIds.rData"))
  save(shelfMask,   envir = ee, file = paste0(ff$fileName, "_shelfMask.rData"))
  save(shelfAreas,  envir = ee, file = paste0(ff$fileName, "_shelfAreas.rData"))
  save(shelfRaster, envir = ee, file = paste0(ff$fileName, "_shelfRaster.rData"))
  
  # Upon completion of each sensor used - print info
  ee$runningTime <- proc.time() - ee$startTime
  print(ee$runningTime)
  cat("\n Saved output to", ff$subFolder, "\n\n", 
      "Script dt02 complete for", ii, "! \n\n")
  beep("ping")
}

# Finished for all sensors
rm(ii)
printLine()
cat("\n Script dt02 complete for", ff$versionInfo, "! \n\n")
