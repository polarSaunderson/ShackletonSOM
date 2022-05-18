## ############################## ##
## dt07_extract_racmo_variables.R ##
## ############################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Extract data from the latest RACMO2.3p3 output by van Dalum et al.
#             (2021; 2022)
#
# Comments: - Run this script to create the data used in Fig. 6e and Fig. 7;
#               - This script just extracts and preps the data
#               - Use scripts an09 and an10 to create those figures
#           - This script:
#               - Crops the RACMO data to the selected shelf (using MEaSURES) 
#               - Calculates the average cumulative summer total for the chosen 
#                 variable on a perPixel basis across all summers requested
#                 - Can choose which months are included as "summer"
#               - Creates plots as it goes to check
#           - Fluxes are divided by 10^6 to store in millions 
#           - As cumulative totals are created, the temperature and albedo need 
#             to be averaged when used elsewhere to have a physical meaning 
#             (i.e. albedo = 1.7 means nothing)
#           - Data is stored as a geotiff in "Data/version/racmo/"
#           - The CRS is hardcoded in because terra does not read it correctly 
#             from the netCDF
#           - RACMO2.3p3 data is publicly available online from: 
#                     https://zenodo.org/record/5512077#.Ylz4t-hBxhF
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

# Which sensor should the data be plotted for?
# Options: snowmelt, smb, precip, subl, t2m, albedo, swsd, lwsd, senf, latf
u_variable <- "t2m"

# Which month should be included in the averages?
u_summerMonths <- c(11, 12, 1, 2)          # for an09
u_summerMonths <- c(12, 1)                 # for an10 & an11

# Which summers should be included?
u_summers      <- c(2003:2011, 2013:2018)  # for an11
u_summers      <- c(2003:2018)             # for an09 & an10

# Save the mean raster, or save all included months as separate layers?
u_meanRaster   <- TRUE                     # an09 & an10 TRUE; an11 FALSE

# Set-Up #######################################################################
source("R/setUp/su01_set_up.R") # Global variables, filepaths, functions

# Read in the correct netCDF file
if (u_variable == "t2m") {
  ff$ncRacmo <- paste0(ff$rawRacmo, "/", u_variable, 
                    "_monthlyA_ANT27_CONsettings_197901_201812.nc") # use if T2m
} else if (u_variable != "albedo") {
  ff$ncRacmo <- paste0(ff$rawRacmo, "/", u_variable, 
                    "_monthlyS_ANT27_CONsettings_197901_201812.nc")
} else { # there is no albedo file, so pretend its swsd for now, and calc later
  ff$ncRacmo <- paste0(ff$rawRacmo, "/", "swsd", 
                    "_monthlyS_ANT27_CONsettings_197901_201812.nc")
}

# Use different colours for different variables too
kulaS <- switch(u_variable, 
                snowmelt = colour("YlOrBr")(11)[2:10],
                smb      = colour("BuRd")(20)[c(9, 11:20)],
                precip   = colour("devon")(14)[13:2], # no white
                subl     = colour("acton")(11),
                t2m      = colour("nuuk")(14),
                albedo   = colour("tokyo")(15)[-c(1, 2, 14, 15)], # need 11
                swsd     = colour("lajolla")(16)[-c(1:2, 14:16)],
                lwsd     = colour("lajolla")(16)[-c(1:3, 14:16)],
                senf     = colour("BuRd")(18)[-c(1, 8, 9, 10, 11, 18)],
                senf     = colour("BuRd")(24)[-c(1, 8, 9, 10, 11, 18)],
                latf     = colour("BuRd")(24)[c(1:11)])

# _____CODE_____ ###############################################################
# Chunk 1: Prep for Data Extraction ============================================
# Retrieve dates of the RACMO data
ee$racmoData  <- open.nc(ff$ncRacmo)
ee$racmoTime  <- as.Date("1950-01-01") + var.get.nc(ee$racmoData, "time")
close.nc(ee$racmoData)
ee$dataMonths <- 1:dim(ee$racmoTime)

# Read netCDF data in as a raster; calculate albedo
if (u_variable != "albedo") {
  ee$racmo <- terra::rast(ff$ncRacmo)
} else { # need to calculate albedo
  # swIn
  ee$swsd <- paste0(ff$rawRacmo, "swsd", 
                 "_monthlyS_ANT27_CONsettings_197901_201812.nc")
  # swOut
  ee$swsu <- paste0(ff$rawRacmo, "swsu", 
                 "_monthlyS_ANT27_CONsettings_197901_201812.nc")
  
  # albedo
  ee$racmo <- (terra::rast(ee$swsu) * -1) / terra::rast(ee$swsd)
}

# terra doesn't read the crs for some reason, so add it explicitly
crs(ee$racmo) <- paste("+proj=ob_tran +o_proj=longlat +o_lat_p=-180.0", 
                    "+lon_0=10.0 -m 57.295779506")

# It's easier to discuss fluxes in millions
if (u_variable %in% c("swsd", "lwsd", "senf", "latf")) {
  ee$racmo <- ee$racmo / 1e6 
}

## Crop to shelf ---------------------------------------------------------------
# Get shelf boundary & reproject it to match racmo data
ee$shelves <- terra::vect(ff$rawShelf)
ee$shelf   <- ee$shelves[ee$shelves$NAME == u_shelf]
ee$shelf   <- terra::project(ee$shelf, ee$racmo)

# Crop data to shelf bounds
ee$racmo <- terra::crop(ee$racmo, ee$shelf)

# Display the output as a test!
plot(subset(ee$racmo, 2), col = kulaS, main = "Crop Test")
lines(ee$shelf)

# Chunk 2: Calculate average summer totals =====================================
# Set plot area for checking the monthly data
layout(matrix(1:(2 * length(u_summerMonths)), 
              nrow = 2, byrow = FALSE))

# for each month of summer,
for (ii in u_summerMonths) {
  # indexing 
  jj <- which(u_summerMonths == ii)
  
  # Which months are being included? Get their index
  ee$incMonths <- ee$dataMonths[ee$dataMonths %% 12 == 0] - 12 + ii
  ee$incSummers <- floor(ee$incMonths / 12)
  
  # Find summer index for each of the included incMonths
  if (ii %in% 4:11) {
    ee$incSummers <- ee$incSummers + 1
  }
  
  # Only include summers we want to include
  ee$incMonths <- ee$incMonths[ee$incSummers %in% (u_summers - 1979)]
  
  # Clarify months are correct via the console
  printLine(width = 1)
  cat("Displaying the average of \n\n")
  print(ee$racmoTime[ee$incMonths])
  
  # Get the required month across all the necessary summers
  ee$monthValue <- terra::subset(ee$racmo, ee$incMonths + 1)
  
  # Keep only the mean, or all of the extracted months?
  if (isTRUE(u_meanRaster)) {
    # Calculate the mean
    ee$monthValue <- ee$monthValue %>% mean()
    
    # Plot to check
    terra::plot(ee$monthValue, col = kulaS, main = paste0("Month: ", ii))
    lines(ee$shelf)
  }
  
  # Add to summer running total
  if (jj == 1) {
    ee$summerValue <- ee$monthValue
  } else {
    ee$summerValue <- ee$summerValue + ee$monthValue
  }
  
  # Plot running total of the summer
  if (isTRUE(u_meanRaster)) {
    terra::plot(ee$summerValue, col = kulaS, main = "Running Total")
    lines(ee$shelf)
  }
}

# Chunk 3: Saving ==============================================================
if (isTRUE(u_meanRaster)) {
  ee$tifEnding <- ".tif"
} else {
  ee$tifEnding <- "_each.tif"
}

# Create a subfolder to store racmo data
createSubFolders(folderPath = ff$versionPath, subFolderName = "racmo") %>% 
  suppressWarnings() # we don't need to know if it already exists each time

# Write raster to file as a geotiff
terra::writeRaster(ee$summerValue,
                   paste0(paste("Data", ff$versionInfo, "racmo/", sep = "/"),
                          paste("racmo2",
                                u_summerMonths[1], last(u_summerMonths),
                                u_variable, sep = "_"), ee$tifEnding))

# Upon completion
printLine()
cat(" Successfully extracted racmo", u_variable, "for", u_summerMonths, 
    "\n\n Script dt07 complete! \n\n")
beep("ping")
