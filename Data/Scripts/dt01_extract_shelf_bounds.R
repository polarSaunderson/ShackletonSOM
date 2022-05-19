## ########################### ##
## dt01_extract_shelf_bounds.R ##
## ########################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Extract, reproject and save the MEaSURES shelf boundary for the
#             selected shelf
#
# Comments: - The shelf outline is necessary in many of the other scripts so we
#             isolate and prepare it first
#           - Use the proper name from MEaSURES for the shelf; using an 
#             incorrect name will print a list of legitimate shelves that can be
#             used to the console
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

# Set-Up #######################################################################
source("R/setUp/su01_set_up.R") # Global variables, filepaths, functions, etc.

# _____CODE_____ ###############################################################
# Load Raw MEaSURES Boundaries
cat("___MEaSURES Dataset: \n")    
ee$shelfBounds  <- ff$rawShelf %>% readOGR()
printLine()

# Catch if there's an error with the shelf name input, else select shelf
if (u_shelf %!in% gg$allShelves) {
  cat("\nAvailable Shelves:\n")
  print(ee$shelfBounds$NAME)
  stop("Shelf not found. Try again with one of the shelves listed above!")
} else {
  # Alert user to progress
  cat("\n   Extracting the", u_shelf, "ice shelf... \n\n\n")
  
  # Isolate shelf - only works with a single MEaSURES shelf 
  ee$shelf <- ee$shelfBounds[ee$shelfBounds$NAME == u_shelf, ] 
}

# Transform Shelf Outline's CRS to EPSG:3976 to match melt data
ee$crs3976   <- CRSargs(CRS("+init=epsg:3976")) # Define 3976
ee$shelf3976 <- spTransform(ee$shelf, CRS(ee$crs3976))

# Plot to verify the shelf is correct
plot(ee$shelf3976, main = u_shelf, axes = TRUE)

# Save
ee$shelfFile <- paste0(ff$analysePath, ff$versionInfo, "_shelf3976.rData")
save(shelf3976, envir = ee, file = ee$shelfFile)

# Display saved files (names & locations)
cat("Shelf Boundary Saved : \n\n",
    ee$shelfFile,  "\n")

# Finished!
printLine()
cat("\nScript dt01 complete \n")
beep("ping")
