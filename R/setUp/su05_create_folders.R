## ##################### ##
## su05_create_folders.R ##
## ##################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Create the folders to store the interim and toAnalyse data in
#
# Comments: - You shouldn't need to do anything here
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# _____CODE_____ ###############################################################
# Load available shelf names
base::load("R/setUp/shelfNames.RData", envir = gg)

# Create Necessary Data Folders
if (u_shelf %in% gg$allShelves) {               # if a legitimate shelf
  if (paste0(u_version, "_", u_shelf) %!in% 
      list.files("Data")) {                      # if doesn't exist,
    createVersionFolders(u_version, u_shelf)      # create folders
  } else {                                       # else   
    defineDataPaths(u_version, u_shelf, ff)       # return ff$filenames for use
  }
} else {                                         # else
  cat("\nAvailable Shelves:\n")                   # warn the user
  print(gg$allShelves)
  stop("Shelf not found. Please try again with a shelf listed above!")
}
