## ######################## ##
## su06_load_interim_data.R ##
## ######################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Load any data already been created for use in later scripts
#
# Comments: - You shouldn't need to do anything here
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# LOAD INTERIM DATA ############################################################
# Load shelf outline
shelfLoadAttempt <- try(base::load(
  paste0(ff$analysePath, ff$versionInfo, "_shelf3976.rData"), envir = gg),
  silent = TRUE) %>% suppressWarnings()

# Warn if the shelf outline doesn't exist yet
if (shelfLoadAttempt != "shelf3976") {
  cat("Warning: You have not created the shelf outline yet! \n")
  printLine()
}
rm(shelfLoadAttempt)

# Load commonMasked interim data if it exists for the later data prep files
ff$interimData <- list.dirs(ff$interimPath, full.names = FALSE)
ff$commonData  <- paste0(ff$versionInfo, "_commonMask")

if (ff$commonData %in% ff$interimData) {
  # Create a separate environment to hold ready toAnalyse data
  dd <- new.env()
  
  # Load & prep interim data
  loadFullFolder(paste0(ff$interimPath, ff$versionInfo, "_commonMask"), dd)
  
  # Load & prep toAnalyse data
  loadFullFolder(ff$analysePath, dd)
}
