## ################################### ##
## su03_prep_libraries_and_functions.R ##
## ################################### ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Load the necessary libraries and custom functions
#
# Comments: - You shouldn't need to do anything here
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# LOAD LIBRARIES ###############################################################
library(beepr)         # notification sound for end of script
library(exactextractr) # for extracting from raster according to polygons
library(khroma)        # for colours
library(magrittr)      # for piping
library(raster)        # for rasters and spatial extents
library(rgdal)         # for projections and transformations
library(RNetCDF)       # for interacting with netCDF files
library(sp)            # for all things spatial

# LOAD FUNCTIONS ###############################################################
customFunctions <- list.files("R/Functions")
for (ii in customFunctions) {
  source(paste0("R/Functions/", ii))
}
rm(ii, customFunctions)

# Check that the scripts are reading each other and functions are being called 
testFunction(u_userName)
