## ############################ ##
## su02_define_raw_data_paths.R ##
## ############################ ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Define the file paths and names for the raw data
#
# Comments: - You need to download the datasets yourself, and then set the file 
#             paths to them here
#           - The melt data will be made available upon acceptance of the final
#             manuscript
#           - RACMO2.3p3 data is publicly available online from: 
#                     https://zenodo.org/record/5512077#.Ylz4t-hBxhF
#           - MEaSURES data is publicly available online from:
#                     https://nsidc.org/data/nsidc-0709 
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# SET FILE PATHS ###############################################################
# Create an environment just for file / folder paths
ff <- new.env()

# Where is raw data stored?
ff$rawDataFolder  <- ("../../Data") 

# Where is the raw melt data stored?
ff$rawMelt  <- paste0(ff$rawDataFolder, "/PicardMelt/")

# What are the file names for the raw melt data?
ff$rawAMSRe <- paste0(ff$rawMelt, "dataAMSR/CumJour-amsre-2002-2011-H19.nc")
ff$rawAMSR2 <- paste0(ff$rawMelt, "dataAMSR/CumJour-amsr2-2012-2021-H19.nc")
ff$rawSSMIS <- paste0(ff$rawMelt, "dataSSMIS/CumJour-ssmi-1979-2021-H19.nc")

# Don't change this line
ff$rawPaths <- c(ff$rawAMSRe, ff$rawAMSR2, ff$rawSSMIS)

# Where are the raw MEaSURES shelf boundaries?
ff$rawShelf <- paste0(ff$rawDataFolder, 
                      "/MEaSURES Boundaries/shelves/",
                      "IceShelf_Antarctica_v02.shp") 

# Where is the RACMO2.3p3 data (from van Dalum et al., 2021)?
ff$rawRacmo <- paste0(ff$rawDataFolder, "/RACMO/R2.3p3/CON_data/")
