## ############# ##
## su01_set_up.R ##
## ############# ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    Runs the necessary scripts for the data processing
#
# Comments: - You shouldn't need to do anything here
#           ! But you can overwrite user settings (u_variable) instead of having
#             to change them individually in each script
#             - u_variables declared in su11 would overwrite those here
#           - This is called in all data and analysis scripts
#
# Updates:
# 2022/05/19  v1.0  Created a tidier version of the script to share
#

# OVERWRITE USER SETTINGS ######################################################
# Optional; uncomment to overwrite any script settings
# u_shelf    <- "Hull"
# u_version  <- "v01"
# u_sensors  <- c("amsre", "amsr2")

# Add a userName to test that the functions are being called correctly
u_userName <- "Dominic"

# RUN SET UP SCRIPTS ###########################################################
source("R/setUp/su02_define_raw_data_paths.R")
source("R/setUp/su03_prep_libraries_and_functions.R")
source("R/setUp/su04_set_global_options.R")
source("R/setUp/su05_create_folders.R")
source("R/setUp/su06_load_interim_data.R")

# Create a new environment to keep the script output / variables tidy
ee <- new.env()
