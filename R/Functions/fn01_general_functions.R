## ######################## ##
## fn01_general_functions.R ##
## ######################## ##

# SCRIPT OVERVIEW ##############################################################
# Author:     Dominic Saunderson      [ dominicSaunderson@gmail.com ]
#
# Purpose:    General purpose custom R functions.
#
# Comments: - These functions were created whilst developing the code for the
#             ShackletonSOM project; they should be relatively transferable, but
#             they have not been tested elsewhere yet.
#           - For more info on these functions, see them below or use docstring,
#             like this: docstring(functionName). Basic documentation will then  
#             appear as it does when using the normal '?' help.
#
# Updates:
# 2022/05/09  v1.0  Created a tidier version of the script
#

# _____FUNCTIONS_____ ##########################################################
# Which functions are in this file?
#-------------------------------------------------------!
#   addColourBar               [   Plot / Layout    ]    
#   setLayoutBar               [   Plot / Layout    ]
#-------------------------------------------------------!
#   addNorthArrow              [   Raster Plotting  ]
#   addPlotScale               [   Raster Plotting  ]
#   annotateShelfPlot          [   Raster Plotting  ]
#   getTrueNorth               [   Raster Plotting  ]
#   rotate4plot                [   Raster Plotting  ]
#-------------------------------------------------------!
#   createSubFolders           [   Files & Folders  ]
#   createVersionFolders       [   Files & Folders  ]
#   defineDataPaths            [   Files & Folders  ]
#   loadFullFolder             [   Files & Folders  ]
#   saveAs                     [   Files & Folders  ]
#-------------------------------------------------------!
#   filterData                 [   Data Wrangling   ]
#   notIn (%!in%)              [   Data Wrangling   ]
#   substringEnd               [   Data Wrangling   ]
#-------------------------------------------------------!
#   fresh                      [   Miscellaneous    ]
#   here                       [   Miscellaneous    ]
#   last                       [   Miscellaneous    ]
#   printLine                  [   Miscellaneous    ]
#   printScriptStart           [   Miscellaneous    ]
#   testFunction               [   Miscellaneous    ]
#-------------------------------------------------------!

# _____CODE_____ ###############################################################
##
addColourBar <- function(col, 
                         zlim = c(0, 1),
                         title = "",
                         cex.title = 1.75,
                         cex.labels = 1.75,
                         allLabels = TRUE,
                         mar = c(3, 6, 3, 6),
                         aboveBelow = "neither",
                         horiz = FALSE,
                         extraCol = FALSE,
                         type = "S",
                         tickSignif = 3,
                         labelOffset = 1.5){
  #' Add a colour bar to the plotting area
  #' 
  #' @description Plots a new figure that is block coloured to display a scale.
  #' Call this after setting the plot layout (par(mfrow)) to plot alongside the 
  #' figure. The sizing is done with the mar argument. 
  #' 
  #' @param col colour palette: Which colours are in the colour bar?
  #' @param zlim vector: Minimum & maximum values for the colour bar
  #' @param mar vector: How large are the margins around the colour bar? Shape!
  #' @param horiz binary: Should the bar be horizontal? Default is FALSE
  #' @param title "string": What is the colour bar showing?
  #' @param cex.title numeric: Size of the bar title; default is 1.75
  #' @param labelOffset numeric: How far from the bar to the title? Default 1.5
  #' @param allLabels: "string": Default adds all labels; "odds" shows every 2nd
  #' label, and "minMax" shows only the extreme z-limits
  #' @param aboveBelow "string": move the extreme zlimits and add a '<=' for 
  #' values "below" those in the colour bar, '>=' for those "above" the maximum; 
  #' "both", surprisingly, adds both! Default is neither
  #' @param extraCol "string": What colour/s should be used to show areas beyond
  #' the z-limits; necessary if aboveBelow is being used. See example!
  #' @param type "string": Is it a Sequential ("S") or Qualitative ("Q") bar?
  #' @usage ## Basic use
  #'        addColourBar(gg$gg$kulaS, 
  #'                     u_zlim, 
  #'                     mar = c(2, 5, 2, 9), 
  #'                     title = "Example", 
  #'                     labelOffset = 2)
  #'        
  #' @usage ## Using aboveBelow (example for "below")
  #'
  #'        # Create mock data
  #'        xx <- matrix(1:100, nrow = 10, byrow = TRUE)
  #'        print(xx)
  #'        
  #'        # rotate plot so that values align with printed to check
  #'        x2 <- rotate4Plot(xx) 
  #' 
  #'        # Set up the colours - requires library(khroma)
  #'        gg$kulaS <- colour("tokyo")(6)[-c(1, 6)]
  #'        gg$kulaE <- colour("tokyo")(6)[c(1, 6)]
  #' 
  #'        # Define z-limits - essentially data below 70 becomes irrelevant
  #'        u_zlim <- c(70, 100)
  #' 
  #'        # Create a plot background
  #'        par(mfrow = c(1, 2))
  #'        image(x2, col = gg$kulaE)
  #' 
  #'        # Add the useful information
  #'        image(x2, col = gg$kulaS, zlim = u_zlim, add = TRUE)
  #' 
  #'        # Add our colour bar, using "below" option and an extra colour
  #'        addColourBar(gg$kulaS, u_zlim,
  #'                     aboveBelow = "below", 
  #'                     extraCol = gg$kulaE,
  #'                     mar = c(3, 10, 3, 10),
  #'                     labelOffset = -1,
  #'                     cex.title = 4,
  #'                     title = "¡Example!")
  
  # Code ----------------------------------------------------------------------!
  # Catch if there is no extra colour for padding
  if (aboveBelow != "neither" & isFALSE(extraCol)) {
    stop("\n\n   You need to provide 1 or 2 extra colours here! \n")
  }
  
  # Create the new colour list if necessary
  if (aboveBelow == "below") {
    col <- c(extraCol[1], col)
  } else if (aboveBelow == "above") {
    col <- c(col, extraCol[1])
  } else if (aboveBelow == "both") {
    if (length(extraCol) == 2) {
      col <- c(extraCol[1], col, extraCol[2])
    } else {
      col <- c(extraCol, col, extraCol)
    }
  }
  
  # How many colours are there? Important for... everything! Columns!
  numOfCols <- length(col)
  
  # Nudge everything a bit so the tick marks sit at true y limits
  nudge     <- 0.5 / (numOfCols - 1)
  yMin      <- 0 - nudge
  yMax      <- 1 + nudge
  
  # Create & plot colour bar
  colMatrix  <- matrix(c(1:numOfCols), nrow = 1)
  if (isTRUE(horiz)) {
    colMatrix <- t(colMatrix)
  }
  par(mar = mar)
  image(colMatrix, 
        col  = col,
        axes = FALSE)
  
  ## Labelling ----------------------------------------------------------------!
  # Where should the labels be placed? 
  labelsAt <- seq(yMin, yMax, length.out = numOfCols + 1)
  forLine  <- labelsAt
  # Need offsetting if using aboveBelow
  if (aboveBelow == "below") {
    labelsAt <- labelsAt[-1]
  } else if (aboveBelow == "above") {
    labelsAt <- labelsAt[-(numOfCols + 1)]
  } else if (aboveBelow == "both") {
    labelsAt <- labelsAt[c(-1, -(numOfCols + 1))]
  }
  
  # Create the label values to display
  labels <- seq(zlim[1], zlim[2], length.out = length(labelsAt))
  
  # All labels, or only the extremes? 
  if (isTRUE(allLabels)) {
    labels    <- labels %>% round(3)
  } else if (allLabels == "minMax") {
    labels    <- c(zlim[1], labels[ceiling(length(labels) / 2)], zlim[2])
    labelsAt  <- labelsAt[c(1, ceiling(length(labelsAt) / 2), length(labelsAt))]
  } else if (allLabels == "minMax2") {
    labels    <- c(zlim[1], zlim[2])
    labelsAt  <- labelsAt[c(1, length(labelsAt))]
  } else if (allLabels == "odds") {
    # Needs an odd number of labels to select every 2nd one: handle if not!
    if (length(labelsAt) %% 2 != 1) { # if not odd...
      if (aboveBelow == "neither") {
        # NEITHER - first, last, odds from 3
        labelsAt <- c(labelsAt[which(labelsAt == labelsAt) %% 2 == 1],
                      labelsAt[length(labels)])
        labels   <- c(labels[which(labels == labels) %% 2 == 1],
                      labels[length(labels)])
      } else if (aboveBelow == "above") {
        # ABOVE - first, last, odds from 3
        labelsAt <- c(labelsAt[which(labelsAt == labelsAt) %% 2 == 1],
                      labelsAt[length(labelsAt)])
        labels   <- c(labels[which(labels == labels) %% 2 == 1],
                      labels[length(labels)])
      } else if (aboveBelow == "both") {
        # BOTH - first, last, even from 2
        labelsAt <- c(labelsAt[1], 
                      labelsAt[which(labelsAt == labelsAt) %% 2 != 1])
        labels   <- c(labels[1], 
                      labels[which(labels == labels) %% 2 != 1])
      } else if (aboveBelow == "below") {
        # BELOW - first, last, evens from 2
        labelsAt <- c(labelsAt[1], 
                      labelsAt[which(labelsAt == labelsAt) %% 2 != 1]) 
        labels   <- c(labels[1], 
                      labels[which(labels == labels) %% 2 != 1])
      }
    } else {
      labels    <- labels[which(labels == labels) %% 2 == 1]
      labelsAt  <- labelsAt[which(labelsAt == labelsAt) %% 2 == 1]
    }
  }
  
  # Centre tick mark for qualitative colour bars
  if (type == "Q") {
    labelsAt  <- seq(0 - nudge, 1 + nudge,
                     length.out = numOfCols + 1) + nudge
    labels    <- seq(zlim[1], zlim[2] + 1, 1)
    if (length(labels) != length(labelsAt)) {
      stop("\n\nThe number of classes and colours don't match! \n\n")
    }
  }
  
  # Round if crazy values
  labels <- signif(labels, tickSignif)
  
  ### Add proper >= or <= signs if using aboveBelow ---------------------------!
  # A length variable eases things ahead!
  xL <- length(labels) 
  
  # # Add > or < sign to the label
  if (aboveBelow == "above") {
    labels[xL] <- parse(text = deparse(bquote("" >= .(labels[xL]))))
  } else if (aboveBelow == "below") {
    labels[1]  <- parse(text = deparse(bquote("" <= .(labels[1]))))
    # labels[1]  <- paste(">", labels[1])
  } else if (aboveBelow == "both") {
    # Deparse the value
    labels[1]  <- (deparse(bquote("" <= .(labels[1]))))
    labels[xL] <- (deparse(bquote("" >= .(labels[xL]))))
    
    # And then parse it back, so that it works - not sure why!
    labels[xL] <- parse(text = labels[xL])
    labels[1]  <- parse(text = labels[1])
  } else if (aboveBelow == "aboveMin") {
    labels[1]  <- paste(">", labels[1])
  }
  
  ### Display labels & titles - prettify! -------------------------------------!
  # Add our new numeric labels to the bar
  if (!isTRUE(horiz)) {
    axis(side   = 4, 
         at     = forLine,
         labels = FALSE,
         lwd.ticks   = FALSE, 
         las    = 2)
    axis(side   = 4, 
         at     = labelsAt,
         labels = labels,
         cex.axis = cex.labels,
         las    = 2)
  } else {
    axis(side   = 1, 
         at     = forLine,
         lwd.ticks = FALSE, 
         labels = FALSE,
         las    = 2)
    axis(side   = 1,
         at     = labelsAt,
         labels = labels,
         cex.axis = cex.labels,
         las    = 1)
  }
  
  # Add title
  barPos <- par("usr")
  par(xpd = TRUE)
  if (!isTRUE(horiz)) {
    text(x = barPos[2] + labelOffset, 
         y = mean(barPos[3:4]),
         title,
         srt = 270, cex = cex.title)
  } else {
    text(x = mean(barPos[1:2]),
         y = barPos[3] - labelOffset,
         title,
         srt = 0, cex = cex.title)
  }
  par(xpd = FALSE)
}

##
addNorthArrow <- function(rasterIn, 
                          placeH = 65, placeV = 92.5,
                          arrowLength = 20, 
                          endLength = 0.15, 
                          crs = 3976,
                          lwd = 1.5, col = "#000000", 
                          nOffset = 1.5, nSize = 0.8){
  #' Add a north arrow to the shelf raster. 
  #'
  #' @description This calls getTrueNorth for the input raster, and manipulates 
  #' the result to add the arrow to the raster. Call this immediately after 
  #' plotting the raster. The default position and length can be adjusted.
  #'
  #' @param rasterIn raster: Which raster needs a north arrow?
  #' @param lwd numeric: How thick should the arrow be?
  #' @param placeH numeric: Where should the horizontal origin of the arrow be, 
  #' as a percentage of the raster width?
  #' @param placeV numeric: Where should the vertical origin of the arrow be, 
  #' as a percentage of the raster height?
  #' @param nOffset numeric: How far from the arrow end should the N be, as a 
  #' percentage of the raster height?
  #' @param nSize numeric: cex value for the N label
  #' @param arrowLength numeric: How long should the arrow be?
  #' @param endLength numeric: How long should the arrow end "v"-shape be?
  #' @param col "string": Colour code for the arrow
  #' @usage addNorthArrow(raster4plotting, 2, "#343412")
  
  # Code ----------------------------------------------------------------------!
  # Set Up
  gridExtents <- raster::extent(rasterIn)
  xVal <- gridExtents[c(1:2)]
  yVal <- gridExtents[c(3:4)]
  
  # Raster centre & bounding
  yMid <- mean(yVal)
  xMid <- mean(xVal)
  xMin <- min(xVal)
  yMin <- min(yVal)
  xMax <- max(xVal)
  yMax <- max(yVal)
  
  # Get angle (from the raster centre)
  angle <- getTrueNorth(x = xMid,
                        y = yMid,
                        crs = crs)
  
  # Turn to radians for tan
  angle <- angle %>% 
    `*`(-1) %>%     # function output is anticlockwise
    `-`(90) %>%     # for triangle's tan angle, not from straight up 
    `/`(57.2957795) # convert degrees to radians
  
  # Get the relative end coordinates of the arrow
  xOff <- (xMax - xMin) * (arrowLength / 100)
  yOff <- tan(angle) * xOff
  
  # We don´t want the arrow in the centre; shift it (relative to bottom left)
  xShift <- (xMax - xMin) * (placeH / 100)
  yShift <- (yMax - yMin) * (placeV / 100)
  
  # Final coordinates, from x0,y0 to x1,y1
  x0 <- xMin + xShift
  y0 <- yMin + yShift
  x1 <- xMin + xOff + xShift
  y1 <- yMin + yOff + yShift
  
  # Add the arrow!
  arrows(x0 = x0,
         y0 = y0,
         x1 = x1,
         y1 = y1,
         length = endLength,
         code = 2,
         lwd = lwd)
  
  # Draw the big "N"
  nOffset <- (yMax - yMin) * (nOffset / 100) # scale to the plot
  text(x = x1 + nOffset,
       y = y1,
       "N", cex = nSize)
}

##
addPlotScale <- function(raster, barLength, labelOffset = 6, 
                         placeV = 2.5, placeH = 75, label = barLength){
  #' Add a scale bar to an existing raster.
  #'
  #' @param raster raster: Which raster is the bar corresponding to?
  #' @param barLength numeric: How many kilometres should the bar represent? 
  #' @param placeH numeric: How far across the plot should the bar be? (as a %)
  #' @param placeV numeric: How far up the plot should the bar be? (as a %)
  #' @param labelOffset numeric: How far from the bar should the label be? 
  #' @usage addPlotScale(raster4plotting, barLength = 25)
  
  # Code ----------------------------------------------------------------------!
  # Get raster extents
  mapExtents <- raster@extent
  
  # Map edges
  xx <- mapExtents[1]
  yy <- mapExtents[3]
  
  # We give it as percentage; scale to fraction
  placeH <- placeH / 100
  placeV <- placeV / 100
  
  # How far from the edges?
  xOff <- diff(mapExtents[1:2]) * placeH
  yOff <- diff(mapExtents[3:4]) * placeV
  
  # Scale bar
  barLength <- barLength * 1000
  polygon(x = c(xx + xOff - (barLength / 2), 
                xx + xOff + (barLength / 2)),
          y = c(yy + yOff, 
                yy + yOff),
          lwd = 2, col = "black")
  
  # Scale the offset to the map
  labelOffset <- labelOffset / 100
  labelOffset <- labelOffset * diff(mapExtents[3:4])
  
  # What is the label?
  if (label == barLength) {
    label <- barLength / 1000
  } 
  
  # Add label
  text(x = xx + xOff,                  # location
       y = yy + yOff + labelOffset,    # place above line
       paste(label, "km"),  # annotation text
       cex = 1, col = "black")         # appearance
}

##
annotateShelfPlot <- function(raster, pos, main, 
                              cex = 1, nudgeH = 0, nudgeV = 0){
  #' Add annotations to the plot of the shelf; should work with any raster
  #' 
  #' @param raster raster: The raster being plotted
  #' @param pos "string": Should the annotation be topleft or bottomright?
  #' @param main "string": What is the text to be annotated in?
  #' @param cex numeric: How large is the annotation font? Default is 1
  #' @param nudgeV numeric: Adjust the vertical position of the text  
  #' @param nudgeH numeric: Adjust the horizontal position of the text  
  #' @usage annotateShelfPlot(raster4Plotting, "bottomright", "annotationText")
  
  # Code ----------------------------------------------------------------------!
  # Get raster extents
  mapExtents <- raster@extent
  
  # Set annotation location dependent on pos input
  if (pos == "topleft") {
    xx <- mapExtents[2] - diff(mapExtents[1:2]) / (1.07 + nudgeH)
    yy <- mapExtents[3] + diff(mapExtents[3:4]) / (1.07 - nudgeV)
  } else if (pos == "bottomright") {
    xx <- mapExtents[2] - diff(mapExtents[1:2]) / (4 + nudgeH)
    yy <- mapExtents[3] + diff(mapExtents[3:4]) / (10 - nudgeV)
  } else {
    xx <- mapExtents[2] - diff(mapExtents[1:2]) / (2 + nudgeH)
    yy <- mapExtents[3] + diff(mapExtents[3:4]) / (2 - nudgeV)
  }
  
  # Add text annotation
  text(x = xx, y = yy, main, cex = cex)
}

##
createSubFolders <- function(folderPath, subFolderName){
  #' Create a subfolder, originally used for storing interim data but repurposed
  #'
  #' @param folderPath    "string": Which folder needs a subfolder?
  #' @param subFolderName "string": What is the subfolder called?
  #' @usage createSubFolder(ff_interimPath, "amsre")
  
  # Code ----------------------------------------------------------------------!
  subFolder <- paste0(folderPath, "/", subFolderName)
  dir.create(file.path(getwd(), subFolder))
  return(subFolder)
}

##
createVersionFolders <- function(version, info, envir = ff){
  #' Create the folder structure for storing interim & toAnalyse data 
  #'
  #' @param version "string": version name & number
  #' @param info    "string": additional info on the version 
  #' @usage createVersionFolders("v01", "Antarctica")
  
  # Code ----------------------------------------------------------------------!
  # Create folder name
  versionName   <- paste0(version, "_", info)
  versionFolder <- paste0("data/", versionName)
  
  # If the folders for this version number don't exist, create them
  if (!dir.exists(versionFolder)) {
    # Create directory for this version
    dir.create(file.path(getwd(), versionFolder))
    
    # Create subfolders
    interimFolder <- paste0(versionFolder, "/interim")
    dir.create(file.path(getwd(), interimFolder))
    
    toAnalyseFolder <- paste0(versionFolder, "/toAnalyse")
    dir.create(file.path(getwd(), toAnalyseFolder))
    
  } else {
    cat("Version name '", versionName, "' already used! \n\n")
    print(list.dirs(paste0("data/", versionName)))
    printLine()
  }
  
  # Define the paths to the above folders
  defineDataPaths(version, info, envir)
}

##
defineDataPaths <- function(version, info, envir = ff){
  #' Define paths for interim & toAnalyse data, and add to global environment
  #'
  #' @param version "string": version name & number
  #' @param info    "string": additional info on the version 
  #' @param envir   env: which environment should the paths be held in?
  #' @usage functionName("v01", "Shackleton")
  
  # Code ----------------------------------------------------------------------!
  # Version Info
  assign("versionInfo", paste(version, info, sep = "_"), 
         envir = envir)
  
  # Version Path
  assign("versionPath", paste("data", ff$versionInfo, sep = "/"), 
         envir = envir)

  # Interim Path
  assign("interimPath", paste(ff$versionPath, "interim/", sep = "/"), 
         envir = envir)
  
  # toAnalyse Path
  assign("analysePath", paste(ff$versionPath, "toAnalyse/", sep = "/"), 
         envir = envir)
}

##
filterData <- function(dataset, 
                       returnColumn, 
                       filterColumn, 
                       filterCondition){
  #' Filter a data frame according to a match in a specific column
  #'
  #' @description Return the values from a specified column in a data frame 
  #' when values in a corresponding column match a specified criteria. For 
  #' example, return all the dates in the "date" column when the corresponding 
  #' "mExtent" column value is 100%. Currently only handles exact matches (e.g. 
  #' cannot do ">" or "<".
  #'
  #' @param dataset dataframe: The dataframe containing the data to be filtered.
  #' @param returnColumn "string": Name of the column to return the data from
  #' @param filterColumn "string": Name of the column using the filter.
  #' @param filterCondition depends!: What needs to be matched in filterColumn?
  #'
  #' @usage filterData(amsrBdata, "date", "extent", 50)
  
  # Code ----------------------------------------------------------------------!
  # Which column of data to return? Get index
  returnCol <- which(colnames(dataset) %in% returnColumn)
  
  # Which column to filter the data on? Get index
  filterCol <- which(colnames(dataset) == filterColumn)
  
  # Apply filtering - filter column must equal filter condition
  dataIndex <- dataset[, c(returnCol)][dataset[, filterCol] == filterCondition]
  
  # Remove NA values that don't get excluded above
  dataIndex <- dataIndex[!is.na(dataIndex)] 
  
  return(dataIndex)
}

##
fresh <- function(toKeep = "f"){
  #' Clear the console, environment, and/or plots
  #'
  #' @param toKeep "string": Which of the four options should be kept? 
  #' v = variable/value; f = functions; p = plots; c = console;
  #' e keeps the environment (i.e. functions & variables)
  #' @usage fresh(""); an empty string clears everthing it all.
  #' @examples sc
  #' Clear the environment and console, but keep the plots:
  #' fresh("p")
  #'
  #' Clear the variables, plots, and console, but keep functions:
  #' fresh("f")
  #'
  #' Clear everything
  #' fresh("")
  
  # Code ----------------------------------------------------------------------!
  # If e (environment), we want v & f
  if (grepl("e", toKeep)) {
    print(toKeep)
    toKeep <- paste0(toKeep, "vf") 
    print(toKeep)
  }
  
  # Keep functions?
  if (!grepl("f", toKeep)) {
    print("Removing functions")
    # if no f, remove functions
    toRemove <- lsf.str(envir = globalenv())
    rm(list = toRemove, envir = globalenv())
  } else {
    print("Keeping functions")
  }
  
  # Keep variables?
  if (!grepl("v", toKeep)) {
    # if no e, remove noon-functions
    print("Removing non-functions")
    
    # List everything in the global environment
    envAll       <- ls(envir = globalenv())
    
    # List functions
    envFunctions <- lsf.str(envir = globalenv())
    
    # Identify non functions
    envNonFuncts <- envAll %in% envFunctions
    
    # List non-functions to remove
    toRemove <- envAll[!envNonFuncts]
    
    # Removing non-functions
    rm(list = toRemove, envir = globalenv())
  } else {
    print("Keeping non-functinos")
  }
  
  # Keep plots?
  if (!grepl("p", toKeep)) {
    print("Removing plots")
    if (length(dev.list()) > 0) {
      dev.off()
    }
  } else {
    print("Keeping Plots")
  }
  
  # Keep or clear console?
  if (!grepl("c", toKeep)) {
    cat("\014")
  } else {
    print("Keeping Console")
    printLine()
  }
}

# Uncomment to test fresh function
# cat("\014")
# print("heheehihiiiiie")
# u_tt <- "testing"
# plot(1:10)
# fresh("v")

##
getTrueNorth <- function(x, y, crs, delta_crs = 0.1, delta_lat = 0.1) {
  #' Calculate the orientation of the true north arrow for a raster.
  #' 
  #' @description Calculate the orientation of the true north arrow for a raster 
  #' (i.e. it doesn't just assume north is up!). This function is copied almost 
  #' exactly from https://rdrr.io/cran/ggspatial/src/R/annotation-north-arrow.R.
  #' However, I don't use ggplot, so the rest of the function call doesn't work
  #' for me. This has only been tested for Shackleton, using EPSG: 3976. In 
  #' theory, it SHOULD work for other shelves/rasters + projections, but has not 
  #' been tested so double-check.
  #' 
  #' @param x numeric: Longitude
  #' @param y numeric: Latitude
  #' @param crs numeric: Which crs is the input raster in?
  #' @param delta_crs numeric: Not entirely sure; default is 0.1
  #' @param delta_lat numeric: Not entirely sure; default is 0.1
  #' 
  #' @usage getTrueNorth(xMidpoint, yMidpoint, crs = 3976)
  
  # Code ----------------------------------------------------------------------!
  # points
  pt_crs <- sf::st_sfc(sf::st_point(c(x, y)), crs = crs)
  pt_crs_coords <- as.data.frame(sf::st_coordinates(pt_crs))
  
  pt_latlon <- sf::st_transform(pt_crs, crs = 4326)
  pt_latlon_coords <- as.data.frame(sf::st_coordinates(pt_latlon))
  
  
  # point directly grid north of x, y
  pt_grid_north <- sf::st_sfc(sf::st_point(c(x, y + delta_crs)), crs = crs)
  pt_grid_north_coords <- as.data.frame(sf::st_coordinates(pt_grid_north))
  
  # point directly true north of x, y
  pt_true_north <- sf::st_transform(
    sf::st_sfc(
      sf::st_point(c(pt_latlon_coords$X, pt_latlon_coords$Y + delta_lat)),
      crs = 4326
    ),
    crs = crs
  )
  pt_true_north_coords <- as.data.frame(sf::st_coordinates(pt_true_north))
  
  a <- c(
    x = pt_true_north_coords$X - pt_crs_coords$X,
    y = pt_true_north_coords$Y - pt_crs_coords$Y
  )
  
  b <- c(
    x = pt_grid_north_coords$X - pt_crs_coords$X,
    y = pt_grid_north_coords$Y - pt_crs_coords$Y
  )
  
  # https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
  theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
  
  # use sign of cross product to indicate + or - rotation
  cross_product <- a[1]*b[2] - a[2]*b[1]
  
  # return in degrees
  rot_degrees <- theta * 180 / pi * sign(cross_product)[1]
  
  return(rot_degrees)
}

##
here <- function(x = "imma here!") {
  stop(x)
}

##
last <- function(x){
  #' Return the last value in a vector
  #' 
  #' @param x The vector!
  
  # Code ----------------------------------------------------------------------!
  y <- x[length(x)]
  return(y)
}

##
loadFullFolder <- function(folder, env = .GlobalEnv){
  #' Load all files in a specified folder, to a specified environment
  #'
  #' @description The purpose of this is to load all R objects in a folder at 
  #' once, but it will fail if other incompatible file types are present. 
  #' @param folder "string": Which folder contains the R data to load?
  #' @param env    "string": Which environment should it be loaded to?
  #' @usage loadFullFolder(ff_interimPath, envAA)
  
  # Code ----------------------------------------------------------------------!
  allFiles <- list.files(folder,
                         full.names = TRUE)
  # Load all files (needs correcting to only load R objects!)
  for (ii in 1:length(allFiles)) {
    base::load(allFiles[ii], env)
  }
}

##
`%!in%` <- function(){} # line just here to show function in side contents menu
`%!in%` <- Negate(`%in%`)

##
printLine <- function(lineType = "=", width = 3){
  #' Print a line to the console to help delineate output
  #'
  #' @param lineType "string": What kind of marker to use across the console?
  #' @param width:   numeric : Change if screen is particularly wide or narrow
  #' @usage functionName("=", 3)
  
  # Code ----------------------------------------------------------------------!
  cat("\n")
  tt <- rep(lineType, width * 21) %>% 
    paste0(collapse = "") %>%
    cat()
  cat("\n\n")
}

##
printScriptStart <- function(name){
  #' Print in the console to show the script has begun. Helps keep track in 
  #' master scripts.
  #'
  #' @param name 'string': Which script is being run?
  #' @usage printScriptStart("dt02")
  
  # Code ----------------------------------------------------------------------!
  printLine("!")
  cat("Initiating script", name)
  printLine("!")
}

##
rotate4Plot <- function(matrix){
  #' Rotate a matrix so it aligns as you'd expect when plotting
  #' 
  #' @description When a matrix is plotted (using image), it doesn't align with 
  #' the order you'd expect from looking at the matrix in the Viewer. This 
  #' rotates the matrix before you plot it, so that the pixel values correspond 
  #' to the Viewer values
  #' 
  #' @param matrix matrix: Which matrix needs rotating?
  #' 
  rotated <- t(matrix)[, nrow(matrix):1]
  return(rotated)
}

##
saveAs <- function(toSave, newName, filePath) {
  #' Rename an R object, and save it
  #'
  #' @description If you run a script for different input data, each one ends
  #' up with the same R object name. If these are then saved and opened 
  #' together elsewhere, they all have the same name and only the last one 
  #' loaded to the environment remains accessible. This functions allows you 
  #' to rename the object before saving it so that it is distinguishable from 
  #' others when they are all loaded elsewhere.
  #'
  #' @param toSave object: What do you want to save?
  #' @param newName "string": What should the objects new name be?
  #' @param filePath "string": Which folder should the object be saved in?
  #'
  #' @usage saveAs(australFluxes, "australFluxes_Neumayer", ff_interimPath)
  
  # Code ----------------------------------------------------------------------!
  assign(x = newName, value = toSave)
  
  # Add .rda to file name if missing
  end <- substringEnd(newName, 4)
  if (end != ".rData" ) {
    saveName <- paste0(newName, ".rData")
    print("Saving as .rData")
  } else {
    saveName <- newName
  }
  
  # Save
  do.call(save,
          list(newName,
               file = paste0(filePath, "/", saveName)))
  
  # Let the user know the object saved with a new name
  cat("\n Successfully renamed", deparse(substitute(toSave)),
      "to", newName, "and saved to", filePath, "\n")
}


##
setLayoutBar <- function(nrows, ncols, onlyOne = FALSE, horiz = FALSE){
  #' Create a layout that includes a side colour bar.
  #'
  #' @description Set the number of rows & columns for multi-plots, flanked by 
  #' a space for a single colour bar.
  #'
  #' @param nrows numeric: How many rows should be created? Excluding the one 
  #' for the colour bar
  #' @param ncols numeric: How many columns should be created?
  #' @param onlyOne binary: Is there only one plot column, and a colour bar? 
  #' @param horiz binary: Is the bar below the plots and horizontal? 
  #'
  #' @usage layout(setLayoutBar(nrows = 3, ncols = 3, onlyOne = FALSE))
  
  # Code ----------------------------------------------------------------------!
  # Get total plot count
  totalPlots <- nrows * ncols
  
  # Create a matrix in the correct order
  matrixOrder <- matrix(1:totalPlots, nrow = nrows, byrow = TRUE)
  
  if (isTRUE(onlyOne)) {
    nrows = 1
    totalPlots = 1
    matrixOrder <- matrix(1, nrow = 1, ncol = ncols)
  }
  
  # Add extra column for bar
  if (!isTRUE(horiz)) {
    matrixOrder <- cbind(rep(0, nrows),
                         matrixOrder, 
                         rep(totalPlots + 1, nrows))
  } else {
    matrixOrder <- rbind(matrixOrder, 
                         rep(totalPlots + 1, ncols))
  }
  return(matrixOrder)
}

##
substringEnd <- function(x, n) {
  #' Extract the last n characters in a string.
  #'
  #' @description Basically substring, but works from the right backwards.
  #' Adapted from https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  #' @param x "string": String input to extract from
  #' @param n numeric: How many values from the end of the string should be 
  #' returned?
  #'
  #' @usage answer <- substringEnd("exampleString", 3)
  #' [1] "ing"
  
  # Code ----------------------------------------------------------------------!
  rr <- substr(x, nchar(x) - n + 1, nchar(x))
  return(rr)
}

##
testFunction <- function(name = "Dominic") {
  #' Test that the custom functions are being loaded correctly
  #'
  #' @param arg1 "string": user name
  #' @usage testFunction("Dominic")
  
  # Code ----------------------------------------------------------------------!
  txt <- paste0("Hi ", name, 
                ", let's go solve Antarctica!")
  cat("\n", txt, "\n")
  printLine()
}
