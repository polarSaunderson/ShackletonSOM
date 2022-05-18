plotSomPatterns <- function(somOutput, 
                            patterns = "all",
                            somRow = u_somRow,
                            somCol = u_somCol,
                            addBar = FALSE,
                            addMapParts = TRUE,
                            plotMar = c(1, 1, 1, 1),
                            shelfOutline = shelf3976,
                            commonMask = dd$commonMask){
  #' Plot the output from the kohonen packages som algorithm as rasters
  #'
  #' @description The output from the SOM algorithm is a vector of values for 
  #' each pixel; this function correctly rearranges the vector back into the 
  #' 2D shape of the shelf, and plots them in the correct order to correspond 
  #' to the default order of the output from the kohonen functions. Need to 
  #' prepare the plot area first with layout() 
  #'
  #' @param somOutput som List: output from the kohonen::som function 
  #' @param patterns  numeric: Which pattern/s should be plotted? "all" is the 
  #' default.
  #' @param somRows numeric: How many rows in the som grid?
  #' @param somCols numeric: How many columns in the som grid?
  #' @param addBar string: Should a scale bar be added afterwards? Needs the 
  #' layout configuring correctly before running the function; select either 
  #' "horiz" or "vertical"
  #' @param addMapParts binary: Add a north arrow and scale bar to the plots?
  #' @param mar vector: What margin is necessary around the plots?
  #' @param shelfOutline spatialPolygon: The shelf boundary
  #' @param commonMask raster: Raster of the shelf to rearrange the som output
  #' back to
  #'
  #' @usage plotSomPatterns(somOutput = somOutput, somRow = 3, somCol = 3,
  #' addBar = FALSE, shelfOutline = shelf3976, commonMask = dd$commonMask)
  
  # Code ---------------------------------------------------------------------
  # Plot all patterns or not?
  if (patterns == "all") {
    patterns <- as.integer(c(1:(somCol * somRow))) # otherwise argument input
  }
  
  # Set up plot area
  par(mar = plotMar)
  pCount <- table(somOutput$unit.classif) # allows count display on the plots
  
  # Loop through the patterns
  for (ii in patterns) {
    if (length(patterns) == somRow * somCol) {
      jj <- matchKohoPlotOrder(ii, somRow, somCol) # kohonen ordered bottom-up
    } else {
      jj <- ii            # otherwise, plot in the order they are entered
    }
    
    # Add a grey background plot area
    plotShelf(x = dd$commonMask,
              col   = c(paste0(gg$kulaQ[7], "99"), "#FFFFFF"),
              axes  = FALSE,
              withArrow = addMapParts,
              shelfOutline = NA)
    
    # Add the som output values to the raster
    plotRaster <- pixelValues2Raster(somOutput$codes[[1]][jj, ], 
                                     dd$commonMask)
    
    # Only display most "extreme" values
    # plotRaster[plotRaster < 0.9 & plotRaster > 0.1] <- NA
    
    # Plot
    plotShelf(plotRaster, 
              col = gg$kulaS, 
              zlim  = c(0, 1), 
              add = TRUE, 
              withArrow = addMapParts,
              shelfOutline = shelfOutline)
    
    # Mappify 
    if (isTRUE(addMapParts)) {
      # Add an annotation to know which pattern the plot shows
      annotateShelfPlot(plotRaster, pos = "topleft", jj, cex = 1.8)
      
      # Add scale bar
      addPlotScale(plotRaster, 25, labelOffset = 6, placeH = 80)
      
    } else if (addMapParts == "counts") {
      annotateShelfPlot(plotRaster, pos = "bottomright", 
                        pCount[jj], cex = 0.75)
    }
  }
  
  # Add colour bar to the side
  if (!isFALSE(addBar)) {
    if (addBar == "horiz") {
      horiz  <- TRUE
      barMar <- c(3, 6, 1, 6)
    } else if (addBar == "vertical" | isTRUE(addBar)) {
      horiz  <- FALSE
      barMar <- c(14, 1.5, 14, 12) # vertical bar
    }
    
    # And display it!
    addColourBar(col = gg$kulaS,
                 mar = barMar,
                 zlim = c(0, 1),
                 horiz = horiz,
                 allLabels = "no",
                 title = "Melt Likelihood",
                 labelOffset = 1)
  }
}

# docstring(plotSomPatterns)
