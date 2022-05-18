##
plotShelf <- function(x, shelfOutline = gg$shelf3976, withArrow = TRUE, ...) {
  #' Plot a raster image of the input shelf data
  #'
  #' @description Essentially a wrapper around the image function to plot the 
  #' raster values up. Also adds the shelf boundary and a north arrow. 
  #' Can overwrite any options available to image. 
  #'
  #' @param x raster: Which shelf raster needs plotting?
  #' @param shelfOutline spatialPolygon: An object holding the shelf outline
  #' @param withArrow binary: Should a north arrow be drawn on the plot?
  #' @param ... Any options available to image.
  
  # Code --------------------------------------------------------------------!
  # Default values
  defaultValues <- list(main = "",
                        asp = 1,
                        col = gg$kulaQ[c(1, 7)],
                        xlab = "",
                        ylab = "")
  
  # User input values
  userValues <- list(...)
  
  # Overwrite defaults with any relevant user input
  defaultValues[names(userValues)] <- userValues
  
  # Apply 
  do.call(image, c(list(x = x), defaultValues))
  
  # Add shelf outline 
  lines(shelfOutline, lwd = 1.1)
  
  # And a north arrow?
  if (withArrow == TRUE) {
    addNorthArrow(rasterIn = x, lwd = 1.1, 
                  endLength = 0.075, placeV = 96,
                  nOffset = 4, col = "black")
  }
}

# docstring(plotShelf)
