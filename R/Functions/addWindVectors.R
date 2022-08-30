addWindVectors <- function(vectorMag, vectorAngle, uData, vData) {
  # Raster Info
  vectorMag   <- raster::raster(vectorMag)
  gridDims    <- dim(vectorMag)
  gridExtents <- raster::extent(vectorMag)
  pixelSize   <- (gridExtents[2] - gridExtents[1]) / gridDims[2]
  
  # Raster Bounds
  xVal <- gridExtents[c(1:2)]
  xMin <- min(xVal)
  xMax <- max(xVal)
  yVal <- gridExtents[c(3:4)]
  yMin <- min(yVal)
  yMax <- max(yVal)
  
  # Create a new grid of coordinates for the points
  xPuntos <- seq(xMin + pixelSize / 2, xMax, pixelSize)
  yPuntos <- seq(yMax - pixelSize / 2, yMin, -pixelSize)
  xMatrix <- matrix(xPuntos, nrow = gridDims[2], 
                    ncol = gridDims[1], byrow = FALSE)
  yMatrix <- matrix(yPuntos, nrow = gridDims[2], 
                    ncol = gridDims[1], byrow = TRUE)
  
  # For each pixel, create and plot an arrow
  for (ii in 1:length(xMatrix)) {
    # Use the simpler approach!
    x0 <- xMatrix[ii]
    y0 <- yMatrix[ii]
    x1 <- (xMatrix[ii] + vData[ii] / 80) %>% unlist() %>% unname()
    y1 <- (yMatrix[ii] - uData[ii] / 80) %>% unlist() %>% unname()

    # Add to plot
    arrows(x0, y0, x1, y1, 
           col = "black", lwd = 1, 
           code = 2,
           length = 0.02)
  }
}
