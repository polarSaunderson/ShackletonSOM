## Populate shelf pixels with input data (ignores non-shelf pixels)
pixelValues2Raster <- function(pixelData, commonMask){
  #' Populate shelf pixels with input data (ignoring non-shelf pixels)
  #' 
  #'@param pixelData matrix: What values are being plotted?
  #'@param commonMask raster: The mask to be populated with data.

  # Code  
  plotMatrix <- as.matrix(commonMask)          # get a matrix to index data into
  plotMatrix[plotMatrix != 0] <- pixelData     # add values to correct pixels
  plotMatrix[as.matrix(commonMask) == 0] <- NA # NA non-shelf pixels
  values(commonMask) <- plotMatrix             # add matrix data to a raster
  return(commonMask)
}

# docstring(pixelValues2Raster)
