##
extractMeltingIds <- function(day, 
                              topLeft, 
                              columnCount, 
                              rowCount, 
                              idMask, 
                              ncData){
  #' Extract ID numbers of the pixels which melt for a given day
  #' 
  #' @description Used in script dt02, with apply; it create a list of lists; 
  #' each list stores the ID numbers of pixels which are melting on the given
  #' day, within the given mask area. Requires the RNetCDF library. The NetCDF 
  #' file must be opened before calling this function, using the open.nc 
  #' function. Don't forgot to close the connection after using it. 
  #'
  #' @param day numeric: Number of the date (i.e. layer of the NetCDF file)
  #' @param topLeft vector: What is the location of the topleft pixel for the 
  #' shelf area, where checking for melt should begin?
  #' @param columnCount numeric: How many columns to look through from topLeft
  #' @param rowCount numeric: How many rows to look through from topLeft
  #' @param idMask raster: A matrix of the mask to exclude non-shelf pixels
  #' @param ncFata string: The NetCDF filename holding the data.
  
  # Code -----------------------------------------------------------------------
  # Extract the cropped melt info for the given day
  picardShelfMelt <- var.get.nc(ncData, "melt",
                                c(topLeft[2], topLeft[1], day),
                                c(columnCount, rowCount, 1))
  
  # Ignore the pixels which Prof. Picard didn't check for melt (high/ocean)
  picardShelfMelt[picardShelfMelt == -10] <- NA
  
  # Ignore pixels which don't melt
  picardShelfMelt[picardShelfMelt == 0]   <- NA
  
  # Mask out pixels outside MEaSURES shelf boundary; transposed to align to R
  ids <- t(picardShelfMelt) * idMask
  
  # Return the id of pixels which had melt for the input day
  ids <- ids[!is.na(ids)]
  return(ids)
}

# docstring(extractMeltingIds)
