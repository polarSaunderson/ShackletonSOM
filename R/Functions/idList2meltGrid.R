##
idList2meltGrid <- function(meltList, 
                            shelfIdMatrix, 
                            day){
  #' Convert ID numbers of melting pixels into a binary melt/no-melt matrix
  #' 
  #'@param meltList list: A list of lists, each containing ID numbers for 
  #'melting pixels 
  #'@param shelfIdMatrix matrix: The correct ID numbers for the shelf pixels
  #'@param day: numeric: Which day to create the melt matrix for? Subsets meltList
  #'@usage idList2meltGrid(meltList, shelfIds, jj)
  
  # Code -----------------------------------------------------------------------
  # Which shelf pixel ID numbers are in the melting pixels list on this day?
  # Change the shelf ID number matrix to a list & compare
  meltingPixels <- shelfIdMatrix %in% meltList[[day]] * 1
  
  # NA any pixels where there is no shelf 
  meltingPixels[is.na(shelfIdMatrix)] <- NA
  
  # Reshape the list back into to a matrix 
  meltingPixels <- matrix(meltingPixels, nrow = dim(shelfIdMatrix)[1])
  
  return(meltingPixels)
}

# docstring(idList2meltGrid)
