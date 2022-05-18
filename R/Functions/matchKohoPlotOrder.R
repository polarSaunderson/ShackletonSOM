##
matchKohoPlotOrder <- function(ii, somRows = u_somRow, somCols = u_somCol){
  #' Set the plotting order for the som output to match the default 
  #'
  #' @description The defaults of the kohonen package plot from bottom-left to 
  #' top-right, but R goes from top-left to bottom-right. This calculates the 
  #' correct index to feed R so that the plot orders match.
  #'
  #' @param ii numeric: The Kohonen map number to convert to an R plot index
  #' @param arg2 numeric: value of input
  #' @param somRows numeric: How many rows in the som grid?
  #' @param somCols numeric: How many columns in the som grid?
  #' 
  #' @usage matchKohoPlotOrder(ii = 3, somRows = 3, somCols = 3)
  
  # Code -----------------------------------------------------------------------
  # How many plots overall?
  plotCount <- somRows * somCols
  
  # Define the default plotting order (i.e. count top left to bottom right)
  defaultOrder <- matrix(1:plotCount,
                         nrow = somRows, ncol = somCols,
                         byrow = TRUE)
  
  # Define the kohonen output plot order (i.e. count bottom left to top right)
  kohoOrder <- apply(defaultOrder, 2, rev)
  
  # Identify the correct plot order to match kohonen SOM pattern order
  jj <- kohoOrder[defaultOrder == ii]
  
  return(jj)
}

# docstring(matchKohoPlotOrder)
