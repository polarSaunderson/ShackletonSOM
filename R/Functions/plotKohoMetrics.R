##
plotKohoMetrics <- function(somOutput, plotName = ""){
  #' Plot the default output plots from the Kohonen package.
  #' 
  #' @param somOutput som List: Output from the kohonen::som function 
  #' @param plotName "string":  A name for the first plot.
  #' 
  
  # Code ---------------------------------------------------------------------
  par(mfrow = c(2, 3))
  plot(somOutput, 
       type = "codes", shape = "straight",
       main = paste("SOM Codes", plotName),
       codeRendering = "lines",
       sub = "Codebook vectors (contribution of pixel to the pattern)")
  plot(somOutput, 
       type = "counts", shape = "straight",
       main = "Pattern Count",
       sub = "Number of Days per Pattern",
       palette.name = colour("bilbao"))
  plot(somOutput,
       type = "mapping", shape = "straight",
       main = "Pattern Mapping")
  plot(somOutput, 
       type = "quality", shape = "straight",
       main = "Pattern Quality",
       sub = "Mean distance of objects to their assigned codebook vector",
       palette.name = colour("bilbao"))
  plot(somOutput,
       type = "dist.neighbours", shape = "straight",
       main = "SOM Distance to Neighbours",
       sub = "Sum of distance to all immediate neighbours; u-matrix",
       palette.name = colour("oslo"))
  par(mar = c(4, 4, 4, 4))
  plot(somOutput,
       type = "changes", shape = "straight",
       main = "SOM Changes",
       sub = "Mean distance to closest vector during training")
}

# docstring(plotKohoMetrics)
