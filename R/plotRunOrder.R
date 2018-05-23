#' Plot peak value by acquisition time
#'
#' plot peak value by acquisition time for a single ROI
#'
#' @param sampleValue (float) vector of sample value to plot
#' @param acquTime (POSIXct) vector of sample acquisition time as POSIXct
#' @param varName (str) Name of the variable to plot
#' @param sampleColour (str) NULL or vector colour for each sample (same length as \code{sampleValue}, \code{acquTime})
#' @param verbose (bool) if TRUE message when NA scans are removed
#' 
#' @return Grob (ggplot object)
plotRunOrder  <- function(sampleValue, acquTime, varName='Variable', sampleColour=NULL, verbose=TRUE) {
  
  ## Check input
  # in case acquTime is not a POSIXct
  if (!("POSIXct" %in% class(acquTime))) {
    stop('Error: "acquTime" must be a vector of POSIXct')
  }
  nbSpl   <- length(sampleValue)
  
  # check peakValue and acquTime
  if (nbSpl!=length(acquTime)) {
    stop('"sampleValue" and "acquTime" must be the same length')
  }

  # set default colour (add a sample color ID that will be match in the plot)
  colourSpl     <- rep("black", nbSpl)
  if (!is.null(sampleColour)) {
    if (nbSpl==length(sampleColour)) {
      colourSpl <- sampleColour
    } else {
      if (verbose) {message("Warning: sampleColour length must match the number of samples; default colour used")}
    }
  } 
  sampleIDColour    <- paste('spl', seq(1:nbSpl), sep="")
  names(colourSpl)  <- sampleIDColour
 

  ## Plot variable by run order
  # init plot
  p_runOrder    <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) + ggplot2::theme_bw() + ggplot2::xlab('Acquisition Time') + ggplot2::ylab(varName) + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1))
  # set colour scale with one color per sample ID
  p_runOrder    <- p_runOrder + ggplot2::scale_colour_manual(values=colourSpl, guide=FALSE)
  # point (add the color ID to each point)
  tmp_pt        <- data.frame(x=acquTime, y=sampleValue, colr=sampleIDColour, stringsAsFactors=F)
  tmp_pt        <- tmp_pt[!is.na(tmp_pt$x),]
  p_runOrder    <- p_runOrder + ggplot2::geom_point(data=tmp_pt, ggplot2::aes(x=x, y=y, colour=colr)) 
  
  return(p_runOrder)
}
