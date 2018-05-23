#' Plot detected peaks width for a single ROI
#'
#' plot for a ROI the value and peakwidth (RT, m/z, ...) of detected peaks across multiple samples (x axis is the value (RT, m/z, ...), y is plotted in the order spectra are passed with the first on top).
#'
#' @param apexValue (float) vector of detected peak apex value
#' @param widthMin (float) vector of detected peak minimum peakwidth value
#' @param widthMax (float) vector of detected peak maximum peakwidth value
#' @param varName (str) Name of the variable to plot (X axis label)
#' @param sampleColour (str) NULL or vector colour for each sample (same length as \code{apexValue}, \code{widthMin}, \code{widthMax})
#' @param verbose (bool) if TRUE message when NA scans are removed
#' 
#' @return Grob (ggplot object)
plotPeakwidth  <- function(apexValue, widthMin, widthMax, varName='Variable', sampleColour=NULL, verbose=TRUE) {
  
  ## Check input
  # check length of input
  nbSpl   <- length(apexValue)
  if ((nbSpl!=length(widthMin)) | (nbSpl!=length(widthMax))) {
    stop('"apexValue", "widthMin" and "widthMax" must be the same length')
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
  
  
  ## Plot apex and peakwidth
  y_placement   <- seq(nbSpl-1, 0) #first spectra on top
  # init plot
  p_peakwidth   <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) + ggplot2::theme_bw() + ggplot2::xlab(varName) + ggplot2::theme(axis.title.y=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(), axis.ticks.y=ggplot2::element_blank()) + ggplot2::scale_y_continuous(breaks=NULL, expand=c(0.007, 0.007))
  # set fill and colour scale, with one color per sample ID
  tmp_col         <- colourSpl
  p_peakwidth     <- p_peakwidth + ggplot2::scale_colour_manual(values=tmp_col, guide=FALSE)
  p_peakwidth     <- p_peakwidth + ggplot2::scale_fill_manual(values=tmp_col, guide=FALSE)
  # value peakwidth (add color ID to each point)
  tmp_pwidth    <- data.frame(x=c(widthMin,widthMax), y=c(y_placement,y_placement), colr=c(sampleIDColour,sampleIDColour), stringsAsFactors=F)
  tmp_pwidth    <- tmp_pwidth[!is.na(tmp_pwidth$x),]
  p_peakwidth   <- p_peakwidth + ggplot2::geom_line(data=tmp_pwidth, ggplot2::aes(x=x, y=y, group=y, colour=colr), size=1)
  # value apex (add the color ID to each point, and an ID pointing to the black stroke)
  tmp_pt        <- data.frame(x=apexValue, y=y_placement, colr=sampleIDColour, stringsAsFactors=F)
  tmp_pt        <- tmp_pt[!is.na(tmp_pt$x),]
  p_peakwidth   <- p_peakwidth + ggplot2::geom_point(data=tmp_pt, ggplot2::aes(x=x, y=y, fill=colr), colour="black", shape=21, stroke=1, size=2)
  
  return(p_peakwidth)
}
