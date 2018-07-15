#' Plot peak value and peakwidth by acquisition time or in input order
#'
#' For a single ROI, plot the peak value and peakwidth (RT, m/z, ...) of detected peaks across multiple samples, by acquisition time or in input order. If \code{rotateAxis=FALSE} x is run order / plot order, y is the \code{apexValue} / \code{widthMin} / \code{widthMax}, if \code{rotateAxis=TRUE} x is the measurement values and y the run order.
#'
#' @param apexValue (float) vector of apex value
#' @param widthMin (float) vector of detected peak minimum peakwidth value or NULL (if NULL no peakwidth)
#' @param widthMax (float) vector of detected peak maximum peakwidth value or NULL (uf NULL no peakwidth)
#' @param acquTime (POSIXct) vector of sample acquisition time as POSIXct or NULL (if NULL points are plotted in the order values are passed as input with the first on top or left)
#' @param varName (str) Name of the variable to plot
#' @param sampleColour (str) NULL or vector colour for each sample (same length as \code{apexValue}, \code{widthMin}, \code{widthMax}, \code{acquTime})
#' @param rotateAxis (bool) if TRUE x and y axis are reversed
#' @param verbose (bool) if TRUE message when NA scans are removed
#' 
#' @return Grob (ggplot object)
#' 
#' @import scales
plotPeakwidth <- function(apexValue, widthMin=NULL, widthMax=NULL, acquTime=NULL, varName='variable', sampleColour=NULL, rotateAxis=FALSE, verbose=TRUE) {
  
  ## Check input
  nbSpl   <- length(apexValue)
  
  # check acquTime
  useRunOrder   <- FALSE
  if (!is.null(acquTime)) {
    # acquTime is not a POSIXct
    if (!("POSIXct" %in% class(acquTime))) {
      stop('Error: "acquTime" must be a vector of POSIXct')
    }
    # NA in acquTime
    if (any(is.na(acquTime))) {
      if (verbose) { message('Warning: "acquTime" contains NA, run order will not be plotted') }
      acquTime <- NULL # helps with unittesting as dates in $plot_env introduce OS differences
    } else {
      # check acquTime length
      if (nbSpl!=length(acquTime)) {
        stop('Error: "apexValue" and "acquTime" must be the same length')
      } else {
        useRunOrder <- TRUE
      }
    }
  }
  
  # check widthMin & widthMax
  useWidth  <- FALSE
  if (!is.null(widthMin) & !is.null(widthMax)) {
    # check length
    if ((nbSpl!=length(widthMin)) | (nbSpl!=length(widthMax))) {
      stop('"apexValue", "widthMin" and "widthMax" must be the same length')
    } else {
      useWidth  <- TRUE
    }
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
  
  
  ## Init plot
  # draw default x/y with y the variable, rotate later if required
  p   <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) + ggplot2::theme_bw() + ggplot2::ylab(varName)
  
  # set fill and colour scale, with one color per sample ID
  p   <- p + ggplot2::scale_colour_manual(values=colourSpl, guide=FALSE)
  p   <- p + ggplot2::scale_fill_manual(values=colourSpl, guide=FALSE)
  
  
  ## tmp x axis
  if (useRunOrder) {
    x_axis    <- acquTime
  } else {
    if (rotateAxis) {
      x_axis  <- seq(nbSpl-1, 0)  #first spectra on top
    } else {
      x_axis  <- seq(0, nbSpl-1)  #first spectra on left
    }
  }
  
  
  ## peak width (must go first to be the bottom most layer)
  if (useWidth) {
    # value peakwidth (add color ID to each point)
    tmp_pwidth  <- data.frame(x=c(x_axis,x_axis), y=c(widthMin,widthMax), colr=c(sampleIDColour,sampleIDColour), stringsAsFactors=F)
    tmp_pwidth  <- tmp_pwidth[!is.na(tmp_pwidth$y),]
    p           <- p + ggplot2::geom_line(data=tmp_pwidth, ggplot2::aes(x=x, y=y, group=x, colour=colr), size=1)
    if (verbose) {message('Peakwidth values plotted')}
  }
  
  
  ## apex value
  if (useRunOrder) {
    p       <- p + ggplot2::xlab('Acquisition Time') #labels are flipped, themes are not
    tmp_pt  <- data.frame(x=acquTime, y=apexValue, colr=sampleIDColour, stringsAsFactors=F)
    if (verbose) {message('Values plotted by run order')}
  } else {
    # labels are flipped, themes are not
    # add apex point(add the color ID to each point, and an ID pointing to the black stroke)
    tmp_pt  <- data.frame(x=x_axis, y=apexValue, colr=sampleIDColour, stringsAsFactors=F)
    if (verbose) {message('Values plotted by input order')}
  }
  # add apex points
  tmp_pt  <- tmp_pt[!is.na(tmp_pt$y),]
  # with black stroke
  if (useWidth) {
    p     <- p + ggplot2::geom_point(data=tmp_pt, ggplot2::aes(x=x, y=y, fill=colr), colour="black", shape=21, stroke=1, size=2)
    # without black stroke
  } else {
    p     <- p + ggplot2::geom_point(data=tmp_pt, ggplot2::aes(x=x, y=y, colour=colr)) 
  }
  
  
  ## rotate axis (labels are carried with the flip, but themes are applied after the fact)
  if (rotateAxis) {
    p <- p + ggplot2::coord_flip()
    
    if (useRunOrder) {
      # Run order
      # if date axis vertical, put dates in order from top to bottom
      # function to reverse date axis
      c_trans   <- function(a, b, breaks = b$breaks, format = b$format) {
        a     <- scales::as.trans(a)
        b     <- scales::as.trans(b)
        name  <- paste(a$name, b$name, sep = "-")
        trans <- function(x) a$trans(b$trans(x))
        inv   <- function(x) b$inverse(a$inverse(x))
        scales::trans_new(name=name, transform=trans, inverse=inv, breaks=breaks, format=format)
      }
      rev_date  <- c_trans("reverse", "time")
      
      p   <- p + ggplot2::theme(axis.text.y=ggplot2::element_text(angle=45, hjust=1)) + ggplot2::scale_x_continuous(trans = rev_date, expand=c(0.007, 0.007))
    } else {
      # no run order
      p   <- p + ggplot2::theme(axis.title.y=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(), axis.ticks.x=ggplot2::element_blank()) + ggplot2::scale_x_continuous(breaks=NULL, expand=c(0.007, 0.007))
    }
    if (verbose) {message('x and y axis rotated')}
    
    # no rotation
  } else {
    if (useRunOrder) {
      p   <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1)) + ggplot2::scale_x_datetime(expand=c(0.007, 0.007))
    } else {
      p   <- p + ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.text.x=ggplot2::element_blank(), axis.ticks.x=ggplot2::element_blank()) + ggplot2::scale_x_continuous(breaks=NULL, expand=c(0.007, 0.007))
    }
  }
  
  return(p)
}
