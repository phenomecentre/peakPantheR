#' Plot samples raw data and detected feature for a single ROI
#'
#' plot a ROI across multiple samples (x axis is RT, y axis is intensity). If curveFit is provided, the fitted curve for each sample is added.
#'
#' @param ROIDataPointSampleList (list) list of \code{data.frame} of raw data points for each sample (retention time "rt", mass "mz" and intensity "int" (as column) of each raw data points (as row)).
#' @param curveFitSampleList (list) NULL or a list of \code{peakPantheR_curveFit} (or NA) for each sample
#' @param rtMin (float) NULL or vector of detected peak minimum retention time (in sec)
#' @param rtMax (float) NULL or vector ofdetected peak maximum retention time (in sec)
#' @param sampling (int) Number of points to employ when plotting fittedCurve
#' @param sampleColour (str) NULL or vector colour for each sample (same length as \code{ROIDataPointSampleList}, \code{rtMin}, \code{rtMax})
#' @param verbose (bool) if TRUE message when NA scans are removed
#' 
#' @return Grob (ggplot object)
plotEICFit  <- function(ROIDataPointSampleList, curveFitSampleList=NULL, rtMin=NULL, rtMax=NULL, sampling=250, sampleColour=NULL, verbose=TRUE) {
  
  ## Check input
  # in case ROIDataPointSampleList is not a list
  if (class(ROIDataPointSampleList) != "list") {
    stop('Error: "ROIDataPointSampleList" must be a list of data.frame')
  }
  nbSpl   <- length(ROIDataPointSampleList)
  
  # check curveFitSampleList, rtMin and rtMax
  plotFit     <- FALSE
  if (!is.null(curveFitSampleList) & !is.null(rtMin) & !is.null(rtMax)) {
    if ((nbSpl==length(curveFitSampleList)) & (nbSpl==length(rtMin)) & (nbSpl==length(rtMax))) {
      plotFit <- TRUE
    } else {
      stop('"curveFitSampleList", "rtMin", "rtMax" and "ROIDataPointSampleList" must be the same length')
    }
  } else {
    if (verbose) {message('"curveFitSampleList", "rtMin" or "rtMax" no provided, curveFit will not be plotted')}
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
  
  
  ## Plot raw spectra and curve fit
  # init plot
  p_spec      <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) + ggplot2::theme_bw() + ggplot2::xlab('Retention Time (sec)') + ggplot2::ylab('Intensity') + ggplot2::scale_y_continuous(expand=c(0.01, 0.01))
  
  # add each spectra and fit to the plot
  for (spectraID in 1:nbSpl){
    # generate EIC
    tmp_EIC <- generateIonChromatogram(ROIDataPointSampleList[[spectraID]], aggregationFunction='sum')
    # plot EIC
    p_spec  <- p_spec + ggplot2::geom_line(data=tmp_EIC, ggplot2::aes_string(x='rt', y='int'), colour=colourSpl[spectraID])

    # fitted curve
    if (plotFit) {
      if(!(is.na(rtMin[spectraID])) & !(is.na(rtMax[spectraID]))) {
        grid_rt   <- seq(from=rtMin[spectraID], to=rtMax[spectraID], by=((rtMax[spectraID]-rtMin[spectraID])/(sampling-1)))
        # only if exist
        if (all(!is.na(curveFitSampleList[[spectraID]]))) {
          # project curve fit
          tmp_fit <- data.frame(rt=grid_rt ,int=predictCurve(curveFitSampleList[[spectraID]], x=grid_rt))
          # plot curve fit
          p_spec  <- p_spec + ggplot2::geom_line(data=tmp_fit, ggplot2::aes_string(x='rt', y='int'), colour=colourSpl[spectraID], linetype="dashed")
        }
      }
    }
  }
  
  return(p_spec)
}
