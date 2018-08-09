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
  
  
  ## Prepare data
  # raw spectra EICs
  all_EIC   <- lapply(seq_along(ROIDataPointSampleList), function(x) {
    # generate ion chromatogram and add sampleIDColour
    tmp_EIC <- generateIonChromatogram(ROIDataPointSampleList[[x]], aggregationFunction='sum')
    tmp_EIC <- cbind(tmp_EIC, specID=rep(paste('spl', x, sep=""), nrow(tmp_EIC)), stringsAsFactors=FALSE)
  })
  input_EIC <- do.call(rbind, all_EIC)
  
  # curve fit
  if (plotFit) {
    all_fit     <- lapply(seq_along(ROIDataPointSampleList), function(x) {
      # check rtMin, rtMax and curveFit exist, project curveFit and add sampleIDColour
      if(!(is.na(rtMin[x])) & !(is.na(rtMax[x])) & all(!is.na(curveFitSampleList[[x]]))) {
        grid_rt <- seq(from=rtMin[x], to=rtMax[x], by=((rtMax[x]-rtMin[x])/(sampling-1)))
        tmp_fit <- data.frame(rt=grid_rt, int=predictCurve(curveFitSampleList[[x]], x=grid_rt))
        tmp_fit <- cbind(tmp_fit, specID=rep(paste('spl', x, sep=""), nrow(tmp_fit)), stringsAsFactors=FALSE)
      }
    })
    input_fit <- do.call(rbind, all_fit)
    # catch no curve fit left to plot
    if (is.null(input_fit)) { input_fit <- data.frame(matrix(,ncol=3, nrow=0, dimnames=list(c(),c('rt','int','specID'))))}
  }
  
  
  ## Plot raw spectra and curve fit
  # init plot
  p_spec    <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) + ggplot2::theme_bw() + ggplot2::xlab('Retention Time (sec)') + ggplot2::ylab('Intensity') + ggplot2::scale_y_continuous(expand=c(0.01, 0.01)) + ggplot2::scale_color_manual(values=colourSpl) + ggplot2::theme(legend.position="none")

  # plot EIC
  p_spec    <- p_spec + ggplot2::geom_line(data=input_EIC, ggplot2::aes(x=rt, y=int, group=factor(specID), colour=factor(specID)))
  
  # plot curve fit
  if (plotFit) {
    p_spec  <- p_spec + ggplot2::geom_line(data=input_fit, ggplot2::aes(x=rt, y=int, group=factor(specID), colour=factor(specID)), linetype="dashed")
  }
  
  return(p_spec)
}
