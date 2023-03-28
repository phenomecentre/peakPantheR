#' @title Plot samples raw data and detected feature for a single ROI
#'
#' @description Plot a ROI across multiple samples (x axis is RT, y axis is
#' intensity) with the matching detected peak rt and peakwidth under it. If
#' curveFit is provided, the fitted curve for each compound is added. RT and
#' peakwidth are plotted in the order spectra are passed, with the first spectra
#' on top.
#'
#' @param ROIDataPointSampleList (list) list of \code{data.frame} of raw data
#' points for each sample (retention time 'rt', mass 'mz' and intensity 'int'
#' (as column) of each raw data points (as row)).
#' @param cpdID (str) Compound ID
#' @param cpdName (str) Compound Name
#' @param rt (float) vector of detected peak apex retention time (in sec)
#' @param rtMin (float) vector of detected peak minimum retention time (in sec)
#' @param rtMax (float) vector ofdetected peak maximum retention time (in sec)
#' @param mzMin (float) ROI minimum m/z (matching EIC)
#' @param mzMax (float) ROI maximum m/z (matching EIC)
#' @param ratio (float) value between 0 and 1 defining the vertical percentage
#' taken by the EICs subplot
#' @param sampling (int) Number of points to employ when plotting fittedCurve
#' @param curveFitSampleList (list) NULL or a list of
#' \code{peakPantheR_curveFit} (or NA) for each sample
#' @param sampleColour (str) NULL or vector colour for each sample (same length
#' as \code{EICs}, \code{rt}, \code{rtMin}, \code{rtMax})
#' @param verbose (bool) if TRUE message when NA scans are removed
#' 
#' @return Grob (ggplot object)
plotEICDetectedPeakwidth <- function(ROIDataPointSampleList, cpdID, cpdName, rt,
    rtMin, rtMax, mzMin, mzMax, ratio = 0.85, sampling = 250,
    curveFitSampleList = NULL, sampleColour = NULL, verbose = TRUE) {

    # Check input, init
    ratio <- plotEICDetectedPeakwidth_checkInp(ROIDataPointSampleList, rt,
                                                ratio, verbose)
    title <- paste("CpdID: ", cpdID, " - ", cpdName, " ", round(mzMin, 4), "-",
                    round(mzMax, 4))
    
    # Plot raw spectra and curve fit
    p_spec <- peakPantheR_plotEICFit(
        ROIDataPointSampleList = ROIDataPointSampleList,
        curveFitSampleList = curveFitSampleList, rtMin = rtMin, rtMax = rtMax,
        sampling = sampling, sampleColour = sampleColour, verbose = verbose)
    p_spec <- p_spec + ggplot2::ggtitle(title) +
            ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                plot.title = ggplot2::element_text(size = ggplot2::rel(1)))
    # Plot peakwidth
    p_peakwidth <- peakPantheR_plotPeakwidth(apexValue = rt, widthMin = rtMin,
        widthMax = rtMax, varName = "Retention Time (sec)", acquTime = NULL,
        sampleColour = sampleColour, rotateAxis = TRUE, verbose = FALSE)
    
    # Set common x lim (due to the rotation, x on p_peakwidth is originally y
    # and accessed as such)
    minX <- min(ggplot2::layer_scales(p_spec)$x$range$range[1],
        ggplot2::layer_scales(p_peakwidth)$y$range$range[1])
    maxX <- max(ggplot2::layer_scales(p_spec)$x$range$range[2],
        ggplot2::layer_scales(p_peakwidth)$y$range$range[2])
    p_spec <- p_spec + ggplot2::xlim(minX, maxX)
    suppressMessages(p_peakwidth <- p_peakwidth + ggplot2::ylim(minX, maxX))
    # convert to gtables
    p_spec <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p_spec))
    p_peakwidth <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p_peakwidth))
    # find the widths of each of the plots, calculate the maximum and then apply
    # it to each of them individually. This effectively applies a uniform layout
    # to each of the plots.
    maxWidth <- grid::unit.pmax(p_spec$widths[2:5], p_peakwidth$widths[2:5])
    p_spec$widths[2:5] <- maxWidth
    p_peakwidth$widths[2:5] <- maxWidth
    
    # Group plots with y axis ratio
    topSize <- ratio * 10
    bottomSize <- (1 - ratio) * 10
    p <- gridExtra::grid.arrange(p_spec, p_peakwidth,
        heights = c(topSize, bottomSize))
    
    return(p)
}
# prepare and check input
plotEICDetectedPeakwidth_checkInp <- function(ROIDataPointSampleList, rt, ratio,
                                                verbose) {
    # check length of input across subplots, (others are checked inside
    # peakPantheR_plotEICFit and peakPantheR_plotPeakwidth)
    nbSpl <- length(ROIDataPointSampleList)
    if (nbSpl != length(rt)) {
        stop("\"ROIDataPointSampleList\", \"rt\", \"rtMin\" and \"rtMax\" must",
            " be the same length")
    }

    # ratio must be between 0 and 1
    if ((ratio < 0) | (ratio > 1)) {
        if (verbose) {
            message("Err","or: ratio must be between 0 and 1, ",
                    "replaced by default value")
        }
        ratio <- 0.85
    }

    return(ratio)
}