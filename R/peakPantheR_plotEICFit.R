#' @title Plot samples raw data and detected feature for a single ROI
#'
#' @description plot a ROI across multiple samples (x axis is RT, y axis is
#' intensity). If curveFit is provided, the fitted curve for each sample is
#' added.
#'
#' @param ROIDataPointSampleList (list) list of \code{data.frame} of raw data
#' points for each sample (retention time 'rt', mass 'mz' and intensity 'int'
#' (as column) of each raw data points (as row)).
#' @param curveFitSampleList (list) NULL or a list of
#' \code{peakPantheR_curveFit} (or NA) for each sample
#' @param rtMin (float) NULL or vector of detected peak minimum retention time
#' (in sec)
#' @param rtMax (float) NULL or vector ofdetected peak maximum retention time
#' (in sec)
#' @param sampling (int) Number of points to employ when plotting fittedCurve
#' @param sampleColour (str) NULL or vector colour for each sample (same length
#' as \code{ROIDataPointSampleList}, \code{rtMin}, \code{rtMax})
#' @param verbose (bool) if TRUE message when NA scans are removed
#' 
#' @return Grob (ggplot object)
#' 
#' @export
#' 
#' @examples
#' ## Input data
#' # fake sample 1
#' # ROI data points
#' rt1             <- seq(990, 1010, by=20/250)
#' mz1             <- rep(522., length(rt1))
#' int1            <- (dnorm(rt1, mean=1000, sd=1.5) * 100) + 1
#' tmp_DataPoints1 <- data.frame(rt=rt1, mz=mz1, int=int1)
#' # fittedCurve
#' fit1        <- list(amplitude=37.068916502809756, center=999.3734222573454,
#'                     sigma=0.58493182568124724, gamma=0.090582029276037035,
#'                     fitStatus=2, curveModel='skewedGaussian')
#' class(fit1)     <- 'peakPantheR_curveFit'
#' 
#' # fake sample 2
#' # ROI data points
#' rt2             <- seq(990, 1010, by=20/250)
#' mz2             <- rep(522., length(rt2))
#' int2            <- (dnorm(rt2, mean=1002, sd=1.5) * 100) + 1
#' tmp_DataPoints2 <- data.frame(rt=rt2, mz=mz2, int=int2)
#' # fittedCurve
#' fit2        <- list(amplitude=37.073067416755556, center=1001.3736564832565,
#'                     sigma=0.58496485738212201, gamma=0.090553713725151905,
#'                     fitStatus=2, curveModel='skewedGaussian')
#' class(fit2)     <- 'peakPantheR_curveFit'
#' 
#' ## Plot features in 1 sample without colours
#' peakPantheR_plotEICFit(ROIDataPointSampleList=list(tmp_DataPoints1),
#'                         curveFitSampleList=list(fit1),
#'                         rtMin=995., rtMax=1005.,
#'                         sampling=250, sampleColour=NULL, verbose=FALSE)
#'
#' ## Plot features in 2 samples with colours
#' peakPantheR_plotEICFit(
#'                 ROIDataPointSampleList=list(tmp_DataPoints1,tmp_DataPoints2),
#'                 curveFitSampleList=list(fit1, fit2),
#'                 rtMin=c(995., 997.), rtMax=c(1005.,1007.),
#'                 sampling=250, sampleColour=c('blue', 'red'), verbose=FALSE)
#'
peakPantheR_plotEICFit  <- function(ROIDataPointSampleList,
                                    curveFitSampleList = NULL,
                                    rtMin = NULL,
                                    rtMax = NULL,
                                    sampling = 250,
                                    sampleColour = NULL,
                                    verbose = TRUE) {
    
    ## Check input in case ROIDataPointSampleList is not a list
    if (!is(ROIDataPointSampleList, "list")) {
        stop("Error: \"ROIDataPointSampleList\" must be a list of data.frame")
    }
    nbSpl <- length(ROIDataPointSampleList)
    
    # check curveFitSampleList, rtMin and rtMax
    plotFit <- FALSE
    if (!is.null(curveFitSampleList) & !is.null(rtMin) & !is.null(rtMax)) {
        if ((nbSpl == length(curveFitSampleList)) & (nbSpl == length(rtMin)) &
            (nbSpl == length(rtMax))) {
            plotFit <- TRUE
        } else {
            stop(paste0('"\"curveFitSampleList\", \"rtMin\", \"rtMax\" and ',
                        '\"ROIDataPointSampleList\" must be the same length'))
        }
    } else {
        if (verbose) {
            message(paste0('\"curveFitSampleList\", \"rtMin\" or \"rtMax\" no',
                            ' provided, curveFit will not be plotted'))
        }
    }
    
    # set default colour (add a sample color ID that will be match in the plot)
    colourSpl <- rep("black", nbSpl)
    if (!is.null(sampleColour)) {
        if (nbSpl == length(sampleColour)) {
            colourSpl <- sampleColour
        } else {
            if (verbose) {
                message(paste0('Warning: sampleColour length must match the ',
                                'number of samples; default colour used'))
            }
        }
    }
    sampleIDColour <- paste("spl", seq(1, nbSpl), sep = "")
    names(colourSpl) <- sampleIDColour
    
    
    ## Prepare data
    # raw spectra EICs
    all_EIC <- lapply(seq_along(ROIDataPointSampleList), function(x) {
        # generate ion chromatogram and add sampleIDColour
        tmp_EIC <- generateIonChromatogram(ROIDataPointSampleList[[x]],
                                            aggregationFunction = "sum")
        tmp_EIC <- cbind(tmp_EIC, specID = rep(paste("spl", x, sep = ""),
                                                    nrow(tmp_EIC)),
                        stringsAsFactors = FALSE)
    })
    input_EIC <- do.call(rbind, all_EIC)
    
    # curve fit
    if (plotFit) {
        all_fit <- lapply(seq_along(ROIDataPointSampleList), function(x) {
            # check rtMin, rtMax and curveFit exist, project curveFit and add
            # sampleIDColour
            if (!(is.na(rtMin[x])) & !(is.na(rtMax[x])) &
                all(!is.na(curveFitSampleList[[x]]))) {
                grid_rt <- seq(from = rtMin[x], to = rtMax[x],
                                by = ((rtMax[x] - rtMin[x])/(sampling - 1)))
                tmp_fit <- data.frame(rt = grid_rt,
                    int = predictCurve(curveFitSampleList[[x]], x = grid_rt))
                tmp_fit <- cbind(tmp_fit, specID = rep(paste("spl", x, sep=""),
                                                        nrow(tmp_fit)),
                            stringsAsFactors = FALSE)
            }
        })
        input_fit <- do.call(rbind, all_fit)
        # catch no curve fit left to plot
        if (is.null(input_fit)) {
            input_fit <- data.frame(matrix(, ncol = 3, nrow = 0,
                            dimnames = list(c(), c("rt", "int", "specID"))))
        }
    }
    
    
    ## Plot raw spectra and curve fit
    # init plot
    p_spec <- ggplot2::ggplot(NULL, ggplot2::aes(x),
                            environment = environment()) +
        ggplot2::theme_bw() + ggplot2::xlab("Retention Time (sec)") +
        ggplot2::ylab("Intensity") +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
        ggplot2::scale_color_manual(values = colourSpl) +
        ggplot2::theme(legend.position = "none")
    
    # plot EIC
    p_spec <- p_spec + ggplot2::geom_line(data = input_EIC,
                        ggplot2::aes(x = rt, y = int, group = factor(specID),
                                    colour = factor(specID)))
    
    # plot curve fit
    if (plotFit) {
        p_spec <- p_spec + ggplot2::geom_line(data = input_fit,
                        ggplot2::aes(x = rt, y = int, group = factor(specID),
                            colour = factor(specID)), linetype = "dashed")
    }
    
    return(p_spec)
}
