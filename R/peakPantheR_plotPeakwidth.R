#' @title Plot peak value and peakwidth by acquisition time or in input order
#'
#' @description For a single ROI, plot the peak value and peakwidth (RT, m/z,
#' ...) of detected peaks across multiple samples, by acquisition time or in
#' input order. If \code{rotateAxis=FALSE} x is run order / plot order, y is the
#' \code{apexValue} / \code{widthMin} / \code{widthMax}, if
#' \code{rotateAxis=TRUE} x is the measurement values and y the run order.
#'
#' @param apexValue (float) vector of apex value
#' @param widthMin (float) vector of detected peak minimum peakwidth value or
#' NULL (if NULL no peakwidth)
#' @param widthMax (float) vector of detected peak maximum peakwidth value or
#' NULL (uf NULL no peakwidth)
#' @param acquTime (POSIXct) vector of sample acquisition time as POSIXct or
#' NULL (if NULL points are plotted in the order values are passed as input with
#' the first on top or left)
#' @param varName (str) Name of the variable to plot
#' @param sampleColour (str) NULL or vector colour for each sample (same length
#' as \code{apexValue}, \code{widthMin}, \code{widthMax}, \code{acquTime})
#' @param rotateAxis (bool) if TRUE x and y axis are reversed
#' @param verbose (bool) if TRUE message when NA scans are removed
#' 
#' @return Grob (ggplot object)
#' 
#' @import scales
#' 
#' @export
#' 
#' @examples
#' ## Input data
#' apexVal <- c(1, 2, 3, 4)
#' minVal  <- c(0, 0, 2, 2)
#' maxVal  <- c(2, 4, 4, 5)
#' acqTime <- as.POSIXct(c('2017-07-13 21:06:14', '2017-07-14 21:06:14', 
#'                         '2017-07-15 21:06:14', '2017-07-16 21:06:14'))
#' 
#' ## Plot 4 sampels with colour
#' peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal,widthMax=maxVal,
#'                         acquTime=NULL, varName='Test variable 1',
#'                         sampleColour=c('blue','red','green','orange'),
#'                         rotateAxis=FALSE, verbose=FALSE)
#' 
#' ## Plot 4 samples with colour by acquisition time
#' peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal,widthMax=maxVal,
#'                         acquTime=acqTime, varName='Test variable 2',
#'                         sampleColour=c('blue','red','green','orange'),
#'                         rotateAxis=FALSE, verbose=FALSE)
#' 
#' ## Plot 4 samples with colour, rotate axis
#' peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal,widthMax=maxVal,
#'                         acquTime=NULL, varName='Test variable 3',
#'                         sampleColour=c('blue','red','green','orange'),
#'                         rotateAxis=TRUE, verbose=FALSE)
#' 
#' ## Plot 4 samples with colour by acquisition time, rotate axis
#' peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal,widthMax=maxVal,
#'                         acquTime=acqTime, varName='Test variable 4',
#'                         sampleColour=c('blue','red','green','orange'),
#'                         rotateAxis=FALSE, verbose=FALSE)
peakPantheR_plotPeakwidth <- function(apexValue, widthMin=NULL, widthMax = NULL,
    acquTime=NULL, varName="variable", sampleColour = NULL, rotateAxis = FALSE,
    verbose = TRUE) {

    ## Check input
    resInp <- plotPeakwidth_checkInput(apexValue, widthMin, widthMax, acquTime,
                                        sampleColour, verbose)
    nbSpl <- resInp$nbSpl; useRunOrder <- resInp$useRunOrder
    useWidth <- resInp$useWidth; colourSpl <- resInp$colourSpl
    sampleIDColour <- resInp$sampleIDColour

    ## Init plot draw default x/y will fill depending on rotation
    p <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) +
        ggplot2::theme_bw()
    # set fill and colour scale, with one color per sample ID
    p <- p + ggplot2::scale_colour_manual(values = colourSpl, guide = "none")
    p <- p + ggplot2::scale_fill_manual(values = colourSpl, guide = "none")

    ## tmp main axis 
    if (useRunOrder) {
        x_axis <- acquTime
    } else {
        x_axis <- seq(0, nbSpl - 1)  #first spectra on left
    }

    ## Plot peakwidth, apex values and apex points (flip x/y if rotateAxis)
    # must rotateAxis inside the function otherwise the checks for NA wouldn't 
    # work with flipped inputs
    p <- plotPeakwidth_plotWidthApex(p, useWidth, rotateAxis, x_axis,
                    widthMin, widthMax, sampleIDColour, apexValue, verbose)

    # some expected messages
    if (verbose) {
        if (useRunOrder) { message("Values plotted by run order")
        } else {           message("Values plotted by input order") }
    }

    ## final ordering and axis labelling
    p <- plotPeakwidth_rotateAxis_setLabels(p, rotateAxis, useRunOrder, 
                                            x_axis, acquTime, varName, verbose)

    return(p)
}


# -----------------------------------------------------------------------------
# peakPantheR_plotPeakwidth helper functions

# Check the input
plotPeakwidth_checkInput <- function(apexValue, widthMin, widthMax, acquTime,
                                    sampleColour, verbose){
    nbSpl <- length(apexValue)
    # check acquTime
    useRunOrder <- FALSE
    if (!is.null(acquTime)) {
        # acquTime is not a POSIXct
        if (!is(acquTime, "POSIXct")) {
            stop("Err","or: \"acquTime\" must be a vector of POSIXct") }
        # NA in acquTime
        if (any(is.na(acquTime))) {
            if (verbose) {
                message('War','ning: \"acquTime\" contains NA, run order ',
                                'will not be plotted') }
            acquTime <- NULL  # helps with unittesting as dates in $plot_env
                                # introduce OS differences
        } else {
            # check acquTime length
            if (nbSpl != length(acquTime)) {
                stop('Err','or: \"apexValue\" and \"acquTime\" must be the',
                            ' same length')
            } else { useRunOrder <- TRUE }
        }
    }
    # check widthMin & widthMax
    useWidth <- FALSE
    if (!is.null(widthMin) & !is.null(widthMax)) {
        # check length
        if ((nbSpl != length(widthMin)) | (nbSpl != length(widthMax))) {
            stop('\"apexValue\", \"widthMin\" and \"widthMax\" must be ',
                        'the same length')
        } else { useWidth <- TRUE }
    }
    # set default colour (add a sample color ID that will be match in the plot)
    colourSpl <- rep("black", nbSpl)
    if (!is.null(sampleColour)) {
        if (nbSpl == length(sampleColour)) {
            colourSpl <- sampleColour
        } else {
            if (verbose) {
                message('War','ning: sampleColour length must match the ',
                                'number of samples; default colour used') }
        }
    }
    sampleIDColour <- paste("spl", seq(1, nbSpl), sep = "")
    names(colourSpl) <- sampleIDColour

    return(list(nbSpl=nbSpl, useRunOrder=useRunOrder, useWidth=useWidth,
                colourSpl=colourSpl, sampleIDColour=sampleIDColour))
}
# Plot peakwidth, apex values and apex points (flip x/y if rotateAxis)
plotPeakwidth_plotWidthApex <- function(p, useWidth, rotateAxis, 
            x_axis, widthMin, widthMax, sampleIDColour, apexValue, verbose) {

    ## Apex value
    # add apex point (add the color ID to each point, and an ID pointing to the 
    # black stroke)
    tmp_pt <- data.frame(x = x_axis, y = apexValue, colr = sampleIDColour,
                        stringsAsFactors = FALSE)
    tmp_pt <- tmp_pt[!is.na(tmp_pt$y), ]

    # with peakwidth and black stroke
    if (useWidth) {
        ## Prepare peakWidth (must go first to be the bottom most layer)
        tmp_pwidth <- data.frame(x = c(x_axis, x_axis),
                                y = c(widthMin, widthMax),
                                colr = c(sampleIDColour, sampleIDColour),
                                stringsAsFactors = FALSE)
        tmp_pwidth <- tmp_pwidth[!is.na(tmp_pwidth$y),]#remove val without pkwdt
        if (rotateAxis) { #flip x and y
            p <- p + ggplot2::geom_line(data=tmp_pwidth,
                ggplot2::aes(x=y, y=x, group=x, colour=colr), linewidth=1)
            p <- p + ggplot2::geom_point(data = tmp_pt, 
                ggplot2::aes(x=y, y=x, fill=colr), 
                colour="black", shape=21, stroke=1, size=2)
        } else {
            p <- p + ggplot2::geom_line(data = tmp_pwidth, 
                ggplot2::aes(x=x, y=y, group=x, colour=colr), linewidth=1)
            p <- p + ggplot2::geom_point(data = tmp_pt, 
                ggplot2::aes(x=x, y=y, fill=colr), 
                colour="black", shape=21, stroke=1, size=2)
        }
        if (verbose) { message("Peakwidth values plotted") }

    # without peakwidth and black stroke
    } else {
        if (rotateAxis) { #flip x and y
            p <- p + ggplot2::geom_point(data=tmp_pt,
                ggplot2::aes(x=y, y=x, colour=colr))
        } else {
            p <- p + ggplot2::geom_point(data = tmp_pt, 
                ggplot2::aes(x=x, y=y, colour=colr))
        }
    }
    return(p)
}
# Set labels and rotate axis if necessary
plotPeakwidth_rotateAxis_setLabels <- function(p, rotateAxis, useRunOrder, 
                                        x_axis, acquTime, varName, verbose) {
    coord_y_datetime <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
        if (!is.null(ylim)) { ylim <- lubridate::as_datetime(ylim) }
        ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = expand)
    }
    ## Ordering of values and axis labelling
    if (useRunOrder) {
        if (rotateAxis) { # date axis rotated (+ flipped)
            p <- p + coord_y_datetime(ylim = c(max(acquTime), min(acquTime))) +
                ggplot2::xlab(varName) + 
                ggplot2::ylab("Acquisition Time") +
                ggplot2::theme(
                        axis.text.y=ggplot2::element_text(angle=45, hjust=1)) +
                ggplot2::scale_y_datetime(expand = c(0.007, 0.007))
            if (verbose) { message("x and y axis rotated") }
        } else {          # date axis not rotated
            p <- p + ggplot2::xlab("Acquisition Time") +
                ggplot2::ylab(varName) + 
                ggplot2::theme(
                        axis.text.x=ggplot2::element_text(angle=45, hjust=1)) +
                ggplot2::scale_x_datetime(expand = c(0.007, 0.007))
        }
    } else {
        if (rotateAxis) { # input order axis rotated (+ flipped)
            suppressMessages(
            p <- p +  #needs to go first
                ggplot2::scale_y_continuous(breaks=NULL, expand=c(0.007,0.007))+
                ggplot2::ylim(max(x_axis), min(x_axis)) +
                ggplot2::xlab(varName) + 
                ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                axis.text.y = ggplot2::element_blank(),
                                axis.ticks.y = ggplot2::element_blank())
            )
            if (verbose) { message("x and y axis rotated") }
        } else {          # input order axis not rotated
            p <- p + ggplot2::ylab(varName) + 
                ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                axis.text.x = ggplot2::element_blank(),
                                axis.ticks.x = ggplot2::element_blank()) +
                ggplot2::scale_x_continuous(breaks=NULL, expand=c(0.007,0.007))
        }
    }
    return(p)
}
