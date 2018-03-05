#' Plot a raw data and detected feature in a ROI
#'
#' plot a ROI (x is RT, y is intensity) with the matching detected peak rt and peakwidth under it
#'
#' @param EIC xcms::chromatograms (single spectra)
#' @param cpdID (str) Compound ID
#' @param cpdName (str) Compound Name
#' @param rt (float) detected peak apex retention time (in sec)
#' @param rtmin (float) detected peak minimum retention time (in sec)
#' @param rtmax (float) detected peak maximum retention time (in sec)
#' @param mzmin (float) ROI minimum m/z (matching EIC)
#' @param mzmax (float) ROI maximum m/z (matching EIC)
#'
#' @return Grob (ggplot object)
plotEICDetectedPeakwidth  <- function(EIC, cpdID, cpdName, rt, rtmin, rtmax, mzmin, mzmax) {

  ## Raw spectra
  # prepare data
  Int         <- xcms::intensity(EIC)
  filterNA    <- !is.na(Int)
  Int         <- Int[filterNA]
  RT          <- xcms::rtime(EIC)[filterNA]
  title       <- paste('CpdID: ', cpdID, ' - ', cpdName, ' ', round(mzmin, 4), '-', round(mzmax, 4))
  # plot
  p_spec1     <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) + ggplot2::ggtitle(title) + ggplot2::theme_bw() + ggplot2::ylab('Intensity') + ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.text.x=ggplot2::element_blank(), plot.title=ggplot2::element_text(size=ggplot2::rel(1)))
  p_spec1     <- p_spec1 + ggplot2::geom_line(data=data.frame(x=RT, y=Int), ggplot2::aes_string(x="x", y="y"))

  ## peakwidth plot
  p_peakwidth <- ggplot2::ggplot(NULL, ggplot2::aes(x), environment = environment()) + ggplot2::theme_bw() + ggplot2::xlab('Retention Time (sec)') + ggplot2::theme(axis.title.y=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(), axis.ticks.y=ggplot2::element_blank()) + ggplot2::scale_y_continuous(breaks=NULL)
  p_peakwidth <- p_peakwidth + ggplot2::geom_point(data=data.frame(x=rt, y=0), ggplot2::aes(x=x, y=y), colour='red', size=3)
  p_peakwidth <- p_peakwidth + ggplot2::geom_line(data=data.frame(x=c(rtmin,rtmax), y=c(0,0)), ggplot2::aes(x=x, y=y, group=y), colour='red', size=1)

  # set common x lim
  minX        <- min(ggplot2::layer_scales(p_spec1)$x$range$range[1], ggplot2::layer_scales(p_peakwidth)$x$range$range[1])
  maxX        <- max(ggplot2::layer_scales(p_spec1)$x$range$range[2], ggplot2::layer_scales(p_peakwidth)$x$range$range[2])
  p_spec1     <- p_spec1 + ggplot2::xlim(minX, maxX)
  p_peakwidth <- p_peakwidth + ggplot2::xlim(minX, maxX)
  # convert to gtables
  p_spec1     <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p_spec1))
  p_peakwidth <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p_peakwidth))
  #  find the widths of each of the plots, calculate the maximum and then apply it to each of them individually. This effectively applies a uniform layout to each of the plots.
  maxWidth                <- grid::unit.pmax(p_spec1$widths[2:3], p_peakwidth$widths[2:3])
  p_spec1$widths[2:3]     <- maxWidth
  p_peakwidth$widths[2:3] <- maxWidth
  p     <- gridExtra::grid.arrange(p_spec1, p_peakwidth, heights=c(6,1))

  return(p)
}
