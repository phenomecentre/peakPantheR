#' Generate ion chromatogram from raw data points
#'
#' On the input raw data, aggregate intensites across the mz range at each retention time to generate an ion chromatogram: \code{sum} for EIC/TIC, \code{max}, \code{min} or \code{mean}. The number of data points returned correspond to the number of unique scans/retention time measurements in the input data
#' @param ROIDataPoint (data.frame) retention time "rt", mass "mz" and intensity "int" (as column) of each raw data points (as row) to use for the ion chromatogram
#' @param aggregationFunction (str) Function to use in order to aggregate intensities across mz in each scan. One of \code{sum}, \code{max}, \code{min}, \code{mean}
#'
#' @return A data.frame of retention time "rt" and aggregated intensities "int"
generateIonChromatogram <- function(ROIDataPoint, aggregationFunction='sum') {
  
  # Check input
  if (class(ROIDataPoint) != 'data.frame') {
    stop('Check input "ROIDataPoint" must be a data.frame')
  } else if (!all(c('rt', 'mz', 'int') %in% colnames(ROIDataPoint))) {
    stop('Check input "ROIDataPoint must have the following columns: "rt", "mz", "int"')
  }
  if (!(aggregationFunction) %in% c('sum', 'mean', 'min', 'max')) {
    stop('Check input "aggregationFunction" must be one of: "sum", "mean", "min", "max"')
  }
  
  unique_scans  <- sort(unique(ROIDataPoint$rt))
  res           <- data.frame(matrix(vector(), nrow=length(unique_scans), ncol=2, dimnames=list(c(),c('rt','int'))))
  
  # for each scan, aggregate intensity
  for (i in 1:length(unique_scans)) {
    cur_scan  <- unique_scans[i]
    tmp_int   <- ROIDataPoint[ROIDataPoint$rt == cur_scan, c('int')]
    res[i,]   <- c(cur_scan, do.call(aggregationFunction, list(tmp_int)))
  }
  
  return(res)
}
