#' Generate ion chromatogram from raw data points
#'
#' On the input raw data, aggregate intensites across the mz range at each retention time to generate an ion chromatogram: \code{sum} for EIC/TIC, \code{max}, \code{min} or \code{mean}. The number of data points returned correspond to the number of unique scans/retention time measurements in the input data
#' @param ROIDataPoint (data.frame) retention time 'rt', mass 'mz' and intensity 'int' (as column) of each raw data points (as row) to use for the ion chromatogram
#' @param aggregationFunction (str) Function to use in order to aggregate intensities across mz in each scan. One of \code{sum}, \code{max}, \code{min}, \code{mean}
#'
#' @return A data.frame of retention time 'rt' and aggregated intensities 'int'
#' 
#' @details
#' ## Examples cannot be computed as the function is not exported:
#' ## Input data points
#' in_rt   <- c(3362.102, 3362.102, 3363.667, 3363.667, 3365.232, 3365.232, 3366.797, 3366.797,
#'             3368.362, 3368.362)
#' in_mz   <- c(496.2, 497.2, 496.2, 497.2, 496.2, 497.2, 496.2, 497.2, 496.2, 497.2)
#' in_int  <- c(39616, 11432, 63344, 18224, 107352, 30936, 182144, 51776, 295232, 81216)
#' input_ROIDataPoints <- data.frame(rt=in_rt, mz=in_mz, int=in_int)
#'
#' ## Aggregate mz to generate EIC
#' EIC <- generateIonChromatogram(input_ROIDataPoints, aggregationFunction='sum')
#' EIC
#' #         rt    int
#' # 1 3362.102  51048
#' # 2 3363.667  81568
#' # 3 3365.232 138288
#' # 4 3366.797 233920
#' # 5 3368.362 376448
generateIonChromatogram <- function(ROIDataPoint, aggregationFunction = "sum") {
    
    # Check input
    if (!is(ROIDataPoint, "data.frame")) {
        stop("Check input \"ROIDataPoint\" must be a data.frame")
    } else if (!all(c("rt", "mz", "int") %in% colnames(ROIDataPoint))) {
        stop("Check input \"ROIDataPoint must have the following columns: \"rt\", \"mz\", \"int\"")
    }
    if (!(aggregationFunction) %in% c("sum", "mean", "min", "max")) {
        stop("Check input \"aggregationFunction\" must be one of: \"sum\", \"mean\", \"min\", \"max\"")
    }
    
    # No data to aggregate
    if (dim(ROIDataPoint)[1] == 0) {
        return(data.frame(rt = numeric(), int = numeric()))
    }
    
    # Init
    unique_scans <- sort(unique(ROIDataPoint$rt))
    res <- data.frame(matrix(vector(), nrow = length(unique_scans), ncol = 2,
                                dimnames = list(c(), c("rt", "int"))))
    
    # for each scan, aggregate intensity
    for (i in seq_len(length(unique_scans))) {
        cur_scan    <- unique_scans[i]
        tmp_int     <- ROIDataPoint[ROIDataPoint$rt == cur_scan, c("int")]
        res[i, ]    <- c(cur_scan, do.call(aggregationFunction, list(tmp_int)))
    }
    
    return(res)
}
