#' @title An S4 class to represent peakPantheR annotation results
#'
#' @description The \code{peakPantheRAnnotation} class is designed to run and store peakPantheR parallel annotation results. Instances of the class are created with the \code{peakPantheRAnnotation} constructor function, which initialises an object of proper dimension with \code{spectraPaths} (set samples to process) and \code{targetFeatTable} (set compounds to target). \code{spectraPaths} is a character vector of spectra file paths. \code{targetFeatTable} is a \code{\link{data.frame}} of compounds to target as rows and parameters as columns: \code{cpdID} (int), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#'
#' @details The \code{validObject} method ensures the conformity of an object to the \code{peakPantheRAnnotation-class}. The number of compounds is based on \code{@cpdID} length, and the number of samples is based on \code{@filepath} length. Slot type is not checked as \code{setClass} enforces it. peakTables and EICs type are checked on the first list element.
#'          \code{annotationTable(object, column)} where \emph{column} is a column from \emph{peakTable}, returns a data.frame of values with the samples as rows, ROI as columns.
#'
#' @slot cpdID A character vector of compound IDs, of length number of compounds
#' @slot cpdName A character vector of compound names, of length number of compounds
#' @slot ROI A data.frame of Regions Of Interest (ROI) with compounds as row and ROI parameters as columns: \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#' @slot FIR A data.frame of Fallback Integration Regions (FIR) with compounds as row and FIR parameters as columns: \code{rtMin} (float in seconds), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mzMax} (float).
#' @slot uROI A data.frame of updated Regions Of Interest (uROI) with compounds as row and uROI parameters as columns: \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#' @slot filepath A character vector of file paths, of length number of spectra files
#' @slot cpdMetadata A data.frame of compound metadata, with compounds as row and metadata as columns
#' @slot spectraMetadata A data.frame of sample metadata, with samples as row and metadata as columns
#' @slot acquisitionTime A character vector of acquisition date-time (converted from POSIXct) or NA
#' @slot uROIExist A logical stating if uROI have been set
#' @slot useUROI A logical stating if uROI are to be used
#' @slot useFIR A logical stating if FIR are to be used
#' @slot TIC A numeric vector of TIC or NA, of length number of spectra files
#' @slot peakTables A list of peakTable data.frame, of length number of spectra files. Each peakTable data.frame has compounds as rows and peak annotation results as columns.
#' @slot dataPoints A list of length number of spectra files. Each list element is a \emph{ROIsDataPoint} list of \code{data.frame} of raw data points for each ROI/uROI (retention time "rt", mass "mz" and intensity "int" (as column) of each raw data points (as row))
#' @slot peakFit A list of length number of spectra files. Each list element is a \emph{curveFit} list of \code{peakPantheR_curveFit} or NA for each ROI
#' @slot isAnnotated A logical stating if the annotation has taken place
#'
#' \subsection{Details:}{
#'   The \emph{peakTables} \code{data.frame} are structured as follow:
#'   \tabular{ll}{
#'     cpdID \tab database compound ID\cr
#'     cpdName \tab compound name\cr
#'     found \tab was the peak found\cr
#'     rt \tab retention time of peak apex (sec)\cr
#'     rtMin \tab leading edge of peak retention time (sec) determined at 0.5\% of apex intensity\cr
#'     rtMax \tab trailing edge of peak retention time (sec) determined at 0.5\% of apex intensity\cr
#'     mz \tab weighted (by intensity) mean of peak m/z across scans\cr
#'     mzMin \tab m/z peak minimum (between rtMin, rtMax)\cr
#'     mzMax \tab m/z peak maximum (between rtMin, rtMax)\cr
#'     peakArea \tab integrated peak area\cr
#'     maxIntMeasured \tab maximum peak intensity in raw data\cr
#'     maxIntPredicted \tab maximum peak intensity based on curve fit\cr
#'     is_filled \tab Logical indicate if the feature was integrated using FIR (Fallback Integration Region)\cr
#'     ppm_error \tab difference in ppm between the expected and measured m/z\cr
#'     rt_dev_sec \tab difference in seconds between the expected and measured rt\cr
#'     tailingFactor \tab the tailing factor is a measure of peak tailing.It is defined as the distance from the front slope of the peak to the back slope divided by twice the distance from the center line of the peak to the front slope, with all measurements made at 5\% of the maximum peak height. The tailing factor of a peak will typically be similar to the asymmetry factor for the same peak, but the two values cannot be directly converted\cr
#'     asymmetryFactor \tab the asymmetry factor is a measure of peak tailing. It is defined as the distance from the center line of the peak to the back slope divided by the distance from the center line of the peak to the front slope, with all measurements made at 10\% of the maximum peak height. The asymmetry factor of a peak will typically be similar to the tailing factor for the same peak, but the two values cannot be directly converted\cr
#'   }
#' }
#'
#' @examples
#' if(requireNamespace("faahKO")){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
#'                   system.file('cdf/KO/ko16.CDF', package = "faahKO"),
#'                   system.file('cdf/KO/ko18.CDF', package = "faahKO"))
#'
#' # targetFeatTable
#' targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID",
#'                          "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
#'                          stringsAsFactors=F)
#' targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
#' targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- sapply(targetFeatTable[,c(3:8)], as.numeric)
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths, targetFeatTable=targetFeatTable)
#'
#' annotation
#' # An object of class peakPantheRAnnotation
#' #  2 compounds in 3 samples.
#' #   updated ROI do not exist (uROI)
#' #   does not use updated ROI (uROI)
#' #   does not use fallback integration regions (FIR)
#' #   is not annotated
#'
#' slotNames(annotation)
#' # [1] "cpdID"       "cpdName"         "ROI"             "FIR"       "uROI"        "filepath"       
#' # [7] "cpdMetadata" "spectraMetadata" "acquisitionTime" "uROIExist" "useUROI"     "useFIR"         
#' # [13] "TIC"        "peakTables"      "dataPoints"      "peakFit"   "isAnnotated"
#'
#' annotation@cpdID
#' # [1] "ID-1" "ID-2"
#' annotation@cpdName
#' # [1] "Cpd 1" "Cpd 2"
#' annotation@ROI
#' #   rtMin       rt rtMax    mzMin    mz    mzMax
#' # 1  3310 3344.888  3390 522.1948 522.2 522.2052
#' # 2  3280 3385.577  3440 496.1950 496.2 496.2050
#' annotation@FIR
#' #   rtMin rtMax mzMin mzMax
#' # 1    NA    NA    NA    NA
#' # 2    NA    NA    NA    NA
#' annotation@uROI
#' #   rtMin rt rtMax mzMin mz mzMax
#' # 1    NA NA    NA    NA NA    NA
#' # 2    NA NA    NA    NA NA    NA
#' annotation@filepath
#' # [1] "C:/R/R-3.4.3/library/faahKO/cdf/KO/ko15.CDF" "C:/R/R-3.4.3/library/faahKO/cdf/KO/ko16.CDF"
#' # [2] "C:/R/R-3.4.3/library/faahKO/cdf/KO/ko18.CDF"
#' annotation@cpdMetadata
#' # data frame with 0 columns and 2 rows
#' annotation@spectraMetadata
#' # data frame with 0 columns and 3 rows
#' annotation@acquisitionTime
#' # [1] NA NA NA
#' annotation@uROIExist
#' # [1] FALSE
#' annotation@useUROI
#' # [1] FALSE
#' annotation@useFIR
#' # [1] FALSE
#' annotation@TIC
#' # [1] NA NA NA
#' annotation@peakTables
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' # [[3]]
#' # NULL
#' annotation@dataPoints
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' # [[3]]
#' # NULL
#' annotation@peakFit
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' # [[3]]
#' # NULL
#' annotation@isAnnotated
#' # [1] FALSE
#' }
#'
#' @family peakPantheR
#' @family parallelAnnotation
#'
#' @aliases peakPantheRAnnotation-class
#'
#' @import methods
#'
#' @export
peakPantheRAnnotation <- setClass("peakPantheRAnnotation",
                                  slot = c(cpdID = "character",
                                           cpdName = "character",
                                           ROI = "data.frame",
                                           FIR = "data.frame",
                                           uROI = "data.frame",
                                           filepath = "character",
                                           cpdMetadata = "data.frame",
                                           spectraMetadata = "data.frame",
                                           acquisitionTime = "character",
                                           uROIExist = "logical",
                                           useUROI = "logical",
                                           useFIR = "logical",
                                           TIC = "numeric",
                                           peakTables = "list",
                                           dataPoints = "list",
                                           peakFit = "list",
                                           isAnnotated = "logical"))

