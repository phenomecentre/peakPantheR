#' @title An S4 class to represent peakPantheR annotation results
#'
#' @description The \code{peakPantheRAnnotation} class is designed to run and store peakPantheR parallel annotation results. Instances of the class are created with the \code{peakPantheRAnnotation} constructor function, which initialises an object of proper dimension with \code{spectraPaths} (set samples to process) and \code{targetFeatTable} (set compounds to target). \code{spectraPaths} is a character vector of spectra file paths. \code{targetFeatTable} is a \code{\link{data.frame}} of compounds to target as rows and parameters as columns: \code{cpdID} (int), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#'
#' @details HERE DETAIL \code{targetFeatTable} and \code{annotationTable} which are generated on the fly from the slots.
#'
#'   The \code{validObject} method ensures the conformity of an object to the \code{peakPantheRAnnotation-class}. The number of compounds is based on @cpdID length, and the number of samples is based on @filepath length. Slot type is not checked as \code{setClass} enforces it. peakTables and EICs type are checked on the first list element.
#'
#' @slot cpdID A numeric vector of compound IDs, of length number of compounds
#' @slot cpdName A character vector of compound names, of length number of compounds
#' @slot ROI A data.frame of Regions Of Interest (ROI) with compounds as row and ROI parameters as columns: \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#' @slot FIR A data.frame of Fallback Integration Regions (FIR) with compounds as row and FIR parameters as columns: \code{rtMin} (float in seconds), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mzMax} (float).
#' @slot uROI A data.frame of updated Regions Of Interest (uROI) with compounds as row and uROI parameters as columns: \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#' @slot filepath A character vector of file paths, of length number of spectra files
#' @slot uROIExist A logical stating if uROI have been set
#' @slot useFIR A logical stating if FIR are to be used
#' @slot TIC A numeric vector of TIC or NA, of length number of spectra files
#' @slot peakTables A list of peakTable data.frame, of length number of spectra files. Each peakTable data.frame has compounds as rows and peak annotation results as columns.
#' @slot EICs A list of length number of spectra files. Each list element is \emph{NULL or list of length number of compounds} of \code{xcms::Chromatogram} matching the ROI or uROI for the given spectra.
#'
#' \subsection{Details:}{
#'   The \emph{peakTables} \code{data.frame} are structured as follow:
#'   \tabular{ll}{
#'     found \tab (bool) TRUE if compound was found in the raw data\cr
#'     mz \tab weighted (by intensity) mean of peak m/z across scans\cr
#'     mzmin \tab m/z peak minimum\cr
#'     mzmax \tab m/z peak maximum\cr
#'     rt \tab retention time of peak midpoint\cr
#'     rtmin \tab leading edge of peak retention time\cr
#'     rtmax \tab trailing edge of peak retention time\cr
#'     into \tab integrated peak intensity\cr
#'     intb \tab baseline corrected integrated peak intensity\cr
#'     maxo \tab maximum peak intensity\cr
#'     sn \tab Signal/Noise ratio, defined as \code{(maxo - baseline)/sd}, where \code{maxo} is the maximum peak intensity, \code{baseline} the estimated baseline value and \code{sd} the standard deviation of local chromatographic noise.\cr
#'     egauss \tab RMSE of Gaussian fit\cr
#'     mu \tab Gaussian parameter mu\cr
#'     sigma \tab Gaussian parameter sigma\cr
#'     h \tab Gaussian parameter h\cr
#'     f \tab Region number of m/z ROI where the peak was localised\cr
#'     dppm \tab m/z deviation of mass trace across scans in ppm\cr
#'     scale \tab Scale on which the peak was localised\cr
#'     scpos \tab Peak position found by wavelet analysis (scan number)\cr
#'     scmin \tab Left peak limit found by wavelet analysis (scan number)\cr
#'     scmax \tab Right peak limit found by wavelet analysis (scan number)\cr
#'     ppm_error \tab difference in ppm between the expected and measured m/z, not available if \code{peakStatistic=FALSE}\cr
#'     rt_dev_sec \tab difference in seconds between the expected and measured rt, not available if \code{peakStatistic=FALSE}\cr
#'     FWHM \tab full width at half maximum (in seconds), not available if \code{fitGauss=FALSE}, not available if \code{peakStatistic=FALSE}\cr
#'     FWHM_ndatapoints \tab number of scans on the peak, not available if \code{peakStatistic=FALSE}\cr
#'     tailingFactor \tab the tailing factor is a measure of peak tailing.It is defined as the distance from the front slope of the peak to the back slope divided by twice the distance from the center line of the peak to the front slope, with all measurements made at 5\% of the maximum peak height. The tailing factor of a peak will typically be similar to the asymmetry factor for the same peak, but the two values cannot be directly converted, not available if \code{peakStatistic=FALSE}\cr
#'     asymmetryFactor \tab the asymmetry factor is a measure of peak tailing. It is defined as the distance from the center line of the peak to the back slope divided by the distance from the center line of the peak to the front slope, with all measurements made at 10\% of the maximum peak height. The asymmetry factor of a peak will typically be similar to the tailing factor for the same peak, but the two values cannot be directly converted, not available if \code{peakStatistic=FALSE}\cr
#'   }
#' }
#'
#' @examples
#' \dontrun{
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
#' targetFeatTable[1,] <- c(1, "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
#' targetFeatTable[2,] <- c(2, "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
#' targetFeatTable[,c(1,3:8)] <- sapply(targetFeatTable[,c(1,3:8)], as.numeric)
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths, targetFeatTable=targetFeatTable)
#'
#' annotation
#' # An object of class peakPantheRAnnotation
#' #  2 compounds in 3 samples.
#' #   without updated ROI (uROI)
#' #   without fallback integration regions (FIR)
#'
#' slotNames(annotation)
#' # [1] "cpdID"      "cpdName"    "ROI"        "FIR"        "uROI"       "filepath"   "uROIExist"
#' # [8] "useFIR"     "TIC"        "peakTables" "EICs"
#'
#' annotation@cpdID
#' # [1] 1 2
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
#' annotation@uROIExist
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
#' annotation@EICs
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' # [[3]]
#' # NULL
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
                                  slot = c(cpdID = "numeric",
                                           cpdName = "character",
                                           ROI = "data.frame",
                                           FIR = "data.frame",
                                           uROI = "data.frame",
                                           filepath = "character",
                                           uROIExist = "logical",
                                           useFIR = "logical",
                                           TIC = "numeric",
                                           peakTables = "list",
                                           EICs = "list"))

