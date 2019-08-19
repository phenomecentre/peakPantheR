#####################################################################
## Constructor for \link{peakPantheRAnnotation-class}, see function
# peakPantheRAnnotation() for the initialisation checks
setMethod("initialize", "peakPantheRAnnotation", function(.Object, ...) {
    ## load ... arguments
    .Object <- methods::callNextMethod(.Object, ...)
    
    # Check the validity of results
    methods::validObject(.Object)
    
    return(.Object)
})



#####################################################################
## show method for \link{peakPantheRAnnotation-class}
setMethod("show",
    signature = "peakPantheRAnnotation",
    definition = function(object) {
        cat("An object of class ", class(object), "\n", sep = "")
        cat(" ", length(object@cpdName), " compounds in ",
            length(object@filepath), " samples. \n", sep = "")
        if (object@uROIExist) {
            cat("  updated ROI exist (uROI)\n", sep = "")
        } else {
            cat("  updated ROI do not exist (uROI)\n", sep = "")
        }
        if (object@useUROI) {
            cat("  uses updated ROI (uROI)\n", sep = "")
        } else {
            cat("  does not use updated ROI (uROI)\n", sep = "")
        }
        if (object@useFIR) {
            cat("  uses fallback integration regions (FIR)\n", sep = "")
        } else {
            cat("  does not use fallback integration regions (FIR)\n", sep = "")
        }
        if (object@isAnnotated) {
            cat("  is annotated\n", sep = "")
        } else {
            cat("  is not annotated\n", sep = "")
        }
        invisible(NULL)
})



#####################################################################
## validObject method for \link{peakPantheRAnnotation-class}.
# Number of compounds based on @cpdID length, number of samples based on
# @filepath length. Slot type is not checked as \code{setClass} enforces it.
# peakTables, dataPoints and peakFit type are checked on first list element.
setValidity("peakPantheRAnnotation",
            function(object) valid_peakPantheRAnnotation(object))



#####################################################################
## Accessors
#####################################################################

# cpdID
setGeneric("cpdID", function(object, ...) standardGeneric("cpdID"))
#' cpdID accessor
#' @param object peakPantheRAnnotation
#' @return (str) A character vector of compound IDs, of length number of
#' compounds
#' @docType methods
#' @aliases cpdID
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                         c('cpdID','cpdName','rtMin','rt','rtMax','mzMin',
#'                         'mz', 'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' cpdID(annotation)
#' # [1] 'ID-1' 'ID-2'
#' }
setMethod("cpdID", "peakPantheRAnnotation", function(object) {
    object@cpdID
})

# cpdName
setGeneric("cpdName", function(object, ...) standardGeneric("cpdName"))
#' cpdName accessor
#' @param object peakPantheRAnnotation
#' @return (str) A character vector of compound names, of length number of
#' compounds
#' @docType methods
#' @aliases cpdName
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                 c('cpdID', 'cpdName', 'rtMin', 'rt', 'rtMax', 'mzMin', 'mz',
#'                 'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' cpdName(annotation)
#' # [1] 'Cpd 1' 'Cpd 2'
#' }
setMethod("cpdName", "peakPantheRAnnotation", function(object) {
    object@cpdName
})

# ROI targetFeatTable with ROI
setGeneric("ROI", function(object, ...) standardGeneric("ROI"))
#' ROI accessor returns targetFeatTable with cpdID, cpdName added
#' @param object peakPantheRAnnotation
#' @return (data.frame) target feature table with compounds as row and ROI
#' parameters as columns
#' @docType methods
#' @aliases ROI
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                 c('cpdID', 'cpdName', 'rtMin', 'rt', 'rtMax', 'mzMin', 'mz',
#'                 'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ROI(annotation)
#' #   rtMin       rt rtMax    mzMin    mz    mzMax cpdID cpdName
#' # 1  3310 3344.888  3390 522.1948 522.2 522.2052  ID-1   Cpd 1
#' # 2  3280 3385.577  3440 496.1950 496.2 496.2050  ID-2   Cpd 2
#' }
setMethod("ROI", "peakPantheRAnnotation", function(object) {
    out <- object@ROI
    out$cpdID <- object@cpdID
    out$cpdName <- object@cpdName
    return(out)
})

# uROI targetFeatTable with uROI
setGeneric("uROI", function(object, ...) standardGeneric("uROI"))
#' uROI accessor returns targetFeatTable with cpdID, cpdName added
#' @param object peakPantheRAnnotation
#' @return (data.frame) target feature table with compounds as row and uROI
#' parameters as columns
#' @docType methods
#' @aliases uROI
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                 c('cpdID', 'cpdName', 'rtMin', 'rt', 'rtMax', 'mzMin', 'mz',
#'                 'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' uROI(annotation)
#' #   rtMin rt rtMax mzMin mz mzMax cpdID cpdName
#' # 1    NA NA    NA    NA NA    NA  ID-1   Cpd 1
#' # 2    NA NA    NA    NA NA    NA  ID-2   Cpd 2
#' }
setMethod("uROI", "peakPantheRAnnotation", function(object) {
    out <- object@uROI
    out$cpdID <- object@cpdID
    out$cpdName <- object@cpdName
    return(out)
})

# FIR similar to targetFeatTable with FIR
setGeneric("FIR", function(object, ...) standardGeneric("FIR"))
#' FIR accessor returns targetFeatTable with cpdID, cpdName added
#' @param object peakPantheRAnnotation
#' @return (data.frame) target feature table with compounds as row and FIR
#' parameters as columns
#' @docType methods
#' @aliases FIR
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                 c('cpdID', 'cpdName', 'rtMin', 'rt', 'rtMax', 'mzMin', 'mz',
#'                 'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' FIR(annotation)
#' #   rtMin rtMax mzMin mzMax cpdID cpdName
#' # 1    NA    NA    NA    NA  ID-1   Cpd 1
#' # 2    NA    NA    NA    NA  ID-2   Cpd 2
#' }
setMethod("FIR", "peakPantheRAnnotation", function(object) {
    out <- object@FIR
    out$cpdID <- object@cpdID
    out$cpdName <- object@cpdName
    return(out)
})

# filepath
setGeneric("filepath", function(object, ...) standardGeneric("filepath"))
#' filepath accessor
#' @param object peakPantheRAnnotation
#' @return (str) A character vector of file paths, of length number of spectra
#' files
#' @docType methods
#' @aliases filepath
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                 c('cpdID', 'cpdName', 'rtMin', 'rt', 'rtMax', 'mzMin', 'mz',
#'                 'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#' filepath(annotation)
#' # [1] 'C:/R/R-3.6.0/library/faahKO/cdf/KO/ko15.CDF'
#' # [2] 'C:/R/R-3.6.0/library/faahKO/cdf/KO/ko16.CDF'
#' # [3] 'C:/R/R-3.6.0/library/faahKO/cdf/KO/ko18.CDF'
#' }
setMethod("filepath", "peakPantheRAnnotation", function(object) {
    object@filepath
})

# cpdMetadata
setGeneric("cpdMetadata",
            function(object, ...) standardGeneric("cpdMetadata"))
#' cpdMetadata accessor
#' @param object peakPantheRAnnotation
#' @return (data.frame) A data.frame of compound metadata, with compounds as row
#' and metadata as columns
#' @docType methods
#' @aliases cpdMetadata
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values not initialised
#' cpdMetadata(annotation)
#' # data frame with 0 columns and 2 rows
#' }
setMethod("cpdMetadata", "peakPantheRAnnotation", function(object) {
    object@cpdMetadata
})

# spectraMetadata
setGeneric("spectraMetadata",
            function(object, ...) standardGeneric("spectraMetadata"))
#' spectraMetadata accessor
#' @param object peakPantheRAnnotation
#' @return (data.frame) A data.frame of sample metadata, with samples as row and
#' metadata as columns
#' @docType methods
#' @aliases spectraMetadata
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values not initialised
#' spectraMetadata(annotation)
#' # data frame with 0 columns and 3 rows
#' }
setMethod("spectraMetadata", "peakPantheRAnnotation", function(object) {
    object@spectraMetadata
})

# acquisitionTime return converted to POSIXct
setGeneric("acquisitionTime",
            function(object, ...) standardGeneric("acquisitionTime"))
#' acquisitionTime accessor returns value as.POSIXct
#' @param object peakPantheRAnnotation
#' @return (POSIXct) A character vector of acquisition date-time (converted from
#' POSIXct) or NA
#' @docType methods
#' @aliases acquisitionTime
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                 c('cpdID', 'cpdName', 'rtMin', 'rt', 'rtMax', 'mzMin', 'mz',
#'                 'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## acquisitionTime can only be extracted from NetCDF files
#' acquisitionTime(annotation)
#' # [1] NA NA NA
#' }
setMethod("acquisitionTime", "peakPantheRAnnotation", function(object) {
    as.POSIXct(object@acquisitionTime)
})

# uROIExist
setGeneric("uROIExist", function(object, ...) standardGeneric("uROIExist"))
#' uROIExist accessor
#' @param object peakPantheRAnnotation
#' @return (bool) flag if uROI have been set
#' @docType methods
#' @aliases uROIExist
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' uROIExist(annotation)
#' # [1] FALSE
#' }
setMethod("uROIExist", "peakPantheRAnnotation", function(object) {
    object@uROIExist
})

# useUROI
setGeneric("useUROI", function(object, ...) standardGeneric("useUROI"))
#' useUROI accessor
#' @param object peakPantheRAnnotation
#' @return (bool) flag if uROI are to be used
#' @docType methods
#' @aliases useUROI
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' useUROI(annotation)
#' # [1] FALSE
#' }
setMethod("useUROI", "peakPantheRAnnotation", function(object) {
    object@useUROI
})

# useFIR
setGeneric("useFIR", function(object, ...) standardGeneric("useFIR"))
#' useFIR accessor
#' @param object peakPantheRAnnotation
#' @return (bool) flag if FIR are to be used
#' @docType methods
#' @aliases useFIR
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' useFIR(annotation)
#' # [1] FALSE
#' }
setMethod("useFIR", "peakPantheRAnnotation", function(object) {
    object@useFIR
})

# TIC
setGeneric("TIC", function(object, ...) standardGeneric("TIC"))
#' TIC accessor
#' @param object peakPantheRAnnotation
#' @return (float) A numeric vector of Total Ion Chromatogram or NA, of length
#' number of spectra files
#' @docType methods
#' @aliases TIC
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' TIC(annotation)
#' # [1] NA NA NA
#' }
setMethod("TIC", "peakPantheRAnnotation", function(object) {
    object@TIC
})

# peakTables
setGeneric("peakTables", function(object, ...) standardGeneric("peakTables"))
#' peakTables accessor with cpdID and cpdName added back
#' @param object peakPantheRAnnotation
#' @return (data.frame) A list of peakTable data.frame, of length number of
#' spectra files. Each peakTable data.frame has compounds as rows and peak
#' annotation results as columns, with added compound ID and name.
#' @docType methods
#' @aliases peakTables
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' peakTables(annotation)
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' # [[3]]
#' # NULL
#' }
setMethod("peakTables", "peakPantheRAnnotation", function(object) {
    tmpPeakTables <- lapply(object@peakTables, function(x) {
        if (!is.null(x)) {
            return(cbind.data.frame(x, cpdID = object@cpdID,
                                    cpdName = object@cpdName,
                                    stringsAsFactors = FALSE))
        } else {
            return(NULL)
        }
    })
    
    return(tmpPeakTables)
})

# dataPoints
setGeneric("dataPoints", function(object, ...) standardGeneric("dataPoints"))
#' dataPoints accessor
#' @param object peakPantheRAnnotation
#' @return A list of length number of spectra files. Each list element is a
#' \emph{ROIsDataPoint} list of \code{data.frame} of raw data points for each
#' ROI/uROI (retention time 'rt', mass 'mz' and intensity 'int' (as column) of
#' each raw data points (as row))
#' @docType methods
#' @aliases dataPoints
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' dataPoints(annotation)
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' # [[3]]
#' # NULL
#' }
setMethod("dataPoints", "peakPantheRAnnotation", function(object) {
    object@dataPoints
})

# peakFit
setGeneric("peakFit", function(object, ...) standardGeneric("peakFit"))
#' peakFit accessor
#' @param object peakPantheRAnnotation
#' @return A list of length number of spectra files. Each list element is a
#' \emph{curveFit} list of \code{peakPantheR_curveFit} or NA for each ROI
#' @docType methods
#' @aliases peakFit
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' peakFit(annotation)
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' # [[3]]
#' # NULL
#' }
setMethod("peakFit", "peakPantheRAnnotation", function(object) {
    object@peakFit
})

# isAnnotated
setGeneric("isAnnotated", function(object, ...) standardGeneric("isAnnotated"))
#' isAnnotated accessor
#' @param object peakPantheRAnnotation
#' @return (bool) flag if the annotation has taken place
#' @docType methods
#' @aliases isAnnotated
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' isAnnotated(annotation)
#' # [1] FALSE
#' }
setMethod("isAnnotated", "peakPantheRAnnotation", function(object) {
    object@isAnnotated
})

# nbSamples
setGeneric("nbSamples", function(object, ...) standardGeneric("nbSamples"))
#' nbSamples accessor established on filepath
#' @param object peakPantheRAnnotation
#' @return (int) number of samples
#' @docType methods
#' @aliases nbSamples
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' nbSamples(annotation)
#' # [1] 3
#' }
setMethod("nbSamples", "peakPantheRAnnotation", function(object) {
    return(length(object@filepath))
})

# nbCompounds
setGeneric("nbCompounds", function(object, ...) standardGeneric("nbCompounds"))
#' nbCompounds accessor established on cpdID
#' @param object peakPantheRAnnotation
#' @return (int) number of samples
#' @docType methods
#' @aliases nbCompounds
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ##  compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' nbCompounds(annotation)
#' # [1] 2
#' }
setMethod("nbCompounds", "peakPantheRAnnotation", function(object) {
    return(length(object@cpdID))
})

# annotationTable
setGeneric("annotationTable",
            function(object, column) standardGeneric("annotationTable"))
#' @title annotationTable accessor
#' @description annotationTable returns a dataframe (row samples, col compounds)
#' filled with a specific peakTable column
#' @param object peakPantheRAnnotation
#' @param column a peakTable columns
#' @return (data.frame) (row samples, col compounds) filled with a specific
#' peakTable column
#' @docType methods
#' @aliases annotationTable
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' annotationTable(annotation)
#' #                                             ID-1 ID-2
#' # C:/R/R-3.6.0/library/faahKO/cdf/KO/ko15.CDF   NA   NA
#' # C:/R/R-3.6.0/library/faahKO/cdf/KO/ko16.CDF   NA   NA
#' # C:/R/R-3.6.0/library/faahKO/cdf/KO/ko18.CDF   NA   NA
#' }
setMethod("annotationTable", "peakPantheRAnnotation", function(object, column) {
    
    ## Expect the object to be valid, therefore peakTables is a list of NULL or
    ## data.frame
    nbCpd <- length(object@cpdID)
    nbSample <- length(object@filepath)
    
    ## Exit with empty data.frame if no samples or only NULL
    if (nbSample < 1) {
        # an empty data.frame
        tmpAnnotation <- data.frame(matrix(vector(), nbSample, nbCpd),
                                    stringsAsFactors = FALSE)
        rownames(tmpAnnotation) <- object@filepath
        colnames(tmpAnnotation) <- object@cpdID
        return(tmpAnnotation)
    }
    if (is.null(object@peakTables[[1]])) {
        # an empty data.frame
        tmpAnnotation <- data.frame(matrix(vector(), nbSample, nbCpd),
                                    stringsAsFactors = FALSE)
        rownames(tmpAnnotation) <- object@filepath
        colnames(tmpAnnotation) <- object@cpdID
        return(tmpAnnotation)
    }
    
    ## Check the column exist
    if (!(column %in% colnames(object@peakTables[[1]]))) {
        stop("input column is not a column of peakTables")
    }
    
    ## Concatenate all the results in a single data.frame
    tmpAnnotation <- vector("list", nbSample)
    for (fl in seq_len(nbSample)) {
        tmpAnnotation[fl] <- object@peakTables[[fl]][column]
    }
    tmpAnnotation <- data.frame(t(do.call("cbind", tmpAnnotation)))
    rownames(tmpAnnotation) <- object@filepath
    colnames(tmpAnnotation) <- object@cpdID
    return(tmpAnnotation)
})

# EICs
setGeneric("EICs",
            function(object, aggregationFunction = "sum", ...)
            standardGeneric("EICs"))
#' EICs accessor
#' @param object peakPantheRAnnotation
#' @param aggregationFunction (str) Function to use in order to aggregate
#' intensities across mz in each scan. One of \code{sum}, \code{max},
#' \code{min}, \code{mean}
#' @return (float) Extracted Ion Chromatogram aggregated across mz in each scan
#' @docType methods
#' @aliases EICs
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' ## default values without annotation
#' EICs(annotation)
#' # [[1]]
#' # list()
#' # [[2]]
#' # list()
#' # [[3]]
#' # list()
#' }
setMethod("EICs", "peakPantheRAnnotation",
            function(object, aggregationFunction) {
    tmpEICs <- lapply(object@dataPoints, function(ROIsDataPoint) {
        lapply(ROIsDataPoint, function(x) {
            generateIonChromatogram(x,aggregationFunction = aggregationFunction)
        })
    })
    return(tmpEICs)
})

# filename
setGeneric("filename", function(object, ...) standardGeneric("filename"))
#' filename accessor by spliting filepath
#' @param object peakPantheRAnnotation
#' @return (str) filename
#' @docType methods
#' @aliases filename
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                     targetFeatTable=targetFeatTable)
#'
#' filename(annotation)
#' # [1] 'ko15' 'ko16' 'ko18'
#' }
setMethod("filename", "peakPantheRAnnotation", function(object) {
    return(tools::file_path_sans_ext(basename(object@filepath)))
})




#####################################################################
# Sub-setting object
#####################################################################
#' extract parts of peakPantheRAnnotation class
#' @param x object from which to extract element(s) or in which to replace
#' element(s).
#' @param i (sample) indices specifying elements to extract or replace
#' @param j (compound) indices specifying elements to extract or replace
#' @param drop not applicable
#' @return (peakPantheRAnnotation) object subset
#'
#' @aliases [,peakPantheRAnnotation-method
#' @docType methods
#'
setMethod("[", "peakPantheRAnnotation", function(x, i, j, drop = "missing") {
    ## i is row, samples j is col, compounds
    
    # check inputs and fallback
    if (missing(i) & missing(j)) {
        return(x)
    }
    if (missing(i)) {
        i <- seq_len(nbSamples(x))
    }
    if (missing(j)) {
        j <- seq_len(nbCompounds(x))
    }
    
    # check dim size
    if (max(i) > nbSamples(x)) {
        stop(paste("i index out of bound: maximum", nbSamples(x)))
    }
    if (max(j) > nbCompounds(x)) {
        stop(paste("j index out of bound: maximum", nbCompounds(x)))
    }
    
    ## sub-setting
    .cpdID <- x@cpdID[j]
    .cpdName <- x@cpdName[j]
    .ROI <- x@ROI[j, , drop = FALSE]
    .FIR <- x@FIR[j, , drop = FALSE]
    .uROI <- x@uROI[j, , drop = FALSE]
    .filepath <- x@filepath[i]
    .cpdMetadata <- x@cpdMetadata[j, , drop = FALSE]
    .spectraMetadata <- x@spectraMetadata[i, , drop = FALSE]
    .acquisitionTime <- x@acquisitionTime[i]
    .uROIExist <- x@uROIExist
    .useUROI <- x@useUROI
    .useFIR <- x@useFIR
    .TIC <- x@TIC[i]
    .isAnnotated <- x@isAnnotated
    
    ## peakTables, filter samples first, then compounds in each table
    tmp_peakTables <- x@peakTables[i]
    if (all(vapply(tmp_peakTables, is.null, FUN.VALUE = logical(1)))) {
        # no cpd filter if all NULL
        .peakTables <- tmp_peakTables
    } else {
        # cpd filter in each table
        .peakTables <- lapply(tmp_peakTables, function(x, y) {
            x[y, ]
        }, y = j)
    }
    
    ## dataPoints, filter samples first, then compounds in each ROIsDataPoint
    tmp_dataPoints <- x@dataPoints[i]
    if (all(vapply(tmp_dataPoints, is.null, FUN.VALUE = logical(1)))) {
        # no cpd filter if all NULL
        .dataPoints <- tmp_dataPoints
    } else {
        # cpd filter in each ROIsDataPoint
        .dataPoints <- lapply(tmp_dataPoints, function(x, y) {
            x[y]
        }, y = j)
    }
    
    ## peakFit, filter samples first, then compounds in each curveFit list
    tmp_peakFit <- x@peakFit[i]
    if (all(vapply(tmp_peakFit, is.null, FUN.VALUE = logical(1)))) {
        # no cpd filter if all NULL
        .peakFit <- tmp_peakFit
    } else {
        # cpd filter in each curveFit list
        .peakFit <- lapply(tmp_peakFit, function(x, y) {
            x[y]
        }, y = j)
    }
    
    ## load value in new object that will need to pass validObject()
    peakPantheRAnnotation(cpdID = .cpdID, cpdName = .cpdName, ROI = .ROI,
        FIR = .FIR, uROI = .uROI, filepath = .filepath,
        cpdMetadata = .cpdMetadata, spectraMetadata = .spectraMetadata,
        acquisitionTime = .acquisitionTime, uROIExist = .uROIExist,
        useUROI = .useUROI, useFIR = .useFIR, TIC = .TIC,
        peakTables = .peakTables, dataPoints = .dataPoints, peakFit = .peakFit,
        isAnnotated = .isAnnotated)
})




#####################################################################
## Fit diagnosis
#####################################################################

## Set uROI and FIR based on annotation results
setGeneric("annotationParamsDiagnostic",
            function(object, verbose = TRUE, ...)
            standardGeneric("annotationParamsDiagnostic"))
#' @title Set uROI and FIR based on annotation results
#' @description Set updated ROI (uROI) and Fallback Integration Regions (FIR)
#' based on the annotation results. If the object is not annotated, it is
#' returned untouched. ROI is not modified. If uROI exist they are left
#' untouched, otherwise they are set as the minimum and maximum found peaks
#' limits (+/-5\% of ROI in retention time). If FIR are used they are left
#' untouched, otherwise they are set as the median of the found limits (rtMin,
#' rtMax, mzMin, mzMax).
#' @param object (peakPantheRAnnotation) Annotated peakPantheRAnnotation object
#' @param verbose (bool) If TRUE message progress of uROI and FIR calculation
#' @return (peakPantheRAnnotation) object with updated ROI and FIR set from
#' annotation results
#' @docType methods
#' @aliases annotationParamsDiagnostic
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' annotationParamsDiagnostic(emptyAnnotation, verbose=TRUE)
#' # Warning: the object has not been annotated, return the object untouched
#' # An object of class peakPantheRAnnotation
#' #  2 compounds in 3 samples.
#' #   updated ROI do not exist (uROI)
#' #   does not use updated ROI (uROI)
#' #   does not use fallback integration regions (FIR)
#' #   is not annotated
#' }
setMethod("annotationParamsDiagnostic", "peakPantheRAnnotation",
    function(object, verbose) {
    ## init
    outAnnotation <- object
    
    ## not annotated, pass
    if (!outAnnotation@isAnnotated) {
        if (verbose) {
            message(paste0('Warning: the object has not been annotated, return',
                            ' the object untouched'))
        }
        return(outAnnotation)
    }
    
    ## uROI uROI doesn't exist, set uROI with min/max of found peaks (if NA,
    ## use ROI value)
    if (!outAnnotation@uROIExist) {
        if (verbose) {
            message(paste0('uROI will be set as mimimum/maximum of found peaks',
                            ' (+/-5% of ROI in retention time)'))
        }
        # rt ROI 5% (no NA in ROI rtMin/rtMax)
        rtMargin <- (ROI(outAnnotation)$rtMax - ROI(outAnnotation)$rtMin) * 0.05
        # rtMin (min found peak -5% of ROI)
        rtMinUROI <- unname(vapply(annotationTable(outAnnotation,"rtMin"), min,
            na.rm = TRUE, FUN.VALUE = numeric(1)))
        rtMinUROI <- rtMinUROI - rtMargin
        if (sum(is.infinite(rtMinUROI)) != 0) {
            if (verbose) {
                message(paste0('uROI min rtMin which are NA are replaced with',
                                ' ROI rtMin'))
            }
            rtMinUROI[is.infinite(rtMinUROI)] <-
                outAnnotation@ROI[is.infinite(rtMinUROI), "rtMin"]
        }
        # rtMax (max found peak +5% of ROI)
        rtMaxUROI <- unname(vapply(annotationTable(outAnnotation,"rtMax"), max,
            na.rm = TRUE, FUN.VALUE = numeric(1)))
        rtMaxUROI <- rtMaxUROI + rtMargin
        if (sum(is.infinite(rtMaxUROI)) != 0) {
            if (verbose) {
                message(paste0('uROI max rtMax which are NA are replaced with',
                                ' ROI rtMax'))
            }
            rtMaxUROI[is.infinite(rtMaxUROI)] <-
                outAnnotation@ROI[is.infinite(rtMaxUROI), "rtMax"]
        }
        # mzMin
        mzMinUROI <- unname(vapply(annotationTable(outAnnotation,"mzMin"), min,
            na.rm = TRUE, FUN.VALUE = numeric(1)))
        if (sum(is.infinite(mzMinUROI)) != 0) {
            if (verbose) {
                message("uROI min mzMin which are NA are replaced ROI mzMin")
            }
            mzMinUROI[is.infinite(mzMinUROI)] <-
                outAnnotation@ROI[is.infinite(mzMinUROI), "mzMin"]
        }
        # mzMax
        mzMaxUROI <- unname(vapply(annotationTable(outAnnotation,"mzMax"), max,
            na.rm = TRUE, FUN.VALUE = numeric(1)))
        if (sum(is.infinite(mzMaxUROI)) != 0) {
            if (verbose) {
                message("uROI max mzMax which are NA are replaced ROI mzMax")
            }
            mzMaxUROI[is.infinite(mzMaxUROI)] <-
                outAnnotation@ROI[is.infinite(mzMaxUROI), "mzMax"]
        }
        # store new uROI values
        outAnnotation@uROI[, "rtMin"] <- rtMinUROI
        outAnnotation@uROI[, "rtMax"] <- rtMaxUROI
        outAnnotation@uROI[, "mzMin"] <- mzMinUROI
        outAnnotation@uROI[, "mzMax"] <- mzMaxUROI
        outAnnotation@uROI[, c("rt", "mz")] <- outAnnotation@ROI[,c("rt", "mz")]
        # set uROIExist
        outAnnotation@uROIExist <- TRUE
        # uROI exist (even not used), no replacement
    } else {
        if (verbose) {
            message("uROI already exist, will not be changed")
        }
    }
    
    ## FIR FIR not used, recalculate (if NA, use uROI value that was set
    ## previously)
    if (!outAnnotation@useFIR) {
        if (verbose) {
            message(paste0('FIR will be calculated as the median of found ',
                            '\"rtMin\",\"rtMax\",\"mzMin\",\"mzMax\"'))
        }
        # rtMin
        rtMinFIR <- unname(vapply(annotationTable(outAnnotation, "rtMin"),
            stats::median, na.rm = TRUE, FUN.VALUE = numeric(1)))
        if (sum(is.na(rtMinFIR)) != 0) {
            if (verbose) {
                message(paste0('FIR median rtMin which are NA are replaced ',
                                'with uROI rtMin'))
            }
            rtMinFIR[is.na(rtMinFIR)] <-
                outAnnotation@uROI[is.na(rtMinFIR), "rtMin"]
        }
        # rtMax
        rtMaxFIR <- unname(vapply(annotationTable(outAnnotation, "rtMax"),
            stats::median, na.rm = TRUE, FUN.VALUE = numeric(1)))
        if (sum(is.na(rtMaxFIR)) != 0) {
            if (verbose) {
                message(paste0('FIR median rtMax which are NA are replaced ',
                                'with uROI rtMax'))
            }
            rtMaxFIR[is.na(rtMaxFIR)] <-
                outAnnotation@uROI[is.na(rtMaxFIR), "rtMax"]
        }
        # mzMin
        mzMinFIR <- unname(vapply(annotationTable(outAnnotation, "mzMin"),
            stats::median, na.rm = TRUE, FUN.VALUE = numeric(1)))
        if (sum(is.na(mzMinFIR)) != 0) {
            if (verbose) {
                message("FIR median mzMin which are NA are replaced uROI mzMin")
            }
            mzMinFIR[is.na(mzMinFIR)] <-
                outAnnotation@uROI[is.na(mzMinFIR), "mzMin"]
        }
        # mzMax
        mzMaxFIR <- unname(vapply(annotationTable(outAnnotation, "mzMax"),
            stats::median, na.rm = TRUE, FUN.VALUE = numeric(1)))
        if (sum(is.na(mzMaxFIR)) != 0) {
            if (verbose) {
                message("FIR median mzMax which are NA are replaced uROI mzMax")
            }
            mzMaxFIR[is.na(mzMaxFIR)] <-
                outAnnotation@uROI[is.na(mzMaxFIR), "mzMax"]
        }
        # store new FIR values
        outAnnotation@FIR[, "rtMin"] <- rtMinFIR
        outAnnotation@FIR[, "rtMax"] <- rtMaxFIR
        outAnnotation@FIR[, "mzMin"] <- mzMinFIR
        outAnnotation@FIR[, "mzMax"] <- mzMaxFIR
        # FIR used, do not recalculate
    } else {
        if (verbose) {
            message("FIR in use, will not be changed")
        }
    }
    
    return(outAnnotation)
})


## Save annotation parameters as CSV
setGeneric("outputAnnotationParamsCSV",
            function(object, saveFolder, verbose = TRUE, ...)
            standardGeneric("outputAnnotationParamsCSV"))
#' @title Save annotation parameters as CSV
#' @description Save annotation parameters (ROI, uROI and FIR) to disk as a CSV
#' file for editing
#' @param object (peakPantheRAnnotation) Annotated peakPantheRAnnotation object
#' @param verbose (bool) If TRUE message progress
#' @param saveFolder (str) Path of folder where annotationParameters_summary.csv
#' will be saved
#' @return None
#' @docType methods
#' @aliases outputAnnotationParamsCSV
setMethod("outputAnnotationParamsCSV", "peakPantheRAnnotation",
            function(object, saveFolder, verbose) {
    # create table
    outTable <- data.frame(matrix(, nrow = nbCompounds(object), ncol = 0))
    outTable <- cbind(outTable, cpdID = cpdID(object), cpdName=cpdName(object))
    # ROI
    tmp_ROI <- ROI(object)[, c("rt", "mz", "rtMin", "rtMax", "mzMin", "mzMax")]
    colnames(tmp_ROI) <- c("ROI_rt", "ROI_mz", "ROI_rtMin", "ROI_rtMax",
                            "ROI_mzMin", "ROI_mzMax")
    outTable <- cbind(outTable, X = rep("|", nbCompounds(object)), tmp_ROI)
    # uROI
    tmp_uROI <- uROI(object)[,c("rtMin", "rtMax", "mzMin", "mzMax", "rt", "mz")]
    colnames(tmp_uROI) <- c("uROI_rtMin", "uROI_rtMax", "uROI_mzMin",
        "uROI_mzMax", "uROI_rt", "uROI_mz")
    outTable <- cbind(outTable, X = rep("|", nbCompounds(object)), tmp_uROI)
    # FIR
    tmp_FIR <- FIR(object)[, c("rtMin", "rtMax", "mzMin", "mzMax")]
    colnames(tmp_FIR) <- c("FIR_rtMin", "FIR_rtMax", "FIR_mzMin", "FIR_mzMax")
    outTable <- cbind(outTable, X = rep("|", nbCompounds(object)), tmp_FIR)
    
    # save table
    dir.create(saveFolder, recursive = TRUE, showWarnings = FALSE)
    targetFile <- paste(saveFolder, "/annotationParameters_summary.csv",sep="")
    if (verbose) {
        message("Annotation parameters saved at ", targetFile)
    }
    utils::write.csv(outTable, file = targetFile, row.names = FALSE,
                    fileEncoding = "UTF-8")
})


## Generate fit diagnostic plots
setGeneric("annotationDiagnosticPlots",
            function(object, sampleColour = NULL, sampling = 250,
                    verbose = TRUE, ...)
            standardGeneric("annotationDiagnosticPlots"))
#' @title Generate fit diagnostic plots
#' @description Generate fit diagnostic plots for each ROI: \code{EICFit} the
#' raw data and detected feature fit, \code{rtPeakwidthVert} detected peaks
#' retention time apex and peakwidth (vertical and no run order),
#' \code{rtPeakwidthHorzRunOrder} detected peaks retention time apex and
#' peakwidth by run order, \code{mzPeakwidthHorzRunOrder} detected peaks m/z
#' apex and peakwidth by run order, \code{areaRunOrder} detected peaks area by
#' run order, \code{rtHistogram} histogram of detected peaks retention time,
#' \code{mzHistogram} histogram of detected peaks m/z, \code{areaHistogram}
#' histogram of detected peaks area.
#' @param object (peakPantheRAnnotation) Annotated peakPantheRAnnotation object
#' @param sampleColour (str) NULL or vector colour for each sample
#' @param sampling (int) Number of points to employ when plotting fittedCurve
#' @param verbose (bool) if TRUE message the plot generation progress
#' @return A list (one list per compound) of diagnostic plots:
#' \code{result[[i]]$EICFit}, \code{result[[i]]$rtPeakwidthVert},
#' \code{result[[i]]$rtPeakwidthHorzRunOrder},
#' \code{result[[i]]$mzPeakwidthHorzRunOrder}, \code{result[[i]]$areaRunOrder},
#' \code{result[[i]]$rtHistogram}, \code{result[[i]]$mzHistogram},
#' \code{result[[i]]$areaHistogram}, \code{result[[i]]$title}
#' @docType methods
#' @aliases annotationDiagnosticPlots
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' annotationDiagnosticPlots(emptyAnnotation)
#' # Warning: the object has not been annotated, return an empty diagnostic plot
#' # list
#' # [[1]]
#' # NULL
#' # [[2]]
#' # NULL
#' }
setMethod("annotationDiagnosticPlots", "peakPantheRAnnotation",
    function(object, sampleColour, sampling, verbose) {
    # Init
    nbCpd <- nbCompounds(object)
    outList <- vector("list", nbCpd)
    
    ## Check object was annotated
    if (!object@isAnnotated) {
        message(paste0('Warning: the object has not been annotated, return an',
                        ' empty diagnostic plot list'))
        return(outList)
    }
    
    # Iterate over compounds
    for (cpd in seq_len(nbCpd)) {
        tmp_annotation <- object[, cpd]
        tmp_plotList <- vector("list", 9)
        names(tmp_plotList) <- c("EICFit", "rtPeakwidthVert",
            "rtPeakwidthHorzRunOrder", "mzPeakwidthHorzRunOrder","areaRunOrder",
            "rtHistogram", "mzHistogram", "areaHistogram", "title")
        
        # title
        tmp_plotList$title <- paste(cpdID(tmp_annotation), "-",
                                    cpdName(tmp_annotation))
        # plotEICFit
        tmp_plotList$EICFit <- peakPantheR_plotEICFit(
            ROIDataPointSampleList = unlist(dataPoints(tmp_annotation),
                                            recursive = FALSE),
            curveFitSampleList = unlist(peakFit(tmp_annotation),
                                            recursive = FALSE),
            rtMin = annotationTable(tmp_annotation, "rtMin")[, 1],
            rtMax = annotationTable(tmp_annotation, "rtMax")[, 1],
            sampling = sampling, sampleColour = sampleColour, verbose = verbose)
        # RT plotPeakwidth vertical
        tmp_plotList$rtPeakwidthVert <- peakPantheR_plotPeakwidth(
            apexValue = annotationTable(tmp_annotation, "rt")[, 1],
            widthMin = annotationTable(tmp_annotation, "rtMin")[, 1],
            widthMax = annotationTable(tmp_annotation, "rtMax")[, 1],
            acquTime = NULL, sampleColour = sampleColour,
            varName = "Retention Time (sec)", rotateAxis=TRUE, verbose=verbose)
        # RT plotPeakwidth horizontal run order
        tmp_plotList$rtPeakwidthHorzRunOrder <- peakPantheR_plotPeakwidth(
            apexValue = annotationTable(tmp_annotation, "rt")[, 1],
            widthMin = annotationTable(tmp_annotation, "rtMin")[, 1],
            widthMax = annotationTable(tmp_annotation, "rtMax")[, 1],
            acquTime = acquisitionTime(tmp_annotation),
            sampleColour = sampleColour, varName = "Retention Time (sec)",
            rotateAxis = FALSE, verbose = verbose)
        # mz plotPeakwidth horizontal run order
        tmp_plotList$mzPeakwidthHorzRunOrder <- peakPantheR_plotPeakwidth(
            apexValue = annotationTable(tmp_annotation, "mz")[, 1],
            widthMin = annotationTable(tmp_annotation, "mzMin")[, 1],
            widthMax = annotationTable(tmp_annotation, "mzMax")[, 1],
            acquTime = acquisitionTime(tmp_annotation),
            sampleColour = sampleColour, varName = "m/z", rotateAxis = FALSE,
            verbose = verbose)
        # peakarea horizontal run order
        tmp_plotList$areaRunOrder <- peakPantheR_plotPeakwidth(
            apexValue = annotationTable(tmp_annotation, "peakArea")[, 1],
            widthMin = NULL, widthMax = NULL,
            acquTime = acquisitionTime(tmp_annotation),
            sampleColour = sampleColour, varName = "Peak Area",
            rotateAxis = FALSE, verbose = verbose)
        # RT plotHistogram
        tmp_plotList$rtHistogram <- plotHistogram(
            var = annotationTable(tmp_annotation, "rt")[, 1],
            varName = "Retention Time (sec)", density = TRUE)
        # mz plotHistogram
        tmp_plotList$mzHistogram <- plotHistogram(
            var = annotationTable(tmp_annotation, "mz")[, 1],
            varName = "m/z", density = TRUE)
        # peak area plotHistogram
        tmp_plotList$areaHistogram <- plotHistogram(
            var = annotationTable(tmp_annotation, "peakArea")[, 1],
            varName = "Peak Area", density = TRUE)
        # store results
        outList[[cpd]] <- tmp_plotList
        if (verbose) {
            message("Compound ", cpd, "/", nbCpd, " done")
        }
    }
    return(outList)
})


## Save to disk the annotation parameters as CSV and a diagnostic plot per
## fitted compound
setGeneric("outputAnnotationDiagnostic",
    function(object, saveFolder, savePlots = TRUE, sampleColour = NULL,
            verbose = TRUE, ncores = 0, ...)
    standardGeneric("outputAnnotationDiagnostic"))
#' @title Save to disk the annotation parameters as CSV and a diagnostic plot
#' per fitted compound
#' @description Save to disk the annotation parameters as CSV (as generated by
#' \code{outputAnnotationParamsCSV()}) and a diagnostic plot per fitted compound
#' (as generated by \code{annotationDiagnosticMultiplot()}) if \code{savePlots}
#' is TRUE
#' @param object (peakPantheRAnnotation) Annotated peakPantheRAnnotation object
#' @param saveFolder (str) Path of folder where annotationParameters_summary.csv
#' and plots will be saved
#' @param savePlots (bool) If TRUE save a diagnostic plot for each compound
#' @param sampleColour (str) NULL or vector colour for each sample
#' @param verbose (bool) If TRUE message progress
#' @param ncores (int) Number of cores to use to save plots in parallel
#' @param ... Additional parameters for plotting i.e. \code{sampling} for the
#' number of points to employ when plotting fittedCurve
#' @return None
#' @docType methods
#' @aliases outputAnnotationDiagnostic
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # Calculate annotation
#' annotation <- peakPantheR_parallelAnnotation(emptyAnnotation, ncores=0,
#'                                 getAcquTime=FALSE, verbose=FALSE)$annotation
#'
#' # temporary location
#' savePath1       <- tempdir()
#' outputAnnotationDiagnostic(annotation, saveFolder=savePath1, savePlots=FALSE,
#'                             verbose=TRUE)
#' }
setMethod("outputAnnotationDiagnostic", "peakPantheRAnnotation",
    function(object, saveFolder, savePlots, sampleColour, verbose, ncores,...) {
    # @param cpdNb (int) cpd number betweem 1 and nbCompounds()
    # @param annotation (peakPantheRAnnotation) Annotation object
    # @param saveFolder (str) Path where plots will be saved
    # @param sampleColour (str) NULL or vector colour for each sample
    # @param verbose (bool) message progress
    # @param ... Additional parameters for plotting
    saveSingleMultiPlot <- function(cpdNb, annotation, saveFolder, sampleColour,
        verbose, ...) {
        # subset annotation to only 1 cpd
        tmp_annotation <- annotation[, cpdNb]
        # diagnostic plots
        tmp_diagPlotList <- annotationDiagnosticPlots(tmp_annotation,
            sampleColour = sampleColour, verbose = FALSE, ...)
        # multiplot
        suppressMessages(suppressWarnings(
            tmp_multiPlot <- annotationDiagnosticMultiplot(tmp_diagPlotList)))
        # save
        if (length(tmp_multiPlot) != 0) {
            # A4 page size
            tmp_targetFile <- paste("cpd_", cpdNb, ".png", sep = "")
            ggplot2::ggsave(file = tmp_targetFile, plot = tmp_multiPlot[[1]],
                device = "png", path = saveFolder, dpi = 100, width = 21,
                height = 29.7, units = "cm", limitsize = FALSE)
            # output path
            if (verbose) {
                message("  Compound ", cpdNb, "/", nbCpd,
                        " diagnostic plot saved at ",
                        paste(saveFolder, "/", tmp_targetFile, sep = ""))
            }
            # no plot to save
        } else {
            if (verbose) {
                message("  No plot to save for compound ", cpdNb, "/", nbCpd)
            }
        }
        return(NA)
    }
    
    
    ## Save standardised csv
    outputAnnotationParamsCSV(object, saveFolder = saveFolder,
                                verbose = verbose)
    
    ## Save all fit diagnostic
    if (savePlots) {
        # iterate over compound (more progressive plot generation and save than
        # generating all plots at once)
        nbCpd <- nbCompounds(object)
        
        # run in parallel
        if (ncores > 0) {
            if (verbose) {
                message("Saving ", nbCpd, " diagnostic plots in ", saveFolder)
            }
            
            # Open parallel interface
            cl <- parallel::makeCluster(ncores)
            doParallel::registerDoParallel(cl)
            # Run
            savedPlots <- foreach::foreach(x = seq_len(nbCpd),
                                            .inorder = TRUE) %dopar%
                saveSingleMultiPlot(cpdNb = x, annotation = object,
                    saveFolder = saveFolder, sampleColour = sampleColour,
                    verbose = verbose, ...)
            # Close
            parallel::stopCluster(cl)
            if (verbose) {
                message("All plots saved")
            }
            
            # run serial
        } else {
            if (verbose) {
                message("Saving diagnostic plots:")
            }
            for (cpd in seq_len(nbCpd)) {
                saveSingleMultiPlot(cpdNb = cpd, annotation = object,
                    saveFolder = saveFolder, sampleColour = sampleColour,
                    verbose = verbose, ...)
            }
        }
    }
})



## Save to disk all annotation results
setGeneric("outputAnnotationResult",
    function(object, saveFolder, annotationName = "annotationResult",
            verbose = TRUE)
    standardGeneric("outputAnnotationResult"))
#' @title Save to disk all annotation results as csv files
#' @description Save to disk all annotation results as
#' \code{annotationName_ ... .csv} files: compound metadata (\code{cpdMetadata},
#' \code{cpdID}, \code{cpdName}) and spectra metadata (\code{spectraMetadata},
#' \code{acquisitionTime}, \code{TIC}), summary of fit (ratio of peaks found:
#' \code{ratio_peaks_found}, ratio of peaks filled: \code{ratio_peaks_filled},
#' mean ppm_error: \code{ppm_error}, mean rt_dev_sec: \code{rt_dev_sec}), and a
#' file for each column of \code{peakTables} (with samples as rows and compounds
#' as columns)
#' @param object (peakPantheRAnnotation) Annotated peakPantheRAnnotation object
#' @param saveFolder (str) Path of folder where the annotation result csv will
#' be saved
#' @param annotationName (str) name of annotation to use in the saved csv
#' @param verbose (bool) If TRUE message progress
#' @return None
#' @docType methods
#' @aliases outputAnnotationResult
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # Calculate annotation
#' annotation      <- peakPantheR_parallelAnnotation(emptyAnnotation, ncores=0,
#'                                 getAcquTime=FALSE, verbose=FALSE)$annotation
#'
#' # temporary location
#' savePath1       <- tempdir()
#' outputAnnotationResult(annotation, saveFolder=savePath1,
#'                         annotationName='testProject', verbose=TRUE)
#' }
setMethod("outputAnnotationResult", "peakPantheRAnnotation",
    function(object, saveFolder, annotationName, verbose) {
    
    ## Check object was annotated
    if (!object@isAnnotated) {
        stop("Object has not been annotated, no annotation results to save")
    }
    
    ## Init folder
    dir.create(saveFolder, recursive = TRUE, showWarnings = FALSE)
    
    ## Save compound metadata
    tmp_cpdID <- cpdID(object)
    tmp_cpdName <- cpdName(object)
    tmp_cpdMetadata <- cpdMetadata(object)
    # tmp_rt, tmp_mz
    nbCpd <- nbCompounds(object)
    tmp_rt <- rep(as.numeric(NA), nbCpd)
    tmp_mz <- rep(as.numeric(NA), nbCpd)
    all_rt <- annotationTable(object, column = "rt")
    all_mz <- annotationTable(object, column = "mz")
    to_keep <- annotationTable(object, column = "found") &
        !annotationTable(object, column = "is_filled")
    for (i in seq_len(nbCpd)) {
        tmp_rt[i] <- mean(all_rt[to_keep[, i], i])
        tmp_mz[i] <- mean(all_mz[to_keep[, i], i])
    }
    tmp_outCpdMeta <- data.frame(cpdID = tmp_cpdID, cpdName = tmp_cpdName,
        rt = tmp_rt, mz = tmp_mz)
    colnames(tmp_outCpdMeta) <- c("cpdID", "cpdName", "Retention Time", "m/z")
    tmp_outCpdMeta <- cbind(tmp_outCpdMeta, tmp_cpdMetadata)
    path_cpdMeta <- paste(saveFolder, "/", annotationName,
                        "_cpdMetadata.csv", sep = "")
    utils::write.csv(tmp_outCpdMeta, file = path_cpdMeta, row.names = FALSE,
                    fileEncoding = "UTF-8")
    if (verbose) {
        message("Compound metadata saved at ", path_cpdMeta)
    }
    
    ## Save spectra metadata
    tmp_filepath <- filepath(object)
    tmp_acqTime <- acquisitionTime(object)
    tmp_TIC <- TIC(object)
    tmp_spectraMetadata <- spectraMetadata(object)
    tmp_outSpecMeta <- data.frame(filepath = tmp_filepath,
                            acquisitionTime = tmp_acqTime, TIC = tmp_TIC)
    tmp_outSpecMeta <- cbind(tmp_outSpecMeta, tmp_spectraMetadata)
    path_specMeta <- paste(saveFolder, "/", annotationName,
        "_spectraMetadata.csv", sep = "")
    utils::write.csv(tmp_outSpecMeta, file = path_specMeta, row.names = FALSE,
                    fileEncoding = "UTF-8")
    if (verbose) {
        message("Spectra metadata saved at ", path_specMeta)
    }
    
    ## Save peakTables columns
    for (i in colnames(object@peakTables[[1]])) {
        tmp_var <- annotationTable(object = object, column = i)
        path_var <- paste(saveFolder, "/", annotationName, "_", i, ".csv",
                        sep = "")
        utils::write.csv(tmp_var, file = path_var, row.names = TRUE,
                        fileEncoding = "UTF-8")
        if (verbose) {
            message("Peak measurement \"", i, "\" saved at ", path_var)
        }
    }
    
    ## Save summary table
    tmp_summary <- data.frame(matrix(ncol = 0, nrow = nbCompounds(object)),
                            stringsAsFactors = FALSE)
    rownames(tmp_summary) <- paste(cpdID(object), "-", cpdName(object))
    # used FIR (found = not filled, filled from is_filled)
    if (object@useFIR) {
        tmp_summary$ratio_peaks_found <- 1 - (colSums(
            annotationTable(object, column = "is_filled"))/nbSamples(object))
        tmp_summary$ratio_peaks_filled <- (colSums(
            annotationTable(object, column = "is_filled"))/nbSamples(object))
        # didn't use FIR (found from found, None are filled)
    } else {
        tmp_summary$ratio_peaks_found <- colSums(
            annotationTable(object, column = "found"))/nbSamples(object)
        tmp_summary$ratio_peaks_filled <- 0
    }
    tmp_summary$ppm_error <-
        colMeans(annotationTable(object, column = "ppm_error"), na.rm = TRUE)
    tmp_summary$rt_dev_sec <-
        colMeans(annotationTable(object, column = "rt_dev_sec"), na.rm = TRUE)
    path_summary <- paste(saveFolder, "/", annotationName, "_summary.csv",
                            sep = "")
    utils::write.csv(tmp_summary, file = path_summary, row.names = TRUE,
                    fileEncoding = "UTF-8")
    if (verbose) {
        message("Summary saved at ", path_specMeta)
    }
})




#####################################################################
# Reset a peakPantheRAnnotation and alter samples or compounds information

setGeneric("resetAnnotation",
    function(previousAnnotation, spectraPaths = NULL, targetFeatTable = NULL,
        uROI = NULL, FIR = NULL, cpdMetadata = NULL, spectraMetadata = NULL,
        uROIExist = NULL, useUROI = NULL, useFIR = NULL, verbose = TRUE, ...)
    standardGeneric("resetAnnotation"))
#' @title Reset a peakPantheRAnnotation and alter samples and compounds
#' information
#' @description Reset a peakPantheRAnnotation (remove results and set
#' \code{isAnnotated=FALSE}). If a different number of samples (
#' \code{spectraPaths}) or compounds (\code{targetFeatTable}) are passed, the
#' object will be initialised to the new size. For input values left as NULL,
#' the slots (\code{filepath} (from \code{spectraPaths}), \code{ROI},
#' \code{cpdID}, \code{cpdName} (from \code{targetFeatTable}), \code{uROI},
#' \code{FIR}, \code{cpdMetadata}, \code{spectraMetadata}, \code{uROIExist},
#' \code{useUROI} and \code{useFIR}) will be filled with values from
#' \code{previousAnnotation}.
#' @param previousAnnotation (peakPantheRAnnotation) object to reset
#' @param spectraPaths NULL or a character vector of spectra file paths, to set
#' samples to process
#' @param targetFeatTable NULL or a \code{\link{data.frame}} of compounds to
#' target as rows and parameters as columns: \code{cpdID} (str), \code{cpdName}
#' (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or
#' \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz}
#' (float or \emph{NA}), \code{mzMax} (float). Set compounds to target.
#' @param uROI NULL or a data.frame of updated Regions Of Interest (uROI) with
#' compounds as row and uROI parameters as columns: \code{rtMin} (float in
#' seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in
#' seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax}
#' (float).
#' @param FIR NULL or a data.frame of Fallback Integration Regions (FIR) with
#' compounds as row and FIR parameters as columns: \code{rtMin} (float in
#' seconds), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mzMax}
#' (float).
#' @param cpdMetadata NULL or a data.frame of compound metadata, with compounds
#' as row and metadata as columns
#' @param spectraMetadata NULL or a data.frame of sample metadata, with samples
#' as row and metadata as columns
#' @param uROIExist NULL or a logical stating if uROI have been set
#' @param useUROI NULL or a logical stating if uROI are to be used
#' @param useFIR NULL or a logical stating if FIR are to be used
#' @param verbose (bool) If TRUE message progress
#' @param ... Additional slots and values to set when resetting the object
#' (\code{cpdID}, \code{cpdName}, \code{ROI}, \code{filepath}, \code{TIC},
#' \code{acquisitionTime}, \code{peakTables}, \code{dataPoints}, \code{peakFit})
#' @return (peakPantheRAnnotation) object reset with previous results removed
#' and slots updated
#' @docType methods
#' @aliases resetAnnotation
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' smallAnnotation  <- peakPantheRAnnotation(spectraPaths=spectraPaths, 
#'                                         targetFeatTable=targetFeatTable)
#' smallAnnotation
#' # An object of class peakPantheRAnnotation
#' #  2 compounds in 2 samples.
#' #  updated ROI do not exist (uROI)
#' #  does not use updated ROI (uROI)
#' #  does not use fallback integration regions (FIR)
#' #  is not annotated
#'
#' # Reset and change number of spectra
#' newSpectraPaths  <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#' largerAnnotation <- resetAnnotation(smallAnnotation,
#'                                     spectraPaths=newSpectraPaths,
#'                                     verbose=TRUE)
#' largerAnnotation
#' # An object of class peakPantheRAnnotation
#' #  2 compounds in 3 samples.
#' #  updated ROI do not exist (uROI)
#' #  does not use updated ROI (uROI)
#' #  does not use fallback integration regions (FIR)
#' #  is not annotated
#' }
setMethod("resetAnnotation", "peakPantheRAnnotation",
    function(previousAnnotation, spectraPaths, targetFeatTable, uROI, FIR,
        cpdMetadata, spectraMetadata, uROIExist, useUROI, useFIR, verbose, ...){
    
    # If input is NULL, use previousAnnotation value, else use the passed value
    # If number of compounds or spectra is changed, the previous values
    # (metadata, uROI, FIR) cannot be reused (risk a mismatch of the metadata)
    if (verbose) { message("peakPantheRAnnotation object being reset:") }
    
    # targetFeatTable (cpdID, cpdName, ROI), uROI, FIR, cpdMetadata
    resetTarg <- resetAnnot_targetFeatTable_uROI_FIR_cpdMeta(previousAnnotation,
                            targetFeatTable, uROI, FIR, cpdMetadata, verbose)
    .targetFeatTable <- resetTarg$target
    .uROI <- resetTarg$uROI
    .FIR <- resetTarg$FIR
    .cpdMetadata <- resetTarg$cpd

    # spectraPaths, spectraMetadata
    resSpectra <- resetAnnot_spectraPathMetadata(previousAnnotation,
                                        spectraPaths, spectraMetadata, verbose)
    .spectraPaths <- resSpectra$spectraPaths
    .spectraMetadata <- resSpectra$spectraMetadata

    # uROIExist
    .uROIExist <- resetAnnot_uROIExist(uROIExist, targetFeatTable,
                                        previousAnnotation, verbose)

    # useUROI, useFIR
    resUSE <- resetAnnot_useUROIuseFIR(useUROI, useFIR, targetFeatTable,
                                        previousAnnotation, verbose)
    .useUROI <- resUSE$useUROI
    .useFIR <- resUSE$useFIR

    # is annotated
    .isAnnotated <- FALSE

    # Create new object In all case (old or new value) spectraPaths and
    # targetFeatTable will trigger the resetting of all results
    peakPantheRAnnotation(spectraPaths = .spectraPaths,
        targetFeatTable = .targetFeatTable, uROI = .uROI, FIR = .FIR,
        cpdMetadata = .cpdMetadata, spectraMetadata = .spectraMetadata,
        uROIExist = .uROIExist, useUROI = .useUROI, useFIR = .useFIR,
        isAnnotated = .isAnnotated, ...)
})
# resetAnnotation targetFeatTable (cpdID, cpdName, ROI), uROI, FIR, cpdMetadata
resetAnnot_targetFeatTable_uROI_FIR_cpdMeta <- function(previousAnnotation,
                            targetFeatTable, uROI, FIR, cpdMetadata, verbose) {
    # reuse previous values
    if (all(is.null(targetFeatTable))) {
        reset = resetAnnot_prevTargetFeatTable(previousAnnotation, uROI, FIR,
                                                cpdMetadata, verbose)
    } else {
        # reset to default
        reset = resetAnnot_newTargetFeatTable(targetFeatTable, uROI, FIR,
                                                cpdMetadata, verbose)
    }
    return(reset)
}
# resetAnnotation reuse previous targetFeatTable
resetAnnot_prevTargetFeatTable <- function(previousAnnotation, uROI, FIR,
                                            cpdMetadata, verbose) {
    # previous values
    .targetFeatTable <- ROI(previousAnnotation)
    if (verbose) {
        message("  Previous \"ROI\", \"cpdID\" and \"cpdName\" value kept")
    }
    # uROI
    if (all(is.null(uROI))) { # previous values
        .uROI <- uROI(previousAnnotation)[, c("rtMin", "rt", "rtMax",
                                                "mzMin", "mz", "mzMax")]
        if (verbose) {
            message("  Previous \"uROI\" value kept")
        }
    } else { # new value
        .uROI <- uROI
        if (verbose) {
            message("  New \"uROI\" value set")
        }
    }
    # FIR
    if (all(is.null(FIR))) { # previous values
        .FIR <- FIR(previousAnnotation)[, c("rtMin", "rtMax", "mzMin",
                                            "mzMax")]
        if (verbose) {
            message("  Previous \"FIR\" value kept")
        }
    } else { # new value
        .FIR <- FIR
        if (verbose) {
            message("  New \"FIR\" value set")
        }
    }
    # cpdMetadata
    if (all(is.null(cpdMetadata))) { # previous values
        .cpdMetadata <- cpdMetadata(previousAnnotation)
        if (verbose) {
            message("  Previous \"cpdMetadata\" value kept")
        }
    } else { # new value
        .cpdMetadata <- cpdMetadata
        if (verbose) {
            message("  New \"cpdMetadata\" value set")
        }
    }
    return(list(target=.targetFeatTable, uROI=.uROI, FIR=.FIR,
                cpd=.cpdMetadata))
}
# resetAnnotation new targetFeatTable
resetAnnot_newTargetFeatTable <- function(targetFeatTable, uROI, FIR,
                                            cpdMetadata, verbose) {
    # reset to default
    .targetFeatTable <- targetFeatTable
    if (verbose) {
        message(paste0('  New \"targetFeatTable\" value set (\"ROI\", ',
                        '\"cpdID\", \"cpdName\"')) }
    # uROI
    if (all(is.null(uROI))) {
        # Do not reuse old values if we change the compounds targeted
        .uROI <- data.frame(rtMin = numeric(), rt = numeric(),
            rtMax = numeric(), mzMin = numeric(), mz = numeric(),
            mzMax = numeric(), stringsAsFactors = FALSE)
        if (verbose) {
            message(paste0('  Targeted compounds changed, previous ',
                            '\"uROI\" cannot be kept and set to default')) }
    } else { # new value
        .uROI <- uROI
        if (verbose) { message("  New \"uROI\" value set") }
    }
    # FIR
    if (all(is.null(FIR))) {
        # Do not reuse old values if we change the compounds targeted
        .FIR <- data.frame(rtMin = numeric(), rtMax = numeric(),
            mzMin = numeric(), mzMax = numeric(), stringsAsFactors = FALSE)
        if (verbose) {
            message(paste0('  Targeted compounds changed, previous \"FIR\"',
                            ' cannot be kept and set to default'))
        }
    } else { # new value
        .FIR <- FIR
        if (verbose) { message("  New \"FIR\" value set") }
    }
    # cpdMetadata
    if (all(is.null(cpdMetadata))) {
        # Do not reuse old values if we change the compounds targeted
        .cpdMetadata <- data.frame()
        if (verbose) {
            message(paste0('  Targeted compounds changed, previous ',
                    '\"cpdMetadata\" cannot be kept and set to default'))
        }
    } else { # new value
        .cpdMetadata <- cpdMetadata
        if (verbose) { message("  New \"cpdMetadata\" value set") }
    }
    return(list(target=.targetFeatTable, uROI=.uROI, FIR=.FIR,
                cpd=.cpdMetadata))
}
# resetAnnotation spectraPaths, spectraMetadata
resetAnnot_spectraPathMetadata <- function(previousAnnotation, spectraPaths,
                                            spectraMetadata, verbose) {
    # reuse previous values
    if (all(is.null(spectraPaths))) {
        .spectraPaths <- filepath(previousAnnotation)
        if (verbose) {
            message("  Previous \"filepath\" value kept")
        }

        # spectraMetadata
        if (all(is.null(spectraMetadata))) {
            # previous values
            .spectraMetadata <- spectraMetadata(previousAnnotation)
            if (verbose) {
                message("  Previous \"spectraMetadata\" value kept")
            }
        } else {
            # new value
            .spectraMetadata <- spectraMetadata
            if (verbose) {
                message("  New \"spectraMetadata\" value set")
            }
        }

        # new values
    } else {
        .spectraPaths <- spectraPaths
        if (verbose) {
            message("  New \"spectraPaths\" value set")
        }

        # spectraMetadata
        if (all(is.null(spectraMetadata))) {
            # Do not reuse old values if we change the spectra
            .spectraMetadata <- data.frame()
            if (verbose) {
                message(paste0('  Targeted spectra changed, previous ',
                    '\"spectraMetadata\" cannot be kept and set to default'))
            }
        } else {
            # new value
            .spectraMetadata <- spectraMetadata
            if (verbose) {
                message("  New \"spectraMetadata\" value set")
            }
        }
    }
    return(list(spectraPaths = .spectraPaths,
                spectraMetadata = .spectraMetadata))
}
# resetAnnotation uROIExist
resetAnnot_uROIExist <- function(uROIExist, targetFeatTable, previousAnnotation,
                                verbose) {
    if (all(is.null(uROIExist))) {
        # previous values cannot be used if targetFeatTable is changed
        if (all(is.null(targetFeatTable))) {
            # previous values
            .uROIExist <- uROIExist(previousAnnotation)
            if (verbose) {
                message("  Previous \"uROIExist\" value kept")
            }
        } else {
            # Do not reuse old values if we change the compounds
            .uROIExist <- FALSE
            if (verbose) {
                message(paste0('  Targeted compounds changed, previous ',
                            '\"uROIExist\" cannot be kept and set to default'))
            }
        }
    } else {
        # new value
        .uROIExist <- uROIExist
        if (verbose) {
            message("  New \"uROIExist\" value set")
        }
    }
    return(.uROIExist)
}
# resetAnnotation useUROI, useFIR
resetAnnot_useUROIuseFIR <- function(useUROI, useFIR, targetFeatTable,
                                    previousAnnotation, verbose){
    # useUROI
    if (all(is.null(useUROI))) {
        # previous values cannot be used if targetFeatTable is changed
        if (all(is.null(targetFeatTable))) {
            # previous values
            .useUROI <- useUROI(previousAnnotation)
            if (verbose) {
                message("  Previous \"useUROI\" value kept")
            }
        } else {
            # Do not reuse old values if we change the compound
            .useUROI <- FALSE
            if (verbose) {
                message(paste0('  Targeted compounds changed, previous ',
                            '\"useUROI\" cannot be kept and set to default'))
            }
        }
    } else { # new value
        .useUROI <- useUROI
        if (verbose) { message("  New \"useUROI\" value set") }
    }

    # useFIR
    if (all(is.null(useFIR))) {
        # previous values cannot be used if targetFeatTable is changed
        if (all(is.null(targetFeatTable))) {
            # previous values
            .useFIR <- useFIR(previousAnnotation)
            if (verbose) {
                message("  Previous \"useFIR\" value kept")
            }
        } else { # Do not reuse old values if we change the compound
            .useFIR <- FALSE
            if (verbose) {
                message(paste0('  Targeted compounds changed, previous ',
                                '\"useFIR\" cannot be kept and set to default'))
            }
        }
    } else { # new value
        .useFIR <- useFIR
        if (verbose) { message("  New \"useFIR\" value set") }
    }
    return(list(useUROI = .useUROI, useFIR = .useFIR))
}


## Reset FIR windows to uROI or ROI values
setGeneric("resetFIR",
    function(object, verbose = TRUE, ...)
    standardGeneric("resetFIR"))
#' Reset FIR windows to uROI or ROI values
#' Reset FIR windows to uROI (or ROI if \code{uROIExist=FALSE})
#' @param object (peakPantheRAnnotation) object for which FIR are to be reset
#' @param verbose (bool) If TRUE message progress
#' @return (peakPantheRAnnotation) object with FIR values reset
#' @docType methods
#' @aliases resetFIR
#' @export
#' @examples
#' ## Initialise a peakPantheRAnnotation object with 2 targeted compounds
#'
#' ## targetFeatTable
#' input_targetFeatTable <- data.frame(matrix(vector(), 2, 8,
#'                             dimnames=list(c(), c('cpdID', 'cpdName', 'rtMin',
#'                             'rt', 'rtMax', 'mzMin', 'mz', 'mzMax'))),
#'                             stringsAsFactors=FALSE)
#' input_targetFeatTable[1,] <- c('ID-1', 'Cpd 1',  3.,  1.,  4.,  5.,  2.,  6.)
#' input_targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 19., 17., 20., 21., 18., 22.)
#' input_targetFeatTable[,c(3:8)] <- sapply(input_targetFeatTable[,c(3:8)],
#'                                         as.numeric)
#' ## FIR
#' input_FIR       <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), 
#'                                 c('rtMin', 'rtMax', 'mzMin', 'mzMax'))),
#'                                 stringsAsFactors=FALSE)
#' input_FIR[1,]   <- c(13., 14., 15., 16.)
#' input_FIR[2,]   <- c(29., 30., 31., 32.)
#'
#' annotation <- peakPantheRAnnotation(targetFeatTable = input_targetFeatTable,
#'                                     FIR = input_FIR, uROIExist = FALSE)
#'
#' ## Reset FIR with ROI values as uROI are not set
#' updatedAnnotation <- resetFIR(annotation, verbose=TRUE)
#' # FIR will be reset with ROI values as uROI values are not set
setMethod("resetFIR", "peakPantheRAnnotation", function(object, verbose) {
    
    # uROI exist
    if (uROIExist(object)) {
        if (verbose) {
            message("FIR will be reset with uROI values")
        }
        newFIR <- uROI(object)[, c("rtMin", "rtMax", "mzMin", "mzMax")]
        
        # uROI not defined
    } else {
        if (verbose) {
            message(paste0('FIR will be reset with ROI values as uROI values ',
                            'are not set'))
        }
        newFIR <- ROI(object)[, c("rtMin", "rtMax", "mzMin", "mzMax")]
    }
    
    object@FIR[, c("rtMin", "rtMax", "mzMin", "mzMax")] <-
        newFIR[, c("rtMin", "rtMax", "mzMin", "mzMax")]
    return(object)
})
