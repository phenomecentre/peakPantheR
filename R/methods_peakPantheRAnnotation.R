#####################################################################
## Constructor for \link{peakPantheRAnnotation-class}, see function peakPantheRAnnotation() for the initialisation checks
setMethod("initialize", "peakPantheRAnnotation",
          function(.Object, ...) {
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
            cat("An object of class ", class(object), "\n", sep="")
            cat(" ", length(object@cpdName), " compounds in ", length(object@filepath), " samples. \n", sep="")
            if(object@uROIExist) {
              cat("  updated ROI exist (uROI)\n", sep="")
            } else {
              cat("  updated ROI do not exist (uROI)\n", sep="")
            }
            if(object@useUROI) {
              cat("  uses updated ROI (uROI)\n", sep="")
            } else {
              cat("  does not use updated ROI (uROI)\n", sep="")
            }
            if(object@useFIR) {
              cat("  uses fallback integration regions (FIR)\n", sep="")
            } else {
              cat("  does not use fallback integration regions (FIR)\n", sep="")
            }
            if(object@isAnnotated) {
              cat("  is annotated\n", sep="")
            } else {
              cat("  is not annotated\n", sep="")
            }
            invisible(NULL)
          })



#####################################################################
## validObject method for \link{peakPantheRAnnotation-class}. Number of compounds based on @cpdID length, number of samples based on @filepath length. Slot type is not checked as \code{setClass} enforces it. peakTables, dataPoints and peakFit type are checked on first list element.
setValidity("peakPantheRAnnotation", function(object) valid_peakPantheRAnnotation(object))



#####################################################################
## Accessors

# cpdID
setGeneric("cpdID", function(object, ...) standardGeneric("cpdID"))
#' cpdID accessor
#' @param object peakPantheRAnnotation
#' @aliases cpdID
#' @export
setMethod("cpdID", "peakPantheRAnnotation",
          function(object) {
            object@cpdID
            })

# cpdName
setGeneric("cpdName", function(object, ...) standardGeneric("cpdName"))
#' cpdName accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases cpdName
#' @export
setMethod("cpdName", "peakPantheRAnnotation",
          function(object) {
            object@cpdName
          })

# ROI
# targetFeatTable with ROI
setGeneric("ROI", function(object, ...) standardGeneric("ROI"))
#' ROI accessor returns targetFeatTable with cpdID, cpdName added
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases ROI
#' @export
setMethod("ROI", "peakPantheRAnnotation",
          function(object) {
            out         <- object@ROI
            out$cpdID   <- object@cpdID
            out$cpdName <- object@cpdName
            return(out)
          })

# uROI
# targetFeatTable with uROI
setGeneric("uROI", function(object, ...) standardGeneric("uROI"))
#' uROI accessor returns targetFeatTable with cpdID, cpdName added
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases uROI
#' @export
setMethod("uROI", "peakPantheRAnnotation",
          function(object) {
            out         <- object@uROI
            out$cpdID   <- object@cpdID
            out$cpdName <- object@cpdName
            return(out)
          })

# FIR
# similar to targetFeatTable with FIR
setGeneric("FIR", function(object, ...) standardGeneric("FIR"))
#' FIR accessor returns targetFeatTable with cpdID, cpdName added
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases FIR
#' @export
setMethod("FIR", "peakPantheRAnnotation",
          function(object) {
            out         <- object@FIR
            out$cpdID   <- object@cpdID
            out$cpdName <- object@cpdName
            return(out)
          })

# filepath
setGeneric("filepath", function(object, ...) standardGeneric("filepath"))
#' filepath accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases filepath
#' @export
setMethod("filepath", "peakPantheRAnnotation",
          function(object) {
            object@filepath
          })

# acquisitionTime
# return converted to POSIXct
setGeneric("acquisitionTime", function(object, ...) standardGeneric("acquisitionTime"))
#' acquisitionTime accessor returns value as.POSIXct
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases acquisitionTime
#' @export
setMethod("acquisitionTime", "peakPantheRAnnotation",
          function(object) {
            as.POSIXct(object@acquisitionTime)
          })

# uROIExist
setGeneric("uROIExist", function(object, ...) standardGeneric("uROIExist"))
#' uROIExist accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases uROIExist
#' @export
setMethod("uROIExist", "peakPantheRAnnotation",
          function(object) {
            object@uROIExist
          })

# useUROI
setGeneric("useUROI", function(object, ...) standardGeneric("useUROI"))
#' useUROI accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases useUROI
#' @export
setMethod("useUROI", "peakPantheRAnnotation",
          function(object) {
            object@useUROI
          })

# useFIR
setGeneric("useFIR", function(object, ...) standardGeneric("useFIR"))
#' useFIR accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases useFIR
#' @export
setMethod("useFIR", "peakPantheRAnnotation",
          function(object) {
            object@useFIR
          })

# TIC
setGeneric("TIC", function(object, ...) standardGeneric("TIC"))
#' TIC accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases TIC
#' @export
setMethod("TIC", "peakPantheRAnnotation",
          function(object) {
            object@TIC
          })

# peakTables
setGeneric("peakTables", function(object, ...) standardGeneric("peakTables"))
#' peakTables accessor with cpdID and cpdName added back
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases peakTables
#' @export
setMethod("peakTables", "peakPantheRAnnotation",
          function(object) {
            tmpPeakTables <- lapply(object@peakTables, function(x) {cbind.data.frame(x, cpdID=object@cpdID, cpdName=object@cpdName, stringsAsFactors=F)})
            return(tmpPeakTables)
          })

# dataPoints
setGeneric("dataPoints", function(object, ...) standardGeneric("dataPoints"))
#' dataPoints accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases dataPoints
#' @export
setMethod("dataPoints", "peakPantheRAnnotation",
          function(object) {
            object@dataPoints
          })

# peakFit
setGeneric("peakFit", function(object, ...) standardGeneric("peakFit"))
#' peakFit accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases peakFit
#' @export
setMethod("peakFit", "peakPantheRAnnotation",
          function(object) {
            object@peakFit
          })

# isAnnotated
setGeneric("isAnnotated", function(object, ...) standardGeneric("isAnnotated"))
#' isAnnotated accessor
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases isAnnotated
#' @export
setMethod("isAnnotated", "peakPantheRAnnotation",
          function(object) {
            object@isAnnotated
          })

# nbSamples
setGeneric("nbSamples", function(object, ...) standardGeneric("nbSamples"))
#' nbSamples accessor established on filepath
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases nbSamples
#' @export
setMethod("nbSamples", "peakPantheRAnnotation",
          function(object) {
            return(length(object@filepath))
          })

# nbCompounds
setGeneric("nbCompounds", function(object, ...) standardGeneric("nbCompounds"))
#' nbCompounds accessor established on cpdID
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases nbCompounds
#' @export
setMethod("nbCompounds", "peakPantheRAnnotation",
          function(object) {
            return(length(object@cpdID))
          })

# annotationTable
setGeneric("annotationTable", function(object, column) standardGeneric("annotationTable"))
#' annotationTable accessor
#' annotationTable returns a dataframe (row samples, col compounds) filled with a specific peakTable column
#' @param object peakPantheRAnnotation
#' @param column a peakTable columns
#' @docType methods
#' @aliases annotationTable
#' @export
setMethod("annotationTable", "peakPantheRAnnotation",
          function(object, column) {

            ## Expect the object to be valid, therefore peakTables is a list of NULL or data.frame
            nbCpd     <- length(object@cpdID)
            nbSample  <- length(object@filepath)

            ## Exit with empty data.frame if no samples or only NULL
            if (nbSample < 1) {
              # an empty data.frame
              tmpAnnotation           <- data.frame(matrix(vector(), nbSample, nbCpd), stringsAsFactors=F)
              rownames(tmpAnnotation) <- object@filepath
              colnames(tmpAnnotation) <- object@cpdName
              return(tmpAnnotation)
            }
            if (is.null(object@peakTables[[1]])) {
              # an empty data.frame
              tmpAnnotation           <- data.frame(matrix(vector(), nbSample, nbCpd), stringsAsFactors=F)
              rownames(tmpAnnotation) <- object@filepath
              colnames(tmpAnnotation) <- object@cpdName
              return(tmpAnnotation)
            }

            ## Check the column exist
            if (!(column %in% colnames(object@peakTables[[1]]))) {
              stop("input column is not a column of peakTables")
            }

            ## Concatenate all the results in a single data.frame
            if(nbCpd == 1) {
              # if only 1 compounds, sapply simplify to vector and not matrix
              tmpAnnotation         <- data.frame(matrix(sapply(object@peakTables, function(x, y){x[,y]}, y=column)), stringsAsFactors=F)
            } else {
              tmpAnnotation         <- data.frame(t(sapply(object@peakTables, function(x, y){x[,y]}, y=column)), stringsAsFactors=F)
            }
            rownames(tmpAnnotation) <- object@filepath
            colnames(tmpAnnotation) <- object@cpdName
            return(tmpAnnotation)
          })

# EICs
setGeneric("EICs", function(object, aggregationFunction='sum', ...) standardGeneric("EICs"))
#' EICs accessor
#' @param object peakPantheRAnnotation
#' @param aggregationFunction (str) Function to use in order to aggregate intensities across mz in each scan. One of \code{sum}, \code{max}, \code{min}, \code{mean}
#' @docType methods
#' @aliases EICs
#' @export
setMethod("EICs", "peakPantheRAnnotation",
          function(object, aggregationFunction) {
            tmpEICs <- lapply(object@dataPoints, function(ROIsDataPoint){lapply(ROIsDataPoint, function(x){generateIonChromatogram(x, aggregationFunction=aggregationFunction)}) })
            return(tmpEICs)
          })

# filename
setGeneric("filename", function(object, ...) standardGeneric("filename"))
#' filename accessor by spliting filepath
#' @param object peakPantheRAnnotation
#' @docType methods
#' @aliases filename
#' @export
setMethod("filename", "peakPantheRAnnotation",
          function(object) {
            return(tools::file_path_sans_ext(basename(object@filepath)))
          })




#####################################################################
## Sub-setting object
#' extract parts of peakPantheRAnnotation class
#' @param x object from which to extract element(s) or in which to replace element(s).
#' @param i (sample) indices specifying elements to extract or replace
#' @param j (compound) indices specifying elements to extract or replace
#' @param drop not applicable
#'
#' @aliases [,peakPantheRAnnotation-method
#' @docType methods
#'
setMethod("[", "peakPantheRAnnotation",
          function(x,i,j,drop="missing") {
            ## i is row, samples
            ## j is col, compounds

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
            .cpdID            <- x@cpdID[j]
            .cpdName          <- x@cpdName[j]
            .ROI              <- x@ROI[j,]
            .FIR              <- x@FIR[j,]
            .uROI             <- x@uROI[j,]
            .filepath         <- x@filepath[i]
            .acquisitionTime  <- x@acquisitionTime[i]
            .uROIExist        <- x@uROIExist
            .useUROI          <- x@useUROI
            .useFIR           <- x@useFIR
            .TIC              <- x@TIC[i]
            .isAnnotated      <- x@isAnnotated

            ## peakTables, filter samples first, then compounds in each table
            tmp_peakTables  <- x@peakTables[i]
            if (all(sapply(tmp_peakTables, is.null))) {
              # no cpd filter if all NULL
              .peakTables   <- tmp_peakTables
            } else {
              # cpd filter in each table
              .peakTables   <- lapply(tmp_peakTables, function(x, y) {x[y,]}, y=j)
            }
            
            ## dataPoints, filter samples first, then compounds in each ROIsDataPoint
            tmp_dataPoints  <- x@dataPoints[i]
            if (all(sapply(tmp_dataPoints, is.null))) {
              # no cpd filter if all NULL
              .dataPoints   <- tmp_dataPoints
            } else {
              # cpd filter in each ROIsDataPoint
              .dataPoints   <- lapply(tmp_dataPoints, function(x, y) {x[y]}, y=j)
            } 
            
            ## peakFit, filter samples first, then compounds in each curveFit list
            tmp_peakFit     <- x@peakFit[i]
            if (all(sapply(tmp_peakFit, is.null))) {
              # no cpd filter if all NULL
              .peakFit      <- tmp_peakFit
            } else {
              # cpd filter in each curveFit list
              .peakFit      <- lapply(tmp_peakFit, function(x, y) {x[y]}, y=j)
            } 
            
            ## load value in new object that will need to pass validObject()
            peakPantheRAnnotation(cpdID = .cpdID,
                                  cpdName = .cpdName,
                                  ROI = .ROI,
                                  FIR = .FIR,
                                  uROI = .uROI,
                                  filepath = .filepath,
                                  acquisitionTime = .acquisitionTime,
                                  uROIExist = .uROIExist,
                                  useUROI = .useUROI,
                                  useFIR = .useFIR,
                                  TIC = .TIC,
                                  peakTables = .peakTables,
                                  dataPoints = .dataPoints,
                                  peakFit = .peakFit,
                                  isAnnotated = .isAnnotated)
          })
