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
## validObject method for \link{peakPantheRAnnotation-class}. Number of compounds based on @cpdID length, number of samples based on @filepath length. Slot type is not checked as \code{setClass} enforces it. peakTables and EICs type are checked on first list element.
setValidity("peakPantheRAnnotation", function(object) valid_peakPantheRAnnotation(object))



#####################################################################
## Accessors
# cpdID
setGeneric("cpdID", function(object, ...) standardGeneric("cpdID"))
setMethod("cpdID", "peakPantheRAnnotation",
          function(object) {
            object@cpdID
            })
# cpdName
setGeneric("cpdName", function(object, ...) standardGeneric("cpdName"))
setMethod("cpdName", "peakPantheRAnnotation",
          function(object) {
            object@cpdName
          })
# ROI
# targetFeatTable with ROI
setGeneric("ROI", function(object, ...) standardGeneric("ROI"))
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
setMethod("FIR", "peakPantheRAnnotation",
          function(object) {
            out         <- object@FIR
            out$cpdID   <- object@cpdID
            out$cpdName <- object@cpdName
            return(out)
          })
# filepath
setGeneric("filepath", function(object, ...) standardGeneric("filepath"))
setMethod("filepath", "peakPantheRAnnotation",
          function(object) {
            object@filepath
          })
# acquisitionTime
# return converted to POSIXct
setGeneric("acquisitionTime", function(object, ...) standardGeneric("acquisitionTime"))
setMethod("acquisitionTime", "peakPantheRAnnotation",
          function(object) {
            as.POSIXct(object@acquisitionTime)
          })
# uROIExist
setGeneric("uROIExist", function(object, ...) standardGeneric("uROIExist"))
setMethod("uROIExist", "peakPantheRAnnotation",
          function(object) {
            object@uROIExist
          })
# useUROI
setGeneric("useUROI", function(object, ...) standardGeneric("useUROI"))
setMethod("useUROI", "peakPantheRAnnotation",
          function(object) {
            object@useUROI
          })
# useFIR
setGeneric("useFIR", function(object, ...) standardGeneric("useFIR"))
setMethod("useFIR", "peakPantheRAnnotation",
          function(object) {
            object@useFIR
          })
# TIC
setGeneric("TIC", function(object, ...) standardGeneric("TIC"))
setMethod("TIC", "peakPantheRAnnotation",
          function(object) {
            object@TIC
          })
# peakTables
# add back cpdID and cpdName
setGeneric("peakTables", function(object, ...) standardGeneric("peakTables"))
setMethod("peakTables", "peakPantheRAnnotation",
          function(object) {
            tmpPeakTables <- lapply(object@peakTables, function(x) {cbind(x, cpdID=object@cpdID, cpdName=object@cpdName)})
            return(tmpPeakTables)
          })
# EICs
setGeneric("EICs", function(object, ...) standardGeneric("EICs"))
setMethod("EICs", "peakPantheRAnnotation",
          function(object) {
            object@EICs
          })
# isAnnotated
setGeneric("isAnnotated", function(object, ...) standardGeneric("isAnnotated"))
setMethod("isAnnotated", "peakPantheRAnnotation",
          function(object) {
            object@isAnnotated
          })
# nbSamples
setGeneric("nbSamples", function(object, ...) standardGeneric("nbSamples"))
setMethod("nbSamples", "peakPantheRAnnotation",
          function(object) {
            return(length(object@filepath))
          })
# nbCompounds
setGeneric("nbCompounds", function(object, ...) standardGeneric("nbCompounds"))
setMethod("nbCompounds", "peakPantheRAnnotation",
          function(object) {
            return(length(object@cpdID))
          })
# annotationTable
# data.frame with cpd as row (rownames are cpdName), samples as col (colnames are filepath)
setGeneric("annotationTable", function(object, column) standardGeneric("annotationTable"))
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
            tmpAnnotation           <- data.frame(t(sapply(object@peakTables, function(x, y){x[,y]}, y=column)), stringsAsFactors=F)
            rownames(tmpAnnotation) <- object@filepath
            colnames(tmpAnnotation) <- object@cpdName
            return(tmpAnnotation)
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

            ## EICs, filter samples first, then compound in each Chromatograms
            tmp_EICs        <- x@EICs[i]
            if (all(sapply(tmp_EICs, is.null))) {
              # no cpd filter if all NULL
              .EICs         <- tmp_EICs
            } else {
              # cpd filter in each Chromatograms
              if (length(j) == 1) {
                # if only 1 sample ensure we don't get a Chromatogram (no S)
                .EICs <- lapply(tmp_EICs, function(x, y) {MSnbase::Chromatograms(data=list(x[y,]))}, y=j)
              } else {
                # filter cpd, get a Chromatograms
                .EICs <- lapply(tmp_EICs, function(x, y) {x[y,]}, y=j)
              }
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
                                  EICs = .EICs,
                                  isAnnotated = .isAnnotated)
          })
