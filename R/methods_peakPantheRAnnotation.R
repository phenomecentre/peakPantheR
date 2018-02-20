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
              cat("  with updated ROI (uROI)\n", sep="")
            } else {
              cat("  without updated ROI (uROI)\n", sep="")
            }
            if(object@useFIR) {
              cat("  uses fallback integration regions (FIR)\n", sep="")
            } else {
              cat("  does not use fallback integration regions (FIR)\n", sep="")
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
# uROIExist
setGeneric("uROIExist", function(object, ...) standardGeneric("uROIExist"))
setMethod("uROIExist", "peakPantheRAnnotation",
          function(object) {
            object@uROIExist
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
setGeneric("peakTables", function(object, ...) standardGeneric("peakTables"))
setMethod("peakTables", "peakPantheRAnnotation",
          function(object) {
            object@peakTables
          })
# EICs
setGeneric("EICs", function(object, ...) standardGeneric("EICs"))
setMethod("EICs", "peakPantheRAnnotation",
          function(object) {
            object@EICs
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
