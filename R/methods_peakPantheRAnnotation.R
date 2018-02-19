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
