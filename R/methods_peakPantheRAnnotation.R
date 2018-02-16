#####################################################################
## Constructor for \link{peakPantheRAnnotation-class}, initialising a new peakPantheRAnnotation with default values, \code{spectraPaths} and \code{targetFeatTable} will set the samples and compounds to process
setMethod("initialize", "peakPantheRAnnotation",
          function(.Object,
                   spectraPaths = NULL,
                   targetFeatTable = NULL,
                   cpdID = numeric(),
                   cpdName = character(),
                   ROI = data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=F),
                   FIR = data.frame(rtMin=numeric(), rtMax=numeric(), mzMin=numeric(), mzMax=numeric(), stringsAsFactors=F),
                   uROI = data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=F),
                   filepath = character(),
                   uROIExist = FALSE,
                   useFIR = FALSE,
                   TIC = numeric(),
                   peakTables = list(),
                   EICs = list(),
                   ...) {
            ## load ... arguments (not really needed, as we initialise all default values)
            .Object <- methods::callNextMethod(.Object, ...)

            ## set spectra if spectraPaths is provided
            if (!is.null(spectraPaths)){
              # check input
              # is a vector of character
              if (!is.vector(spectraPaths) | is.list(spectraPaths) | !is.character(spectraPaths)){
                stop("specified spectraPaths is not a vector of character")
              }
              # load values and allocate size
              nbSpectra       <- length(spectraPaths)
              filepath        <- spectraPaths
              # set TIC default if no TIC passed in
              if (length(TIC) == 0) {
                TIC           <- as.numeric(rep(NA,nbSpectra))
              }
              # set peakTables default if no peakTables passed in
              if (length(peakTables) == 0) {
                peakTables    <- vector("list", nbSpectra)
              }
              # set EICs default if no EICs passed in
              if (length(EICs) == 0) {
                EICs          <- vector("list", nbSpectra)
              }
            }

            ## set compounds if targetFeatTable is provided
            if (!is.null(targetFeatTable)){
              # check input
              # is data.frame
              if (class(targetFeatTable) != "data.frame"){
                stop("specified targetFeatTable is not a data.frame")
              }
              # required columns are present
              if (!all(c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax") %in% colnames(targetFeatTable))){
                stop("expected columns in targetFeatTable are \"cpdID\", \"cpdName\", \"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\" and \"mzMax\"")
              }
              # column type
              if (dim(targetFeatTable)[1] != 0){
                if (!is.numeric(targetFeatTable$cpdID[1])){
                  stop("targetFeatTable$cpdID must be numeric")
                }
                if (!is.character(targetFeatTable$cpdName[1])){
                  stop("targetFeatTable$cpdName must be character")
                }
                if (!is.numeric(targetFeatTable$rtMin[1])){
                  stop("targetFeatTable$rtMin must be numeric")
                }
                if (!(is.numeric(targetFeatTable$rt[1]) | is.na(targetFeatTable$rt[1]))){
                  stop("targetFeatTable$rt must be numeric or NA")
                }
                if (!is.numeric(targetFeatTable$rtMax[1])){
                  stop("targetFeatTable$rtMax must be numeric")
                }
                if (!is.numeric(targetFeatTable$mzMin[1])){
                  stop("targetFeatTable$mzMin must be numeric")
                }
                if (!(is.numeric(targetFeatTable$mz[1]) | is.na(targetFeatTable$mz[1]))){
                  stop("targetFeatTable$mz must be numeric or NA")
                }
                if (!is.numeric(targetFeatTable$mzMax[1])){
                  stop("targetFeatTable$mzMax must be numeric")
                }
              }
              # load values and allocate size
              nbCompound  <- dim(targetFeatTable)[1]
              cpdID       <- targetFeatTable$cpdID
              cpdName     <- targetFeatTable$cpdName
              ROI         <- targetFeatTable[,c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
              # only set FIR and uROI to the correct size if not provided in input (at the correct size)
              if (dim(FIR)[1] != nbCompound) {
                FIR       <- data.frame(rtMin=as.numeric(rep(NA,nbCompound)), rtMax=as.numeric(rep(NA,nbCompound)), mzMin=as.numeric(rep(NA,nbCompound)), mzMax=as.numeric(rep(NA,nbCompound)), stringsAsFactors=F)
              }
              if (dim(uROI)[1] != nbCompound) {
                uROI      <- data.frame(rtMin=as.numeric(rep(NA,nbCompound)), rt=as.numeric(rep(NA,nbCompound)), rtMax=as.numeric(rep(NA,nbCompound)), mzMin=as.numeric(rep(NA,nbCompound)), mz=as.numeric(rep(NA,nbCompound)), mzMax=as.numeric(rep(NA,nbCompound)), stringsAsFactors=F)
                # if we reset, it doesn't exist
                uROIExist <- FALSE
              }
            }

            ## apply the final values
            .Object@cpdID       <- cpdID
            .Object@cpdName     <- cpdName
            .Object@ROI         <- ROI
            .Object@FIR         <- FIR
            .Object@uROI        <- uROI
            .Object@filepath    <- filepath
            .Object@uROIExist   <- uROIExist
            .Object@useFIR      <- useFIR
            .Object@TIC         <- TIC
            .Object@peakTables  <- peakTables
            .Object@EICs        <- EICs

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
