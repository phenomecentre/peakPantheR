#' @title Search, integrate and report targeted features in a multiple spectra
#'
#' @description Integrate all target features in all files defined in the
#' initialised input object and store results. The use of updated ROI and the
#' integration of FIR are controled by the input object slots \code{useUROI} and
#' \code{useFIR}. Files are processed in parallel using
#' \link{peakPantheR_singleFileSearch}; \code{nCores} controls the number of
#' cores used for parallelisation, with \code{nCores=1} corresponding to serial
#' processing. If the processing of a file fails (file does not exist or error
#' during execution) the sample is removed from the outputed object.
#'
#' @param object (peakPantheRAnnotation) Initialised peakPantheRAnnotation
#' object defining the samples to process and compounds to target. The slots
#' \code{useUROI} and \code{useFIR} controls if uROI must be used and FIR
#' integrated if a feature is not found
#' @param BPPARAM (BiocParallel::BiocParallelParam) Settings for parallel 
#' processing. Must be a BiocParallelParam object
#' @param nCores (int) Number of cores to use for parallelisation. Default 1 for
#' no parallelisation.
#' @param getAcquTime (bool) If TRUE will extract sample acquisition date-time
#' from the mzML metadata (the additional file access will impact run time)
#' @param centroided (bool) use TRUE if the data is centroided, used by
#' \code{\link[MSnbase]{readMSData}} when reading the raw data files
#' @param curveModel (str) specify the peak-shape model to fit,
#' by default \code{skewedGaussian}.
#' Accepted values are \code{skewedGaussian} and \code{emgGaussian}
#' @param verbose (bool) If TRUE message calculation progress, time taken,
#' number of features found (total and matched to targets) and failures
#' @param ... Passes arguments to \code{findTargetFeatures} to alter
#' peak-picking parameters
#'
#' @return a list: \code{list()$result} \emph{(peakPantheRAnnotation)} fully
#' annotated object, \code{list()$failures} \emph{(list)} list of failed samples
#' and error message
#'
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Load data
#' library(faahKO)
#' 
#' # 3 files
#' input_spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                         system.file('cdf/KO/ko16.CDF', package = 'faahKO'),
#'                         system.file('cdf/KO/ko18.CDF', package = 'faahKO'))
#' 
#' # 4 features
#' input_ROI     <- data.frame(matrix(vector(), 4, 8,
#'                     dimnames=list(c(), c('cpdID', 'cpdName', 'rtMin', 'rt',
#'                                         'rtMax', 'mzMin', 'mz', 'mzMax'))),
#'                     stringsAsFactors=FALSE)
#' input_ROI[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                     522.2, 522.205222)
#' input_ROI[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                     496.2, 496.204962)
#' input_ROI[3,] <- c('ID-3', 'Cpd 3', 3420., 3454.435, 3495., 464.195358,
#'                     464.2, 464.204642)
#' input_ROI[4,] <- c('ID-4', 'Cpd 4', 3670., 3701.697, 3745., 536.194638,
#'                     536.2, 536.205362)
#' input_ROI[,c(3:8)] <- vapply(input_ROI[,c(3:8)], as.numeric,
#'                             FUN.VALUE=numeric(4))
#' 
#' # Initialise object
#' initAnnotation <- peakPantheRAnnotation(spectraPaths=input_spectraPaths,
#'                                         targetFeatTable=input_ROI)
#' # to use updated ROI:
#' # uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI
#' # to use FallBack Integration Regions:
#' # useFIR=TRUE, FIR=input_FIR
#' 
#' # Run serially
#' result_parallelAnnotation <- peakPantheR_parallelAnnotation(initAnnotation,
#'                                                         nCores=1,
#'                                                         getAcquTime=FALSE,
#'                                                         verbose=TRUE)
#' # Processing 4 compounds in 3 samples:
#' #  uROI:\tFALSE
#' #  FIR:\tFALSE
#' # ----- ko15 -----
#' # Polarity can not be extracted from netCDF files, please set manually the
#' #  polarity with the 'polarity' method.
#' # Reading data from 4 windows
#' # Data read in: 0.24 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #1
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #3
#' # Found 4/4 features in 0.06 secs
#' # Peak statistics done in: 0.02 secs
#' # Feature search done in: 0.76 secs
#' # ----- ko16 -----
#' # Polarity can not be extracted from netCDF files, please set manually the
#' #  polarity with the 'polarity' method.
#' # Reading data from 4 windows
#' # Data read in: 0.24 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #1
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #2
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #3
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #4
#' # Found 4/4 features in 0.08 secs
#' # Peak statistics done in: 0 secs
#' # Feature search done in: 0.71 secs
#' # ----- ko18 -----
#' # Polarity can not be extracted from netCDF files, please set manually the
#' #  polarity with the 'polarity' method.
#' # Reading data from 4 windows
#' # Data read in: 0.25 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #1
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #2
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #  mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #  ROI$mzMax for ROI #4
#' # Found 4/4 features in 0.06 secs
#' # Peak statistics done in: 0 secs
#' # Feature search done in: 0.71 secs
#' # ----------------
#' # Parallel annotation done in: 2.18 secs
#' 
#' # No failures
#' result_parallelAnnotation$failures
#'
#' result_parallelAnnotation$annotation
#' # An object of class peakPantheRAnnotation
#' #  4 compounds in 3 samples. 
#' #    updated ROI do not exist (uROI)
#' #    does not use updated ROI (uROI)
#' #    does not use fallback integration regions (FIR)
#' #    is annotated
#' }
#'
#' @family peakPantheR
#' @family parallelAnnotation
#'
#' @import mzR
#' @import BiocParallel
#'
#' @export
peakPantheR_parallelAnnotation <- function(object, BPPARAM=NULL, nCores = 1,
    getAcquTime = TRUE, centroided = TRUE,
    curveModel='skewedGaussian', verbose=TRUE, ...){

    # Check inputs, Initialise variables and outputs
    initRes    <- parallelAnnotation_init(object, BPPARAM, nCores, verbose)
    file_paths<-initRes$file_paths; target_peak_table<-initRes$target_peak_table
    input_FIR  <- initRes$input_FIR; BPPARAMObject <- initRes$BPPARAMObject

    stime <- Sys.time()

    # Run singleFileSearch
    # (list, each item is the result of a file, errors are passed into the list)
    allFilesRes <- parallelAnnotation_runSingleFileSearch(file_paths,
        target_peak_table, input_FIR, BPPARAM = BPPARAMObject, getAcquTime,
        centroided, curveModel, verbose,...)

    # Collect, process and reorder results
    res <- parallelAnnotation_process(allFilesRes, object, verbose)
    outObject <- res$outObject; fail_table <- res$fail_table

    ## check validity and exit
    validObject(outObject)
    
    etime <- Sys.time()
    if (verbose) {
        message("----------------")
        message("Parallel annotation done in: ",
            round(as.double(difftime(etime, stime)), 2), " ",
            units(difftime(etime, stime)))
        message("  ", dim(fail_table)[1], " failure(s)")
    }
    
    return(list(annotation = outObject, failures = fail_table))
}


# ------------------------------------------------------------------------------

## Check input file exist, wrap \code{peakPantheR_singleFileSearch} in a
## try cratch, add a failure status
# @param singleSpectraDataPath (str) path to netCDF or mzML raw data
# file (centroided, \strong{only with the channel of interest}).
# @param targetFeatTable a \code{\link{data.frame}} of compounds to
# target as rows. Columns: \code{cpdID} (str), \code{cpdName} (str),
# \code{rtMin} (float in seconds), \code{rt} (float in seconds, or
# \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float),
# \code{mz} (float or \emph{NA}), \code{mzMax} (float).
# @param FIR (data.frame or NULL) If not NULL, integrate Fallback
# Integration Regions (FIR) when a feature is not found. Compounds as
# row are identical to \code{targetFeatTable}, columns are \code{rtMin}
# (float in seconds), \code{rtMax} (float in seconds), \code{mzMin}
# (float), \code{mzMax} (float).
# @param getAcquTime (bool) If TRUE will extract sample acquisition
# date-time from the mzML metadata (the additional file access will
# impact run time) @param verbose (bool) If TRUE message calculation
# progress, time taken and number of features found (total and matched
# to targets)
# @param ... Passes arguments to \code{findTargetFeatures} to alter
# peak-picking parameters @return a list: \code{list()$TIC} \emph{(int)}
# TIC value, \code{list()$peakTable} \emph{(data.frame)} targeted
# features results (see Details), \code{list()$curveFit} \emph{(list)}
# list of \code{peakPantheR_curveFit} or NA for each ROI,
# \code{list()$acquTime} \emph{(POSIXct or NA)} date-time of sample
# acquisition from mzML metadata, \code{list()$ROIsDataPoint}
# \emph{(list)} a list of \code{data.frame} of raw data points for each
# ROI (retention time 'rt', mass 'mz' and intensity 'int' (as column) of
# each raw data points (as row)). \code{list()$failure} \emph{(named str
#  or NULL)} a string detailing the error (named with the
#  singleSpectraDataPath) or NA if the processing is successful.
parallelAnnotation_parallelHelper <- function(singleSpectraDataPath,
targetFeatTable, inFIR=NULL, inGetAcquTime=FALSE,centr=TRUE,
curveModel='skewedGaussian', inVerbose=TRUE,...){
    # Check input path exist or exit with error message
    if (!file.exists(singleSpectraDataPath)) {
        if (inVerbose) { message("Err","or file does not exist: ",
                singleSpectraDataPath) }
        # add error status
        failureMsg <- paste("Err","or file does not exist: ",
            singleSpectraDataPath, sep = "")
        names(failureMsg) <- singleSpectraDataPath
        # return basic values and failure message
        return(list(TIC = as.numeric(NA), peakTable = NULL,
        acquTime = as.character(NA), curveFit = NULL,
        ROIsDataPoint = NULL, failure = failureMsg)) }
    # Run singleFileSearch in try catch
    file_name <- tools::file_path_sans_ext(basename(singleSpectraDataPath))
    # progress
    if (inVerbose) { message("----- ", file_name, " -----") }
    # try catch
    result <- tryCatch({
        # singleFileSearch
        tmpResult <- peakPantheR_singleFileSearch(singleSpectraDataPath,
            targetFeatTable, peakStatistic = TRUE, plotEICsPath = NA,
            getAcquTime = inGetAcquTime, FIR = inFIR, centroided = centr,
            curveModel = curveModel, verbose = inVerbose, ...)
        # add failure status
        failureMsg <- NA
        names(failureMsg) <- singleSpectraDataPath
        tmpResult$failure <- failureMsg
        # last evaluation of Try is returned
        return(tmpResult)
    }, error = function(err) {
        # message error
        if (inVerbose) {
            message("-----")
            message("Err","or processing file: ", file_name)
            message(err$message)
            message("\n-----") }
        # add error status
        failureMsg <- err$message
        names(failureMsg) <- singleSpectraDataPath
        # return basic values and failure message
        return(list(TIC = as.numeric(NA), peakTable = NULL,
        acquTime = as.character(NA), curveFit = NULL,
            ROIsDataPoint = NULL, failure = failureMsg))  })
    gc(verbose = FALSE) # try clearing variables
    return(result)  # return singleFileSearch results with failure status
}


## Check inputs, Initialise variables and outputs
parallelAnnotation_init <- function(object, BPPARAM, nCores, verbose) {
    # check validity of object
    validObject(object)

    nCores <- as.integer(nCores)
    if (nCores < 1) {
        stop("Check input, nCores must be a positive integer")
    }

    # Handle default BPParams
    if (is.null(BPPARAM)) {
        if (nCores > 1) {
            if (.Platform$OS.type == 'windows') {
            BPPARAM <- BiocParallel::SnowParam(workers = nCores)
            }
            else {
            BPPARAM <- BiocParallel::MulticoreParam(workers = nCores)
            }

        } else {
            BPPARAM <- BiocParallel::SerialParam()}
    }
    else if (!is(BPPARAM, 'BiocParallelParam')) {
        stop("Check input, BPPARAM must be a BiocParallel Param object")
    }

    # Initialise parameters from object
    use_uROI <- useUROI(object)
    use_FIR <- useFIR(object)
    file_paths <- filepath(object)
    if (use_uROI) {
        target_peak_table <- uROI(object)
    } else {
        target_peak_table <- ROI(object)
    }
    if (use_FIR) {
        input_FIR <- FIR(object)
    } else {
        input_FIR <- NULL
    }

    # Output parameters
    if (verbose & isAnnotated(object)) {
        message("!! Data was already annotated, results will be overwritten !!")
    }
    if (verbose) {
        message("Processing ", nbCompounds(object), " compounds in ",
            nbSamples(object), " samples:")
        message("  uROI:\t", use_uROI)
        message("  FIR:\t", use_FIR)
    }

    return(list(file_paths=file_paths, target_peak_table=target_peak_table,
                input_FIR=input_FIR, BPPARAMObject=BPPARAM))
}


## Run singleFileSearch
# (list, each item is the result of a file, errors are passed into the list)
parallelAnnotation_runSingleFileSearch <- function(file_paths,
target_peak_table, input_FIR, BPPARAM, getAcquTime, centroided,
curveModel, verbose, ...) {

    BiocParallel::register(BPPARAM)
    BiocParallel::bpstart(BPPARAM)
    allFilesRes <- BiocParallel::bplapply(X=file_paths, 
                    FUN=parallelAnnotation_parallelHelper,
                    targetFeatTable=target_peak_table,
                    inGetAcquTime=getAcquTime, inFIR=input_FIR,
                    centr=centroided, curveModel=curveModel, inVerbose=verbose,
                    BPPARAM = BPPARAM, ...)

    BiocParallel::bpstop(BPPARAM)
    return(allFilesRes) }

## Collect, process and reorder results
parallelAnnotation_process <- function(allFilesRes, object, verbose) {
    # identify annotations that failed
    fail_status <- unlist(lapply(allFilesRes,
        function(x) {x$failure}), use.names = TRUE)
    failures <- !is.na(fail_status)
    names(failures) <- NULL
    fail_table <- data.frame(matrix(c(names(fail_status)[failures],
        fail_status[failures]), ncol = 2, byrow = FALSE,
        dimnames = list(c(), c("file", "error"))), stringsAsFactors = FALSE)
    # message failures
    if ((sum(failures) != 0) & verbose) {
        message("----------------")
        message(sum(failures), " file(s) failed to process:\n",
            paste0(utils::capture.output(fail_table), collapse = "\n")) }
    # remove failures
    allFilesRes <- allFilesRes[!failures]
    # reshape the output object to match (remove failed samples)
    outObject <- object[!failures, ]
    # unlist result into final object (if there is a minimum of 1 file left)
    if (sum(!failures) > 0) {
        # acquisitionTime
        outObject@acquisitionTime <- vapply(allFilesRes, function(x) {
            as.character(x$acquTime)}, FUN.VALUE = character(1))
        # TIC
        outObject@TIC <- vapply(allFilesRes, function(x) {
            x$TIC}, FUN.VALUE = numeric(1))
        # peakTables (all columns but cpdID and cpdName)
        outObject@peakTables <- lapply(allFilesRes,
            function(x) {x$peakTable[, !names(x$peakTable) %in%
                c("cpdID", "cpdName")] })
        # dataPoints
        outObject@dataPoints <- lapply(allFilesRes,function(x){x$ROIsDataPoint})
        # peakFit
        outObject@peakFit <- lapply(allFilesRes, function(x) { x$curveFit })
        # isAnnotated
        outObject@isAnnotated <- TRUE
    } else { # All files failed
        if (verbose) { message("No file left in the object!") }
    }
    # reorder results by acquisition date if available
    if (sum(is.na(acquisitionTime(outObject))) == 0) {
        if (verbose) {
            message("Annotation object reordered by sample acquisition date") }
        outObject <- outObject[order(acquisitionTime(outObject)), ]
    } else {
        if (verbose) {
            message('Annotation object cannot be reordered by sample ',
                    'acquisition date') }
    }
    return(list(outObject=outObject, fail_table=fail_table)) }
