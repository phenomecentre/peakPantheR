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
#' @param compounds (character or NULL) Optional. When supplied, restrict
#' processing to the subset of compounds whose \code{cpdID} matches one of
#' these values. Requires \code{@isAnnotated = TRUE}, since unselected
#' compound rows retain their prior per-sample \code{@peakTables},
#' \code{@peakFit} and \code{@dataPoints}. The returned annotation has the
#' same \code{nbCompounds} as the input; only rows matching \code{compounds}
#' are refit. Unknown cpdIDs raise an error; duplicates in \code{compounds}
#' are silently deduplicated.
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
    curveModel='skewedGaussian', verbose=TRUE, compounds = NULL, ...){

    # Dispatch to the compound-subset path when `compounds` narrows the set.
    # Requires @isAnnotated=TRUE: unselected rows keep their prior contents
    # and would otherwise be undefined.
    if (!is.null(compounds)) {
        if (!isAnnotated(object)) {
            stop("Check input, 'compounds' requires @isAnnotated = TRUE; ",
                "run a full annotation before refitting a compound subset")
        }
        j <- resolve_compound_selector(compounds, cpdID(object))
        if (length(j) < nbCompounds(object)) {
            return(parallelAnnotation_compoundSubset(object, j,
                BPPARAM = BPPARAM, nCores = nCores,
                getAcquTime = getAcquTime, centroided = centroided,
                curveModel = curveModel, verbose = verbose, ...))
        }
        # length(j) == nbCompounds(object): selector covers every compound,
        # fall through to the standard full-width path unchanged.
    }

    # Check inputs, Initialise variables and outputs
    initRes    <- parallelAnnotation_init(object, BPPARAM, nCores, verbose)
    file_paths<-initRes$file_paths; target_peak_table<-initRes$target_peak_table
    input_FIR  <- initRes$input_FIR; BPPARAMObject <- initRes$BPPARAMObject

    # Build per-file cache descriptor (from @dataPoints) when the input is
    # already annotated and cached data covers current bounds
    cacheList <- build_parallelAnnotation_cache(object, target_peak_table,
        verbose)
    # Hit mask: cache entries that skipped disk I/O; used by process() to keep
    # the prior @TIC[i] / @acquisitionTime[i] instead of overwriting with NA
    cacheHit <- vapply(cacheList,
        function(ce) !is.null(ce$dataPoints), logical(1))

    # Manage worker lifecycle at the top level, alongside BPPARAM creation
    started_here <- !BiocParallel::bpisup(BPPARAMObject)
    if (started_here) BiocParallel::bpstart(BPPARAMObject)
    on.exit(if (started_here) BiocParallel::bpstop(BPPARAMObject), add = TRUE)

    stime <- Sys.time()

    # Run singleFileSearch on all files
    # (list, each item is the result of a file, errors are passed into the list)
    allFilesRes <- BiocParallel::bpmapply(
                    FUN=parallelAnnotation_parallelHelper,
                    singleSpectraDataPath=file_paths,
                    cacheEntry=cacheList,
                    MoreArgs=list(targetFeatTable=target_peak_table,
                        inGetAcquTime=getAcquTime, inFIR=input_FIR,
                        centr=centroided, curveModel=curveModel,
                        inVerbose=verbose, ...),
                    SIMPLIFY=FALSE, USE.NAMES=FALSE,
                    BPPARAM=BPPARAMObject)

    # Collect, process and reorder results
    res <- parallelAnnotation_process(allFilesRes, object, verbose,
        cacheHit = cacheHit)
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
curveModel='skewedGaussian', inVerbose=TRUE, cacheEntry=NULL, ...){
    useCache <- !is.null(cacheEntry) && !is.null(cacheEntry$dataPoints)
    # Check input path exist or exit with error message (skip if cache hit)
    if (!useCache && !file.exists(singleSpectraDataPath)) {
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
    # TIC and acquTime are not threaded via the cache: parallelAnnotation_process
    # preserves @TIC[i] / @acquisitionTime[i] for cache-hit samples and only
    # overwrites for cache misses (where singleFileSearch returns real values).
    result <- tryCatch({
        tmpResult <- peakPantheR::peakPantheR_singleFileSearch(singleSpectraDataPath,
            targetFeatTable, peakStatistic = TRUE, plotEICsPath = NA,
            getAcquTime = inGetAcquTime, FIR = inFIR, centroided = centr,
            curveModel = curveModel, verbose = inVerbose,
            cachedDataPoints = if (useCache) cacheEntry$dataPoints else NULL,
            cachedROI = if (useCache) cacheEntry$ROI else NULL,
            ...)
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
    return(result)  # return singleFileSearch results with failure status
}


## Resolve BPPARAM: return a valid BiocParallelParam, constructing a default if needed
.resolveBPPARAM <- function(BPPARAM, nCores) {
    if (is.null(BPPARAM)) {
        if (nCores > 1) {
            if (.Platform$OS.type == 'windows') {
                BPPARAM <- BiocParallel::SnowParam(workers = nCores)
            } else {
                BPPARAM <- BiocParallel::MulticoreParam(workers = nCores)
            }
        } else {
            BPPARAM <- BiocParallel::SerialParam()
        }
    } else if (!is(BPPARAM, 'BiocParallelParam')) {
        stop("Check input, BPPARAM must be a BiocParallel Param object")
    }
    return(BPPARAM)
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
    BPPARAM <- .resolveBPPARAM(BPPARAM, nCores)

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


## Collect, process and reorder results
parallelAnnotation_process <- function(allFilesRes, object, verbose,
    cacheHit = NULL) {
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
    # Drop failed samples (and their cacheHit entries) in lockstep
    if (!is.null(cacheHit)) { cacheHit <- cacheHit[!failures] }
    allFilesRes <- allFilesRes[!failures]
    # reshape the output object to match (remove failed samples)
    outObject <- object[!failures, ]
    # unlist result into final object (if there is a minimum of 1 file left)
    if (sum(!failures) > 0) {
        # acquisitionTime: cache-hit rows keep prior value; misses use result
        acq_new <- vapply(allFilesRes, function(x) {
            as.character(x$acquTime)}, FUN.VALUE = character(1))
        if (!is.null(cacheHit) && any(cacheHit)) {
            acq_new[cacheHit] <- as.character(
                outObject@acquisitionTime[cacheHit])
        }
        outObject@acquisitionTime <- acq_new
        # TIC: same rule - preserve prior for cache hits
        tic_new <- vapply(allFilesRes, function(x) {
            x$TIC}, FUN.VALUE = numeric(1))
        if (!is.null(cacheHit) && any(cacheHit)) {
            tic_new[cacheHit] <- outObject@TIC[cacheHit]
        }
        outObject@TIC <- tic_new
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


# Per-file cache boundary check: the cache was filled using `cachedROI`
# (i.e. `object@ROI`) as the extraction envelope, so a hit requires
# `targetFeatTable[i, ] ⊆ cachedROI[i, ]` for every row, plus the cache
# list shape to match. Returns FALSE if any row's cache is empty/missing, or
# the target window sits outside what was read from disk, triggering a fresh
# disk read instead of silently truncating the EIC.
cache_bounds_ok <- function(cachedDataPoints, targetFeatTable, cachedROI) {
    if (is.null(cachedDataPoints)) { return(FALSE) }
    if (length(cachedDataPoints) != nrow(targetFeatTable)) { return(FALSE) }
    if (is.null(cachedROI) ||
        nrow(cachedROI) != nrow(targetFeatTable)) { return(FALSE) }
    for (i in seq_len(nrow(targetFeatTable))) {
        df <- cachedDataPoints[[i]]
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
            return(FALSE)
        }
        tgt <- targetFeatTable[i, ]
        env <- cachedROI[i, ]
        if (env$rtMin > tgt$rtMin || env$rtMax < tgt$rtMax ||
            env$mzMin > tgt$mzMin || env$mzMax < tgt$mzMax) {
            return(FALSE)
        }
    }
    return(TRUE)
}


# Run parallelAnnotation on a compound-wise subset and merge results back into
# the full-width input `object`. `j` is an integer vector of compound column
# positions (strict subset of seq_len(nbCompounds(object))). Unselected rows
# of @peakTables/@peakFit/@dataPoints are preserved from `object`. Samples
# that fail during the sub-run are dropped, matching the existing behaviour
# of peakPantheR_parallelAnnotation for failed files.
parallelAnnotation_compoundSubset <- function(object, j, BPPARAM, nCores,
    getAcquTime, centroided, curveModel, verbose, ...) {
    sub <- object[, j]
    subRes <- peakPantheR_parallelAnnotation(sub, BPPARAM = BPPARAM,
        nCores = nCores, getAcquTime = getAcquTime, centroided = centroided,
        curveModel = curveModel, verbose = verbose, compounds = NULL, ...)
    sub_out <- subRes$annotation
    fail_table <- subRes$failures

    # All samples failed: return empty full-width object
    if (nbSamples(sub_out) == 0L) {
        return(list(annotation = object[integer(0), ],
                    failures = fail_table))
    }

    # Align original object to the sub-run's sample order (which may have
    # been reordered by acquisition date and may have dropped failed files)
    orig_idx <- match(filepath(sub_out), filepath(object))
    full_out <- object[orig_idx, ]

    # Overwrite rows `j` of per-sample peakTables / peakFit / dataPoints
    new_pt <- full_out@peakTables
    new_pf <- full_out@peakFit
    new_dp <- full_out@dataPoints
    for (s in seq_len(nbSamples(full_out))) {
        if (!is.null(new_pt[[s]]) && !is.null(sub_out@peakTables[[s]])) {
            new_pt[[s]][j, ] <- sub_out@peakTables[[s]]
        }
        if (!is.null(new_pf[[s]]) && !is.null(sub_out@peakFit[[s]])) {
            new_pf[[s]][j] <- sub_out@peakFit[[s]]
        }
        if (!is.null(new_dp[[s]]) && !is.null(sub_out@dataPoints[[s]])) {
            new_dp[[s]][j] <- sub_out@dataPoints[[s]]
        }
    }
    full_out@peakTables <- new_pt
    full_out@peakFit <- new_pf
    full_out@dataPoints <- new_dp

    # TIC and acquisitionTime are whole-file quantities: take from sub_out
    full_out@TIC <- sub_out@TIC
    full_out@acquisitionTime <- sub_out@acquisitionTime
    full_out@isAnnotated <- TRUE

    validObject(full_out)
    return(list(annotation = full_out, failures = fail_table))
}


# Resolve a `compounds` selector (character cpdID values) to integer positions
# within `ids`. Returns NULL when `compounds` is NULL (signals "no filter").
# Errors on: non-character `compounds`, empty character vector, duplicated
# `ids`, or any value in `compounds` not found in `ids`. Duplicated entries
# in `compounds` are silently deduplicated, preserving first-seen order.
resolve_compound_selector <- function(compounds, ids) {
    if (is.null(compounds)) { return(NULL) }
    if (!is.character(compounds)) {
        stop("Check input, 'compounds' must be a character vector of cpdID ",
            "values (or NULL); got ", class(compounds)[1])
    }
    if (length(compounds) == 0L) {
        stop("Check input, 'compounds' must contain at least one cpdID ",
            "when not NULL")
    }
    if (anyDuplicated(ids) > 0L) {
        dup <- unique(ids[duplicated(ids)])
        stop("Check input, cpdID must be unique to select by cpdID; ",
            "duplicated IDs: ", paste(dup, collapse = ", "))
    }
    compounds <- unique(compounds)
    unknown <- setdiff(compounds, ids)
    if (length(unknown) > 0L) {
        stop("Check input, unknown cpdID(s) in 'compounds': ",
            paste(unknown, collapse = ", "))
    }
    match(compounds, ids)
}


# Build a per-file cache descriptor list. Each element is either a list with
# $dataPoints and $ROI (cache hit) or an empty list (cache miss / fall back to
# disk). TIC and acquisitionTime are not included - parallelAnnotation_process
# preserves the object's own @TIC[i] / @acquisitionTime[i] for cache hits.
# Returns a list of length nbSamples(object).
build_parallelAnnotation_cache <- function(object, target_peak_table, verbose){
    nFiles <- length(filepath(object))
    cacheList <- vector("list", nFiles)
    for (i in seq_len(nFiles)) { cacheList[[i]] <- list() }

    if (!isAnnotated(object)) { return(cacheList) }

    dp_all <- object@dataPoints
    anyHit <- FALSE
    for (i in seq_len(nFiles)) {
        dp_i <- if (length(dp_all) >= i) dp_all[[i]] else NULL
        if (cache_bounds_ok(dp_i, target_peak_table, object@ROI)) {
            cacheList[[i]] <- list(dataPoints = dp_i, ROI = object@ROI)
            anyHit <- TRUE
        }
    }
    if (verbose && anyHit) {
        message("  (reusing cached EIC data where valid)")
    }
    return(cacheList)
}
