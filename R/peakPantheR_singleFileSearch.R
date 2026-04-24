################################################################################
#                                                                              #
#  --- peakPantheR: detect and integrate pre-defined features in MS files ---  #
#                                                                              #
################################################################################


#' @title Search, integrate and report targeted features in a raw spectra
#'
#' @description Report for a raw spectra the TIC, acquisition time, integrated
#' targeted features, fitted curves and datapoints for each region of interest.
#' Optimised to reduce the number of file access. Features not detected can be
#' integrated using fallback integration regions (FIR).
#'
#' @param singleSpectraDataPath (str) path to netCDF or mzML raw data file
#' (centroided, \strong{only with the channel of interest}).
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as
#' rows. Columns: \code{cpdID} (str), \code{cpdName} (str), \code{rtMin} (float
#' in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float
#' in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}),
#' \code{mzMax} (float).
#' @param peakStatistic (bool) If TRUE calculates additional peak statistics:
#' 'ppm_error', 'rt_dev_sec', 'tailing factor' and 'asymmetry factor'
#' @param plotEICsPath (str or NA) If not NA, will save a \emph{.png} of all ROI
#' EICs at the path provided (\code{'filepath/filename.png'} expected). If NA no
#' plot saved
#' @param getAcquTime (bool) If TRUE will extract sample acquisition date-time
#' from the mzML metadata (the additional file access will impact run time)
#' @param FIR (data.frame or NULL) If not NULL, integrate Fallback Integration
#' Regions (FIR) when a feature is not found.  Compounds as row are identical to
#' \code{targetFeatTable}, columns are \code{rtMin} (float in seconds),
#' \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mzMax} (float).
#' @param centroided (bool) use TRUE if the data is centroided, used by
#' \code{\link[MSnbase]{readMSData}} when reading the raw data file
#' @param curveModel (str) specify the peak-shape model to fit,
#' by default \code{skewedGaussian}.
#' Accepted values are \code{skewedGaussian} and \code{emgGaussian}
#' @param verbose (bool) If TRUE message calculation progress, time taken and
#' number of features found
#' @param ... Passes arguments to \code{findTargetFeatures} to alter
#' peak-picking parameters (e.g. \code{curveModel}, \code{sampling},
#' \code{params} as a list of parameters for each ROI or 'guess',...)
#' @param cacheROI (data.frame or NULL) Optional extraction / cache envelope:
#' rows are one per compound with columns \code{rtMin}, \code{rtMax},
#' \code{mzMin}, \code{mzMax}. Each row must contain the corresponding
#' \code{targetFeatTable} row (\code{targetFeatTable[i,]} is a subset of
#' \code{cacheROI[i,]}). When supplied, the raw EIC is read (or
#' \code{cachedDataPoints} is validated) at \code{cacheROI} bounds, the
#' returned \code{ROIsDataPoint} also spans \code{cacheROI}, and the curve
#' fit operates on the \code{targetFeatTable} subset. When \code{NULL}
#' (default) \code{targetFeatTable} is used as the envelope too, matching
#' the legacy single-window behaviour.
#' @param cachedDataPoints (list or NULL) Optional. If supplied, skip reading
#' the raw file and reuse this list of \code{data.frame(rt, mz, int)} (one
#' element per row of \code{targetFeatTable}, as produced by
#' \code{extractSignalRawData}). Must have been extracted at \code{cacheROI}
#' bounds (which must therefore also be supplied). Rows are row-subset to
#' \code{targetFeatTable} bounds on demand for fitting.
#'
#' @return a list: \code{list()$TIC} \emph{(int)} TIC value,
#' \code{list()$peakTable} \emph{(data.frame)} targeted features results
#' (see Details), \code{list()$curveFit} \emph{(list)} list of
#' \code{peakPantheR_curveFit} or NA for each ROI, \code{list()$acquTime}
#' \emph{(POSIXct or NA)} date-time of sample acquisition from mzML metadata,
#' \code{list()$ROIsDataPoint} \emph{(list)} a list of \code{data.frame} of raw
#' data points for each ROI (retention time 'rt', mass 'mz' and intensity 'int'
#' (as column) of each raw data points (as row)).
#'
#' \subsection{Details:}{
#' The returned \emph{peakTable} \code{data.frame} is structured as follow:
#' \tabular{ll}{
#' cpdID \tab database compound ID\cr
#' cpdName \tab compound name\cr
#' found \tab was the peak found\cr
#' rt \tab retention time of peak apex (sec)\cr
#' rtMin \tab leading edge of peak retention time (sec) determined at 0.5\% of
#' apex intensity\cr
#' rtMax \tab trailing edge of peak retention time (sec) determined at 0.5\% of
#' apex intensity\cr
#' mz \tab weighted (by intensity) mean of peak m/z across scans\cr
#' mzMin \tab m/z peak minimum (between rtMin, rtMax)\cr
#' mzMax \tab m/z peak maximum (between rtMin, rtMax)\cr
#' peakArea \tab integrated peak area\cr
#' peakAreaRaw \tab integrated peak area from raw data points\cr
#' maxIntMeasured \tab maximum peak intensity in raw data\cr
#' maxIntPredicted \tab maximum peak intensity based on curve fit\cr
#' is_filled \tab Logical indicate if the feature was integrated using FIR
#' (Fallback Integration Region)\cr
#' ppm_error \tab difference in ppm between the expected and measured m/z\cr
#' rt_dev_sec \tab difference in seconds between the expected and measured rt\cr
#' tailingFactor \tab the tailing factor is a measure of peak tailing.It is
#' defined as the distance from the front slope of the peak to the back slope
#' divided by twice the distance from the center line of the peak to the front
#' slope, with all measurements made at 5\% of the maximum peak height. The
#' tailing factor of a peak will typically be similar to the asymmetry factor
#' for the same peak, but the two values cannot be directly converted\cr
#' asymmetryFactor \tab the asymmetry factor is a measure of peak tailing. It is
#' defined as the distance from the center line of the peak to the back slope
#' divided by the distance from the center line of the peak to the front slope,
#' with all measurements made at 10\% of the maximum peak height. The asymmetry
#' factor of a peak will typically be similar to the tailing factor for the same
#' peak, but the two values cannot be directly converted\cr
#' }
#' }
#'
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Load data
#' library(faahKO)
#' netcdfFilePath <- system.file('cdf/KO/ko15.CDF', package = 'faahKO')
#'
#' ## targetFeatTable
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
#' res <- peakPantheR_singleFileSearch(netcdfFilePath,targetFeatTable,
#'                                     peakStatistic=TRUE)
#' # Polarity can not be extracted from netCDF files, please set manually the
#' #    polarity with the 'polarity' method.
#' # Reading data from 2 windows
#' # Data read in: 0.16 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for
#' #   mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and
#' #   ROI$mzMax for ROI #1
#' # Found 2/2 features in 0.05 secs
#' # Peak statistics done in: 0 secs
#' # Feature search done in: 0.75 secs
#' 
#' res
#' # $TIC
#' # [1] 2410533091
#' #
#' # $peakTable
#' #   found    rtMin       rt    rtMax    mzMin    mz    mzMax peakArea
#' # 1  TRUE 3309.759 3346.828 3385.410 522.1948 522.2 522.2052 26133727
#' # 2  TRUE 3345.377 3386.529 3428.279 496.2000 496.2 496.2000 35472141
#' #   peakAreaRaw maxIntMeasured maxIntPredicted cpdID cpdName is_filled
#' # 1    26071378         889280        901015.8  ID-1   Cpd 1     FALSE
#' # 2    36498367        1128960       1113576.7  ID-2   Cpd 2     FALSE
#' #    ppm_error   rt_dev_sec  tailingFactor  asymmetryFactor
#' # 1 0.02337616    1.9397590       1.015357         1.026824
#' # 2 0.02460103    0.9518072       1.005378         1.009318
#' #
#' # $acquTime
#' # [1] NA
#' #
#' #
#' # $curveFit
#' # $curveFit[[1]]
#' # $amplitude
#' # [1] 162404.8
#' # 
#' # $center
#' # [1] 3341.888
#' # 
#' # $sigma
#' # [1] 0.07878613
#' # 
#' # $gamma
#' # [1] 0.00183361
#' # 
#' # $fitStatus
#' # [1] 2
#' # 
#' # $curveModel
#' # [1] 'skewedGaussian'
#' # 
#' # attr(,'class')
#' # [1] 'peakPantheR_curveFit'
#' # 
#' # $curveFit[[2]]
#' # $amplitude
#' # [1] 199249.1
#' # 
#' # $center
#' # [1] 3382.577
#' # 
#' # $sigma
#' # [1] 0.07490442
#' # 
#' # $gamma
#' # [1] 0.00114719
#' # 
#' # $fitStatus
#' # [1] 2
#' # 
#' # $curveModel
#' # [1] 'skewedGaussian'
#' # 
#' # attr(,'class')
#' # [1] 'peakPantheR_curveFit'
#' #
#' #
#' # $ROIsDataPoint
#' # $ROIsDataPoint[[1]]
#' #          rt    mz    int
#' # 1  3315.154 522.2   2187
#' # 2  3316.719 522.2   3534
#' # 3  3318.284 522.2   6338
#' # 4  3319.849 522.2  11718
#' # 5  3321.414 522.2  21744
#' # 6  3322.979 522.2  37872
#' # 7  3324.544 522.2  62424
#' # 8  3326.109 522.2  98408
#' # 9  3327.673 522.2 152896
#' # 10 3329.238 522.2 225984
#' # ...
#' #
#' # $ROIsDataPoint[[2]]
#' #          rt    mz     int
#' # 1  3280.725 496.2    1349
#' # 2  3290.115 496.2    2069
#' # 3  3291.680 496.2    3103
#' # 4  3293.245 496.2    5570
#' # 5  3294.809 496.2   10730
#' # 6  3296.374 496.2   20904
#' # 7  3297.939 496.2   38712
#' # 8  3299.504 496.2   64368
#' # 9  3301.069 496.2   97096
#' # 10 3302.634 496.2  136320
#' # ...
#' }
#'
#' @family peakPantheR
#' @family realTimeAnnotation
#' @family parallelAnnotation
#'
#' @export
peakPantheR_singleFileSearch <- function(singleSpectraDataPath, targetFeatTable,
    peakStatistic = FALSE, plotEICsPath = NA, getAcquTime = FALSE, FIR = NULL,
    centroided = TRUE, curveModel='skewedGaussian', verbose = TRUE,
    cacheROI = NULL, cachedDataPoints = NULL, ...) {
    stime <- Sys.time()
    useCache <- !is.null(cachedDataPoints)
    if (useCache && is.null(cacheROI)) {
        stop("Check input, 'cacheROI' is required when 'cachedDataPoints' ",
            "is supplied") }
    # Single envelope for the whole call: where data was (or will be) read
    # from disk, and what ROIsDataPoint will span when returned. When
    # cacheROI is NULL we fall back to targetFeatTable, matching the legacy
    # single-window behaviour.
    extBounds <- if (is.null(cacheROI)) targetFeatTable else cacheROI
    if (useCache && !cache_bounds_ok(cachedDataPoints, extBounds)) {
        stop("Check input, 'cachedDataPoints' shape does not match ",
            "'cacheROI' (wrong length or empty per-compound data.frame)") }
    # Check input (skip file-existence when cache is supplied)
    # FIR outside the cache envelope requires raw data — fall back
    if (useCache && !is.null(FIR) &&
        !all(.boundsContains(extBounds, FIR))) {
        useCache <- FALSE
        if (verbose) {
            message("FIR extends beyond cached ROI; falling back to ",
                    "disk read") }
    }
    resInp <- singleFileSearch_checkInput(singleSpectraDataPath,targetFeatTable,
                                            plotEICsPath, FIR, curveModel,
                                            skipFileExists = useCache)
    singleSpectraDataPath <- resInp$specPath
    plotEICsPath <- resInp$plotPath
    useFIR <- resInp$useFIR

    # ROIsDataPoint always spans extBounds (the cache envelope). The caller
    # writes it back to @dataPoints, so keeping it wide stops the cache from
    # decaying toward the current uROI across refits. The fit uses a
    # row-subset to targetFeatTable, computed on demand below.
    if (useCache) {
        if (verbose) { message("Reusing cached EIC data for ",
            tools::file_path_sans_ext(basename(singleSpectraDataPath))) }
        raw_data      <- NULL
        TICvalue      <- NA_real_
        AcquTime      <- NA
        ROIsDataPoint <- cachedDataPoints
    } else {
        raw_data <- MSnbase::readMSData(singleSpectraDataPath,
                                        centroided = centroided, mode = "onDisk")
        TICvalue <- sum(MSnbase::tic(raw_data))
        AcquTime <- NA
        if (getAcquTime) {
            AcquTime <- getAcquisitionDatemzML(mzMLPath = singleSpectraDataPath,
                                                verbose = verbose) }
        ROIsDataPoint <- extractSignalRawData(raw_data,
                                    rt = extBounds[, c("rtMin", "rtMax")],
                                    mz = extBounds[, c("mzMin", "mzMax")],
                                    verbose = verbose)
    }

    fitDataPoint <- if (identical(extBounds, targetFeatTable)) ROIsDataPoint
        else subset_ROIsDataPoint_toBounds(ROIsDataPoint, targetFeatTable)

    # Integrate (fit runs on the targetFeatTable-subset; FIR sees the
    # extBounds-wide ROIsDataPoint directly)
    resInt <- singleFileSearch_integrate(raw_data, targetFeatTable,
            fitDataPoint, peakStatistic, useFIR, FIR, plotEICsPath,
            curveModel, verbose, roiData = ROIsDataPoint,
            roiBounds = extBounds, ...)
    finalOutput <- resInt$finalOutput
    curveFit <- resInt$curveFit

    etime <- Sys.time()
    if (verbose) { message("Feature search done in: ",
                            round(as.double(difftime(etime, stime)), 2),
                            " ", units(difftime(etime, stime))) }

    # clear variables
    rm(raw_data)
    gc(verbose = FALSE)

    return(list(TIC = TICvalue, peakTable = finalOutput, acquTime = AcquTime,
                curveFit = curveFit, ROIsDataPoint = ROIsDataPoint))
}


# -----------------------------------------------------------------------------
# peakPantheR_singleFileSearch helper functions

# Check inputs
singleFileSearch_checkInput <- function(singleSpectraDataPath, targetFeatTable,
                                        plotEICsPath, FIR, curveModel,
                                        skipFileExists = FALSE) {
    singleSpectraDataPath <- normalizePath(singleSpectraDataPath,
                                            mustWork = FALSE)
    if (!skipFileExists && !file.exists(singleSpectraDataPath)) {
    stop("Check input, file \"", singleSpectraDataPath, "\" does not exist") }

    if (dim(targetFeatTable)[1] != 0) {
        # rtMin < rtMax and mzMin < mzMax
        if (!all(targetFeatTable[, "rtMax"] >= targetFeatTable[, "rtMin"])) {
            stop("Check input, \"rtMin\" must be <= to \"rtMax\"") }
        if (!all(targetFeatTable[, "mzMax"] >= targetFeatTable[, "mzMin"])) {
            stop("Check input, \"mzMin\" must be <= to \"mzMax\"") } }

    if (!is.na(plotEICsPath)) {
        plotEICsPath <- normalizePath(plotEICsPath, mustWork = FALSE)
        # folder exist
        if (!file.exists(dirname(plotEICsPath))) {
            stop("Check input, plotEICsPath folder \"", dirname(plotEICsPath),
                "\" does not exist") }
        # png extension
        if (stringr::str_sub(basename(plotEICsPath), start = -4) != ".png") {
            stop("Check input, plotEICsPath file name \"",
                basename(plotEICsPath), "\" lacks a \".png\" extension") } }

    useFIR <- FALSE
    if (!is.null(FIR)) {
        # FIR is Data.frame
        if (!is.data.frame(FIR)) {
            stop("Check input, FIR must be a data.frame not ", class(FIR)) }
        # FIR number of rows
        if (dim(FIR)[1] != dim(targetFeatTable)[1]) {
            stop('Check input, FIR must have the same number of rows as',
                        ' targetFeatTable') }
        # FIR columns
        if (!all(c("mzMin", "mzMax", "rtMin", "rtMax") %in% colnames(FIR))) {
            stop('Check input, FIR must have \"mzMin\", \"mzMax\", ',
                        '\"rtMin\" and \"rtMax\" as columns') }
        useFIR <- TRUE
    }
    # known curveModel
    known_curveModel <- c("skewedGaussian", "emgGaussian")
    if (!(curveModel %in% known_curveModel)) {
        stop(paste0("Err","or: \"curveModel\" must be one of: ",
            paste(known_curveModel, collapse=', '))) }

    return(list(specPath=singleSpectraDataPath, plotPath=plotEICsPath,
                useFIR=useFIR))
}
# Integrate if there is at minimum 1 target feature. `ROIsDataPoint` is
# already row-subset to `targetFeatTable` for findTargetFeatures(); `roiData`
# spans the wider `roiBounds` envelope and is what FIR reuses.
singleFileSearch_integrate <- function(raw_data, targetFeatTable, ROIsDataPoint,
                        peakStatistic, useFIR, FIR, plotEICsPath,
                        curveModel, verbose, roiData, roiBounds, ...){
    if (dim(targetFeatTable)[1] != 0) { #Only integrate if there is min 1 target
        # Integrate features using ROI
        foundPeaks <- findTargetFeatures(ROIsDataPoint, targetFeatTable,
                                        curveModel=curveModel,
                                        verbose = verbose, ...)
        foundPeakTable <- foundPeaks$peakTable
        curveFit <- foundPeaks$curveFit
        # Add compound information
        finalOutput <- foundPeakTable
        finalOutput$cpdID <- targetFeatTable$cpdID
        finalOutput$cpdName <- targetFeatTable$cpdName
        finalOutput$is_filled <- as.logical(FALSE)
        # Add deviation, Tailing factor, Asymmetry factor
        if (peakStatistic) {
            finalOutput <- getTargetFeatureStatistic(curveFit, targetFeatTable,
                                                finalOutput, verbose = verbose)}
        # Fill features not found based on FIR
        if (useFIR) {
            firData <- buildFIRData(raw_data = raw_data,
                ROIsDataPoint = roiData, ROI = roiBounds, FIR = FIR,
                needsFilling_idx = which(!finalOutput$found),
                verbose = verbose)
            finalOutput <- integrateFIR(firData, FIR, finalOutput,
                                        verbose = verbose) }
        # Save all EICs plot
        if (!is.na(plotEICsPath)) {
            saveSingleFileMultiEIC(ROIsDataPoint, curveFit, finalOutput,
                                    plotEICsPath, width = 15, height = 15,
                                    verbose = verbose) }

    } else { #No targeted features, initialise empty integration res and EICs
        if (verbose) {
            message("- No target features passed in 'targetFeatTable', ",
                            "no integration, only TIC will be reported -") }
        if (peakStatistic) {
            finalOutput <- data.frame(matrix(vector(), 0, 18,
                dimnames = list(c(), c("cpdID", "cpdName", "found", "rt",
                "rtMin", "rtMax", "mz", "mzMin", "mzMax", "peakArea",
                "peakAreaRaw", "maxIntMeasured", "maxIntPredicted",
                "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor",
                "asymmetryFactor"))), stringsAsFactors = FALSE)
        } else {
            finalOutput <- data.frame(matrix(vector(), 0, 14,
                dimnames = list(c(), c("cpdID", "cpdName", "found", "rt",
                "rtMin", "rtMax", "mz",  "mzMin",  "mzMax", "peakArea",
                "peakAreaRaw", "maxIntMeasured",
                "maxIntPredicted", "is_filled"))), stringsAsFactors = FALSE)}
        curveFit <- list()
    }
    return(list(finalOutput=finalOutput, curveFit=curveFit))
}

# Row-wise bounds containment predicate. TRUE where each row of `inner` fits
# inside the matching row of `outer`. NAs become FALSE. Shared by FIR reuse
# and any other caller that needs "is this window contained in that window".
.boundsContains <- function(outer, inner) {
    ok <- (outer$rtMin <= inner$rtMin) & (outer$rtMax >= inner$rtMax) &
            (outer$mzMin <= inner$mzMin) & (outer$mzMax >= inner$mzMax)
    ok[is.na(ok)] <- FALSE
    ok
}


# Row-subset each ROIsDataPoint entry by the per-row rt/mz bounds in
# targetFeatTable. Used when data in memory spans a wider window than the
# current request (e.g. cached @dataPoints read at @ROI, now fitting at uROI).
subset_ROIsDataPoint_toBounds <- function(ROIsDataPoint, targetFeatTable) {
    n <- nrow(targetFeatTable)
    out <- vector("list", n)
    for (i in seq_len(n)) {
        df <- ROIsDataPoint[[i]]
        if (is.null(df) || nrow(df) == 0) {
            out[[i]] <- df
            next
        }
        keep <- df$rt >= targetFeatTable$rtMin[i] &
                df$rt <= targetFeatTable$rtMax[i] &
                df$mz >= targetFeatTable$mzMin[i] &
                df$mz <= targetFeatTable$mzMax[i]
        sub <- df[keep, , drop = FALSE]
        rownames(sub) <- NULL
        out[[i]] <- sub
    }
    return(out)
}


# Build firData for integrateFIR. When FIR[i] is contained in ROI[i], subset
# from ROIsDataPoint; otherwise pull missing rows in a single batched
# extractSignalRawData() call. In cache mode (raw_data = NULL) a non-contained
# FIR triggers an error, since re-extraction requires raw data.
# Caller decides what (ROIsDataPoint, ROI) to pass:
#   - non-cache: the just-extracted data and targetFeatTable (its bounds)
#   - cache:     the full cached @dataPoints and @ROI (their extraction bounds)
buildFIRData <- function(raw_data, ROIsDataPoint, ROI, FIR,
                            needsFilling_idx, verbose = TRUE) {
    n <- length(needsFilling_idx)
    if (n == 0) { return(list()) }

    iIdx      <- needsFilling_idx
    contained <- .boundsContains(outer = ROI[iIdx, , drop = FALSE],
                                    inner = FIR[iIdx, , drop = FALSE])

    out    <- vector("list", n)
    nReuse <- sum(contained)

    if (nReuse != 0) {
        if (verbose) {
            message("FIR data reused from ROI for ", nReuse, "/", n,
                    " windows") }
        reuseLocal <- which(contained)
        for (k in reuseLocal) {
            i    <- iIdx[k]
            roi  <- ROIsDataPoint[[i]]
            keep <- roi$rt >= FIR$rtMin[i] & roi$rt <= FIR$rtMax[i] &
                    roi$mz >= FIR$mzMin[i] & roi$mz <= FIR$mzMax[i]
            sub  <- roi[keep, , drop = FALSE]
            rownames(sub) <- NULL
            out[[k]] <- sub
        }
    }

    if (nReuse != n) {
        extractLocal <- which(!contained)
        extractIdx   <- iIdx[extractLocal]
        if (is.null(raw_data)) {
            stop("Cached annotation in use; FIR window for row(s) ",
                paste(extractIdx, collapse = ", "),
                " not contained in ROI - re-extraction requires raw data")
        }
        extracted <- extractSignalRawData(raw_data,
            mz = data.frame(mzMin = FIR$mzMin[extractIdx],
                            mzMax = FIR$mzMax[extractIdx]),
            rt = data.frame(rtMin = FIR$rtMin[extractIdx],
                            rtMax = FIR$rtMax[extractIdx]),
            verbose = verbose)
        for (j in seq_along(extractLocal)) {
            out[[extractLocal[j]]] <- extracted[[j]]
        }
    }

    return(out)
}
