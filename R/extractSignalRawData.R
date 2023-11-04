#' @title Extract signal in a multiple defined mz rt window from a raw data file
#'
#' @description Extract all signal from multiple defined mz rt window from raw
#' data and returns a data.frame. If no rt-mz window is provided, all signal in
#' the raw data file are returned
#'
#' @param rawSpec an \code{\link[MSnbase]{OnDiskMSnExp-class}}
#' @param rt (numeric(2) or two-column matrix) the lower and upper retention
#' time range from which the data should be extracted. If a matrix is passed,
#' each row corresponds to a different window. If not provided, the full
#' retention time range will be extracted.
#' @param mz (numeric(2) or two-column matrix) the lower and upper mass range
#' from which the data should be extracted. If a matrix is passed, each row
#' corresponds to a different window. If not provided, the full mass range will
#' be extracted.
#' @param msLevel (int) the MS level at which the data should be extracted
#' (default to MS level 1)
#' @param verbose (bool) If TRUE message progress and warnings
#'
#' @return a list (one entry per window) of data.frame with signal as row and
#' retention time ('rt'), mass ('mz') and intensity ('int) as columns.
#' 
#' @details
#' ## Examples cannot be computed as the function is not exported:
#' ## Use a file form the faahKO package and extract data from a region of
#' ## interest
#' library(faahKO)
#' rawSpec <- MSnbase::readMSData(
#'                             system.file('cdf/KO/ko15.CDF',package='faahKO'),
#'                             centroided=TRUE,
#'                             mode='onDisk')
#' dataPoints  <- extractSignalRawData(rawSpec,
#'                                     rt = c(3290., 3410.), 
#'                                     mz = c(522.194778, 522.205222), 
#'                                     verbose=TRUE)
#' # Reading data from 1 windows
#' 
#' dataPoints
#' # [[1]]
#' #          rt    mz    int
#' # 1  3290.115 522.2   1824
#' # 2  3291.680 522.2   1734
#' # 3  3293.245 522.2   1572
#' # 4  3294.809 522.2   1440
#' # 5  3299.504 522.2   1008
#' # 6  3301.069 522.2    871
#' # 7  3302.634 522.2    786
#' # 8  3304.199 522.2    802
#' # 9  3305.764 522.2    834
#' # 10 3307.329 522.2    839
#' # 11 3315.154 522.2   2187
#' # 12 3316.719 522.2   3534
#' # 13 3318.284 522.2   6338
#' # 14 3319.849 522.2  11718
#' # 15 3321.414 522.2  21744
#' # 16 3322.979 522.2  37872
#' # 17 3324.544 522.2  62424
#' # 18 3326.109 522.2  98408
#' # 19 3327.673 522.2 152896
#' # 20 3329.238 522.2 225984
#' # 21 3330.803 522.2 308672
#' # 22 3332.368 522.2 399360
#' # 23 3333.933 522.2 504000
#' # 24 3335.498 522.2 614656
#' # 25 3337.063 522.2 711872
#' # 26 3338.628 522.2 784704
#' # 27 3340.193 522.2 836608
#' # 28 3341.758 522.2 866304
#' # 29 3343.323 522.2 882304
#' # 30 3344.888 522.2 889280
#' # 31 3346.453 522.2 888256
#' # 32 3348.018 522.2 866816
#' # 33 3349.583 522.2 827392
#' # 34 3351.148 522.2 777728
#' # 35 3352.713 522.2 727040
#' # 36 3354.278 522.2 678464
#' # 37 3355.843 522.2 629120
#' # 38 3357.408 522.2 578048
#' # 39 3358.973 522.2 524288
#' # 40 3360.538 522.2 471040
#' # 41 3362.102 522.2 416320
#' # 42 3363.667 522.2 360064
#' # 43 3365.232 522.2 302400
#' # 44 3366.797 522.2 249152
#' # 45 3368.362 522.2 202560
#' # 46 3369.927 522.2 161024
#' # 47 3371.492 522.2 123520
#' # 48 3373.057 522.2  93160
#' # 49 3374.622 522.2  71856
#' # 50 3376.187 522.2  58392
#' # 51 3377.752 522.2  51072
#' # 52 3379.317 522.2  48376
#' # 53 3380.882 522.2  49168
#' # 54 3382.447 522.2  53120
#' # 55 3384.012 522.2  62488
#' # 56 3385.577 522.2  78680
#' # 57 3387.142 522.2 102840
#' # 58 3388.707 522.2 134656
#' # 59 3390.272 522.2 173440
#' # 60 3391.837 522.2 217088
#' # 61 3393.402 522.2 268864
#' # 62 3394.966 522.2 330496
#' # 63 3396.531 522.2 395776
#' # 64 3398.096 522.2 453376
#' # 65 3399.661 522.2 499072
#' # 66 3401.226 522.2 537024
#' # 67 3402.791 522.2 570304
#' # 68 3404.356 522.2 592512
#' # 69 3405.921 522.2 598912
#' # 70 3407.486 522.2 595008
#' # 71 3409.051 522.2 588416
extractSignalRawData <- function(rawSpec, rt, mz, msLevel = 1L, verbose = TRUE){
    stime <- Sys.time()

    # Check input and init output
    outParam <- extractSignalRawData_init(rt, mz, msLevel, verbose)
    rt <- outParam$rt
    mz <- outParam$mz
    empty_res <- outParam$empty_res

    # Read scan in file
    scanRead <- extractSignalRawData_extractScans(rawSpec, rt, msLevel, verbose)
    if (is.null(scanRead)) { return(empty_res) } # no scans were found
    spectraData <- scanRead$spectraData
    spec_rt <- scanRead$spec_rt


    # Get data points from each window (subset rt and check mz)
    res <- extractSignalRawData_extractFromWindows(empty_res, rt, mz, spec_rt,
                                                    spectraData, verbose)
    
    # clear variables
    rm(spectraData, spec_rt)
    gc(verbose = FALSE)
    
    # Out
    etime <- Sys.time()
    if (verbose) {
        message("Data read in: ",
                round(as.double(difftime(etime, stime)), 2),
                " ", units(difftime(etime, stime)))
    }
    return(res)
}

# Looping tryCatch with count and sleep ---------------------------------------
retry <- function(expr, isError=function(x) "try-error" %in% class(x), 
                maxErrors=5, sleep=0) {
    attempts = 0
    retval = try(eval(expr))
    while (isError(retval)) {
        attempts = attempts + 1
        if (attempts >= maxErrors) {
            msg = sprintf("retry: too many retries [[%s]]", 
                            utils::capture.output(str(retval)))
            stop(msg)
        } else {
            msg = sprintf("retry: error in attempt %i/%i", attempts, maxErrors)
            message(msg)
        }
        if (sleep > 0) Sys.sleep(sleep)
        retval = try(eval(expr))
    }
    return(retval)
}

# -----------------------------------------------------------------------------
# extractSignalRawData helper functions

# Check input type and dimensions of msLevel
extractSignalRawData_checkInput <- function(rt, mz, msLevel, verbose) {
    # Check input check type and dimensions msLevel
    if (!is.integer(msLevel)) {
        stop("Check input \"msLevel\" must be integer")
    }
    # rt
    if (!missing(rt)) {
        if (!is(rt, "numeric") & !is(rt, "matrix") & !is(rt, "data.frame")) {
            stop("Check input \"rt\" must be numeric, matrix or data.frame")}
        if (is(rt, "numeric") & (length(rt) != 2)) {
            stop("Check input \"rt\" must be numeric of length 2")}
        if (is(rt, "matrix") | is(rt, "data.frame")) {
            if (ncol(rt) != 2) {
                stop('Check input \"rt\" must be a ',
                'matrix or data.frame with 2 columns')
            }
        }
    }
    # mz
    if (!missing(mz)) {
        if (!is(mz, "numeric") & !is(mz, "matrix") & !is(mz, "data.frame")) {
            stop("Check input \"mz\" must be numeric, matrix or data.frame")}
        if (is(mz, "numeric") & (length(mz) != 2)) {
            stop("Check input \"mz\" must be numeric of length 2")}
        if (is(mz, "matrix") | is(mz, "data.frame")) {
            if (ncol(mz) != 2) {
                stop('Check input \"mz\" must be a matrix',
                ' or data.frame with 2 columns')
            }
        }
    }
    # both rt and mz have same number of rows (unless one of them has only 1row)
    if (!missing(rt) & !missing(mz)) {
        if (any(is(rt, "matrix"), is(rt, "data.frame")) &
            any(is(mz, "matrix"), is(mz, "data.frame"))) {
            if (nrow(rt) != nrow(mz)) {
                if ((nrow(rt) != 1) & (nrow(mz) != 1)) {
                    stop('Check input \"rt\" and \"mz\" matrix or ',
                    'data.frame must have the same number of rows')
                } else {
                    if (verbose) {
                        message('\"rt\" or \"mz\" is a matrix/data.frame',
                        ' of 1 row, rows will be duplicated to match',
                        ' the other input')
                    }
                }
            }
        }
    }
}
# Init outputs
extractSignalRawData_init <- function(rt, mz, msLevel, verbose) {
    # Check input check type and dimensions msLevel
    extractSignalRawData_checkInput(rt, mz, msLevel, verbose)

    # Express rt and mz as matrix/data.frame of identical number of rows
    # replace missing by whole range
    if (missing(rt)) {
        rt <- matrix(c(-Inf, Inf), ncol = 2, byrow = TRUE)}
    if (missing(mz)) {
        mz <- matrix(c(-Inf, Inf), ncol = 2, byrow = TRUE)}

    # convert all numeric to matrix
    if (is(rt, "numeric")) {
        rt <- matrix(rt, nrow = 1, ncol = 2, byrow = TRUE)}
    if (is(mz, "numeric")) {
        mz <- matrix(mz, nrow = 1, ncol = 2, byrow = TRUE)}

    # Replicate rows (if only 1) to match other
    if (nrow(rt) == 1) {
        rt <- matrix(rep(as.numeric(rt), nrow(mz)), ncol = 2, byrow = TRUE)}
    if (nrow(mz) == 1) {
        mz <- matrix(rep(as.numeric(mz), nrow(rt)), ncol = 2, byrow = TRUE)}
    # now both rt and mz are either a matrix/data.frame of matching size

    if (verbose) {message("Reading data from ", nrow(rt), " windows")}
    # empty output
    empty_res <- lapply(vector("list", nrow(rt)), function(x) {
        data.frame(rt = numeric(), mz = numeric(), int = integer())
    })

    return(list(rt=rt, mz=mz, empty_res=empty_res))
}
# Extract scans for all windows in a single file access
extractSignalRawData_extractScans <- function(rawSpec, rt, msLevel, verbose) {
    # Filter msLevel
    msFilteredSpec <- MSnbase::filterMsLevel(rawSpec, msLevel = msLevel)
    # if msLevel doesn't exist, exit
    if (length(msFilteredSpec) == 0) {
        if (verbose) {message("No data exist for MS level ", msLevel)}
        return(NULL)
    }

    # Filter only scans falling into the rt of interest (across all windows)
    file_rt         <- MSnbase::rtime(msFilteredSpec)
    keep_scan_idx   <- sort(unique(as.integer(unlist(apply(rt, MARGIN = 1,
        function(x) {
            which((file_rt >= x[1]) & (file_rt <= x[2]))
        }), use.names = FALSE))))
    # if no scans
    if (length(keep_scan_idx) == 0) {
        if (verbose) {message("No data exist for the rt provided")}
        return(NULL)
    }
    # file with only the needed scans
    rtFilteredSpec <- msFilteredSpec[keep_scan_idx]

    # Extract only scans we need (only file access)
    spectraData <- retry(MSnbase::spectra(rtFilteredSpec), 
                        maxErrors=1000, sleep=1)
    spec_rt     <- MSnbase::rtime(rtFilteredSpec)
    
    # clear variables
    rm(msFilteredSpec, file_rt, keep_scan_idx, rtFilteredSpec)
    gc(verbose = FALSE)

    return(list(spectraData=spectraData, spec_rt=spec_rt))
}
# Get data points from each window (subset rt and check mz)
extractSignalRawData_extractFromWindows <- function(empty_res, rt, mz, spec_rt,
                                                    spectraData, verbose) {
    res <- empty_res

    # iterage over windows
    for (i in seq_len(nrow(rt))) {

        # subset the scans we need from the ones we have extracted
        scans_to_keep <- (spec_rt >= rt[i, 1]) & (spec_rt <= rt[i, 2])
        if (!any(scans_to_keep)) {
            if (verbose) {message("No data exist for window ", i)}
            # move to next window (empty df was already initialised)
            next
        }

        # only keep scans matching the window
        scanSubset <- spectraData[scans_to_keep]

        # subset each scan based on mz and extract datapoints
        scanDatapoint <- lapply(scanSubset, function(scan, mzScan) {
            # filter mz
            suppressWarnings(filtScan <- MSnbase::filterMz(scan, mzScan))
            # extract datapoint
            if (!filtScan@peaksCount) {
                # no datapoints
                data.frame(rt = numeric(), mz = numeric(), int = integer())
            } else {
                data.frame(rt = rep_len(filtScan@rt, length(filtScan@mz)),
                            mz = filtScan@mz, int = filtScan@intensity)
            }
        }, mzScan = mz[i, ])

        # concatenate all scans data.frame
        scanTable           <- do.call(rbind, scanDatapoint)
        rownames(scanTable) <- NULL

        # store results
        res[[i]] <- scanTable
    }

    # clear variables
    rm(scanDatapoint, scanTable)
    gc(verbose = FALSE)

    return(res)
}
