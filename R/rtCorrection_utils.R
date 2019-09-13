#' Correct targeted retention time based on reference compounds
#'
#' Correct targeted features retention time using the RT and RT deviation of
#' previously fitted compounds. The `method` and `params` are used to select and
#' parametrise the retention time correction method employed. If `diagnostic` is
#' TRUE, RT correction diagnostic plots are returned (specific to each
#' correction method). This method will generate a peakPanthe
#'
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as
#' rows and parameters as columns: \code{cpdID} (str), \code{cpdName} (str),
#' \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}),
#' \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or
#' \emph{NA}), \code{mzMax} (float).
#' @param referenceTable a \code{\link{data.frame}} of reference compound
#' information as rows and properties as columns: \code{cpdID} (str),
#' \code{cpdName} (str), \code{rt} (float), \code{rt_dev_sec} (float)
#' @param method (str) name of RT correction method to use (currently
#' \code{RANSAC})
#' @param params (list or str) either 'guess' for automated parametrisation or
#' list of parameters (specific to each correction method)
#' @param diagnostic (bool) If TRUE returns diagnostic plots (specific to each
#' correction method)
#' @param verbose (bool) If TRUE message progress of RT correction
#' @param ... optional method specific parameters
#' 
#' @return a targetFeatTable with corrected RT, or diagnostic information
#' 
#' @export
peakPantheR_applyRTCorrection <- function(targetFeatTable, referenceTable,
                                        method='RANSAC', params='guess',
                                        diagnostic=FALSE, verbose=TRUE, ...) {

    # Check inputs
    applyRTCorrection_checkInput(targetFeatTable, referenceTable, method,params)

    # Init
    corrected_targetFeatTable <- targetFeatTable

    ## Run correction
    # RANSAC
    if (method == 'RANSAC') {

        ransac_correctionFunction <- fit_RANSAC(corrected_targetFeatTable$rt, targetFeatTable$rt_dev_sec)
        ## Guess parameters and bounds
        #if (useGuess) {
        #  new_params   <- guess_RANSAC(x, y)
        #} else {
        #  new_params   <- params
        #}

        # fit
        #  fit_RANSAC()
        # predict
        #   predict_RANSAC()
        # put results in table

    }
    # for future curve shapes
    #} else if () {
    #}

    return(corrected_targetFeatTable)

    ## TODO
    # unittest
    # @example
    # vignette, where it is integrated in the workflow
}
# applyRTCorrection check input
applyRTCorrection_checkInput <- function(targetFeatTable, referenceTable,
                                            method, params) {
    ## Check targetFeatTable
    applyRTCorrection_checkTargetFeatTable(targetFeatTable)

    ## Check referenceTable
    # is data.frame
    if (!is(referenceTable, "data.frame")){
        stop("specified referenceTable is not a data.frame")
    }
    # required columns are present
    if (!all(c("cpdID", "cpdName", "rt", "rt_dev_sec") %in%
    colnames(referenceTable))){
        stop("expected columns in referenceTable are \"cpdID\", \"cpdName\", ",
        "\"rt\" and \"rt_dev_sec\"")
    }
    # column type
    if (dim(referenceTable)[1] != 0){
        if (!is.character(referenceTable$cpdID[1])){
            stop("referenceTable$cpdID must be character")
        }
        if (!is.character(referenceTable$cpdName[1])){
            stop("referenceTable$cpdName must be character")
        }
        if (!(is.numeric(referenceTable$rt[1]) | is.na(referenceTable$rt[1]))){
            stop("referenceTable$rt must be numeric or NA")
        }
        if (!(is.numeric(referenceTable$rt_dev_sec[1]) |
        is.na(referenceTable$rt_dev_sec[1]))){
            stop("referenceTable$rt_dev_sec must be numeric or NA") }
    }

    ## Check method
    KNOWN_CORRECTIONMETHODs <- c('RANSAC')
    if (!(method %in% KNOWN_CORRECTIONMETHODs)) {
        stop(paste('Error: "method" must be one of:', KNOWN_CORRECTIONMETHODs))
    }

    ## Check params input
    if (!(is.character(params) | is.list(params))) {
        stop('Check input, "params" must be "guess" or list') }
    # params is 'guess' if character
    if (is.character(params)){
        if (params != 'guess') {
            stop('Check input, "params" must be "guess" if not list')
        }
    }
}
# check input targetFeatTable
applyRTCorrection_checkTargetFeatTable <- function(targetFeatTable) {
    # is data.frame
    if (!is(targetFeatTable, "data.frame")){
        stop("specified targetFeatTable is not a data.frame")
    }

    # required columns are present
    if (!all(c("cpdID", "cpdName", "rtMin", "rt", "rtMax",
    "mzMin", "mz", "mzMax") %in% colnames(targetFeatTable))){
        stop("expected columns in targetFeatTable are \"cpdID\", \"cpdName\", ",
        "\"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\" and \"mzMax\"")
    }

    # column type
    if (dim(targetFeatTable)[1] != 0){
        if (!is.character(targetFeatTable$cpdID[1])){
            stop("targetFeatTable$cpdID must be character")
        }
        if (!is.character(targetFeatTable$cpdName[1])){
            stop("targetFeatTable$cpdName must be character")
        }
        if (!is.numeric(targetFeatTable$rtMin[1])){
            stop("targetFeatTable$rtMin must be numeric")
        }
        if (!(is.numeric(targetFeatTable$rt[1]) |
                is.na(targetFeatTable$rt[1]))){
            stop("targetFeatTable$rt must be numeric or NA")
        }
        if (!is.numeric(targetFeatTable$rtMax[1])){
            stop("targetFeatTable$rtMax must be numeric")
        }
        if (!is.numeric(targetFeatTable$mzMin[1])){
            stop("targetFeatTable$mzMin must be numeric")
        }
        if (!(is.numeric(targetFeatTable$mz[1]) |
                is.na(targetFeatTable$mz[1]))){
            stop("targetFeatTable$mz must be numeric or NA")
        }
        if (!is.numeric(targetFeatTable$mzMax[1])){
            stop("targetFeatTable$mzMax must be numeric")
        }
    }
}

## -----------------------------------------------------------------------------
##        RANSAC
## -----------------------------------------------------------------------------
# New retention time calibration functions can be added below
# All fitting functions should have the following argument:
# of the kind x = theoreticalrt, y= Deviation (Rt_{obs} - Rt{exp}}

# guess_RANSAC <- function(x, y)


predict_Rt <- function(x, y, correctionFunction=NA) {

    if (is.na(correctionFunction)) {
        stop("correctionFunction must be a previously fitted retention
        time correction expression")
    }

}
# fit_RANSAC <- function()
fit_LOESS <- function(x, y, ...) {

    loess_fun <- loess()

    return (loess_fun)
}
# predict_RANSAC <- function()


