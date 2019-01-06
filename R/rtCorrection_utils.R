#' Correct targeted retention time based on reference compounds
#'
#' Correct targeted features retention time using the RT and RT deviation of previously fitted compounds. The `method` and `params` are used to select and parametrise the retention time correction method employed. If `diagnostic` is TRUE, RT correction diagnostic plots are returned (specific to each correction method).
#'
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as rows and parameters as columns: \code{cpdID} (str), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#' @param referenceTable a \code{\link{data.frame}} of reference compound information as rows and properties as columns: \code{cpdID} (str), \code{cpdName} (str), \code{rt} (float), \code{rt_dev_sec} (float)
#' @param method (str) name of RT correction method to use (currently \code{RANSAC})
#' @param params (list or str) either 'guess' for automated parametrisation or list of parameters (specific to each correction method)
#' @param diagnostic (bool) If TRUE returns diagnostic plots (specific to each correction method)
#' @param verbose (bool) If TRUE message progress of RT correction
#' @param ... optional method specific parameters
#' 
#' @return a targetFeatTable with corrected RT, or diagnostic information
#' 
#' @export
peakPantheR_applyRTCorrection <- function(targetFeatTable, referenceTable, method='RANSAC', params='guess', diagnostic=FALSE, verbose=TRUE, ...) {
  ## TODO
  # check input tables and method
  # trigger the method
  # collect results and put them in targetFeature
  # out
  # unittest
  # vignette
  
  return(NA)
}

## --------------------------------------------------------------------------------------------------
##        RANSAC
## --------------------------------------------------------------------------------------------------

# fit_RANSAC <- function()

# predict_RANSAC <- function()


