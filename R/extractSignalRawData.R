#' Extract signal in a defined mz rt window from a raw data file
#'
#' Extract all signal from a defined mz rt window from raw data and returns a data.frame. If no rt-mz window is provided, all signal in the raw data file are returned
#'
#' @param rawSpec an \code{\link[MSnbase]{OnDiskMSnExp-class}}
#' @param rt (numeric(2)) the retention time range from which the data should be extracted
#' @param mz (numeric(2)) the mass range from which the data should be extracted
#' @param msLevel (int) the MS level at which the data should be extracted (default to MS level 1)
#'
#' @return a data.frame with signal as row and retention time ("rt"), mass ("mz") and intensity ("int) as columns
extractSignalRawData <- function(rawSpec, rt, mz, msLevel=1L) {
  
  ## Check input
  # if missing the filtering functions will return all scans
  if (!missing(mz)) {
    if (length(mz) != 2) {stop('Check input "mz" is not a numeric of length 2')}
  }
  if (!missing(rt)) {
    if (length(rt) != 2) {stop('Check input "rt" is not a numeric of length 2')}
  }
  if (!is.integer(msLevel)) {stop('Check input "msLevel" must be integer')}
  
  ## Only keep scans falling into rt-mz windows
  filteredSpec  <- MSnbase::filterMz(MSnbase::filterRt(MSnbase::filterMsLevel(rawSpec, msLevel=msLevel), rt=rt), mz=mz)
  suppressWarnings(
    allScans    <- mzR::spectra(filteredSpec)
  )
  # if no scan, return an empty data.frame
  if(length(allScans) == 0) {
    return(data.frame(rt=numeric(), mz=numeric(), int=integer()))
  }
  
  ## extract scan data
  allScansDF    <- lapply(allScans, function(x) {
    if(!x@peaksCount) {
      return(data.frame(rt=numeric(), mz=numeric(), int=integer()))
    } else {
      data.frame(rt=rep_len(x@rt, length(x@mz)), mz=x@mz, int=x@intensity)
    }
  })
  
  # concatenate all scans data.frame
  outTable            <- do.call(rbind, allScansDF)
  rownames(outTable)  <- NULL

  return(outTable)
}

