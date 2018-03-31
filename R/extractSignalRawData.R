#' Extract signal in a multiple defined mz rt window from a raw data file
#'
#' Extract all signal from multiple defined mz rt window from raw data and returns a data.frame. If no rt-mz window is provided, all signal in the raw data file are returned
#'
#' @param rawSpec an \code{\link[MSnbase]{OnDiskMSnExp-class}}
#' @param rt (numeric(2) or two-column matrix) the lower and upper retention time range from which the data should be extracted. If a matrix is passed, each row corresponds to a different window. If not provided, the full retention time range will be extracted.
#' @param mz (numeric(2) or two-column matrix) the lower and upper mass range from which the data should be extracted. If a matrix is passed, each row corresponds to a different window. If not provided, the full mass range will be extracted.
#' @param msLevel (int) the MS level at which the data should be extracted (default to MS level 1)
#' @param verbose (bool) If TRUE message progress and warnings
#'
#' @return a list (one entry per window) of data.frame with signal as row and retention time ("rt"), mass ("mz") and intensity ("int) as columns.
extractSignalRawData <- function(rawSpec, rt, mz, msLevel=1L, verbose=TRUE) {

  ## Check input
  # check type and dimensions
  #   msLevel
  if (!is.integer(msLevel)) {stop('Check input "msLevel" must be integer')}
  #   rt
  if (!missing(rt)) {
    if (!(class(rt) %in% c('numeric', 'matrix', 'data.frame'))) {stop('Check input "rt" must be numeric, matrix or data.frame')}
    if ((class(rt) == 'numeric') & (length(rt) != 2)) {stop('Check input "rt" must be numeric of length 2')}
    if (class(rt) %in% c('matrix','data.frame')) { if (ncol(rt) != 2) {stop('Check input "rt" must be a matrix or data.frame with 2 columns')} }
  }
  #   mz
  if (!missing(mz)) {
    if (!(class(mz) %in% c('numeric', 'matrix', 'data.frame'))) {stop('Check input "mz" must be numeric, matrix or data.frame')}
    if ((class(mz) == 'numeric') & (length(mz) != 2)) {stop('Check input "mz" must be numeric of length 2')}
    if (class(mz) %in% c('matrix','data.frame')) { if (ncol(mz) != 2) {stop('Check input "mz" must be a matrix or data.frame with 2 columns')} }
  }
  #   both rt and mz have same number of rows (unless one of them has only 1 rowe)
  if (!missing(rt) & !missing(mz)) {
    if ((class(rt) %in% c('matrix','data.frame')) & (class(mz) %in% c('matrix','data.frame'))) {
      if (nrow(rt) != nrow(mz)) {
        if ((nrow(rt) != 1) & (nrow(mz) !=1)) {
          stop('Check input "rt" and "mz" matrix or data.frame must have the same number of rows')
        } else {
          if (verbose) {message('"rt" or "mz" is a matrix/data.frame of 1 row, rows will be ducplicated to match the other input')}
        }
      }
    } 
  }
  
  ##  Express rt and mz as matrix/data.frame of identical number of rows
  # replace missing by whole range
  if (missing(rt)) {
    rt <- matrix(c(-Inf, Inf), ncol=2, byrow=T)
  }
  if (missing(mz)) {
    mz <- matrix(c(-Inf, Inf), ncol=2, byrow=T)
  }
  # convert all numeric to matrix
  if (class(rt) == 'numeric') {
    rt <- matrix(rt, nrow=1, ncol=2, byrow=T)
  }
  if (class(mz) == 'numeric') {
    mz <- matrix(mz, nrow=1, ncol=2, byrow=T)
  }
  # Replicate rows (if only 1) to match other
  if (nrow(rt) == 1) {
    rt <- matrix(rep(as.numeric(rt), nrow(mz)), ncol=2, byrow=T)
  }
  if (nrow(mz) == 1) {
    mz <- matrix(rep(as.numeric(mz), nrow(rt)), ncol=2, byrow=T)
  }
    
  ## now both rt and mz are either a matrix/data.frame of matching size
  if (verbose) { message('Reading data from ', nrow(rt), ' windows') }
  # empty output
  empty_res <- lapply(vector('list', nrow(rt)), function(x) {data.frame(rt=numeric(), mz=numeric(), int=integer())})
  
  ## Filter msLevel
  msFilteredSpec <- MSnbase::filterMsLevel(rawSpec, msLevel=msLevel)
  # if msLevel doesn't exist, exit
  if (length(msFilteredSpec) == 0) {
    if (verbose) { message('No data exist for MS level ', msLevel) }
    return(empty_res)
  }
  
  ## Filter only scans falling into the rt of interest (across all windows)
  file_rt       <- MSnbase::rtime(msFilteredSpec)
  keep_scan_idx <- sort(unique(as.integer(unlist(apply(rt, MARGIN=1, function(x) {which((file_rt >= x[1]) & (file_rt <= x[2]))}), use.names=F))))
  # if no scans
  if (length(keep_scan_idx) == 0) {
    if (verbose) { message('No data exist for the rt provided') }
    return(empty_res)
  }
  # file with only the needed scans
  rtFilteredSpec <- msFilteredSpec[keep_scan_idx]
  
  # Extract only scans we need (only file access)
  spectraData <- MSnbase::spectra(rtFilteredSpec)
  spec_rt     <- MSnbase::rtime(rtFilteredSpec)
  
  ## Get data points from each window (subset rt and check mz)
  res   <- empty_res
  # iterage over windows
  for (i in 1:nrow(rt)) {
    
    # subset the scans we need from the ones we have extracted
    scans_to_keep   <- (spec_rt >= rt[i, 1]) & (spec_rt <= rt[i, 2])
    if (!any(scans_to_keep)) {
      if (verbose) { message('No data exist for window ', i) }
      # move to next window (empty df was already initialised)
      next
    }
    
    # only keep scans matching the window
    scanSubset  <- spectraData[scans_to_keep]

    # subset each scan based on mz and extract datapoints
    scanDatapoint <- lapply(scanSubset, function(scan, mzScan) {
      # filter mz
      suppressWarnings(
        filtScan  <- MSnbase::filterMz(scan, mzScan)
      )
      # extract datapoint
      if(!filtScan@peaksCount) {
        # no datapoints
        data.frame(rt=numeric(), mz=numeric(), int=integer())
      } else {
        data.frame(rt=rep_len(filtScan@rt, length(filtScan@mz)), mz=filtScan@mz, int=filtScan@intensity)
      }
    }, mzScan = mz[i,])
    
    # concatenate all scans data.frame
    scanTable            <- do.call(rbind, scanDatapoint)
    rownames(scanTable)  <- NULL
    
    # store results
    res[[i]] <- scanTable
  }
  
  return(res)
}

