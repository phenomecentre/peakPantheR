#' Parse acquisition date from a mzML file
#'
#' Extract acquisition date (`'startTimeStamp'``) from a mzML file. In case of failure (or the file is not a \code{mzML}) returns NULL
#'
#' @param mzMLPath (str) path to mzML raw data file
#' @param verbose (bool) if TRUE message progress
#'
#' @return POSIXct or NA
getAcquisitionDatemzML  <- function(mzMLPath, verbose=TRUE) {

  ## Check input
  mzMLPath  <- normalizePath(mzMLPath, mustWork=FALSE)
  # not a mzML extension
  if (tolower(stringr::str_sub(basename(mzMLPath), start=-5)) != '.mzml') {
    if (verbose) { message('Check input, mzMLPath must be a .mzML') }
    return(NA)
  }

  stime <- Sys.time()

  ## Parse XML
  acqTime <- tryCatch(
    {
      ## try
      xmlfile   <- XML::xmlParse(mzMLPath)
      xmltop    <- XML::xmlRoot(xmlfile)
      # check top level structure
      if (XML::xmlName(xmltop) != "indexedmzML") {
        if (verbose) { message('Check input, mzMLPath is not a valid mzML file') }
        return(NA)
      } else {
        acqTime   <- strptime(XML::xmlGetAttr(xmltop[['mzML']][['run']], "startTimeStamp"), format = '%Y-%m-%dT%H:%M:%S')
      }
    },
    error=function(cond) {
      ## catch
      if (verbose) { message('Check input, failure while parsing mzMLPath') }
      return(NA)
    }
  )

  ## Output
  etime <- Sys.time()
  if (verbose) { message('Acquisition date parsed in: ', round(as.double(difftime(etime,stime)),2),' ',units( difftime(etime,stime))) }

  return(acqTime)
}
