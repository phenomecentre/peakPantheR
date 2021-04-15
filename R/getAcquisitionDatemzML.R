#' @title Parse acquisition date from a mzML file
#'
#' @description Extract acquisition date (`'startTimeStamp'``) from a mzML file.
#' In case of failure (or the file is not a \code{mzML}) returns NULL
#'
#' @param mzMLPath (str) path to mzML raw data file
#' @param verbose (bool) if TRUE message progress
#'
#' @return POSIXct or NA
getAcquisitionDatemzML <- function(mzMLPath, verbose = TRUE) {
    
    ## Check input
    mzMLPath <- normalizePath(mzMLPath, mustWork = FALSE)
    # not a mzML extension
    if (tolower(stringr::str_sub(basename(mzMLPath), start = -5)) != ".mzml") {
        if (verbose) {
            message("Check input, mzMLPath must be a .mzML")
        }
        return(NA)
    }
    
    stime <- Sys.time()
    
    ## Parse XML
    acqTime <- tryCatch({
        ## try
        nLinesRead <- 1
        mzMLFile <- file(mzMLPath, "r")
        while (TRUE) {
            currentLine <- readLines(mzMLFile, n = 1)
            if (nLinesRead == 2) {
                if (isFALSE(grepl("<indexedmzML", currentLine, fixed=TRUE))) {
                    if (verbose) {
                    message("Check input, mzMLPath is not a valid mzML file")
                    }
                    return(NA)
                }
            }
            regex1 <-  stringr::str_extract(string = currentLine,
                    pattern = "(?<=startTimeStamp=\")(.*?)(?=\")")
            if (isFALSE(is.na(regex1))) {
                acqTime <- strptime(regex1, format = "%Y-%m-%dT%H:%M:%S")
                ## Output
                etime <- Sys.time()
                if (verbose) {
                    message("Acquisition date parsed in: ",
                            round(as.double(difftime(etime, stime)), 2),
                    " ", units(difftime(etime, stime)))}
                return(acqTime) }
            nLinesRead <- nLinesRead + 1 }
    }, error = function(cond) {
        ## catch
        if (verbose) {
            message("Check input, failure while parsing mzMLPath")}
        return(NA)
    })
}
