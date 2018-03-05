#' Generate a Region Of Interest (ROI) List
#'
#' Generate a ROIList as expected by \code{\link[xcms]{findChromPeaks-centWave}} from a \code{\link{data.frame}} with compounds to target as rows. \emph{length} and \emph{intensity} are set to \code{-1} as centWave does not use these values.
#'
#' @param rawSpec an \code{\link[MSnbase]{OnDiskMSnExp-class}} used to get the scans corresponding to each retention time.
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as rows. Columns: \code{cpdID} (str), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#'
#' @return a list of ROIs
#'
#' @examples
#' \dontrun{
#' ## Load data
#' library(faahKO)
#' library(MSnbase)
#' netcdfFilePath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
#' raw_data       <- MSnbase::readMSData(netcdfFilePath, centroided=TRUE, mode='onDisk')
#'
#' ## targetFeatTable
#' targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID",
#'                          "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
#'                          stringsAsFactors=F)
#' targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
#' targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
#' targetFeatTable[,c(1,3:8)] <- sapply(targetFeatTable[,c(1,3:8)], as.numeric)
#'
#' ROIList <- makeROIList(raw_data, targetFeatTable)
#' ROIList[[1]]
#' # $mz
#' # [1] 522.2
#' #
#' # $mzmin
#' # [1] 522.194778
#' #
#' # $mzmax
#' # [1] 522.205222
#' #
#' # $scmin
#' # [1] 518
#' #
#' # $scmax
#' # [1] 569
#' #
#' # $length
#' # [1] -1
#' #
#' # $intensity
#' # [1] -1
#' }
makeROIList        <- function(rawSpec, targetFeatTable) {
  ROIList <- list()
	# single load of scanID/RT from file
	RTtime 	<- MSnbase::rtime(rawSpec)
  for (i in 1:dim(targetFeatTable)[1]) {
    # find the closest scan matching the retention time
    scmin 	<- which.min(abs(targetFeatTable$rtMin[i] - RTtime))[[1]]
    scmax 	<- which.min(abs(targetFeatTable$rtMax[i] - RTtime))[[1]]
    # mz, length and intensity are not used by centWave
    ROIList[[i]] <- list(mz=targetFeatTable$mz[i], mzmin=targetFeatTable$mzMin[i], mzmax=targetFeatTable$mzMax[i], scmin=scmin, scmax=scmax, length=-1, intensity=-1)
  }
  return(ROIList)
}
