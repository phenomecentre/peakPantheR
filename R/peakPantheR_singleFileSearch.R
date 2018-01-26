###################################################################################################
#                                                                                                 #
#     --- peakPantheR: detect and integrate pre-defined features in MS files using XCMS3 ---      #
#                                                                                                 #
###################################################################################################


#' Search, integrate and report targeted features in a raw spectra
#'
#' Report TIC, EIC and integrated targeted features in a raw spectra. Optimised to reduce the number of file access.
#'
#' @param singleSpectraDataPath (str) path to netCDF or mzML raw data file (centroided, \strong{only with the channel of interest}).
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as rows. Columns: \code{cpdID} (int), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (double in seconds), \code{mzMin} (float in seconds), \code{mz} (float in seconds, or \emph{NA}), \code{mzMax} (float in seconds).
#' @param fitGauss (bool) if TRUE fits peak with option \code{CentWaveParam(..., fitgauss=TRUE)}.
#' @param peakStatistic (bool) If TRUE calculates additional peak statistics: deviation, FWHM, Tailing factor, Assymetry factor
#' @param getEICs (bool) If TRUE returns a list of EICs corresponding to each ROI. (subsequently passed to \code{link{getTargetFeatureStatistic}} to reduce the number of file reads). If \code{plotEICsPath}, EICs will be loaded.
#' @param plotEICsPath (str or NA) If not NA, will save a \emph{.png} of all ROI EICs at the path provided (\code{'filepath/filename.png'} expected). If NA no plt saved
#' @param verbose (bool) If TRUE message calculation progresss, time taken and number of features found (total and matched to targets)
#' @param ... Passes arguments to \code{findTargetFeatures} to alter peak-picking parameters
#'
#' @return a list: \code{list()$TIC} \emph{(int)} TIC value, \code{list()$peakTable} \emph{data.frame} targeted features results (see Details), \code{list()$EICs} \emph{(list or NA)} list of \code{xcms::Chromatogram} matching the ROI.
#'
#' \subsection{Details:}{
#'   The returned \emph{peakTable} \code{data.frame} is structured as follow:
#'   \tabular{ll}{
#'     cpdID \tab database compound ID\cr
#'     cpdName \tab compound name\cr
#'     found \tab (bool) TRUE if compound was found in the raw data\cr
#'     mz \tab weighted (by intensity) mean of peak m/z across scans\cr
#'     mzmin \tab m/z peak minimum\cr
#'     mzmax \tab m/z peak maximum\cr
#'     rt \tab retention time of peak midpoint\cr
#'     rtmin \tab leading edge of peak retention time\cr
#'     rtmax \tab trailing edge of peak retention time\cr
#'     into \tab integrated peak intensity\cr
#'     intb \tab baseline corrected integrated peak intensity\cr
#'     maxo \tab maximum peak intensity\cr
#'     sn \tab Signal/Noise ratio, defined as \code{(maxo - baseline)/sd}, where \code{maxo} is the maximum peak intensity, \code{baseline} the estimated baseline value and \code{sd} the standard deviation of local chromatographic noise.\cr
#'     egauss \tab RMSE of Gaussian fit\cr
#'     mu \tab Gaussian parameter mu\cr
#'     sigma \tab Gaussian parameter sigma\cr
#'     h \tab Gaussian parameter h\cr
#'     f \tab Region number of m/z ROI where the peak was localised\cr
#'     dppm \tab m/z deviation of mass trace across scans in ppm\cr
#'     scale \tab Scale on which the peak was localised\cr
#'     scpos \tab Peak position found by wavelet analysis (scan number)\cr
#'     scmin \tab Left peak limit found by wavelet analysis (scan number)\cr
#'     scmax \tab Right peak limit found by wavelet analysis (scan number)\cr
#'     ppm_error \tab difference in ppm between the expected and measured m/z, not available if \code{peakStatistic=FALSE}\cr
#'     rt_dev_sec \tab difference in seconds between the expected and measured rt, not available if \code{peakStatistic=FALSE}\cr
#'     FWHM \tab full width at half maximum (in seconds), not available if \code{fitGauss=FALSE}, not available if \code{peakStatistic=FALSE}\cr
#'     FWHM_ndatapoints \tab number of scans on the peak, not available if \code{peakStatistic=FALSE}\cr
#'     tailingFactor \tab the tailing factor is a measure of peak tailing.It is defined as the distance from the front slope of the peak to the back slope divided by twice the distance from the center line of the peak to the front slope, with all measurements made at 5\% of the maximum peak height. The tailing factor of a peak will typically be similar to the asymmetry factor for the same peak, but the two values cannot be directly converted, not available if \code{peakStatistic=FALSE}\cr
#'     assymmetryFactor \tab the asymmetry factor is a measure of peak tailing. It is defined as the distance from the center line of the peak to the back slope divided by the distance from the center line of the peak to the front slope, with all measurements made at 10\% of the maximum peak height. The asymmetry factor of a peak will typically be similar to the tailing factor for the same peak, but the two values cannot be directly converted, not available if \code{peakStatistic=FALSE}\cr
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' ## Load data
#' library(MSnbase)
#'
#' ## Raw data file
#' netcdfFilePath <- './my_spectra.CDF'
#'
#' ## targetFeatTable from outside source
#' targetFeatTable     <- data.frame(matrix(vector(), 11, 8, dimnames=list(c(), c("cpdID",
#'                          "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
#'                          stringsAsFactors=F)
#' targetFeatTable[1,] <- c(1, "LPC (9:0/0:0)", 29.4, NA, 38.4, 398.2108, 398.2308, 398.2508)
#' targetFeatTable[2,] <- c(2, "PC (11:0/11:0)", 138.0, NA, 198.0, 594.3935, 594.4135, 594.4335)
#' targetFeatTable[,c(1,3:8)] <- sapply(targetFeatTable[,c(1,3:8)], as.numeric)
#'
#' res <- peakPantheR_singleFileSearch(netcdfFilePath,targetFeatTable[1:2,], peakStatistic=TRUE)
#' # Polarity can not be extracted from netCDF files, please set manually the polarity with the
#' #	'polarity' method.
#' # Detecting chromatographic peaks in 2 regions of interest ... OK: 7 found.
#' # Found 2/2 features (7 total) in 27.8 secs
#' # Peak statistics done in: 10.37 secs
#' # Feature search done in: 38.61 secs
#' res
#' # $TIC
#' # [1] 13406775774
#' #
#' # $peakTable
#' #    cpdID        cpdName found       mz    mzmin    mzmax      rt   rtmin   rtmax    into
#' # 1      1  LPC (9:0/0:0)  TRUE 398.2319 398.2306 398.2330  33.296  31.468  35.352 1919498
#' # 2      2 PC (11:0/11:0)  TRUE 594.4140 594.4120 594.4149 179.046 176.762 181.103 2233011
#' #    	 intb     maxo     sn     egauss       mu    sigma         h f dppm  scale scpos scmin
#' # 1  1919222 942711.5  15739 0.04867894 136.1303 3.458960  979537.2 1    3      4   136   132
#' # 2  2232784 1038315.5 30275 0.03296024 770.7220 3.832822 1034072.3 2    1     -1    -1    -1
#' #    scmax lmin lmax sample is_filled ppm_error rt_dev_sec   FWHM FWHM_ndatapoints tailingFactor
#' # 1  	140  128 145      1         0  2.744491         NA 1.86026                9     0.8868141
#' # 2   	-1   375 394      1         0  0.830051         NA      NA               NA     1.0053721
#' #    assymmetryFactor
#' # 1         1.1248373
#' # 2         0.9744952
#' }
#'
#' @family peakPantheR
#' @family realTimeAnnotation
#' @family parallelAnnotation
#'
#' @export
peakPantheR_singleFileSearch <- function(singleSpectraDataPath, targetFeatTable, fitGauss=FALSE, peakStatistic=FALSE, getEICs=FALSE, plotEICsPath=NA, verbose=TRUE, ...) {
  stime <- Sys.time()

  ## Check input
  singleSpectraDataPath   <- normalizePath(singleSpectraDataPath, mustWork=FALSE)

  if (!file.exists(singleSpectraDataPath)) {
    stop('Check input, file \"', singleSpectraDataPath ,'\" does not exist')
  }

  if (!is.na(plotEICsPath)) {
    plotEICsPath  <- normalizePath(plotEICsPath, mustWork=FALSE)
    # folder exist
    if (!file.exists(dirname(plotEICsPath))) {
      stop('Check input, plotEICsPath folder \"', dirname(plotEICsPath) ,'\" does not exist')
    }
    # png extension
    if (stringr::str_sub(basename(plotEICsPath), start=-4) != '.png') {
      stop('Check input, plotEICsPath file name \"', basename(plotEICsPath) ,'\" lacks a \".png\" extension')
    }
    getEICs <- TRUE
  }

  ## Read file
  raw_data  <- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')

  ## Get TIC
  TICvalue  <- sum(MSnbase::tic(raw_data))#, initial=FALSE to calculate from raw and not header

  ## Generate Region of Interest List (ROIList)
  ROIList  	<- makeROIList(raw_data, targetFeatTable)

  ## Integrate features using ROI
  foundPeakTable <- findTargetFeatures(raw_data, ROIList, verbose=verbose, fitGauss=fitGauss, ...)

	## Collect ROI EICs
	EICs 				<- NA
	if (getEICs) {
		eicstime 	<- Sys.time()
		EICs			<- xcms::chromatogram(raw_data, rt = data.frame(rt_lower=targetFeatTable$rtMin, rt_upper=targetFeatTable$rtMax), mz = data.frame(mz_lower=targetFeatTable$mzMin, mz_upper=targetFeatTable$mzMax))
		eicetime 	<- Sys.time()
		if (verbose) { message('EICs loaded in: ', round(as.double(difftime(eicetime,eicstime)),2),' ',units(difftime(eicetime,eicstime)))}
	}

  ## Add compound information
  finalOutput         <- foundPeakTable
  finalOutput$cpdID   <- targetFeatTable$cpdID
  finalOutput$cpdName <- targetFeatTable$cpdName

  ## Add deviation, FWHM, Tailing factor, Assymetry factor
  if(peakStatistic){
		# don't read EICs from file if already done
    finalOutput   <- getTargetFeatureStatistic(raw_data, targetFeatTable, finalOutput, usePreviousEICs=EICs, verbose=verbose)
  }

  ## Save all EICs plot
  if(!is.na(plotEICsPath)) {
    save_multiEIC(EICs, finalOutput, plotEICsPath, width=15, height=15, verbose=verbose)
  }

  etime <- Sys.time()
	if (verbose) {
    message('Feature search done in: ', round(as.double(difftime(etime,stime)),2),' ',units( difftime(etime,stime)))
  }

  return(list(TIC=TICvalue, peakTable=finalOutput, EICs=EICs))
}
