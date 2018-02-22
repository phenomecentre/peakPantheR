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
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as rows. Columns: \code{cpdID} (int), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#' @param fitGauss (bool) if TRUE fits peak with option \code{CentWaveParam(..., fitgauss=TRUE)}.
#' @param peakStatistic (bool) If TRUE calculates additional peak statistics: deviation, FWHM, Tailing factor, Asymmetry factor
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
#'     asymmetryFactor \tab the asymmetry factor is a measure of peak tailing. It is defined as the distance from the center line of the peak to the back slope divided by the distance from the center line of the peak to the front slope, with all measurements made at 10\% of the maximum peak height. The asymmetry factor of a peak will typically be similar to the tailing factor for the same peak, but the two values cannot be directly converted, not available if \code{peakStatistic=FALSE}\cr
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' ## Load data
#' library(faahKO)
#' netcdfFilePath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
#'
#' ## targetFeatTable
#' targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID",
#'                          "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
#'                          stringsAsFactors=F)
#' targetFeatTable[1,] <- c(1, "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
#' targetFeatTable[2,] <- c(2, "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
#' targetFeatTable[,c(1,3:8)] <- sapply(targetFeatTable[,c(1,3:8)], as.numeric)
#'
#' res <- peakPantheR_singleFileSearch(netcdfFilePath,targetFeatTable, peakStatistic=TRUE)
#' # Polarity can not be extracted from netCDF files, please set manually the polarity with the
#' #	'polarity' method.
#' # Detecting chromatographic peaks in 2 regions of interest ... OK: 3 found.
#' # Found 2/2 features (3 total) in 1.12 secs
#' # Peak statistics done in: 0.15 secs
#' # Feature search done in: 1.71 secs
#' res
#' # $TIC
#' # [1] 2410533091
#' #
#' # $peakTable
#' #   found   mz mzmin mzmax        rt    rtmin    rtmax     into     intb    maxo   sn egauss
#' # 1  TRUE 522.2 522.2 522.2 3344.888 3322.979 3379.317 25792525 25768308  889280 1840     NA
#' # 2  TRUE 496.2 496.2 496.2 3382.447 3362.102 3409.051 32873727 32818664 1128960 1471     NA
#' #   mu sigma  h f dppm scale scpos scmin scmax lmin lmax sample is_filled cpdID cpdName
#' # 1 NA    NA NA 1    0     5   540   535   545   24   60      1         0     1   Cpd 1
#' # 2 NA    NA NA 2    0     5   564   559   569   68   98      1         0     2   Cpd 2
#' #    ppm_error rt_dev_sec FWHM FWHM_ndatapoints tailingFactor asymmetryFactor
#' # 1 0.02337616       0.00   NA               11            NA        1.484000
#' # 2 0.02460103       3.13   NA               11            NA        2.708291
#' #
#' # $EICs
#' # [1] NA
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


  ## Only integrate if there is at minimum 1 target feature.
  if (dim(targetFeatTable)[1] != 0){

    ## Generate Region of Interest List (ROIList)
    ROIList  	<- makeROIList(raw_data, targetFeatTable)

    ## Integrate features using ROI
    foundPeakTable <- findTargetFeatures(raw_data, ROIList, verbose=verbose, fitGauss=fitGauss, ...)

    # Only keep going if an integration region is found
    if (sum(foundPeakTable$found)!=0) {

    	## Collect ROI EICs
    	EICs 				<- NULL
    	if (getEICs) {
    		eicstime 	<- Sys.time()
    		if (dim(targetFeatTable)[1] == 1) {
    		  # only one row (targeted feature)
    		  EICs			<- xcms::chromatogram(raw_data, rt = c(rt_lower=targetFeatTable$rtMin, rt_upper=targetFeatTable$rtMax), mz = c(mz_lower=targetFeatTable$mzMin, mz_upper=targetFeatTable$mzMax))
    		} else {
    		  # multiple row (targeted feature)
    		  EICs			<- xcms::chromatogram(raw_data, rt = data.frame(rt_lower=targetFeatTable$rtMin, rt_upper=targetFeatTable$rtMax), mz = data.frame(mz_lower=targetFeatTable$mzMin, mz_upper=targetFeatTable$mzMax))
    		}
    		eicetime 	<- Sys.time()
    		if (verbose) { message('EICs loaded in: ', round(as.double(difftime(eicetime,eicstime)),2),' ',units(difftime(eicetime,eicstime)))}
    	}

      ## Add compound information
      finalOutput         <- foundPeakTable
      finalOutput$cpdID   <- targetFeatTable$cpdID
      finalOutput$cpdName <- targetFeatTable$cpdName

      ## Add deviation, FWHM, Tailing factor, Asymmetry factor
      if(peakStatistic){
    		# don't read EICs from file if already done
        finalOutput   <- getTargetFeatureStatistic(raw_data, targetFeatTable, finalOutput, usePreviousEICs=EICs, verbose=verbose)
      }

      ## Save all EICs plot
      if(!is.na(plotEICsPath)) {
        save_multiEIC(EICs, finalOutput, plotEICsPath, width=15, height=15, verbose=verbose)
      }

    ## No integration region found, initialise empty integration results and EICs
    } else {
      if (verbose) {message('- No features found to integrate, only TIC will be reported -')}
      finalOutput <- data.frame(matrix(vector(), 0, 33, dimnames=list(c(), c('found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'cpdID', 'cpdName', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor'))), stringsAsFactors=F)
      EICs        <- NULL
    }

  ## No target features, initialise empty integration results and EICs
  } else {
    if (verbose) {message('- No target features passed in \'targetFeatTable\', no integration, only TIC will be reported -')}
    finalOutput <- data.frame(matrix(vector(), 0, 33, dimnames=list(c(), c('found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'cpdID', 'cpdName', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor'))), stringsAsFactors=F)
    EICs        <- NULL
  }

  etime <- Sys.time()
	if (verbose) {
    message('Feature search done in: ', round(as.double(difftime(etime,stime)),2),' ',units( difftime(etime,stime)))
  }

  return(list(TIC=TICvalue, peakTable=finalOutput, EICs=EICs))
}
