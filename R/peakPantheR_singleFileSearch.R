###################################################################################################
#                                                                                                 #
#     --- peakPantheR: detect and integrate pre-defined features in MS files ---                  #
#                                                                                                 #
###################################################################################################


#' Search, integrate and report targeted features in a raw spectra
#'
#' Report for a raw spectra the TIC, acquisition time, integrated targeted features, fitted curves and datapoints for each region of interest. Optimised to reduce the number of file access. Features not detected can be integrated using fallback integration regions (FIR).
#'
#' @param singleSpectraDataPath (str) path to netCDF or mzML raw data file (centroided, \strong{only with the channel of interest}).
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as rows. Columns: \code{cpdID} (str), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
#' @param peakStatistic (bool) If TRUE calculates additional peak statistics: 'ppm_error', 'rt_dev_sec', 'tailing factor' and 'asymmetry factor'
#' @param plotEICsPath (str or NA) If not NA, will save a \emph{.png} of all ROI EICs at the path provided (\code{'filepath/filename.png'} expected). If NA no plot saved
#' @param getAcquTime (bool) If TRUE will extract sample acquisition date-time from the mzML metadata (the additional file access will impact run time)
#' @param FIR (data.frame or NULL) If not NULL, integrate Fallback Integration Regions (FIR) when a feature is not found.  Compounds as row are identical to \code{targetFeatTable}, columns are \code{rtMin} (float in seconds), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mzMax} (float).
#' @param verbose (bool) If TRUE message calculation progress, time taken and number of features found
#' @param ... Passes arguments to \code{findTargetFeatures} to alter peak-picking parameters (e.g. \code{curveModel}, \code{sampling}, \code{params} as a list of parameters for each ROI or 'guess',...)
#'
#' @return a list: \code{list()$TIC} \emph{(int)} TIC value, \code{list()$peakTable} \emph{(data.frame)} targeted features results (see Details), \code{list()$curveFit} \emph{(list)} list of \code{peakPantheR_curveFit} or NA for each ROI, \code{list()$acquTime} \emph{(POSIXct or NA)} date-time of sample acquisition from mzML metadata, \code{list()$ROIsDataPoint} \emph{(list)} a list of \code{data.frame} of raw data points for each ROI (retention time "rt", mass "mz" and intensity "int" (as column) of each raw data points (as row)).
#'
#' \subsection{Details:}{
#'   The returned \emph{peakTable} \code{data.frame} is structured as follow:
#'   \tabular{ll}{
#'     cpdID \tab database compound ID\cr
#'     cpdName \tab compound name\cr
#'     found \tab was the peak found\cr
#'     rt \tab retention time of peak apex (sec)\cr
#'     rtMin \tab leading edge of peak retention time (sec) determined at 0.5\% of apex intensity\cr
#'     rtMax \tab trailing edge of peak retention time (sec) determined at 0.5\% of apex intensity\cr
#'     mz \tab weighted (by intensity) mean of peak m/z across scans\cr
#'     mzMin \tab m/z peak minimum (between rtMin, rtMax)\cr
#'     mzMax \tab m/z peak maximum (between rtMin, rtMax)\cr
#'     peakArea \tab integrated peak area\cr
#'     maxIntMeasured \tab maximum peak intensity in raw data\cr
#'     maxIntPredicted \tab maximum peak intensity based on curve fit\cr
#'     is_filled \tab Logical indicate if the feature was integrated using FIR (Fallback Integration Region)\cr
#'     ppm_error \tab difference in ppm between the expected and measured m/z\cr
#'     rt_dev_sec \tab difference in seconds between the expected and measured rt\cr
#'     tailingFactor \tab the tailing factor is a measure of peak tailing.It is defined as the distance from the front slope of the peak to the back slope divided by twice the distance from the center line of the peak to the front slope, with all measurements made at 5\% of the maximum peak height. The tailing factor of a peak will typically be similar to the asymmetry factor for the same peak, but the two values cannot be directly converted\cr
#'     asymmetryFactor \tab the asymmetry factor is a measure of peak tailing. It is defined as the distance from the center line of the peak to the back slope divided by the distance from the center line of the peak to the front slope, with all measurements made at 10\% of the maximum peak height. The asymmetry factor of a peak will typically be similar to the tailing factor for the same peak, but the two values cannot be directly converted\cr
#'   }
#' }
#'
#' @examples
#' if(requireNamespace("faahKO")){
#' ## Load data
#' library(faahKO)
#' netcdfFilePath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
#'
#' ## targetFeatTable
#' targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID",
#'                          "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
#'                          stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
#' targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- sapply(targetFeatTable[,c(3:8)], as.numeric)
#'
#' res <- peakPantheR_singleFileSearch(netcdfFilePath,targetFeatTable, peakStatistic=TRUE)
#' # Polarity can not be extracted from netCDF files, please set manually the polarity with the
#' #   'polarity' method.
#' # Reading data from 2 windows
#' # Data read in: 0.16 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation,
#' #   approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1
#' # Found 2/2 features in 0.05 secs
#' # Peak statistics done in: 0 secs
#' # Feature search done in: 0.75 secs
#' 
#' res
#' # $TIC
#' # [1] 2410533091
#' #
#' # $peakTable
#' #   found    rtMin       rt    rtMax    mzMin    mz    mzMax peakArea maxIntMeasured
#' # 1  TRUE 3309.759 3346.828 3385.410 522.1948 522.2 522.2052 26133727         889280
#' # 2  TRUE 3345.377 3386.529 3428.279 496.2000 496.2 496.2000 35472141        1128960
#' #   maxIntPredicted cpdID cpdName is_filled  ppm_error rt_dev_sec tailingFactor
#' # 1        901015.8  ID-1   Cpd 1     FALSE 0.02337616  1.9397590      1.015357
#' # 2       1113576.7  ID-2   Cpd 2     FALSE 0.02460103  0.9518072      1.005378
#' #    asymmetryFactor
#' # 1        1.026824
#' # 2        1.009318
#' #
#' # $acquTime
#' # [1] NA
#' #
#' #
#' # $curveFit
#' # $curveFit[[1]]
#' # $amplitude
#' # [1] 162404.8
#' # 
#' # $center
#' # [1] 3341.888
#' # 
#' # $sigma
#' # [1] 0.07878613
#' # 
#' # $gamma
#' # [1] 0.00183361
#' # 
#' # $fitStatus
#' # [1] 2
#' # 
#' # $curveModel
#' # [1] "skewedGaussian"
#' # 
#' # attr(,"class")
#' # [1] "peakPantheR_curveFit"
#' # 
#' # $curveFit[[2]]
#' # $amplitude
#' # [1] 199249.1
#' # 
#' # $center
#' # [1] 3382.577
#' # 
#' # $sigma
#' # [1] 0.07490442
#' # 
#' # $gamma
#' # [1] 0.00114719
#' # 
#' # $fitStatus
#' # [1] 2
#' # 
#' # $curveModel
#' # [1] "skewedGaussian"
#' # 
#' # attr(,"class")
#' # [1] "peakPantheR_curveFit"
#' #
#' #
#' # $ROIsDataPoint
#' # $ROIsDataPoint[[1]]
#' #          rt    mz    int
#' # 1  3315.154 522.2   2187
#' # 2  3316.719 522.2   3534
#' # 3  3318.284 522.2   6338
#' # 4  3319.849 522.2  11718
#' # 5  3321.414 522.2  21744
#' # 6  3322.979 522.2  37872
#' # 7  3324.544 522.2  62424
#' # 8  3326.109 522.2  98408
#' # 9  3327.673 522.2 152896
#' # 10 3329.238 522.2 225984
#' # ...
#' #
#' # $ROIsDataPoint[[2]]
#' #          rt    mz     int
#' # 1  3280.725 496.2    1349
#' # 2  3290.115 496.2    2069
#' # 3  3291.680 496.2    3103
#' # 4  3293.245 496.2    5570
#' # 5  3294.809 496.2   10730
#' # 6  3296.374 496.2   20904
#' # 7  3297.939 496.2   38712
#' # 8  3299.504 496.2   64368
#' # 9  3301.069 496.2   97096
#' # 10 3302.634 496.2  136320
#' # ...
#' }
#'
#' @family peakPantheR
#' @family realTimeAnnotation
#' @family parallelAnnotation
#'
#' @export
peakPantheR_singleFileSearch <- function(singleSpectraDataPath, targetFeatTable, peakStatistic=FALSE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, verbose=TRUE, ...) {
  stime <- Sys.time()

  ## Check input
  singleSpectraDataPath   <- normalizePath(singleSpectraDataPath, mustWork=FALSE)

  if (!file.exists(singleSpectraDataPath)) {
    stop('Check input, file \"', singleSpectraDataPath ,'\" does not exist')
  }

  if (dim(targetFeatTable)[1] != 0){
    # rtMin < rtMax and mzMin < mzMax
    if(!all(targetFeatTable[,'rtMax'] >= targetFeatTable[,'rtMin'])) {stop('Check input, "rtMin" must be <= to "rtMax"')}
    if(!all(targetFeatTable[,'mzMax'] >= targetFeatTable[,'mzMin'])) {stop('Check input, "mzMin" must be <= to "mzMax"')}
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
  }

  useFIR <- FALSE
  if (!is.null(FIR)) {
    # FIR is Data.frame
    if (!is.data.frame(FIR)) {
      stop('Check input, FIR must be a data.frame not ', class(FIR))
    }
    # FIR number of rows
    if (dim(FIR)[1] != dim(targetFeatTable)[1]) {
      stop('Check input, FIR must have the same number of rows as targetFeatTable')
    }
    # FIR columns
    if (!all(c("mzMin","mzMax","rtMin","rtMax") %in% colnames(FIR))) {
      stop('Check input, FIR must have "mzMin", "mzMax", "rtMin" and "rtMax" as columns')
    }
    useFIR <- TRUE
  }


  ## Read file
  raw_data  <- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')
  
  ## Get TIC
  TICvalue  <- sum(MSnbase::tic(raw_data))#, initial=FALSE to calculate from raw and not header

  ## Get AcquTime
  AcquTime  <- NA
  if (getAcquTime) {
    AcquTime    <- getAcquisitionDatemzML(mzMLPath=singleSpectraDataPath, verbose=verbose)
  }
  
  ## Get ROIsDataPoint (return empty list if no windows)
  ROIsDataPoint <- extractSignalRawData(raw_data, rt=targetFeatTable[,c('rtMin','rtMax')], mz=targetFeatTable[,c('mzMin','mzMax')], verbose=verbose)
  

  ## Only integrate if there is at minimum 1 target feature.
  if (dim(targetFeatTable)[1] != 0){
    
    ## Integrate features using ROI
    foundPeaks      <- findTargetFeatures(ROIsDataPoint, targetFeatTable, verbose=verbose, ...)
    foundPeakTable  <- foundPeaks$peakTable
    curveFit        <- foundPeaks$curveFit

    ## Add compound information
    finalOutput           <- foundPeakTable
    finalOutput$cpdID     <- targetFeatTable$cpdID
    finalOutput$cpdName   <- targetFeatTable$cpdName
    finalOutput$is_filled <- as.logical(FALSE)


    ## Add deviation, Tailing factor, Asymmetry factor
    if(peakStatistic){
      finalOutput   <- getTargetFeatureStatistic(curveFit, targetFeatTable, finalOutput, verbose=verbose)
    }

    ## Fill features not found based on FIR
    if (useFIR) {
      finalOutput   <- integrateFIR(raw_data, FIR, finalOutput, verbose=verbose)
    }

    ## Save all EICs plot
    if(!is.na(plotEICsPath)) {
      saveSingleFileMultiEIC(ROIsDataPoint, curveFit, finalOutput, plotEICsPath, width=15, height=15, verbose=verbose)
    }

  ## No targeted features, initialise empty integration results and EICs
  } else {
    if (verbose) {message('- No target features passed in \'targetFeatTable\', no integration, only TIC will be reported -')}
    if (peakStatistic) {
      finalOutput <- data.frame(matrix(vector(), 0, 17, dimnames=list(c(), c('cpdID', 'cpdName', 'found', 'rt', 'rtMin', 'rtMax', 'mz', 'mzMin', 'mzMax', 'peakArea', 'maxIntMeasured', 'maxIntPredicted', 'is_filled', 'ppm_error', 'rt_dev_sec', 'tailingFactor', 'asymmetryFactor'))), stringsAsFactors=FALSE)
    } else {
      finalOutput <- data.frame(matrix(vector(), 0, 13, dimnames=list(c(), c('cpdID', 'cpdName', 'found', 'rt', 'rtMin', 'rtMax', 'mz', 'mzMin', 'mzMax', 'peakArea', 'maxIntMeasured', 'maxIntPredicted', 'is_filled'))), stringsAsFactors=FALSE)
    }
    curveFit    <- list()
  }

  etime <- Sys.time()
	if (verbose) {
    message('Feature search done in: ', round(as.double(difftime(etime,stime)),2),' ',units( difftime(etime,stime)))
  }

  # clear variables
  rm(raw_data)
  gc(verbose=FALSE)
  
  return(list(TIC=TICvalue, peakTable=finalOutput, acquTime=AcquTime, curveFit=curveFit, ROIsDataPoint=ROIsDataPoint))
}
