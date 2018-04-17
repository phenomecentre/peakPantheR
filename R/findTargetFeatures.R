#' Find and integrate target features in each ROI
#' 
#' For each ROI, fit a curve and integrate the largest feature in the box. Each entry in \code{ROIsDataPoints} must match the corresponding row in \code{ROI}. The curve shape to employ for fitting can be changed with \code{curveModel} while fitting parameters can be changed with \code{params} (list with one param per ROI window). \code{rtMin} and \code{rtMax} are established at 0.5% of apex intensity using a moving window from the apex outward (the window is the ROI width); if after 20 iterations \code{rtMin} or \code{rtMax} is not found, NA is returned. \code{peakArea} is calculated from \code{rtMin} to \code{rtMax} (if they haven't been found, 2x the ROI window width left or right from the apex is used as limit). \code{mz} is the weighted (by intensity) average mz of datapoints falling into the \code{rtMin} to \code{rtMax} range, \code{mzMin} and \code{mzMax} are the minimum and maxmimum mass in these range. If \code{rtMin} or \code{rtMax} falls outside of ROI (extracted scans), \code{mzMin} or \code{mzMax} are returned as the input ROI limits and \code{mz} is an approximation on the datapoints available.
#'
#' @param ROIsDataPoints (list) A list (one entry per ROI window) of data.frame with signal as row and retention time ("rt"), mass ("mz") and intensity ("int) as columns. Must match each row of ROI.
#' @param ROI (data.frame) A data.frame of compounds to target as rows. Columns: \code{rtMin} (float in seconds), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mzMax} (float)
#' @param curveModel (str) Name of the curve model to fit (currently \code{skewedGaussian})
#' @param params (list or str) Either 'guess' for automated parametrisation or list (one per ROI windows) of "guess" or list of curve fit parameters
#' @param sampling (int) Number of points to employ when subsampling the fittedCurve (rt, rtMin, rtMax, integral calculation)
#' @param verbose (bool) If TRUE message the time taken and number of features found
#' @param ... Passes arguments to \code{fitCurve} to alter peak fitting (\code{params})
#'
#' @return A list: \code{list()$peakTable} (\emph{data.frame}) with targeted features as rows and peak measures as columns (see Details), \code{list()$curveFit} (\emph{list}) a list of \code{peakPantheR_curveFit} or NA for each ROI.
#'
#' \subsection{Details:}{
#'   The returned \code{data.frame} is structured as follow:
#'   \tabular{ll}{
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
#'   }
#' }
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
#' targetFeatTable[,3:8] <- sapply(targetFeatTable[,3:8], as.numeric)
#'
#' ROIsPt         <- extractSignalRawData(raw_data, rt=targetFeatTable[,c('rtMin','rtMax')],
#'                                        mz=targetFeatTable[,c('mzMin','mzMax')], verbose=TRUE)
#' # Reading data from 2 windows
#'
#' foundPeaks <- findTargetFeatures(ROIsPt, targetFeatTable, verbose=T)
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation,
#' # approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1
#' # Found 2/2 features in 0.07 secs
#' 
#' foundPeaks
#' # $peakTable
#' #   found    rtMin       rt    rtMax    mzMin    mz    mzMax peakArea maxIntMeasured maxIntPredicted
#' # 1  TRUE 3309.759 3346.828 3385.410 522.1948 522.2 522.2052 26133727         889280        901015.8
#' # 2  TRUE 3345.377 3386.529 3428.279 496.2000 496.2 496.2000 35472141        1128960       1113576.7
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
#' }
findTargetFeatures <- function(ROIsDataPoints, ROI, curveModel='skewedGaussian', params='guess', sampling=250, verbose=FALSE, ...){
  stime <- Sys.time()
  
  ## Check inputs
  nROI <- nrow(ROI)
  # ROIsDataPoints match ROI
  if (length(ROIsDataPoints) != nROI) {
    stop('Check input, number of ROIsDataPoints entries must match the number of rows of ROI')
  }
  # Check all data points fall into the corresponding ROI
  for(r in 1:nROI) {
    if(!all((ROIsDataPoints[[r]]$rt >= ROI[r,c('rtMin')]) & (ROIsDataPoints[[r]]$rt <= ROI[r,c('rtMax')]))) {stop('Check input not all datapoints for window #', r, ' are into the corresponding ROI (rt)')}
    if(!all((ROIsDataPoints[[r]]$mz >= ROI[r,c('mzMin')]) & (ROIsDataPoints[[r]]$mz <= ROI[r,c('mzMax')]))) {stop('Check input not all datapoints for window #', r, ' are into the corresponding ROI (mz)')}
  }
  # Check params input
  if (!(is.character(params) | is.list(params))) {
    stop('Check input, "params" must be "guess" or list')
  }
  # params is 'guess' if character
  if (is.character(params)){
    if (params != 'guess') {
      stop('Check input, "params" must be "guess" if not list')
    }
  }
  # length if params is list
  if (is.list(params)){
    if (length(params) != nROI) {
      stop('Check input, number of parameters must match number of rows of ROI')
    }
  }
  
  
  ## Init output
  outTable        <- data.frame(matrix(vector(), nROI, 10, dimnames=list(c(),c('found','rtMin','rt','rtMax','mzMin','mz','mzMax','peakArea','maxIntMeasured','maxIntPredicted'))), stringsAsFactors=F)
  outTable$found  <- rep(FALSE, nROI)     # set found to FALSE
  outCurveFit     <- rep(list(NA), nROI)
  # use input params or guess 
  if (any(params != 'guess')) {
    useParams <- TRUE
    if (verbose) {message('Curve fitting parameters passed as input employed')}
  } else {
    useParams <- FALSE
  }
  
  
  ## Iterate over ROIs
  for (i in 1:nROI) {
    # set params for fitting
    new_params    <- 'guess'
    if (useParams) {
      new_params  <- params[[i]]
    }
    
    ## EIC to fit
    tmp_EIC     <- generateIonChromatogram(ROIDataPoint=ROIsDataPoints[[i]], aggregationFunction='sum')
    
    ## fit curve to EIC (store output). If failure move to next window
    fittedCurve <- tryCatch(
      {
        ## try
        fittedCurve <- fitCurve(x=tmp_EIC$rt, y=tmp_EIC$int, curveModel=curveModel, params=new_params, ...)
      },
      error=function(cond) {
        ## catch
        return(NA)
      }
    )
    # catch fit failure
    if (all(is.na(fittedCurve))) {
      if (verbose) {message('Fit of ROI #', i,' is unsuccessful (try error)')}
      # move to next window (empty df row was already initialised)
      next
    }
    # discard fit if nls.lm fit status indicates unsuccessful completion
    if ((fittedCurve$fitStatus == 0) | (fittedCurve$fitStatus ==5) | (fittedCurve$fitStatus ==-1)) {
      if (verbose) {message('Fit of ROI #', i,' is unsuccessful (fit status)')}
      # move to next window (empty df row was already initialised)
      next
    } else {
      outCurveFit[[i]] <- fittedCurve
    }
    
    ## rt (search on same bounds as peak fit +/-3s)
    rt_EICmax       <- tmp_EIC$rt[which.max(tmp_EIC$int)]
    grid_rt         <- seq(from=rt_EICmax-3, to=rt_EICmax+3, by=(6/(sampling-1)))
    close_apex_int  <- predictCurve(fittedCurve, x=grid_rt)
    rt              <- grid_rt[which.max(close_apex_int)]
    
    ## maxIntMeasured, maxIntPredicted
    maxIntMeasured  <- max(tmp_EIC$int)
    maxIntPredicted <- predictCurve(fittedCurve=fittedCurve, x=rt)
    
    
    ## rtMin, rtMax (look for 0.5% from max int, by rolling away from apex until match or too many iterations)
    peakLim_int <- 0.005 * maxIntPredicted
    deltaRt     <- ROI$rtMax[i] - ROI$rtMin[i]
    ## Up slope
    # init
    rtMin       <- as.numeric(NA)
    boxMin      <- rt
    cntr        <- 0
    # search rtMin
    while (is.na(rtMin) & cntr<=20) {
      # box moves earlier in rt each time
      boxMax      <- boxMin
      boxMin      <- boxMax - deltaRt
      cntr        <- cntr+1
      grid_rt     <- seq(from=boxMax, to=boxMin, by=((boxMin-boxMax)/(sampling-1))) # reverse order for up slope
      slope_int   <- predictCurve(fittedCurve, x=grid_rt)
      cutoff_pt   <- match(-1,sign(slope_int - peakLim_int))  # pos of 1st point past cutoff
      if (is.na(cutoff_pt)) {
        rtMin     <- as.numeric(NA)
        next
      }
      key_pt      <- c(cutoff_pt-1, cutoff_pt)                # points left and right from rtMin
      rtMin       <- stats::approx(x=slope_int[key_pt], y=grid_rt[key_pt], xout=peakLim_int)$y # linear interpolation of exact rt
    }
    if (is.na(rtMin) & verbose) {message('Warning: rtMin cannot be determined for ROI #',i)}
    ## Down slope
    # init
    rtMax       <- as.numeric(NA)
    boxMax      <- rt
    cntr        <- 0
    # search rtMax
    while (is.na(rtMax) & cntr<=20) {
      # box moves later in rt each time
      boxMin      <- boxMax
      boxMax      <- boxMin + deltaRt
      cntr        <- cntr+1
      grid_rt     <- seq(from=boxMin, to=boxMax, by=((boxMax-boxMin)/(sampling-1)))
      slope_int   <- predictCurve(fittedCurve, x=grid_rt)
      cutoff_pt   <- match(-1,sign(slope_int - peakLim_int))  # pos of 1st point past cutoff
      if (is.na(cutoff_pt)) {
        rtMax     <- as.numeric(NA)
        next
      }
      key_pt      <- c(cutoff_pt-1, cutoff_pt)                # points left and right from rtMax
      rtMax       <- stats::approx(x=slope_int[key_pt], y=grid_rt[key_pt], xout=peakLim_int)$y # linear interpolation of exact rt
    }
    if (is.na(rtMax) & verbose) {message('Warning: rtMax cannot be determined for ROI #',i)}
    
    
    ## mz, mzMin, mzMax
    # if rtMin, rtMax are NA, or outside of ROI, we cannot calculate mzMin, mzMax and mz:
    # default to ROI$mzMIn, ROI$mzMax as a safe choice, and approximate mz
    tmpRtMin    <- rtMin
    tmpRtMax    <- rtMax
    tmpMzMin    <- ROI$mzMin[i]
    tmpMzMax    <- ROI$mzMax[i]
    tmpROIData  <- ROIsDataPoints[[i]]
    isValid     <- TRUE
    # deal with NA
    if (is.na(tmpRtMin) | is.na(tmpRtMax)) {
      isValid   <- FALSE
      if (verbose) { message('Warning: rtMin/rtMax cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #',i) }
      # replace rtMin by ROI$rtMin
      if (is.na(tmpRtMin)) { tmpRtMin <- ROI$rtMin[i] }
      # replace rtMax by ROI$rtMax
      if (is.na(tmpRtMax)) { tmpRtMax <- ROI$rtMax[i] }
    } 
    # deal with rtMin rtMax outside of ROI
    if ((tmpRtMin < ROI$rtMin[i]) | (tmpRtMax > ROI$rtMax[i])) {
      isValid   <- FALSE
      if (verbose) { message('Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #',i) }
      # replace rtMin by ROI$rtMin
      if (tmpRtMin < ROI$rtMin[i]) { tmpRtMin <- ROI$rtMin[i] }
      # replace rtMax by ROI$rtMax
      if (tmpRtMax > ROI$rtMax[i]) { tmpRtMax <- ROI$rtMax[i] }
    } 
    # subset datapoints mz to rtMin/rtMax range
    tmpPt       <- tmpROIData[(tmpROIData$rt > tmpRtMin) & (tmpROIData$rt < tmpRtMax), ]
    # rtMin rtMax range can be used for mzMin mzMax calculation
    if (isValid) {
      tmpMzMin  <- min(tmpPt$mz)
      tmpMzMax  <- max(tmpPt$mz)
    }
    # calculate mz (might be an approx) (weighted average of total intensity across all rt for each unique mz)
    mzRange           <- unique(tmpPt$mz)
    mzTotalIntensity  <- sapply(mzRange, function(x) {sum(tmpPt$int[tmpPt$mz == x])})
    mz                <- stats::weighted.mean(mzRange, mzTotalIntensity)
    # tidy
    mzMin   <- tmpMzMin
    mzMax   <- tmpMzMax
    
    
    ## integrate curve
    tmpRtMin  <- rtMin
    tmpRtMax  <- rtMax
    # 2x rt window should be enough to reach bottom level left and right if we didn't fint rtMin/Max
    if (is.na(tmpRtMin)) {tmpRtMin <- rt - 2*deltaRt}
    if (is.na(tmpRtMax)) {tmpRtMax <- rt + 2*deltaRt}
    #         __a__
    #       /|     \
    #     /  h      \ 
    #   /____|__b____\
    #  \     |      /
    #   \    h     /
    #    \___|__c_/
    #
    #   Area = (a+b)/2 * h + (b+c)/2 * h
    #        = (a+2b+c)/2 * h
    h         <- (tmpRtMax-tmpRtMin)/(sampling-1)
    grid_rt   <- seq(from=tmpRtMin, to=tmpRtMax, by=h)
    val_int   <- predictCurve(fittedCurve, x=grid_rt)
    dist      <- sum( c(val_int, val_int[2:(sampling-1)]) )/2
    peakArea  <- dist * h
    
    
    ## Set values
    outTable$found[i]            <- TRUE
    outTable$rt[i]               <- rt
    outTable$rtMin[i]            <- as.numeric(rtMin)
    outTable$rtMax[i]            <- as.numeric(rtMax)
    outTable$mz[i]               <- mz
    outTable$mzMin[i]            <- mzMin
    outTable$mzMax[i]            <- mzMax
    outTable$peakArea[i]         <- peakArea
    outTable$maxIntMeasured[i]   <- maxIntMeasured
    outTable$maxIntPredicted[i]  <- maxIntPredicted
  }
  
  ## output
  etime <- Sys.time()
  if (verbose) {
    message('Found ', sum(outTable$found), '/', nROI, ' features in ', round(as.double(difftime(etime,stime)),2),' ',units( difftime(etime,stime)))
  }
  
  return(list(peakTable=outTable, curveFit=outCurveFit))
}
