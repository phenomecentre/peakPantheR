#' Search, integrate and report targeted features in a multiple spectra
#'
#' Integrate all target features in all files defined in the initialised input object and store results. The use of updated ROI and the integration of FIR are controled by the input object slots \code{useUROI} and \code{useFIR}. Files are processed in parallel using \code{link{peakPantheR_singleFileSearch}}; \code{ncores} controls the number of cores used for parallelisation, with \code{ncores=0} corresponding to serial processing. If the processing of a file fails (file does not exist or error during execution) the sample is removed from the outputed object.
#'
#' @param object (peakPantheRAnnotation) Initialised peakPantheRAnnotation object defining the samples to process and compounds to target. The slots \code{useUROI} and \code{useFIR} controls if uROI must be used and FIR integrated if a feature is not found
#' @param ncores (int) Number of cores to use for parallelisation. Default 0 for no parallelisation.
#' @param getAcquTime (bool) If TRUE will extract sample acquisition date-time from the mzML metadata (the additional file access will impact run time)
#' @param resetWorkers (int) If 0, the parallel cluster is only initiated once. If >0 the cluster will be reset (and the memory of each worker freed) once \code{ncores * resetWorkers} files have been processed. Default value is 1, the cluster is reset once \code{ncores} files have been processed. While potentially impacting performance (need to wait until all \code{ncores * resetWorkers} files are processed before restarting the cluster), shutting down the workers processes regularly will ensure the OS can reallocate memory more efficiently. For values >1, ensure sufficient system memory is available
#' @param verbose (bool) If TRUE message calculation progress, time taken, number of features found (total and matched to targets) and failures
#' @param ... Passes arguments to \code{findTargetFeatures} to alter peak-picking parameters
#'
#' @return a list: \code{list()$result} \emph{(peakPantheRAnnotation)} fully annotated object, \code{list()$failures} \emph{(list)} list of failed samples and error message
#'
#' @examples
#' \dontrun{
#' ## Load data
#' library(faahKO)
#' 
#' # 3 files
#' input_spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
#'                         system.file('cdf/KO/ko16.CDF', package = "faahKO"),
#'                         system.file('cdf/KO/ko18.CDF', package = "faahKO"))
#' 
#' # 4 features
#' input_ROI     <- data.frame(matrix(vector(), 4, 8,
#'                                    dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt",
#'                                                         "rtMax", "mzMin", "mz", "mzMax"))),
#'                                    stringsAsFactors=F)
#' input_ROI[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
#' input_ROI[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
#' input_ROI[3,] <- c("ID-3", "Cpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
#' input_ROI[4,] <- c("ID-4", "Cpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
#' input_ROI[,c(3:8)] <- sapply(input_ROI[,c(3:8)], as.numeric)
#' 
#' # Initialise object
#' initAnnotation <- peakPantheRAnnotation(spectraPaths=input_spectraPaths,
#'                                         targetFeatTable=input_ROI)
#' # to use updated ROI:
#' # uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI
#' # to use FallBack Integration Regions:
#' # useFIR=TRUE, FIR=input_FIR
#' 
#' # Run serially
#' result_parallelAnnotation <- peakPantheR_parallelAnnotation(initAnnotation, ncores=0,
#'                                                             getAcquTime=FALSE,
#'                                                             verbose=TRUE)
#' # Processing 4 compounds in 3 samples:
#' #  uROI:	FALSE
#' #  FIR:	FALSE
#' # ----- ko15 -----
#' # Polarity can not be extracted from netCDF files, please set manually the polarity
#' #  with the 'polarity' method.
#' # Reading data from 4 windows
#' # Data read in: 0.24 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3
#' # Found 4/4 features in 0.06 secs
#' # Peak statistics done in: 0.02 secs
#' # Feature search done in: 0.76 secs
#' # ----- ko16 -----
#' # Polarity can not be extracted from netCDF files, please set manually the polarity
#' #  with the 'polarity' method.
#' # Reading data from 4 windows
#' # Data read in: 0.24 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #2
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #4
#' # Found 4/4 features in 0.08 secs
#' # Peak statistics done in: 0 secs
#' # Feature search done in: 0.71 secs
#' # ----- ko18 -----
#' # Polarity can not be extracted from netCDF files, please set manually the polarity 
#' #  with the 'polarity' method.
#' # Reading data from 4 windows
#' # Data read in: 0.25 secs
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #2
#' # Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax 
#' #  calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #4
#' # Found 4/4 features in 0.06 secs
#' # Peak statistics done in: 0 secs
#' # Feature search done in: 0.71 secs
#' # ----------------
#' # Parallel annotation done in: 2.18 secs
#' 
#' # No failures
#' result_parallelAnnotation$failures
#
#' result_parallelAnnotation$annotation
#' # An object of class peakPantheRAnnotation
#' #  4 compounds in 3 samples. 
#' #    updated ROI do not exist (uROI)
#' #    does not use updated ROI (uROI)
#' #    does not use fallback integration regions (FIR)
#' #    is annotated
#' }
#'
#' @family peakPantheR
#' @family parallelAnnotation
#'
#' @import foreach
#' @import doParallel
#'
#' @export
peakPantheR_parallelAnnotation <- function(object, ncores=0, getAcquTime=TRUE, resetWorkers=1, verbose=TRUE, ...) {
  ## ------------------------------------------------------------
  parallel_helper <- function(singleSpectraDataPath, targetFeatTable, inFIR=NULL, inGetAcquTime=FALSE, inVerbose=TRUE, ...){
    # Check input file exist, wrap \code{peakPantheR_singleFileSearch} in a try cratch, add a failure status
    #
    # @param singleSpectraDataPath (str) path to netCDF or mzML raw data file (centroided, \strong{only with the channel of interest}).
    # @param targetFeatTable a \code{\link{data.frame}} of compounds to target as rows. Columns: \code{cpdID} (str), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
    # @param FIR (data.frame or NULL) If not NULL, integrate Fallback Integration Regions (FIR) when a feature is not found.  Compounds as row are identical to \code{targetFeatTable}, columns are \code{rtMin} (float in seconds), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mzMax} (float).
    # @param getAcquTime (bool) If TRUE will extract sample acquisition date-time from the mzML metadata (the additional file access will impact run time)
    # @param verbose (bool) If TRUE message calculation progress, time taken and number of features found (total and matched to targets)
    # @param ... Passes arguments to \code{findTargetFeatures} to alter peak-picking parameters
    #
    # @return a list: \code{list()$TIC} \emph{(int)} TIC value, \code{list()$peakTable} \emph{(data.frame)} targeted features results (see Details), \code{list()$curveFit} \emph{(list)} list of \code{peakPantheR_curveFit} or NA for each ROI, \code{list()$acquTime} \emph{(POSIXct or NA)} date-time of sample acquisition from mzML metadata, \code{list()$ROIsDataPoint} \emph{(list)} a list of \code{data.frame} of raw data points for each ROI (retention time "rt", mass "mz" and intensity "int" (as column) of each raw data points (as row)). \code{list()$failure} \emph{(named str or NULL)} a string detailing the error (named with the singleSpectraDataPath) or NA if the processing is successful.

    ## Check input path exist or exit with error message
    if (!file.exists(singleSpectraDataPath)) {
      if (verbose) {
        message(paste('Error file does not exist: ', singleSpectraDataPath, sep=""))
      }
      # add error status
      failureMsg        <- paste('Error file does not exist: ', singleSpectraDataPath, sep="")
      names(failureMsg) <- singleSpectraDataPath
      # return basic values and failure message
      return(list(TIC=as.numeric(NA), peakTable=NULL, acquTime=as.character(NA), curveFit=NULL, ROIsDataPoint=NULL, failure=failureMsg))
    }


    ## Run singleFileSearch in try catch
    file_name <- tools::file_path_sans_ext(basename(singleSpectraDataPath))

    # progress
    if (inVerbose) {
      message(paste('-----', file_name, '-----'))
    }

    # try catch
    result <- tryCatch(
      {
        # singleFileSearch
        tmpResult     <- peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, peakStatistic=TRUE, plotEICsPath=NA, getAcquTime=inGetAcquTime, FIR=inFIR, verbose=inVerbose, ...)
        
        # add failure status
        failureMsg        <- NA
        names(failureMsg) <- singleSpectraDataPath
        tmpResult$failure <- failureMsg
        # last evaluation of Try is returned
        return(tmpResult)
      },
      error=function(err) {
        # message error
        if (inVerbose) {
          message('-----')
          message(paste('Error processing file:', file_name))
          message(err$message)
          message('\n-----')
        }
        # add error status
        failureMsg        <- err$message
        names(failureMsg) <- singleSpectraDataPath
        # return basic values and failure message
        return(list(TIC=as.numeric(NA), peakTable=NULL, acquTime=as.character(NA), curveFit=NULL, ROIsDataPoint=NULL, failure=failureMsg))
      }
    )
    
    # try clear variables
    gc(verbose=FALSE)
    # return singleFileSearch results with failure status
    return(result)
  }
  ## ------------------------------------------------------------


  ## check validity of object
  validObject(object)
  # check resetWorkers value
  if (!is.numeric(resetWorkers)) {stop('Check input, resetWorkers must be an integer')}
  resetWorkersMulti <- as.integer(resetWorkers)
  if (resetWorkersMulti < 0) {stop('Check input, resetWorkers must be a positive integer')}
  
  stime <- Sys.time()

  ## Initialise parameters from object
  use_uROI    <- useUROI(object)
  use_FIR     <- useFIR(object)
  file_paths  <- filepath(object)
  if (use_uROI) {
    target_peak_table <- uROI(object)
  } else {
    target_peak_table <- ROI(object)
  }
  if (use_FIR) {
    input_FIR <- FIR(object)
  } else {
    input_FIR <- NULL
  }


  ## Output parameters
  if (verbose & isAnnotated(object)) {
    message("!! Data was already annotated, results will be overwritten !!")
  }
  if (verbose) {
    message("Processing ", nbCompounds(object), " compounds in ", nbSamples(object), " samples:")
    message("  uROI:\t", use_uROI)
    message("  FIR:\t", use_FIR)
  }


  ## Run singleFileSearch (list, each item is the result for a file, errors are passed into the list)
  # Parallel
  if( ncores!=0 ) {
    
    # Reinitialise the cluster after ncores files (reset worker processes, freed memory can be reallocated by the OS)
    if (resetWorkersMulti != 0) {
      # Init
      nFiles          <- nbSamples(object)
      allFilesRes     <- vector('list', nFiles)
      nFilesPerClust  <- (ncores*resetWorkersMulti)
      nClust          <- ceiling(nFiles/nFilesPerClust)
      if (verbose) {message("Running ", nClust, " clusters of ", nFilesPerClust, " files over ", ncores, " cores:")}
      
      # in each round start a new cluster and store results
      for (iClust in seq(0,nClust-1,1)) {
        if (verbose) {message("  starting cluster ", iClust+1, "/", nClust)}
        # init
        idxStart        <- 1 + iClust*nFilesPerClust
        idxEnd          <- min( (nFilesPerClust + iClust*nFilesPerClust), nFiles) # to not overshoot the number of files
        tmp_file_paths  <- file_paths[idxStart : idxEnd]
        # Open parallel interface
        cl              <- parallel::makeCluster(ncores)
        doParallel::registerDoParallel(cl)
        # Run
        allFilesRes[idxStart : idxEnd] <- foreach::foreach( x=tmp_file_paths, .packages=c("MSnbase","mzR"), .inorder=TRUE) %dopar% parallel_helper(x, target_peak_table, inFIR=input_FIR, inGetAcquTime=getAcquTime, inVerbose=verbose, ...)
        # Close
        parallel::stopCluster(cl)
      }
    
    # Single cluster initialisation (workload can be balanced across workers)
    } else {
      # Open parallel interface
      cl          <- parallel::makeCluster(ncores)
      doParallel::registerDoParallel(cl)
      # Run      
      allFilesRes <- foreach::foreach( x=file_paths, .packages=c("MSnbase","mzR"), .inorder=TRUE) %dopar% parallel_helper(x, target_peak_table, inFIR=input_FIR, inGetAcquTime=getAcquTime, inVerbose=verbose, ...) #, .errorhandling='pass' #.export=c('findTargetFeatures', 'getTargetFeatureStatistic') 
      # Close
      parallel::stopCluster(cl)
    }
    
  # Serial
  } else {
    allFilesRes   <- lapply(file_paths, function(x) parallel_helper(x, target_peak_table, inFIR=input_FIR, inGetAcquTime=getAcquTime, inVerbose=verbose, ...))
  }


  ## Collect and process results
  # identify annotations that failed
  fail_status     <- sapply(allFilesRes, function(x){x$failure})
  failures        <- !is.na(fail_status)
  names(failures) <- NULL
  fail_table      <- data.frame(matrix(c(names(fail_status)[failures], fail_status[failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # message failures
  if ((sum(failures) != 0) & verbose) {
    message('----------------')
    message(paste(sum(failures), 'file(s) failed to process:\n', paste0(capture.output(fail_table), collapse = "\n")))
  }
  # remove failures
  allFilesRes <- allFilesRes[!failures]
  # reshape the output object to match (remove failed samples)
  outObject   <- object[!failures,]

  # unlist result into final object (if there is a minimum of 1 file left)
  if (sum(!failures) > 0) {
    # acquisitionTime
    outObject@acquisitionTime <- sapply(allFilesRes, function(x) {as.character(x$acquTime)})
    # TIC
    outObject@TIC             <- sapply(allFilesRes, function(x) {x$TIC})
    # peakTables (all columns but cpdID and cpdName)
    outObject@peakTables      <- lapply(allFilesRes, function(x) {x$peakTable[,!names(x$peakTable) %in% c("cpdID", "cpdName")] })
    # dataPoints
    outObject@dataPoints      <- lapply(allFilesRes, function(x) {x$ROIsDataPoint})
    # peakFit
    outObject@peakFit         <- lapply(allFilesRes, function(x) {x$curveFit})
    
    # isAnnotated
    outObject@isAnnotated     <- TRUE

  # All files failed
  } else {
    if (verbose) { message("No file left in the object!")}
  }
  
  # reorder results by acquisition date if available
  if (sum(is.na(acquisitionTime(outObject))) == 0) {
    if (verbose) { message("Annotation object reordered by sample acquisition date") }
    outObject   <- outObject[order(acquisitionTime(outObject)),]
  } else {
    if (verbose) { message("Annotation object cannot be reordered by sample acquisition date") }
  }

  ## check validity and exit
  validObject(outObject)

  etime <- Sys.time()
  if (verbose) {
    message('----------------')
    message('Parallel annotation done in: ', round(as.double(difftime(etime,stime)),2),' ',units( difftime(etime,stime)))
    message('  ', dim(fail_table)[1], ' failure(s)')
  }

  return(list(annotation=outObject, failures=fail_table))
}
