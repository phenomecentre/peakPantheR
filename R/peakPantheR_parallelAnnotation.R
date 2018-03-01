
#' Search, integrate and report targeted features in a multiple spectra
#'
#' Integrate all target features in all files defined in the initialised input object and store results. The use of updated ROI and the integration of FIR are controled by the input object slots \code{useUROI} and \code{useFIR}. Files are processed in parallel using \code{link{peakPantheR_singleFileSearch}}; \code{ncores} controls the number of cores used for parallelisation, with \code{ncores=0} corresponding to serial processing. If the processing of a file fails (file does not exist or error during execution) the sample is removed from the outputed object.
#'
#' @param object (peakPantheRAnnotation) Initialised peakPantheRAnnotation object defining the samples to process and compounds to target. The slots \code{useUROI} and \code{useFIR} controls if uROI must be used and FIR integrated if a feature is not found
#' @param ncores (int) Number of cores to use for parallelisation. Default 0 for no parallelisation.
#' @param fitGauss (bool) if TRUE fits peak with option \code{CentWaveParam(..., fitgauss=TRUE)}.
#' @param getAcquTime (bool) If TRUE will extract sample acquisition date-time from the mzML metadata (the additional file access will impact run time)
#' @param verbose (bool) If TRUE message calculation progress, time taken, number of features found (total and matched to targets) and failures
#' @param ... Passes arguments to \code{findTargetFeatures} to alter peak-picking parameters
#'
#' @return a list: \code{list()$result} \emph{(peakPantheRAnnotation)} fully annotated object, \code{list()$failures} \emph{(list)} list of failed samples and error message
#'
#' @examples
#' \dontrun{
#' # placeholder
#' }
#'
#' @family peakPantheR
#' @family parallelAnnotation
#'
#' @import foreach
#' @import doParallel
#'
#' @export
peakPantheR_parallelAnnotation <- function(object, ncores=0, fitGauss=FALSE, getAcquTime=TRUE, verbose=TRUE, ...) {
  ## ------------------------------------------------------------
  parallel_helper <- function(singleSpectraDataPath, targetFeatTable, fitGauss=FALSE, getAcquTime=FALSE, verbose=TRUE, ...){
    # Check input file exist, wrap \code{peakPantheR_singleFileSearch} in a try cratch, add a failure status
    #
    # @param singleSpectraDataPath (str) path to netCDF or mzML raw data file (centroided, \strong{only with the channel of interest}).
    # @param targetFeatTable a \code{\link{data.frame}} of compounds to target as rows. Columns: \code{cpdID} (int), \code{cpdName} (str), \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float).
    # @param fitGauss (bool) if TRUE fits peak with option \code{CentWaveParam(..., fitgauss=TRUE)}.
    # @param getAcquTime (bool) If TRUE will extract sample acquisition date-time from the mzML metadata (the additional file access will impact run time)
    # @param verbose (bool) If TRUE message calculation progress, time taken and number of features found (total and matched to targets)
    # @param ... Passes arguments to \code{findTargetFeatures} to alter peak-picking parameters
    #
    # @return a list: \code{list()$TIC} \emph{(int)} TIC value, \code{list()$peakTable} \emph{data.frame} targeted features results (see Details), \code{list()$EICs} \emph{(list or NA)} list of \code{xcms::Chromatogram} matching the ROI, \code{list()$acquTime} \emph{(POSIXct or NA)} date-time of sample acquisition from mzML metadata, \code{list()$failure} \emph{(named str or NULL)} a string detailing the error (named with the singleSpectraDataPath) or NA if the processing is successful.

    ## Check input path exist or exit with error message
    if (!file.exists(singleSpectraDataPath)) {
      if (verbose) {
        message(paste('Error file does not exist: ', singleSpectraDataPath, sep=""))
      }
      # add error status
      failureMsg        <- paste('Error file does not exist: ', singleSpectraDataPath, sep="")
      names(failureMsg) <- singleSpectraDataPath
      # return basic values and failure message
      return(list(TIC=as.numeric(NA), peakTable=NULL, EICs=NULL, acquTime=as.character(NA), failure=failureMsg))
    }


    ## Run singleFileSearch in try catch
    file_name <- tools::file_path_sans_ext(basename(singleSpectraDataPath))

    # progress
    if (verbose) {
      message(paste('-----', file_name, '-----'))
    }

    # try catch
    result <- tryCatch(
      {
        # singleFileSearch
        tmpResult     <- peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, fitGauss=fitGauss, peakStatistic=TRUE, getEICs=TRUE, plotEICsPath=NA, getAcquTime=getAcquTime, verbose=verbose, ...)
        # add failure status
        failureMsg        <- NA
        names(failureMsg) <- singleSpectraDataPath
        tmpResult$failure <- failureMsg
        # last evaluation of Try is returned
        return(tmpResult)
      },
      error=function(err) {
        # message error
        if (verbose) {
          message('-----')
          message(paste('Error processing file:', file_name))
          message(err)
          message('\n-----')
        }
        # add error status
        failureMsg        <- paste(err)
        names(failureMsg) <- singleSpectraDataPath
        # return basic values and failure message
        return(list(TIC=as.numeric(NA), peakTable=NULL, EICs=NULL, acquTime=as.character(NA), failure=failureMsg))
      }
    )

  # return singleFileSearch results with failure status
  return(result)
  }
  ## ------------------------------------------------------------


  ## check validity of object
  validObject(object)

  ## Open parallel interface
  if (ncores!=0) {
    cl <- parallel::makeCluster( ncores )
    doParallel::registerDoParallel( cl )
  }

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


  ## Run singleFileSearch (list, each item is the result for a file, errors are passed into the list)
  if( ncores!=0 ) {
    allFilesRes   <- foreach::foreach( x=file_paths, .export=c('peakPantheR_singleFileSearch', 'makeROIList', 'findTargetFeatures', 'getTargetFeatureStatistic', 'getAcquisitionDatemzML'), .inorder=TRUE, .errorhandling='pass' ) %dopar% parallel_helper(x, target_peak_table, fitGauss=fitGauss, getAcquTime=getAcquTime, verbose=verbose, ...)
  } else {
    allFilesRes   <- lapply(file_paths, function(x) parallel_helper(x, target_peak_table, fitGauss=fitGauss, getAcquTime=getAcquTime, verbose=verbose, ...))
  }


  ## Collect and process results
  # identify annotations that failed
  fail_status     <- sapply(allFilesRes, function(x){x$failure})
  failures        <- !is.na(fail_status)
  names(failures) <- NULL
  fail_table      <- data.frame(matrix(c(names(fail_status)[failures], fail_status[failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # message failures
  if ((sum(failures) != 0) & verbose) {
    message('-----')
    message(paste(sum(failures), 'file(s) failed to process:\n', paste0(capture.output(fail_table), collapse = "\n")))
    message()
    message('-----')
  }
  # remove failures
  allFilesRes <- allFilesRes[!failures]
  # reshape the output object to match (remove failed samples)
  outObject   <- object[!failures,]

  # unlist result into final object
  # acquisitionTime
  outObject@acquisitionTime <- sapply(allFilesRes, function(x) {as.character(x$acquTime)})
  # TIC
  outObject@TIC             <- sapply(allFilesRes, function(x) {x$TIC})
  # peakTables (all columns but cpdID and cpdName)
  outObject@peakTables      <- lapply(allFilesRes, function(x) {x$peakTable[,!names(x$peakTable) %in% c("cpdID", "cpdName")] })
  # EICs
  outObject@EICs            <- lapply(allFilesRes, function(x) {x$EIC})


  ## check validity and exit
  validObject(outObject)

  etime <- Sys.time()
  if (verbose) {
    message('Parallel annotation done in: ', round(as.double(difftime(etime,stime)),2),' ',units( difftime(etime,stime)))
  }

  return(list(annotation=outObject, failures=fail_table))
}
