## valid method for \link{peakPantheRAnnotation-class}
## Number of compounds based on @cpdID length, number of samples based on @filepath length.
## Slot type is not checked as \code{setClass} enforces it.
## peakTables and EICs type are checked on first list element.
valid_peakPantheRAnnotation <- function(object) {
  # init
  msg       <- NULL
  valid     <- TRUE
  nbCpd     <- length(object@cpdID)
  nbSample  <- length(object@filepath)

  # number of cpdName
  if (length(object@cpdName) != nbCpd) {
    valid <- FALSE
    msg   <- c(msg, paste("cpdName has ", length(object@cpdName), " elements (compound). Should be ", nbCpd, sep=""))
  }

  # ROI number of rows
  if (dim(object@ROI)[1] != nbCpd) {
    valid <- FALSE
    msg   <- c(msg, paste("ROI has ", dim(object@ROI)[1], " rows (compound). Should be ", nbCpd, sep=""))
  }
  # ROI number of columns
  if (dim(object@ROI)[2] != 6) {
    valid <- FALSE
    msg   <- c(msg, paste("ROI has ", dim(object@ROI)[2], " columns. Should be 6 (\"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\")"))
  }
  # ROI column names
  if (sort(colnames(object@ROI)) != sort(c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))) {
    valid <- FALSE
    msg   <- c(msg, paste("ROI columns should be \"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\", not ", colnames(object@ROI)))
  }

  # FIR number of rows
  if (dim(object@FIR)[1] != nbCpd) {
    valid <- FALSE
    msg   <- c(msg, paste("FIR has ", dim(object@FIR)[1], " rows (compound). Should be ", nbCpd, sep=""))
  }
  # FIR number of columns
  if (dim(object@FIR)[2] != 4) {
    valid <- FALSE
    msg   <- c(msg, paste("FIR has ", dim(object@FIR)[2], " columns. Should be 4 (\"rtMin\", \"rtMax\", \"mzMin\", \"mzMax\")"))
  }
  # FIR column names
  if (sort(colnames(object@FIR)) != sort(c("rtMin", "rtMax", "mzMin", "mzMax"))) {
    valid <- FALSE
    msg   <- c(msg, paste("FIR columns should be \"rtMin\", \"rtMax\", \"mzMin\", \"mzMax\", not ", colnames(object@FIR)))
  }

  # uROI number of rows
  if (dim(object@uROI)[1] != nbCpd) {
    valid <- FALSE
    msg   <- c(msg, paste("uROI has ", dim(object@uROI)[1], " rows (compound). Should be ", nbCpd, sep=""))
  }
  # uROI number of columns
  if (dim(object@uROI)[2] != 6) {
    valid <- FALSE
    msg   <- c(msg, paste("uROI has ", dim(object@uROI)[2], " columns. Should be 6 (\"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\")"))
  }
  # uROI column names
  if (sort(colnames(object@uROI)) != sort(c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))) {
    valid <- FALSE
    msg   <- c(msg, paste("uROI columns should be \"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\", not ", colnames(object@uROI)))
  }

  # number of TIC
  if (length(object@TIC) != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("TIC has ", length(object@TIC), " elements (samples). Should be ", nbSample, sep=""))
  }

  # number of peakTables
  if (length(object@peakTables) != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("peakTables has ", length(object@peakTables), " elements (samples). Should be ", nbSample, sep=""))
  }
  # only check peakTables if min 1 sample and not NULL
  if (nbSample >= 1){
    if (!is.null(object@peakTables[[1]])) {
      # individual peakTable is data.frame
      if (!is.data.frame(object@peakTables[[1]])) {
        valid <- FALSE
        msg   <- c(msg, paste("peakTables must be data.frame or NULL not ", typeof(object@peakTables[[1]]), sep=""))
      }
      # individual peakTable data.frame number of rows
      if (dim(object@peakTables[[1]])[1] != nbCpd) {
        valid <- FALSE
        msg   <- c(msg, paste("peakTables[[1]] has ", dim(object@peakTables[[1]])[1], "rows (compounds). Should be ", nbCpd, sep=""))
      }
      # individual peakTable data.frame number of columns
      if (dim(object@peakTables[[1]])[2] != 33) {
        valid <- FALSE
        msg   <- c(msg, paste("peakTables[[1]] has ", dim(object@peakTables[[1]])[2], "columns. Should be 33", sep=""))
      }
      # individual peakTable data.frame column names
      if (sort(colnames(object@peakTables[[1]])) != sort(c('found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'cpdID', 'cpdName', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor'))) {
        valid <- FALSE
        msg   <- c(msg, paste("peakTables[[1]] columns should be 'found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'cpdID', 'cpdName', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor', not ", colnames(object@peakTables[[1]])))
      }
    }
  }

  # number of EICs
  if (length(object@EICs) != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("EICs has ", length(object@EICs), " elements (samples). Should be ", nbSample, sep=""))
  }
  # only check EICs if min 1 sample and not NULL
  if (nbSample >= 1){
    if (!is.null(object@EICs[[1]])) {
      # individual EIC is list or chromatogram
      if (!(is.list(object@EICs[[1]]) | (class(object@EICs[[1]])=="Chromatogram"))) {
        valid <- FALSE
        msg   <- c(msg, paste("EICs[[1]] must be a list or xcms::Chromatogram, not  ", class(object@EICs[[1]]), sep=""))
      }
      # individual EIC has entry for each compound
      if (length(object@EICs[[1]]) != nbCpd) {
        valid <- FALSE
        msg   <- c(msg, paste("EICs[[1]] contains,  ", length(object@EICs[[1]]), " EICs (compound). Should be ", nbCpd, sep=""))
      }
      if (nbCpd >= 1) {
        # individual EIC compound entry is Chromatogram
        if (class(object@EICs[[1]][[1]]) != "Chromatogram") {
          valid <- FALSE
          msg   <- c(msg, paste("EICs[[1]][[1]] must a xcms::Chromatogram, not  ", class(object@EICs[[1]][[1]]), sep=""))
        }
      }
    }
  }

  # output
  if (valid) {
    return(TRUE)
  } else {
    return(msg)
  }
}
