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
    msg   <- c(msg, paste("ROI has ", dim(object@ROI)[2], " columns. Should be 6 (\"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\")", sep=""))
  } else {
    # ROI column names
    if (!all(colnames(object@ROI) %in% c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))) {
      valid <- FALSE
      msg   <- c(msg, paste("ROI columns should be \"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\", not ", paste(colnames(object@ROI), collapse=" "), sep=""))
    } else {
      # ROI column type
      if (nbCpd >= 1) {
        if (!is.numeric(object@ROI$rtMin[1])){
          valid <- FALSE
          msg   <- c(msg, paste("ROI$rtMin should be numeric, not ", typeof(object@ROI$rtMin[1]), sep=""))
        }
        if (!is.numeric(object@ROI$rt[1])){
          valid <- FALSE
          msg   <- c(msg, paste("ROI$rt should be numeric, not ", typeof(object@ROI$rt[1]), sep=""))
        }
        if (!is.numeric(object@ROI$rtMax[1])){
          valid <- FALSE
          msg   <- c(msg, paste("ROI$rtMax should be numeric, not ", typeof(object@ROI$rtMax[1]), sep=""))
        }
        if (!is.numeric(object@ROI$mzMin[1])){
          valid <- FALSE
          msg   <- c(msg, paste("ROI$mzMin should be numeric, not ", typeof(object@ROI$mzMin[1]), sep=""))
        }
        if (!is.numeric(object@ROI$mz[1])){
          valid <- FALSE
          msg   <- c(msg, paste("ROI$mz should be numeric, not ", typeof(object@ROI$mz[1]), sep=""))
        }
        if (!is.numeric(object@ROI$mzMax[1])){
          valid <- FALSE
          msg   <- c(msg, paste("ROI$mzMax should be numeric, not ", typeof(object@ROI$mzMax[1]), sep=""))
        }
      }
    }
  }

  # FIR number of rows
  if (dim(object@FIR)[1] != nbCpd) {
    valid <- FALSE
    msg   <- c(msg, paste("FIR has ", dim(object@FIR)[1], " rows (compound). Should be ", nbCpd, sep=""))
  }
  # FIR number of columns
  if (dim(object@FIR)[2] != 4) {
    valid <- FALSE
    msg   <- c(msg, paste("FIR has ", dim(object@FIR)[2], " columns. Should be 4 (\"rtMin\", \"rtMax\", \"mzMin\", \"mzMax\")", sep=""))
  } else {
    # FIR column names
    if (!all(colnames(object@FIR) %in% c("rtMin", "rtMax", "mzMin", "mzMax"))) {
      valid <- FALSE
      msg   <- c(msg, paste("FIR columns should be \"rtMin\", \"rtMax\", \"mzMin\", \"mzMax\", not ", paste(colnames(object@FIR), collapse=" "), sep=""))
    } else {
      # FIR column type
      if (nbCpd >= 1) {
        if (!is.numeric(object@FIR$rtMin[1])){
          valid <- FALSE
          msg   <- c(msg, paste("FIR$rtMin should be numeric, not ", typeof(object@FIR$rtMin[1]), sep=""))
        }
        if (!is.numeric(object@FIR$rtMax[1])){
          valid <- FALSE
          msg   <- c(msg, paste("FIR$rtMax should be numeric, not ", typeof(object@FIR$rtMax[1]), sep=""))
        }
        if (!is.numeric(object@FIR$mzMin[1])){
          valid <- FALSE
          msg   <- c(msg, paste("FIR$mzMin should be numeric, not ", typeof(object@FIR$mzMin[1]), sep=""))
        }
        if (!is.numeric(object@FIR$mzMax[1])){
          valid <- FALSE
          msg   <- c(msg, paste("FIR$mzMax should be numeric, not ", typeof(object@FIR$mzMax[1]), sep=""))
        }
      }
    }
  }

  # uROI number of rows
  if (dim(object@uROI)[1] != nbCpd) {
    valid <- FALSE
    msg   <- c(msg, paste("uROI has ", dim(object@uROI)[1], " rows (compound). Should be ", nbCpd, sep=""))
  }
  # uROI number of columns
  if (dim(object@uROI)[2] != 6) {
    valid <- FALSE
    msg   <- c(msg, paste("uROI has ", dim(object@uROI)[2], " columns. Should be 6 (\"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\")", sep=""))
  } else {
    # uROI column names
    if (!all(colnames(object@uROI) %in% c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))) {
      valid <- FALSE
      msg   <- c(msg, paste("uROI columns should be \"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\", \"mzMax\", not ", paste(colnames(object@uROI), collapse=" "), sep=""))
    } else {
      # uROI column type
      if (nbCpd >= 1) {
        if (!is.numeric(object@uROI$rtMin[1])){
          valid <- FALSE
          msg   <- c(msg, paste("uROI$rtMin should be numeric, not ", typeof(object@uROI$rtMin[1]), sep=""))
        }
        if (!is.numeric(object@uROI$rt[1])){
          valid <- FALSE
          msg   <- c(msg, paste("uROI$rt should be numeric, not ", typeof(object@uROI$rt[1]), sep=""))
        }
        if (!is.numeric(object@uROI$rtMax[1])){
          valid <- FALSE
          msg   <- c(msg, paste("uROI$rtMax should be numeric, not ", typeof(object@uROI$rtMax[1]), sep=""))
        }
        if (!is.numeric(object@uROI$mzMin[1])){
          valid <- FALSE
          msg   <- c(msg, paste("uROI$mzMin should be numeric, not ", typeof(object@uROI$mzMin[1]), sep=""))
        }
        if (!is.numeric(object@uROI$mz[1])){
          valid <- FALSE
          msg   <- c(msg, paste("uROI$mz should be numeric, not ", typeof(object@uROI$mz[1]), sep=""))
        }
        if (!is.numeric(object@uROI$mzMax[1])){
          valid <- FALSE
          msg   <- c(msg, paste("uROI$mzMax should be numeric, not ", typeof(object@uROI$mzMax[1]), sep=""))
        }
      }
    }
  }

  # number of acquisitionTime
  if (length(object@acquisitionTime) != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("acquisitionTime has ", length(object@acquisitionTime), " elements (samples). Should be ", nbSample, sep=""))
  }

  # cannot useUROI if uROIExist=FALSE
  if (object@useUROI & !(object@uROIExist)) {
    valid <- FALSE
    msg   <- c(msg, paste("useUROI cannot be TRUE while uROIExist is FALSE", sep=""))
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
  } else {
    # only check peakTables if min 1 sample and not NULL
    if (nbSample >= 1){
      # if ALL peakTables are not NULL
      peakTables_isNULL <- sapply(object@peakTables, is.null)
      if (!all(peakTables_isNULL)) {
        # if one peakTable is nuLL but not all, raise an error
        if (any(peakTables_isNULL)) {
          valid <- FALSE
          msg   <- c(msg, paste("peakTables must all either be data.frame or NULL", sep=""))
        } else {
          # individual peakTable is data.frame
          if (!is.data.frame(object@peakTables[[1]])) {
            valid <- FALSE
            msg   <- c(msg, paste("peakTables must be data.frame or NULL not ", typeof(object@peakTables[[1]]), sep=""))
          } else {
            # individual peakTable data.frame number of rows
            if (dim(object@peakTables[[1]])[1] != nbCpd) {
              valid <- FALSE
              msg   <- c(msg, paste("peakTables[[1]] has ", dim(object@peakTables[[1]])[1], " rows (compounds). Should be ", nbCpd, sep=""))
            }
            # individual peakTable data.frame number of columns
            if (dim(object@peakTables[[1]])[2] != 31) {
              valid <- FALSE
              msg   <- c(msg, paste("peakTables[[1]] has ", dim(object@peakTables[[1]])[2], " columns. Should be 31", sep=""))
            } else {
              # individual peakTable data.frame column names
              if (!all(colnames(object@peakTables[[1]]) %in% c('found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor'))) {
                valid <- FALSE
                msg   <- c(msg, paste("peakTables[[1]] columns should be 'found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor', not ", paste(colnames(object@peakTables[[1]]), collapse=" "), sep=""))
              }
            }
          }
        }
      }
    }
  }

  # number of EICs
  if (length(object@EICs) != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("EICs has ", length(object@EICs), " elements (samples). Should be ", nbSample, sep=""))
  } else {
    # only check EICs if min 1 sample and not NULL
    if (nbSample >= 1){
      # if ALL EICs are not NULL
      EICs_isNULL <- sapply(object@EICs, is.null)
      if (!all(EICs_isNULL)) {
        # if one EIC is nuLL but not all, raise an error
        if (any(EICs_isNULL)) {
          valid <- FALSE
          msg   <- c(msg, paste("EICs must all either be MSnbase::Chromatograms/list or NULL", sep=""))
        } else {
          # individual EIC is list or chromatograms
          if (!(is.list(object@EICs[[1]]) | (class(object@EICs[[1]])=="Chromatograms"))) {
            valid <- FALSE
            msg   <- c(msg, paste("EICs[[1]] must be a list or MSnbase::Chromatograms, not ", paste(class(object@EICs[[1]]), collapse=" "), sep=""))
          } else {
            # individual EIC has entry for each compound
            if (length(object@EICs[[1]]) != nbCpd) {
              valid <- FALSE
              msg   <- c(msg, paste("EICs[[1]] contains, ", length(object@EICs[[1]]), " EICs (compound). Should be ", nbCpd, sep=""))
            } else {
              if (nbCpd >= 1) {
                # individual EIC compound entry is Chromatogram
                if (class(object@EICs[[1]][[1]]) != "Chromatogram") {
                  valid <- FALSE
                  msg   <- c(msg, paste("EICs[[1]][[1]] must be a MSnbase::Chromatogram, not ", class(object@EICs[[1]][[1]]), sep=""))
                }
              }
            }
          }
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
