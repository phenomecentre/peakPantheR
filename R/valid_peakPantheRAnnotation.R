## valid method for \link{peakPantheRAnnotation-class}
## Number of compounds based on @cpdID length, number of samples based on @filepath length.
## Slot type is not checked as \code{setClass} enforces it.
## peakTables, peakFit and dataPoints type are checked on first list element.
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
  
  # number of compounds (rows) in cpdMetadata
  if (dim(object@cpdMetadata)[1] != nbCpd) {
    valid <- FALSE
    msg   <- c(msg, paste("cpdMetadata has ", dim(object@cpdMetadata)[1], " rows (compounds). Should be ", nbCpd, sep=""))
  }
  
  # number of spectra (rows) in spectraMetadata
  if (dim(object@spectraMetadata)[1] != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("spectraMetadata has ", dim(object@spectraMetadata)[1], " rows (spectra). Should be ", nbSample, sep=""))
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
      peakTables_isNULL <- vapply(object@peakTables, is.null, FUN.VALUE=logical(1))
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
            if (dim(object@peakTables[[1]])[2] != 15) {
              valid <- FALSE
              msg   <- c(msg, paste("peakTables[[1]] has ", dim(object@peakTables[[1]])[2], " columns. Should be 15", sep=""))
            } else {
              # individual peakTable data.frame column names
              if (!all(colnames(object@peakTables[[1]]) %in% c('found', 'rt', 'rtMin', 'rtMax', 'mz', 'mzMin', 'mzMax', 'peakArea', 'maxIntMeasured', 'maxIntPredicted', 'is_filled', 'ppm_error', 'rt_dev_sec', 'tailingFactor', 'asymmetryFactor'))) {
                valid <- FALSE
                msg   <- c(msg, paste("peakTables[[1]] columns should be 'found', 'rt', 'rtMin', 'rtMax', 'mz', 'mzMin', 'mzMax', 'peakArea', 'maxIntMeasured', 'maxIntPredicted', 'is_filled', 'ppm_error', 'rt_dev_sec', 'tailingFactor', 'asymmetryFactor', not ", paste(colnames(object@peakTables[[1]]), collapse=" "), sep=""))
              }
            }
          }
        }
      }
    }
  }

  # number of dataPoints
  if (length(object@dataPoints) != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("dataPoints has ", length(object@dataPoints), " elements (samples). Should be ", nbSample, sep=""))
  } else {
    # only check dataPoints if min 1 sample and not NULL
    if (nbSample >= 1){
      # if ALL dataPoints are not NULL
      dataPoints_isNULL <- vapply(object@dataPoints, is.null, FUN.VALUE=logical(1))
      if (!all(dataPoints_isNULL)) {
        # if one dataPoints is NULL but not all, raise an error
        if (any(dataPoints_isNULL)) {
          valid <- FALSE
          msg   <- c(msg, paste("dataPoints must all either be list of ROI data points or NULL", sep=""))
        } else {
          # individual dataPoints is list
          if (!(is.list(object@dataPoints[[1]]))) {
            valid <- FALSE
            msg   <- c(msg, paste("dataPoints[[1]] must be a list of ROI data points, not ", paste(class(object@dataPoints[[1]]), collapse=" "), sep=""))
          } else {
            # individual dataPoints has entry for each compound (ROI)
            if (length(object@dataPoints[[1]]) != nbCpd) {
              valid <- FALSE
              msg   <- c(msg, paste("dataPoints[[1]] contains, ", length(object@dataPoints[[1]]), " dataPoints (compound). Should be ", nbCpd, sep=""))
            } else {
              if (nbCpd >= 1) {
                # individual dataPoints compound entry is data.frame
                if (!is.data.frame(object@dataPoints[[1]][[1]])) {
                  valid <- FALSE
                  msg   <- c(msg, paste("dataPoints[[1]][[1]] must be a data.frame, not ", class(object@dataPoints[[1]][[1]]), sep=""))
                } else {
                  # individual peakTable data.frame number of columns
                  if (dim(object@dataPoints[[1]][[1]])[2] != 3) {
                    valid <- FALSE
                    msg   <- c(msg, paste("dataPoints[[1]][[1]] has ", dim(object@dataPoints[[1]][[1]])[2], " columns. Should be 3", sep=""))
                  } else {
                    # individual peakTable data.frame column names
                    if (!all(colnames(object@dataPoints[[1]][[1]]) %in% c('rt', 'mz', 'int'))) {
                      valid <- FALSE
                      msg   <- c(msg, paste("dataPoints[[1]][[1]] columns should be 'rt', 'mz', 'int', not ", paste(colnames(object@dataPoints[[1]][[1]]), collapse=" "), sep=""))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  # number of peakFit
  if (length(object@peakFit) != nbSample) {
    valid <- FALSE
    msg   <- c(msg, paste("peakFit has ", length(object@peakFit), " elements (samples). Should be ", nbSample, sep=""))
  } else {
    # only check peakFit if min 1 sample and not NULL
    if (nbSample >= 1){
      # if ALL peakFit are not NULL
      peakFit_isNULL <- vapply(object@peakFit, is.null, FUN.VALUE=logical(1))
      if (!all(peakFit_isNULL)) {
        # if one peakFit is NULL but not all, raise an error
        if (any(peakFit_isNULL)) {
          valid <- FALSE
          msg   <- c(msg, paste("peakFit must all either be list of ROI curveFit or NULL", sep=""))
        } else {
          # individual peakFit is list
          if (!(is.list(object@peakFit[[1]]))) {
            valid <- FALSE
            msg   <- c(msg, paste("peakFit[[1]] must be a list of ROI curveFit or NA, not ", paste(class(object@peakFit[[1]]), collapse=" "), sep=""))
          } else {
            # individual peakFit has entry for each compound (ROI)
            if (length(object@peakFit[[1]]) != nbCpd) {
              valid <- FALSE
              msg   <- c(msg, paste("peakFit[[1]] contains, ", length(object@peakFit[[1]]), " peakPantheR_curveFit or NA (compound). Should be ", nbCpd, sep=""))
            } else {
              # only check peakFit if min 1 compound
              if (nbCpd >= 1) {
                # individual peakFit compound entry is peakPantheR_curveFit or NA
                if (!all(is.na(object@peakFit[[1]][[1]])) & !is.peakPantheR_curveFit(object@peakFit[[1]][[1]])) {
                  valid <- FALSE
                  msg   <- c(msg, paste("peakFit[[1]][[1]] must be NA or a peakPantheR_curveFit, not ", class(object@peakFit[[1]][[1]]), sep=""))
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
