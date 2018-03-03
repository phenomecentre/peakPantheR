context('peakPantheRAnnotation_validObject-method()')

## Test the validObject method / valid_peakPantheRAnnotation function

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

## Input data
# spectraPaths
input_spectraPaths  <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko18.CDF', package = "faahKO"))

# targetFeatTable
input_targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
input_targetFeatTable[1,] <- c(1, "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] <- c(2, "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_targetFeatTable[,c(1,3:8)] <- sapply(input_targetFeatTable[,c(1,3:8)], as.numeric)

# TICs
input_TIC <- c(2410533091, 2524040155, 2332817115)

# acquisitionTime
input_acquisitionTime <- c(as.character(Sys.time()), as.character(Sys.time()+900), as.character(Sys.time()+1800))

# peakTables
# 1
peakTable1      <- data.frame(matrix(vector(), 2, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable1[1,]  <- c(TRUE, 522.2, 522.2, 522.2, 3346.453, 3322.979, 3379.317, 25792525, 25768308, 889280, 1840, 0.05400866, 541.2220, 7.464513, 897391.7, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.02337616, 1.565, 27.50892, 11, NA, 1.296709)
peakTable1[2,]  <- c(TRUE, 496.2, 496.2, 496.2, 3385.577, 3362.102, 3409.051, 32873727, 32818664, 1128960, 1471, 0.07199871, 566.3076, 7.788152, 1133465.7, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.02460103, 0., 28.70062, 11, NA, 2.134513)
peakTable1[,1]  <- sapply(peakTable1[,1], as.logical)
# 2
peakTable2      <- data.frame(matrix(vector(), 2, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable2[1,]  <- c(TRUE, 522.2, 522.2, 522.2, 3365.232, 3340.193, 3394.966, 24052220, 24015094, 761664, 577, 0.05649760, 553.1203, 8.079863, 783682.1, 1, 0, 5, 551, 546, 556, 35, 70, 1, 0, 0.02337616, 20.344, 29.77567, 11, NA, NA)
peakTable2[2,]  <- c(TRUE, 496.2, 496.2, 496.2, 3407.486, 3382.447, 3432.525, 34958375, 34866657, 1099264, 598, 0.08422028, 579.6167, 8.534918, 1104863.3, 2, 0, 5, 577, 572, 582, 81, 113, 1, 0, 0.02460103, 21.909, 31.45268, 11, NA, NA)
peakTable2[,1]  <- sapply(peakTable2[,1], as.logical)
# 3
peakTable3      <- data.frame(matrix(vector(), 2, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable3[1,]  <- c(TRUE, 522.2, 522.2, 522.2, 3368.363, 3346.454, 3396.533, 21421981, 21114393, 758336, 4527, 0.05799548, 555.4057, 7.455757, 751440.4, 1, 0, 5, 554, 549, 559, 39, 71, 1, 0, 0.02337616, 23.475, 27.47565, 11, NA, NA)
peakTable3[2,]  <- c(TRUE, 496.2, 496.2, 496.2, 3413.747, 3390.273, 3437.221, 33874835, 33445046, 1149440, 246, 0.07273868, 583.7579, 7.956053, 1144862.9, 2, 0, 5, 582, 577, 587, 86, 116, 1, 0, 0.02460103, 28.170, 29.31939, 11, NA, NA)
peakTable3[,1]  <- sapply(peakTable3[,1], as.logical)

# EICs
# 1
file1  <- MSnbase::readMSData(input_spectraPaths[1], centroided=TRUE, mode='onDisk')
EIC1	 <- xcms::chromatogram(file1, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# 2
file2  <- MSnbase::readMSData(input_spectraPaths[2], centroided=TRUE, mode='onDisk')
EIC2	 <- xcms::chromatogram(file2, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# 3
file3  <- MSnbase::readMSData(input_spectraPaths[3], centroided=TRUE, mode='onDisk')
EIC3	 <- xcms::chromatogram(file3, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# single compound
tmp_EIC <- xcms::chromatogram(file1, rt = c(rt_lower=input_targetFeatTable$rtMin[1], rt_upper=input_targetFeatTable$rtMax[1]), mz = c(mz_lower=input_targetFeatTable$mzMin[1], mz_upper=input_targetFeatTable$mzMax[1]))

# Object, no samples, no compounds
defaultInit_empty       <- peakPantheRAnnotation()
# Object, init samples and compounds
defaultInit_cpd_spectra <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)
# Object, fully filled
filledAnnotation        <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, acquisitionTime=input_acquisitionTime, TIC=input_TIC, peakTables=list(peakTable1, peakTable2, peakTable3), EICs=list(EIC1, EIC2, EIC3), isAnnotated=TRUE)


test_that('initialised objects are valid', {
  # no samples, no compounds
  expect_true(validObject(defaultInit_empty))
  # init samples and compounds
  expect_true(validObject(defaultInit_cpd_spectra))
})

test_that('a fully filled object is valid', {
  # fully filled
  expect_true(validObject(filledAnnotation))
})

test_that('validObject() raises errors', {
  # number of cpdName
  wrong1          <- filledAnnotation
  wrong1@cpdName  <- c("Cpd 1", "Cpd 2", "Cpd 3")
  msg1            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: cpdName has 3 elements (compound). Should be 2', sep='')
  expect_error(validObject(wrong1), msg1, fixed=TRUE)

  # ROI number of rows
  wrong2          <- filledAnnotation
  wrong2@ROI      <- wrong2@ROI[1,]
  msg2            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI has 1 rows (compound). Should be 2', sep='')
  expect_error(validObject(wrong2), msg2, fixed=TRUE)
  # ROI number of columns
  wrong3          <- filledAnnotation
  wrong3@ROI      <- wrong3@ROI[,1:5]
  msg3            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI has 5 columns. Should be 6 ("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")', sep='')
  expect_error(validObject(wrong3), msg3, fixed=TRUE)
  # ROI column names
  wrong4                <- filledAnnotation
  colnames(wrong4@ROI)  <- c("wrongCol", "rt", "rtMax", "mzMin", "mz", "mzMax")
  msg4            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI columns should be "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", not wrongCol rt rtMax mzMin mz mzMax', sep='')
  expect_error(validObject(wrong4), msg4 , fixed=TRUE)
  # ROI$rtMin numeric
  wrong5            <- filledAnnotation
  wrong5@ROI$rtMin  <- c("not numeric", "not numeric")
  msg5              <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI$rtMin should be numeric, not character', sep='')
  expect_error(validObject(wrong5), msg5, fixed=TRUE)
  # ROI$rt numeric
  wrong6            <- filledAnnotation
  wrong6@ROI$rt     <- c("not numeric", "not numeric")
  msg6              <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI$rt should be numeric, not character', sep='')
  expect_error(validObject(wrong6), msg6, fixed=TRUE)
  # ROI$rtMax numeric
  wrong7            <- filledAnnotation
  wrong7@ROI$rtMax  <- c("not numeric", "not numeric")
  msg7              <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI$rtMax should be numeric, not character', sep='')
  expect_error(validObject(wrong7), msg7, fixed=TRUE)
  # ROI$mzMin numeric
  wrong8            <- filledAnnotation
  wrong8@ROI$mzMin  <- c("not numeric", "not numeric")
  msg8              <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI$mzMin should be numeric, not character', sep='')
  expect_error(validObject(wrong8), msg8, fixed=TRUE)
  # ROI$mz numeric
  wrong9            <- filledAnnotation
  wrong9@ROI$mz     <- c("not numeric", "not numeric")
  msg9              <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI$mz should be numeric, not character', sep='')
  expect_error(validObject(wrong9), msg9, fixed=TRUE)
  # ROI$mzMax numeric
  wrong10            <- filledAnnotation
  wrong10@ROI$mzMax  <- c("not numeric", "not numeric")
  msg10              <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: ROI$mzMax should be numeric, not character', sep='')
  expect_error(validObject(wrong10), msg10, fixed=TRUE)

  # FIR number of rows
  wrong11          <- filledAnnotation
  wrong11@FIR      <- wrong11@FIR[1,]
  msg11            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: FIR has 1 rows (compound). Should be 2', sep='')
  expect_error(validObject(wrong11), msg11, fixed=TRUE)
  # FIR number of columns
  wrong12          <- filledAnnotation
  wrong12@FIR      <- wrong12@FIR[,1:3]
  msg12            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: FIR has 3 columns. Should be 4 ("rtMin", "rtMax", "mzMin", "mzMax")', sep='')
  expect_error(validObject(wrong12), msg12, fixed=TRUE)
  # FIR column names
  wrong13                 <- filledAnnotation
  colnames(wrong13@FIR)   <- c("wrongCol", "rtMax", "mzMin", "mzMax")
  msg13                   <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: FIR columns should be "rtMin", "rtMax", "mzMin", "mzMax", not wrongCol rtMax mzMin mzMax', sep='')
  expect_error(validObject(wrong13), msg13, fixed=TRUE)
  # FIR$rtMin numeric
  wrong14             <- filledAnnotation
  wrong14@FIR$rtMin   <- c("not numeric", "not numeric")
  msg14               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: FIR$rtMin should be numeric, not character', sep='')
  expect_error(validObject(wrong14), msg14, fixed=TRUE)
  # FIR$rtMax numeric
  wrong15             <- filledAnnotation
  wrong15@FIR$rtMax   <- c("not numeric", "not numeric")
  msg15               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: FIR$rtMax should be numeric, not character', sep='')
  expect_error(validObject(wrong15), msg15, fixed=TRUE)
  # FIR$mzMin numeric
  wrong16             <- filledAnnotation
  wrong16@FIR$mzMin   <- c("not numeric", "not numeric")
  msg16               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: FIR$mzMin should be numeric, not character', sep='')
  expect_error(validObject(wrong16), msg16, fixed=TRUE)
  # FIR$mzMax numeric
  wrong17             <- filledAnnotation
  wrong17@FIR$mzMax   <- c("not numeric", "not numeric")
  msg17               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: FIR$mzMax should be numeric, not character', sep='')
  expect_error(validObject(wrong17), msg17, fixed=TRUE)

  # uROI number of rows
  wrong18           <- filledAnnotation
  wrong18@uROI      <- wrong18@uROI[1,]
  msg18             <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI has 1 rows (compound). Should be 2', sep='')
  expect_error(validObject(wrong18), msg18, fixed=TRUE)
  # uROI number of columns
  wrong19           <- filledAnnotation
  wrong19@uROI      <- wrong19@uROI[,1:5]
  msg19             <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI has 5 columns. Should be 6 ("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")', sep='')
  expect_error(validObject(wrong19), msg19, fixed=TRUE)
  # uROI column names
  wrong20                 <- filledAnnotation
  colnames(wrong20@uROI)  <- c("wrongCol", "rt", "rtMax", "mzMin", "mz", "mzMax")
  msg20                   <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI columns should be "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", not wrongCol rt rtMax mzMin mz mzMax', sep='')
  expect_error(validObject(wrong20), msg20, fixed=TRUE)
  # uROI$rtMin numeric
  wrong21             <- filledAnnotation
  wrong21@uROI$rtMin  <- c("not numeric", "not numeric")
  msg21               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI$rtMin should be numeric, not character', sep='')
  expect_error(validObject(wrong21), msg21, fixed=TRUE)
  # uROI$rt numeric
  wrong22             <- filledAnnotation
  wrong22@uROI$rt     <- c("not numeric", "not numeric")
  msg22               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI$rt should be numeric, not character', sep='')
  expect_error(validObject(wrong22), msg22, fixed=TRUE)
  # uROI$rtMax numeric
  wrong23             <- filledAnnotation
  wrong23@uROI$rtMax  <- c("not numeric", "not numeric")
  msg23               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI$rtMax should be numeric, not character', sep='')
  expect_error(validObject(wrong23), msg23, fixed=TRUE)
  # uROI$mzMin numeric
  wrong24             <- filledAnnotation
  wrong24@uROI$mzMin  <- c("not numeric", "not numeric")
  msg24               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI$mzMin should be numeric, not character', sep='')
  expect_error(validObject(wrong24), msg24, fixed=TRUE)
  # uROI$mz numeric
  wrong25             <- filledAnnotation
  wrong25@uROI$mz     <- c("not numeric", "not numeric")
  msg25               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI$mz should be numeric, not character', sep='')
  expect_error(validObject(wrong25), msg25, fixed=TRUE)
  # uROI$mzMax numeric
  wrong26             <- filledAnnotation
  wrong26@uROI$mzMax  <- c("not numeric", "not numeric")
  msg26               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: uROI$mzMax should be numeric, not character', sep='')
  expect_error(validObject(wrong26), msg26, fixed=TRUE)

  # number of TIC
  wrong27       <- filledAnnotation
  wrong27@TIC   <- c(1, 2)
  msg27         <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: TIC has 2 elements (samples). Should be 3', sep='')
  expect_error(validObject(wrong27), msg27, fixed=TRUE)

  # number of peakTables
  wrong28             <- filledAnnotation
  wrong28@peakTables  <- wrong28@peakTables[1:2]
  msg28               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: peakTables has 2 elements (samples). Should be 3', sep='')
  expect_error(validObject(wrong28), msg28, fixed=TRUE)
  # not all peakTables are NULL or initialised
  wrong29             <- filledAnnotation
  wrong29@peakTables  <- list(NULL, wrong29@peakTables[[2]], wrong29@peakTables[[3]])
  msg29               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: peakTables must all either be data.frame or NULL', sep='')
  expect_error(validObject(wrong29), msg29, fixed=TRUE)
  # peakTables is data.frame
  wrong30             <- filledAnnotation
  wrong30@peakTables  <- list("not data.frame", "not data.frame", "not data.frame")
  msg30               <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: peakTables must be data.frame or NULL not character', sep='')
  expect_error(validObject(wrong30), msg30, fixed=TRUE)
  # peakTables data.frame number of rows
  wrong31                 <- filledAnnotation
  wrong31@peakTables[[1]] <- wrong31@peakTables[[1]][1,]
  msg31                   <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: peakTables[[1]] has 1 rows (compounds). Should be 2', sep='')
  expect_error(validObject(wrong31), msg31, fixed=TRUE)
  # peakTables data.frame number of columns
  wrong32                 <- filledAnnotation
  wrong32@peakTables[[1]] <- wrong32@peakTables[[1]][,1:2]
  msg32                   <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: peakTables[[1]] has 2 columns. Should be 31', sep='')
  expect_error(validObject(wrong32), msg32, fixed=TRUE)
  # peakTables column names
  wrong33                           <- filledAnnotation
  colnames(wrong33@peakTables[[1]]) <- c('wrongCol', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor')
  msg33                             <- paste("invalid class ", dQuote('peakPantheRAnnotation')," object: peakTables[[1]] columns should be 'found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor', not wrongCol mz mzmin mzmax rt rtmin rtmax into intb maxo sn egauss mu sigma h f dppm scale scpos scmin scmax lmin lmax sample is_filled ppm_error rt_dev_sec FWHM FWHM_ndatapoints tailingFactor asymmetryFactor", sep='')
  expect_error(validObject(wrong33), msg33, fixed=TRUE)

  # number of EIC
  wrong34       <- filledAnnotation
  wrong34@EICs  <- wrong34@EICs[1:2]
  msg34         <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: EICs has 2 elements (samples). Should be 3', sep='')
  expect_error(validObject(wrong34), msg34, fixed=TRUE)
  # not all peakTables are NULL or initialised
  wrong35       <- filledAnnotation
  wrong35@EICs  <- list(NULL, wrong35@EICs[[2]], wrong35@EICs[[3]])
  msg35         <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: EICs must all either be MSnbase::Chromatograms/list or NULL', sep='')
  expect_error(validObject(wrong35), msg35, fixed=TRUE)
  # individual EIC is list or Chromatograms
  wrong36       <- filledAnnotation
  wrong36@EICs  <- list("not list or Chromatograms", "not list or Chromatograms", "not list or Chromatograms")
  msg36         <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: EICs[[1]] must be a list or MSnbase::Chromatograms, not character', sep='')
  expect_error(validObject(wrong36), msg36, fixed=TRUE)
  # fail if individual EIC is Chromatogram (no S)
  wrong37       <- filledAnnotation
  wrong37@EICs  <- list(wrong37@EICs[[1]][1,],  wrong37@EICs[[2]][1,],  wrong37@EICs[[3]][1,])
  msg37         <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: EICs[[1]] must be a list or MSnbase::Chromatograms, not Chromatogram', sep='')
  expect_error(validObject(wrong37), msg37, fixed=TRUE)
  # individual EIC has entry for each compound
  wrong38           <- filledAnnotation
  wrong38@EICs[[1]] <- tmp_EIC
  msg38             <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: EICs[[1]] contains, 1 EICs (compound). Should be 2', sep='')
  expect_error(validObject(wrong38), msg38, fixed=TRUE)
  # individual EIC compound entry is chromatogram
  wrong39                 <- filledAnnotation
  wrong39@EICs[[1]][[1]]  <- "not a chromatogram"
  msg39                   <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: EICs[[1]][[1]] must be a MSnbase::Chromatogram, not character', sep='')
  expect_error(validObject(wrong39), msg39, fixed=TRUE)

  # number of acquisitionTime
  wrong40                 <- filledAnnotation
  wrong40@acquisitionTime <- filledAnnotation@acquisitionTime[c(1,2)]
  msg40                   <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: acquisitionTime has 2 elements (samples). Should be 3', sep='')
  expect_error(validObject(wrong40), msg40, fixed=TRUE)

  # cannot useUROI=TRUE if uROIExist=FALSE
  wrong41           <- filledAnnotation
  wrong41@useUROI   <- TRUE
  wrong41@uROIExist <- FALSE
  msg41             <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: useUROI cannot be TRUE while uROIExist is FALSE', sep='')
  expect_error(validObject(wrong41), msg41, fixed=TRUE)
})
