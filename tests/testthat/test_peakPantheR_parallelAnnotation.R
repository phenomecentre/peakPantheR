context('peakPantheR_parallelAnnotation()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input and expected data
# 3 files
input_spectraPaths          <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                                 system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                                 system.file('cdf/KO/ko18.CDF', package = "faahKO"))
## 1 missing file
input_missingSpectraPaths   <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                                 "aaa/bbb.cdf",
                                 system.file('cdf/KO/ko18.CDF', package = "faahKO"))

# 4 features
input_targetFeatTable     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=F)
input_targetFeatTable[1,] 	<- c(1, "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] 	<- c(2, "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_targetFeatTable[3,] 	<- c(3, "Cpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
input_targetFeatTable[4,] 	<- c(4, "Cpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
input_targetFeatTable[,c(1,3:8)] <- sapply(input_targetFeatTable[,c(1,3:8)], as.numeric)

# FIR
input_FIR     	            <- data.frame(matrix(vector(), 4, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))),stringsAsFactors=F)
input_FIR[1,] 	            <- c(3336.542, 3390.272, 522.1995, 522.2005)
input_FIR[2,] 	            <- c(3378.274, 3426.266, 496.1995, 496.2005)
input_FIR[3,] 	            <- c(3444.524, 3478.431, 464.1995, 464.2005)
input_FIR[4,] 	            <- c(3689.7,   3738.213, 536.1995, 536.2005)
input_FIR[,c(1:4)]          <- sapply(input_FIR[,c(1:4)], as.numeric)

# uROI (ROI are wrong, uROI are right)
input_uROI                  <- input_targetFeatTable[,c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
input_badtargetFeatTable    <- input_targetFeatTable
input_badtargetFeatTable[1, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(0,10000, 0,1000)
input_badtargetFeatTable[2, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(0,10000, 0,1000)
input_badtargetFeatTable[3, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(0,10000, 0,1000)
input_badtargetFeatTable[4, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(0,10000, 0,1000)


# Expected EICs
# 1
raw_data1 <- MSnbase::readMSData(input_spectraPaths[1], centroided=TRUE, mode='onDisk')
EICs1	    <- xcms::chromatogram(raw_data1, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# 2
raw_data2 <- MSnbase::readMSData(input_spectraPaths[2], centroided=TRUE, mode='onDisk')
EICs2	    <- xcms::chromatogram(raw_data2, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# 3
raw_data3 <- MSnbase::readMSData(input_spectraPaths[3], centroided=TRUE, mode='onDisk')
EICs3	    <- xcms::chromatogram(raw_data3, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))

# Expected peakTables noFitGauss
# 1
peakTable_noFitGauss1      <- data.frame(matrix(vector(), 4, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable_noFitGauss1[1,]  <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3344.888, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, NA, NA, NA, NA, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.023376160866574614, 0.00, NA, 11, NA, 1.4839997536872727)
peakTable_noFitGauss1[2,]  <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3382.447, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, NA, NA, NA, NA, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.024601030353423384, 3.13, NA, 11, NA, 2.7082909206059269)
peakTable_noFitGauss1[3,]  <- c(TRUE, 464.2000122, 464.2000122, 464.2000122, 3454.435, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, NA, NA, NA, NA, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0, 0.026296922148575364, 0.00, NA, 11, NA, 1.5506880253870328)
peakTable_noFitGauss1[4,]  <- c(TRUE, 536.2000122, 536.2000122, 536.2000122, 3701.697, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, NA, NA, NA, NA, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0, 0.022765817240815486, 0.00, NA, 11, 1.4026668257511667, 1.7784490549015182)
peakTable_noFitGauss1[,c(1,25)]  <- sapply(peakTable_noFitGauss1[,c(1,25)], as.logical)
# 2
peakTable_noFitGauss2      <- data.frame(matrix(vector(), 4, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable_noFitGauss2[1,]  <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3362.102, 3340.193, 3394.966, 24052220.229, 24015093.525, 761664, 577, NA, NA, NA, NA, 1, 0, 5, 551, 546, 556, 35, 70, 1, FALSE, 0.023376160866574614, 17.214, NA, 11, NA, NA)
peakTable_noFitGauss2[2,]  <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3402.791, 3382.447, 3432.525, 34958375.123, 34866657.266, 1099264, 598, NA, NA, NA, NA, 2, 0, 5, 577, 572, 582, 81, 113, 1, FALSE, 0.024601030353423384, 17.214, NA, 11, NA, NA)
peakTable_noFitGauss2[3,]  <- c(TRUE, 464.2000122, 464.2000122, 464.2000122, 3463.824, 3441.915, 3491.993, 10978975.525, 10873063.5007, 366720, 359, NA, NA, NA, NA, 3, 0, 5, 616, 611, 621, 30, 62, 1, FALSE, 0.026296922148575364, 9.389, NA, 11, NA, NA)
peakTable_noFitGauss2[4,]  <- c(TRUE, 536.2000122, 536.2000122, 536.2000122, 3717.347, 3695.437, 3748.646, 7707921.46876, 7653403.1100, 220096, 224, NA, NA, NA, NA, 4, 0, 5, 778, 773, 783, 32, 66, 1, FALSE, 0.022765817240815486, 15.650, NA, 11, NA, NA)
peakTable_noFitGauss2[,c(1,25)]   <- sapply(peakTable_noFitGauss2[,c(1,25)], as.logical)
peakTable_noFitGauss2[,c(30,31)]  <- sapply(peakTable_noFitGauss2[,c(30,31)], as.logical)
# 3
peakTable_noFitGauss3      <- data.frame(matrix(vector(), 4, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable_noFitGauss3[1,]  <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3366.798, 3346.454, 3396.533, 21421980.83625, 21114392.8013, 758336, 4527, NA, NA, NA, NA, 1, 0e+00, 5, 554, 549, 559, 39, 71, 1, FALSE, 0.023376160866574614, 21.910, NA, 11, NA, NA)
peakTable_noFitGauss3[2,]  <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3410.617, 3390.273, 3437.221, 33874834.8810, 33445045.86788, 1149440, 246, NA, NA, NA, NA, 2, 0e+00, 5, 582, 577, 587, 86, 116, 1, FALSE, 0.024601030353423384, 25.040, NA, 11, NA, NA)
peakTable_noFitGauss3[3,]  <- c(TRUE, 464.2000122, 0.000000000, 464.2000122, 3462.261, 3459.131, 3463.826, 981768.320, 981765.1900, 319488, 319487, NA, NA, NA, NA, 3, 1e+06, 1, 615, 614, 616, 41, 44, 1, FALSE, 0.026296922148575364, 7.826, NA, 3, NA, NA)
peakTable_noFitGauss3[4,]  <- c(TRUE, 536.2000122, 536.2000122, 536.2000122, 3711.088, 3690.744, 3736.127, 5800147.711, 5741251.53169, 196160, 98, NA, NA, NA, NA, 4, 0e+00, 5, 774, 769, 779, 29, 58, 1, FALSE, 0.022765817240815486, 9.391, NA, 11, NA, NA)
peakTable_noFitGauss3[,c(1,25)]   <- sapply(peakTable_noFitGauss3[,c(1,25)], as.logical)
peakTable_noFitGauss3[,c(30,31)]  <- sapply(peakTable_noFitGauss3[,c(30,31)], as.logical)



test_that('3 files, 4 compounds, no uROI, no FIR, no fitGauss, no getAcquTime, no verbose', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)

  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  expected_annotation@peakTables  <- list(peakTable_noFitGauss1, peakTable_noFitGauss2, peakTable_noFitGauss3)
  expected_annotation@EICs        <- list(EICs1, EICs2, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 9 found.\n")

	# results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=FALSE, getAcquTime=FALSE, verbose=FALSE))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  expect_equal(length(result_parallelAnnotation$messages), 9)
  expect_equal(result_parallelAnnotation$messages, expected_message)
})

test_that('3 files (1 missing), 4 compounds, no uROI, no FIR, no fitGauss, no getAcquTime, no verbose', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_targetFeatTable)

  # Expected annotation
  expected_annotation             <- initAnnotation[c(1,3),]
  expected_annotation@TIC         <- c(2410533091, 2332817115)
  expected_annotation@peakTables  <- list(peakTable_noFitGauss1, peakTable_noFitGauss3)
  expected_annotation@EICs        <- list(EICs1, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
  names(tmp_status)   <- 'aaa/bbb.cdf'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 9 found.\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=FALSE, getAcquTime=FALSE, verbose=FALSE))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  expect_equal(length(result_parallelAnnotation$messages), 6)
  expect_equal(result_parallelAnnotation$messages, expected_message)
})

test_that('3 files, 4 compounds, no uROI, no FIR, no fitGauss, no getAcquTime, no verbose, modify parameter with ..., peaks not found are not replaced', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)

  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  tmp_peakTable1                  <- peakTable_noFitGauss1
  tmp_peakTable1[4,]              <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable1$tailingFactor    <- sapply(tmp_peakTable1$tailingFactor, as.logical)
  tmp_peakTable2                  <- peakTable_noFitGauss2
  tmp_peakTable2[3,]              <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable2[4,]              <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3                  <- peakTable_noFitGauss3
  tmp_peakTable3[2,]              <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[4,]              <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, NA, NA, NA, NA, NA, NA)
  expected_annotation@peakTables  <- list(tmp_peakTable1, tmp_peakTable2, tmp_peakTable3)
  expected_annotation@EICs        <- list(EICs1, EICs2, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 4 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 2 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 6 found.\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=FALSE, getAcquTime=FALSE, verbose=FALSE, snthresh=20))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  expect_equal(length(result_parallelAnnotation$messages), 9)
  expect_equal(result_parallelAnnotation$messages, expected_message)
})

test_that('3 files, 4 compounds, no uROI, FIR replace peaks not found, no fitGauss, no getAcquTime, no verbose', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, useFIR=TRUE, FIR=input_FIR)

  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  # 1
  tmp_peakTable1                  <- peakTable_noFitGauss1
  tmp_peakTable1[4,]              <- c(TRUE, 536.2, 536.1995, 536.2005, 3701.697, 3689.700, 3738.213, 8022477, 8022477, 330176, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable1[,c(1,25,30)]     <- sapply(tmp_peakTable1[,c(1,25,30)], as.logical)
  # 2
  tmp_peakTable2                  <- peakTable_noFitGauss2
  tmp_peakTable2[3,]              <- c(TRUE, 464.2, 464.1995, 464.2005, 3463.824, 3444.524, 3478.431, 8675224, 8675224, 366720, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable2[4,]              <- c(TRUE, 536.2, 536.1995, 536.2005, 3717.347, 3689.700, 3738.213, 6612656, 6612656, 220096, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable2[,c(1,25,30,31)]  <- sapply(tmp_peakTable2[,c(1,25,30,31)], as.logical)
  # 3
  tmp_peakTable3                  <- peakTable_noFitGauss3
  tmp_peakTable3[2,]              <- c(TRUE, 496.2, 496.1995, 496.2005, 3412.182, 3378.274, 3426.266, 27614663.3, 27614663.3, 1149440, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[4,]              <- c(TRUE, 536.2, 536.1995, 536.2005, 3711.088, 3689.700, 3738.213, 5716824.6, 5716824.6, 196160, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[,c(1,25,30,31)]  <- sapply(tmp_peakTable3[,c(1,25,30,31)], as.logical)
  expected_annotation@peakTables  <- list(tmp_peakTable1, tmp_peakTable2, tmp_peakTable3)
  expected_annotation@EICs        <- list(EICs1, EICs2, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 4 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 2 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 6 found.\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=FALSE, getAcquTime=FALSE, verbose=FALSE, snthresh=20))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  expect_equal(length(result_parallelAnnotation$messages), 9)
  expect_equal(result_parallelAnnotation$messages, expected_message)
})

test_that('3 files, 4 compounds, uROI, no FIR, no fitGauss, no getAcquTime, no verbose', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI)

  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  expected_annotation@peakTables  <- list(peakTable_noFitGauss1, peakTable_noFitGauss2, peakTable_noFitGauss3)
  expected_annotation@EICs        <- list(EICs1, EICs2, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 9 found.\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=FALSE, getAcquTime=FALSE, verbose=FALSE))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  expect_equal(length(result_parallelAnnotation$messages), 9)
  expect_equal(result_parallelAnnotation$messages, expected_message)
})

test_that('serial: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found, fitGauss, getAcquTime, verbose', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI, useFIR=TRUE, FIR=input_FIR)

  # Expected annotation
  expected_annotation             <- initAnnotation[c(1,3),]
  expected_annotation@TIC         <- c(2410533091, 2332817115)
  # 1
  tmp_peakTable1                  <- peakTable_noFitGauss1
  tmp_peakTable1$rt               <- c(3346.453, 3385.577, 3456.000, 3701.697)
  tmp_peakTable1$egauss           <- c(0.054008657336897373, 0.071998710997560611, 0.044897312316291224, NA)
  tmp_peakTable1$mu               <- c(541.22204104244724, 566.30756187792758, 610.69456227934620, NA)
  tmp_peakTable1$sigma            <- c(7.4645133175478202, 7.7881518794091047, 7.5047131493649966, NA)
  tmp_peakTable1$h                <- c(897391.73704987112, 1133465.71982824523, 381973.27358806110, NA)
  tmp_peakTable1$rt_dev_sec       <- c(1.565, 0.000, 1.565, NA)
  tmp_peakTable1$FWHM             <- c(27.508921442969950, 28.700624489426446, 27.656069616659352, NA)
  tmp_peakTable1$asymmetryFactor  <- c(1.2967091657127203, 2.1345129619692504, 1.3741191650431608, NA)
  tmp_peakTable1[4,]              <- c(TRUE, 536.2, 536.1995, 536.2005, 3701.697, 3689.700, 3738.213, 8022477, 8022477, 330176, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable1[,c(1,25,30)]     <- sapply(tmp_peakTable1[,c(1,25,30)], as.logical)
  # 3
  tmp_peakTable3                  <- peakTable_noFitGauss3
  tmp_peakTable3[1,]              <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3368.363, 3346.454, 3396.533, 21421980.83624988049, 21114392.80130613223, 758336, 4527, 0.057995477632078921, 555.40571618721754, 7.4557569905883767, 751440.43912957679, 1, 0e+00, 5, 554, 549, 559, 39, 71, 1, FALSE, 0.023376160866574614, 23.474999999999909, 27.475651809276314, 11, NA, NA)
  tmp_peakTable3[2,]              <- c(TRUE, 496.2000122, 496.19949, 496.2005, 3412.182, 3378.274, 3426.266, 27614663.3, 27614663.3, 1149440, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[4,]              <- c(TRUE, 536.2000122, 536.19949, 536.2005, 3711.088, 3689.700, 3738.213, 5716824.6, 5716824.6, 196160, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[,c(1,25,30,31)]  <- sapply(tmp_peakTable3[,c(1,25,30,31)], as.logical)
  expected_annotation@peakTables  <- list(tmp_peakTable1, tmp_peakTable3)
  expected_annotation@EICs        <- list(EICs1, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
  names(tmp_status)   <- 'aaa/bbb.cdf'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Processing 4 compounds in 3 samples:\n", "  uROI:\tTRUE\n", "  FIR:\tTRUE\n", "----- ko15 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 4 found.\n", "Previously loaded EICs used for peak statistics\n", "1 features to integrate with FIR\n", "Check input, mzMLPath must be a .mzML\n", "Error file does not exist: aaa/bbb.cdf\n", "----- ko18 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 6 found.\n", "Previously loaded EICs used for peak statistics\n", "2 features to integrate with FIR\n", "Check input, mzMLPath must be a .mzML\n", "----------------\n", "1 file(s) failed to process:\n          file                                  error\n1 aaa/bbb.cdf Error file does not exist: aaa/bbb.cdf\n", "----------------\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=TRUE, getAcquTime=TRUE, verbose=TRUE, snthresh=20))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  expect_equal(length(result_parallelAnnotation$messages), 32)
  expect_equal(result_parallelAnnotation$messages[c(1:7, 10, 12, 14, 16:20, 23, 25, 27, 29:31)], expected_message)
})

test_that('parallel: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found, fitGauss, getAcquTime, verbose', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI, useFIR=TRUE, FIR=input_FIR)

  # Expected annotation
  expected_annotation             <- initAnnotation[c(1,3),]
  expected_annotation@TIC         <- c(2410533091, 2332817115)
  # 1
  tmp_peakTable1                  <- peakTable_noFitGauss1
  tmp_peakTable1$rt               <- c(3346.453, 3385.577, 3456.000, 3701.697)
  tmp_peakTable1$egauss           <- c(0.054008657336897373, 0.071998710997560611, 0.044897312316291224, NA)
  tmp_peakTable1$mu               <- c(541.22204104244724, 566.30756187792758, 610.69456227934620, NA)
  tmp_peakTable1$sigma            <- c(7.4645133175478202, 7.7881518794091047, 7.5047131493649966, NA)
  tmp_peakTable1$h                <- c(897391.73704987112, 1133465.71982824523, 381973.27358806110, NA)
  tmp_peakTable1$rt_dev_sec       <- c(1.565, 0.000, 1.565, NA)
  tmp_peakTable1$FWHM             <- c(27.508921442969950, 28.700624489426446, 27.656069616659352, NA)
  tmp_peakTable1$asymmetryFactor  <- c(1.2967091657127203, 2.1345129619692504, 1.3741191650431608, NA)
  tmp_peakTable1[4,]              <- c(TRUE, 536.2, 536.1995, 536.2005, 3701.697, 3689.700, 3738.213, 8022477, 8022477, 330176, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable1[,c(1,25,30)]     <- sapply(tmp_peakTable1[,c(1,25,30)], as.logical)
  # 3
  tmp_peakTable3                  <- peakTable_noFitGauss3
  tmp_peakTable3[1,]              <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3368.363, 3346.454, 3396.533, 21421980.83624988049, 21114392.80130613223, 758336, 4527, 0.057995477632078921, 555.40571618721754, 7.4557569905883767, 751440.43912957679, 1, 0e+00, 5, 554, 549, 559, 39, 71, 1, FALSE, 0.023376160866574614, 23.474999999999909, 27.475651809276314, 11, NA, NA)
  tmp_peakTable3[2,]              <- c(TRUE, 496.2000122, 496.19949, 496.2005, 3412.182, 3378.274, 3426.266, 27614663.3, 27614663.3, 1149440, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[4,]              <- c(TRUE, 536.2000122, 536.19949, 536.2005, 3711.088, 3689.700, 3738.213, 5716824.6, 5716824.6, 196160, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[,c(1,25,30,31)]  <- sapply(tmp_peakTable3[,c(1,25,30,31)], as.logical)
  expected_annotation@peakTables  <- list(tmp_peakTable1, tmp_peakTable3)
  expected_annotation@EICs        <- list(EICs1, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
  names(tmp_status)   <- 'aaa/bbb.cdf'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Processing 4 compounds in 3 samples:\n", "  uROI:\tTRUE\n", "  FIR:\tTRUE\n", "----------------\n", "1 file(s) failed to process:\n          file                                  error\n1 aaa/bbb.cdf Error file does not exist: aaa/bbb.cdf\n", "----------------\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=1, fitGauss=TRUE, getAcquTime=TRUE, verbose=TRUE, snthresh=20))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  expect_equal(length(result_parallelAnnotation$messages), 7)
  expect_equal(result_parallelAnnotation$messages[c(1:6)], expected_message)
})

test_that('serial and parallel give the same result: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found, fitGauss, getAcquTime, verbose', {
  # Object fully initialised
  initAnnotation  <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI, useFIR=TRUE, FIR=input_FIR)

  # results
  result_serial   <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=TRUE, getAcquTime=TRUE, verbose=TRUE, snthresh=20))
  result_parallel <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=1, fitGauss=TRUE, getAcquTime=TRUE, verbose=TRUE, snthresh=20))

  # Check results
  expect_equal(result_serial$result, result_parallel$result)
})

test_that('already annotated message in verbose', {
  # use "serial: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found, fitGauss, getAcquTime, verbose"
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI, useFIR=TRUE, FIR=input_FIR, isAnnotated=TRUE)

  # Expected annotation
  expected_annotation             <- initAnnotation[c(1,3),]
  expected_annotation@TIC         <- c(2410533091, 2332817115)
  # 1
  tmp_peakTable1                  <- peakTable_noFitGauss1
  tmp_peakTable1$rt               <- c(3346.453, 3385.577, 3456.000, 3701.697)
  tmp_peakTable1$egauss           <- c(0.054008657336897373, 0.071998710997560611, 0.044897312316291224, NA)
  tmp_peakTable1$mu               <- c(541.22204104244724, 566.30756187792758, 610.69456227934620, NA)
  tmp_peakTable1$sigma            <- c(7.4645133175478202, 7.7881518794091047, 7.5047131493649966, NA)
  tmp_peakTable1$h                <- c(897391.73704987112, 1133465.71982824523, 381973.27358806110, NA)
  tmp_peakTable1$rt_dev_sec       <- c(1.565, 0.000, 1.565, NA)
  tmp_peakTable1$FWHM             <- c(27.508921442969950, 28.700624489426446, 27.656069616659352, NA)
  tmp_peakTable1$asymmetryFactor  <- c(1.2967091657127203, 2.1345129619692504, 1.3741191650431608, NA)
  tmp_peakTable1[4,]              <- c(TRUE, 536.2, 536.1995, 536.2005, 3701.697, 3689.700, 3738.213, 8022477, 8022477, 330176, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable1[,c(1,25,30)]     <- sapply(tmp_peakTable1[,c(1,25,30)], as.logical)
  # 3
  tmp_peakTable3                  <- peakTable_noFitGauss3
  tmp_peakTable3[1,]              <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3368.363, 3346.454, 3396.533, 21421980.83624988049, 21114392.80130613223, 758336, 4527, 0.057995477632078921, 555.40571618721754, 7.4557569905883767, 751440.43912957679, 1, 0e+00, 5, 554, 549, 559, 39, 71, 1, FALSE, 0.023376160866574614, 23.474999999999909, 27.475651809276314, 11, NA, NA)
  tmp_peakTable3[2,]              <- c(TRUE, 496.2000122, 496.19949, 496.2005, 3412.182, 3378.274, 3426.266, 27614663.3, 27614663.3, 1149440, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[4,]              <- c(TRUE, 536.2000122, 536.19949, 536.2005, 3711.088, 3689.700, 3738.213, 5716824.6, 5716824.6, 196160, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE, NA, NA, NA, NA, NA, NA)
  tmp_peakTable3[,c(1,25,30,31)]  <- sapply(tmp_peakTable3[,c(1,25,30,31)], as.logical)
  expected_annotation@peakTables  <- list(tmp_peakTable1, tmp_peakTable3)
  expected_annotation@EICs        <- list(EICs1, EICs3)
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
  names(tmp_status)   <- 'aaa/bbb.cdf'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("!! Data was already annotated, results will be overwritten !!\n", "Processing 4 compounds in 3 samples:\n", "  uROI:\tTRUE\n", "  FIR:\tTRUE\n", "----- ko15 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 4 found.\n", "Previously loaded EICs used for peak statistics\n", "1 features to integrate with FIR\n", "Check input, mzMLPath must be a .mzML\n", "Error file does not exist: aaa/bbb.cdf\n", "----- ko18 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 6 found.\n", "Previously loaded EICs used for peak statistics\n", "2 features to integrate with FIR\n", "Check input, mzMLPath must be a .mzML\n", "----------------\n", "1 file(s) failed to process:\n          file                                  error\n1 aaa/bbb.cdf Error file does not exist: aaa/bbb.cdf\n", "----------------\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=TRUE, getAcquTime=TRUE, verbose=TRUE, snthresh=20))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages
  expect_equal(length(result_parallelAnnotation$messages), 33)
  expect_equal(result_parallelAnnotation$messages[c(1:8, 11, 13, 15, 17:21, 24, 26, 28, 30:32)], expected_message)
})

test_that('catch file that doesnt exist, catch error processing, no file left', {
  # Object fully initialised
  wrongPaths                      <- c("aaa/bbb.cdf", system.file("testdata/test_fakemzML.mzML", package = "peakPantheR"))
  initAnnotation                  <- peakPantheRAnnotation(spectraPaths=wrongPaths, targetFeatTable=input_targetFeatTable)

  # Expected annotation
  expected_annotation             <- initAnnotation[c(FALSE, FALSE),]
  expected_annotation@isAnnotated <- FALSE
  # Expected message
  expected_message    <- c("Processing 4 compounds in 2 samples:\n", "  uROI:\tFALSE\n", "  FIR:\tFALSE\n", "Error file does not exist: aaa/bbb.cdf\n", "----- test_fakemzML -----\n", "-----\n", "Error processing file: test_fakemzML\n", "[SpectrumList_mzML::create()] Bad istream.\n", "\n-----\n", "----------------\n", "No file left in the object!\n", "----------------\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, fitGauss=FALSE, getAcquTime=FALSE, verbose=TRUE))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation)
  # cannot check the failure paths
  expect_equal(dim(result_parallelAnnotation$result$failures)[1], 2)
  expect_equal(dim(result_parallelAnnotation$result$failures)[2], 2)

  # Check messages
  expect_equal(length(result_parallelAnnotation$messages), 14)
  expect_equal(result_parallelAnnotation$messages[c(1:10, 12, 13)], expected_message)
})

test_that('raise errors', {
  # Object fails validation on input
  wrongInit       <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)
  wrongInit@TIC   <- c(1, 2)
  msg1            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: TIC has 2 elements (samples). Should be 3', sep='')
  expect_error(peakPantheR_parallelAnnotation(wrongInit, ncores=0, fitGauss=FALSE, getAcquTime=FALSE, verbose=FALSE), msg1, fixed=TRUE)
})
