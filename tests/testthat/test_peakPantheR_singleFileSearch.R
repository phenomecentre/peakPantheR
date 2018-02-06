context('peakPantheR_singleFileSearch()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input and expected data
# use ko15.CDf file from the pkg faahKO
singleSpectraDataPath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")

# targeted features in faahKO
targetFeatTable     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=F)
targetFeatTable[1,] 	<- c(1, "testCpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
targetFeatTable[2,] 	<- c(2, "testCpd 2, 2 peaks in box", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
targetFeatTable[3,] 	<- c(3, "testCpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
targetFeatTable[4,] 	<- c(4, "testCpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
targetFeatTable[,c(1,3:8)] <- sapply(targetFeatTable[,c(1,3:8)], as.numeric)

# peakTable fitGauss
peakTable_FitGauss      <- data.frame(matrix(vector(), 4, 33, dimnames=list(c(), c("cpdID", "cpdName", "found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable_FitGauss[1,]  <- c(1, NA, TRUE, 522.2000122, 522.2000122, 522.2000122, 3346.453, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, 0.05400865734, 541.2220410, 7.464513318, 897391.7370, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.023362696095338677, 1.565, 27.508921444636144, 11, NA, 1.2967091657127203)
peakTable_FitGauss[2,]  <- c(2, NA, TRUE, 496.2000122, 496.2000122, 496.2000122, 3385.577, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, 0.07199871100, 566.3075619, 7.788151879, 1133465.7198, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.024586860166611640, 0.000, 28.700624487918503, 11, NA, 2.1345129619692504)
peakTable_FitGauss[3,]  <- c(3, NA, TRUE, 464.2000122, 464.2000122, 464.2000122, 3456.000, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, 0.04489731232, 610.6945623, 7.504713149, 381973.2736, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0, 0.026281775128549539, 1.565, 27.656069615314664, 11, NA, 1.3741191650431608)
peakTable_FitGauss[4,]  <- c(4, NA, TRUE, 536.2000122, 536.2000122, 536.2000122, 3704.827, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, 0.07353430967, 769.5553377, 6.824007014, 324408.4744, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0, 0.022752704030186234, 3.130, 25.147467808758392, 11, 1.1968986203515855, 1.3314781556202822)
peakTable_FitGauss[,2]  <- c("testCpd 1", "testCpd 2, 2 peaks in box", "testCpd 3", "testCpd 4")
peakTable_FitGauss[,3]  <- sapply(peakTable_FitGauss[,3], as.logical)

# peakTable no fitGauss
peakTable_noFitGauss      <- data.frame(matrix(vector(), 4, 33, dimnames=list(c(), c("cpdID", "cpdName", "found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable_noFitGauss[1,]  <- c(1, NA, TRUE, 522.2000122, 522.2000122, 522.2000122, 3344.888, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, NA, NA, NA, NA, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.023376160866574614, 0.00, NA, 11, NA, 1.4839997536872727)
peakTable_noFitGauss[2,]  <- c(2, NA, TRUE, 496.2000122, 496.2000122, 496.2000122, 3382.447, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, NA, NA, NA, NA, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.024601030353423384, 3.13, NA, 11, NA, 2.7082909206059269)
peakTable_noFitGauss[3,]  <- c(3, NA, TRUE, 464.2000122, 464.2000122, 464.2000122, 3454.435, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, NA, NA, NA, NA, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0, 0.026296922148575364, 0.00, NA, 11, NA, 1.5506880253870328)
peakTable_noFitGauss[4,]  <- c(4, NA, TRUE, 536.2000122, 536.2000122, 536.2000122, 3701.697, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, NA, NA, NA, NA, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0, 0.022765817240815486, 0.00, NA, 11, 1.4026668257511667, 1.7784490549015182)
peakTable_noFitGauss[,2]  <- c("testCpd 1", "testCpd 2, 2 peaks in box", "testCpd 3", "testCpd 4")
peakTable_noFitGauss[,3]  <- sapply(peakTable_noFitGauss[,3], as.logical)

# expected EICs
raw_data  <- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')
EICs	    <- xcms::chromatogram(raw_data, rt = data.frame(rt_lower=targetFeatTable$rtMin, rt_upper=targetFeatTable$rtMax), mz = data.frame(mz_lower=targetFeatTable$mzMin, mz_upper=targetFeatTable$mzMax))


test_that('with fitGauss, no peakStatistic, no getEICs, no plotEICsPath, no verbose', {
  # Expected TIC
  expected_TIC        <- 2410533091
  # Expected peakTable
  expected_peakTable  <- peakTable_FitGauss[,c(3:27,1:2)]
  # Expected EICs
  expected_EICs       <- NA
  # Expected messages
  expected_messages   <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n")

	# results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, fitGauss=TRUE, peakStatistic=FALSE, getEICs=FALSE, plotEICsPath=NA, verbose=FALSE))

  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable)
  expect_equal(result_singleFileSearch$result$EICs, expected_EICs)

  # Check messages (from centwave)
  expect_equal(result_singleFileSearch$messages, expected_messages)
})

test_that('no fitGauss, with peakStatistic, no getEICs, no plotEICsPath, no verbose', {
  # Expected TIC
  expected_TIC        <- 2410533091
  # Expected peakTable
  expected_peakTable  <- peakTable_noFitGauss[,c(3:27,1:2,28:33)]
  # Expected EICs
  expected_EICs       <- NA
  # Expected messages
  expected_messages   <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n")

  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, fitGauss=FALSE, peakStatistic=TRUE, getEICs=FALSE, plotEICsPath=NA, verbose=FALSE))

  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable)
  expect_equal(result_singleFileSearch$result$EICs, expected_EICs)

  # Check messages (from centwave)
  expect_equal(result_singleFileSearch$messages, expected_messages)
})

test_that('no fitGauss, no peakStatistic, with getEICs, no plotEICsPath, verbose', {
  # Expected TIC
  expected_TIC        <- 2410533091
  # Expected peakTable
  expected_peakTable  <- peakTable_noFitGauss[,c(3:27,1:2)]
  # Expected EICs
  expected_EICs       <- EICs
  # Expected messages
  expected_messages   <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n")

  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, fitGauss=FALSE, peakStatistic=FALSE, getEICs=TRUE, plotEICsPath=NA, verbose=TRUE))

  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable)
  expect_equal(result_singleFileSearch$result$EICs, expected_EICs)

  # Check messages (3 from centwave, 3 with run time (can't check))
  expect_equal(length(result_singleFileSearch$messages), 6)
  expect_equal(result_singleFileSearch$messages[1:3], expected_messages)
})

test_that('no fitGauss, no peakStatistic, no getEICs, with plotEICsPath (should switch getEICs automatically), verbose', {
  # Expected TIC
  expected_TIC        <- 2410533091
  # Expected peakTable
  expected_peakTable  <- peakTable_noFitGauss[,c(3:27,1:2)]
  # Expected EICs
  expected_EICs       <- EICs
  # Expected messages
  expected_messages   <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n")
  # temporary file
  savePath            <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.png')

  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, fitGauss=FALSE, peakStatistic=FALSE, getEICs=FALSE, plotEICsPath=savePath, verbose=TRUE))

  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable)
  expect_equal(result_singleFileSearch$result$EICs, expected_EICs)
  # Check plot has been produced
  expect_true(file.exists(savePath))

  # Check messages (3 from centwave, 3 with run time (can't check))
  expect_equal(length(result_singleFileSearch$messages), 7)
  expect_equal(result_singleFileSearch$messages[1:3], expected_messages)
})

test_that('change centwave param with ... (change snthresh), no fitGauss, no peakStatistic, no getEICs, no plotEICsPath, no verbose', {
  # Expected TIC
  expected_TIC        <- 2410533091
  # Expected peakTable
  expected_peakTable            <- peakTable_noFitGauss[,c(3:27,1:2)]
  expected_peakTable[4,]        <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4, NA)
  expected_peakTable$cpdName[4] <- 'testCpd 4'
  expected_peakTable$found      <- sapply(expected_peakTable$found, as.logical)
  # Expected EICs
  expected_EICs       <- NA
  # Expected messages
  expected_messages   <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Detecting chromatographic peaks in 4 regions of interest ...", " OK: 4 found.\n")

  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, fitGauss=FALSE, peakStatistic=FALSE, getEICs=FALSE, plotEICsPath=NA, verbose=FALSE, snthresh=20))

  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable)
  expect_equal(result_singleFileSearch$result$EICs, expected_EICs)

  # Check messages (3 from centwave)
  expect_equal(result_singleFileSearch$messages, expected_messages)
})

test_that('no targetFeatures on import', {
  # Empty targetFeatures
  emptyTargetFeatures <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=F)

  # Expected TIC
  expected_TIC        <- 2410533091
  # Expected peakTable
  expected_peakTable  <- data.frame(matrix(vector(), 0, 33, dimnames=list(c(), c('found', 'mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'into', 'intb', 'maxo', 'sn', 'egauss', 'mu', 'sigma', 'h', 'f', 'dppm', 'scale', 'scpos', 'scmin', 'scmax', 'lmin', 'lmax', 'sample', 'is_filled', 'cpdID', 'cpdName', 'ppm_error', 'rt_dev_sec', 'FWHM', 'FWHM_ndatapoints', 'tailingFactor', 'asymmetryFactor'))), stringsAsFactors=F)
  # Expected EICs
  expected_EICs       <- NA
  # Expected messages
  expected_messages   <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "- No target features passed in 'targetFeatTable', no integration, only TIC will be reported -\n")

  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, emptyTargetFeatures, fitGauss=FALSE, peakStatistic=FALSE, getEICs=FALSE, plotEICsPath=NA, verbose=TRUE))

  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable)
  expect_equal(result_singleFileSearch$result$EICs, expected_EICs)

  # Check messages (2 can be checked, third is run time)
  expect_equal(length(result_singleFileSearch$messages), 3)
  expect_equal(result_singleFileSearch$messages[1:2], expected_messages)
})

test_that('raise errors', {
  # paths to trigger errors
  fakeInputPath     <- './notAPath/test.CDF'
  fakeOutputPath    <- './notAPath/save.png'
  wrongOutputFormat <- './notAPNG.txt'

  # singleSpectraDataPath doesnt exist
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath=fakeInputPath, targetFeatTable), "Check input, file*")
  # save folder doesnt exist
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, plotEICsPath=fakeOutputPath), "Check input, plotEICsPath folder*")
  # save file is not a .png
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, targetFeatTable, plotEICsPath=wrongOutputFormat), "Check input, plotEICsPath file name*")
})
