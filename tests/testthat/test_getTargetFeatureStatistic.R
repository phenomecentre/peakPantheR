context('getTargetFeatureStatistic()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input and expected data
# use ko15.CDf file from the pkg faahKO
singleSpectraDataPath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
raw_data  						<- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')

# targeted features in faahKO
targetFeatTable     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=F)
targetFeatTable[1,] 	<- c(1, "testCpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
targetFeatTable[2,] 	<- c(2, "testCpd 2, 2 peaks in box", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
targetFeatTable[3,] 	<- c(3, "testCpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
targetFeatTable[4,] 	<- c(4, "testCpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
targetFeatTable[,c(1,3:8)] <- sapply(targetFeatTable[,c(1,3:8)], as.numeric)

# found peaks no fitGauss
foundPeaks_noFitGauss     <- data.frame(matrix(vector(), 4, 25, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled"))),stringsAsFactors=F)
foundPeaks_noFitGauss[1,] <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3344.888, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, NA, NA, NA, NA, 1, 0, 5, 540, 535, 545, 24, 60, 1,0)
foundPeaks_noFitGauss[2,] <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3382.447, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, NA, NA, NA, NA, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0)
foundPeaks_noFitGauss[3,] <- c(TRUE, 464.2000122, 464.2000122, 464.2000122, 3454.435, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, NA, NA, NA, NA, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0)
foundPeaks_noFitGauss[4,] <- c(TRUE, 536.2000122, 536.2000122, 536.2000122, 3701.697, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, NA, NA, NA, NA, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0)
foundPeaks_noFitGauss[,1] <- sapply(foundPeaks_noFitGauss[,c(1)], as.logical)

# found peaks with fitGauss
foundPeaks_FitGauss     <- data.frame(matrix(vector(), 4, 25, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled"))),stringsAsFactors=F)
foundPeaks_FitGauss[1,] <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3346.453, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, 0.05400865734, 541.2220410, 7.464513318, 897391.7370, 1, 0, 5, 540, 535, 545, 24, 60, 1,0)
foundPeaks_FitGauss[2,] <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3385.577, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, 0.07199871100, 566.3075619, 7.788151879, 1133465.7198, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0)
foundPeaks_FitGauss[3,] <- c(TRUE, 464.2000122, 464.2000122, 464.2000122, 3456.000, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, 0.04489731232, 610.6945623, 7.504713149, 381973.2736, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0)
foundPeaks_FitGauss[4,] <- c(TRUE, 536.2000122, 536.2000122, 536.2000122, 3704.827, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, 0.07353430967, 769.5553377, 6.824007014, 324408.4744, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0)
foundPeaks_FitGauss[,1] <- sapply(foundPeaks_FitGauss[,c(1)], as.logical)

# load EICs outside of getTargetFeatureStatistic
EICs	<- xcms::chromatogram(raw_data, rt = data.frame(rt_lower=targetFeatTable$rtMin, rt_upper=targetFeatTable$rtMax), mz = data.frame(mz_lower=targetFeatTable$mzMin, mz_upper=targetFeatTable$mzMax))

# peak statistics with fitGauss
peakStatistic_FitGauss      <- data.frame(matrix(vector(), 4, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakStatistic_FitGauss[1,]  <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3346.453, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, 0.05400865734, 541.2220410, 7.464513318, 897391.7370, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.023362696095338677, 1.565, 27.508921444636144, 11, NA, 1.2967091657127203)
peakStatistic_FitGauss[2,]  <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3385.577, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, 0.07199871100, 566.3075619, 7.788151879, 1133465.7198, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.024586860166611640, 0.000, 28.700624487918503, 11, NA, 2.1345129619692504)
peakStatistic_FitGauss[3,]  <- c(TRUE, 464.2000122, 464.2000122, 464.2000122, 3456.000, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, 0.04489731232, 610.6945623, 7.504713149, 381973.2736, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0, 0.026281775128549539, 1.565, 27.656069615314664, 11, NA, 1.3741191650431608)
peakStatistic_FitGauss[4,]  <- c(TRUE, 536.2000122, 536.2000122, 536.2000122, 3704.827, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, 0.07353430967, 769.5553377, 6.824007014, 324408.4744, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0, 0.022752704030186234, 3.130, 25.147467808758392, 11, 1.1968986203515855, 1.3314781556202822)
peakStatistic_FitGauss[,1]  <- sapply(peakStatistic_FitGauss[,c(1)], as.logical)

# peak statistics without fitGauss
peakStatistic_noFitGauss      <- data.frame(matrix(vector(), 4, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakStatistic_noFitGauss[1,]  <- c(TRUE, 522.2000122, 522.2000122, 522.2000122, 3344.888, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, NA, NA, NA, NA, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.023362696095338677, 0.00, NA, 11, NA, 1.4839997536872727)
peakStatistic_noFitGauss[2,]  <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3382.447, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, NA, NA, NA, NA, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.024586860166611640, 3.13, NA, 11, NA, 2.7082909206059269)
peakStatistic_noFitGauss[3,]  <- c(TRUE, 464.2000122, 464.2000122, 464.2000122, 3454.435, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, NA, NA, NA, NA, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0, 0.026281775128549539, 0.00, NA, 11, NA, 1.5506880253870328)
peakStatistic_noFitGauss[4,]  <- c(TRUE, 536.2000122, 536.2000122, 536.2000122, 3701.697, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, NA, NA, NA, NA, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0, 0.022752704030186234, 0.00, NA, 11, 1.4026668257511667, 1.7784490549015182)
peakStatistic_noFitGauss[,1]  <- sapply(peakStatistic_noFitGauss[,c(1)], as.logical)


test_that('default parameters with fitGauss, no previous EICs, no verbose', {
  # expected foundPeaks
  expected_featureStatistic <- peakStatistic_FitGauss

	# results (output, warnings and messages)
  result_featureStatistic   <- evaluate_promise(getTargetFeatureStatistic(raw_data, targetFeatTable, foundPeaks_FitGauss, usePreviousEICs=NULL, verbose=FALSE))

  # Check result table
  expect_equal(result_featureStatistic$result, expected_featureStatistic)

  # Check no result messages
  expect_equal(length(result_featureStatistic$messages), 0)
})

test_that('default parameters without fitGauss, no previous EICs, verbose', {
  # expected foundPeaks
  expected_featureStatistic <- peakStatistic_noFitGauss

  # results (output, warnings and messages)
  result_featureStatistic   <- evaluate_promise(getTargetFeatureStatistic(raw_data, targetFeatTable, foundPeaks_noFitGauss, usePreviousEICs=NULL, verbose=TRUE))

  # Check result table
  expect_equal(result_featureStatistic$result, expected_featureStatistic)

  # Check result messages (message from verbose returns run time, cannot be matched)
  expect_equal(length(result_featureStatistic$messages), 1)
})

test_that('default parameters with fitGauss, previous EICs, verbose', {
  # expected foundPeaks
  expected_featureStatistic <- peakStatistic_FitGauss

  # expected messages
  expected_message          <- "Previously loaded EICs used for peak statistics\n"

  # results (output, warnings and messages)
  result_featureStatistic   <- evaluate_promise(getTargetFeatureStatistic(raw_data, targetFeatTable, foundPeaks_FitGauss, usePreviousEICs=EICs, verbose=TRUE))

  # Check result table
  expect_equal(result_featureStatistic$result, expected_featureStatistic)

  # Check result messages (message for EICs and run time, run time cannot be matched)
  expect_equal(length(result_featureStatistic$messages), 2)
  expect_equal(result_featureStatistic$message[1], expected_message)
})

test_that('raise errors', {
  # targetFeatTable and foundPeakTable dimension mismatch
  expect_error(getTargetFeatureStatistic(raw_data, targetFeatTable, foundPeaks_FitGauss[1:3,], usePreviousEICs=NULL, verbose=FALSE), "Number of features in targetFeatTable*")

  # usePreviousEICs is not a list
  expect_error(getTargetFeatureStatistic(raw_data, targetFeatTable, foundPeaks_FitGauss, usePreviousEICs='not a list', verbose=FALSE), "usePreviousEICs is not a list of xcms::Chromatogram")

  # Number of chromatograms in usePreviousEICs list and features in targetFeatTable mismatch
  expect_error(getTargetFeatureStatistic(raw_data, targetFeatTable, foundPeaks_FitGauss, usePreviousEICs=list('a','b','c'), verbose=FALSE), "Number of chromatograms in usePreviousEICs*")

  # usePreviousEICs is not a list of xcms::Chromatogram
  expect_error(getTargetFeatureStatistic(raw_data, targetFeatTable, foundPeaks_FitGauss, usePreviousEICs=list('not a chrom','not a chrom','not a chrom','not a chrom'), verbose=FALSE), "usePreviousEICs is not a list of xcms::Chromatogram")
})
