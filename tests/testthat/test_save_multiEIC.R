context('save_multiEIC()')

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
foundPeakTable     <- data.frame(matrix(vector(), 4, 27, dimnames=list(c(), c("cpdID", "cpdName", "found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled"))),stringsAsFactors=F)
foundPeakTable[1,] <- c(1, NA, TRUE, 522.2000122, 522.2000122, 522.2000122, 3344.888, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, NA, NA, NA, NA, 1, 0, 5, 540, 535, 545, 24, 60, 1,0)
foundPeakTable[2,] <- c(2, NA, TRUE, 496.2000122, 496.2000122, 496.2000122, 3382.447, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, NA, NA, NA, NA, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0)
foundPeakTable[3,] <- c(3, NA, TRUE, 464.2000122, 464.2000122, 464.2000122, 3454.435, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, NA, NA, NA, NA, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0)
foundPeakTable[4,] <- c(4, NA, TRUE, 536.2000122, 536.2000122, 536.2000122, 3701.697, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, NA, NA, NA, NA, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0)
foundPeakTable[,2] <- c("testCpd 1", "testCpd 2, 2 peaks in box", "testCpd 3", "testCpd 4")
foundPeakTable[,3] <- sapply(foundPeakTable[,3], as.logical)

# load EICs outside of getTargetFeatureStatistic
EICs	<- xcms::chromatogram(raw_data, rt = data.frame(rt_lower=targetFeatTable$rtMin, rt_upper=targetFeatTable$rtMax), mz = data.frame(mz_lower=targetFeatTable$mzMin, mz_upper=targetFeatTable$mzMax))


test_that('default parameters, verbose', {
  # temporary file
  savePath1  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.png')

	# results (output, warnings and messages)
  result_plot <- evaluate_promise(save_multiEIC(EICs, foundPeakTable, savePath1, width=15, height=15, verbose=TRUE))

  # Check plot has been produced
  expect_true(file.exists(savePath1))

  # Check result messages (save path)
  expect_equal(length(result_plot$messages), 1)
})

test_that('default parameters, no verbose', {
  # temporary file
  savePath2  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.png')

  # results (output, warnings and messages)
  result_plot <- evaluate_promise(save_multiEIC(EICs, foundPeakTable, savePath2, width=15, height=15, verbose=FALSE))

  # Check plot has been produced
  expect_true(file.exists(savePath2))

  # Check no result messages
  expect_equal(length(result_plot$messages), 0)
})

test_that('only one plot, no verbose', {
  # input
  singleEICs            <- xcms::chromatogram(raw_data, rt = c(rt_lower=targetFeatTable$rtMin, rt_upper=targetFeatTable$rtMax), mz = c(mz_lower=targetFeatTable$mzMin, mz_upper=targetFeatTable$mzMax))
  singleFoundPeakTable  <- foundPeakTable[1,]
  # temporary file
  savePath3  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.png')

  # results (output, warnings and messages)
  result_plot <- evaluate_promise(save_multiEIC(singleEICs, singleFoundPeakTable, savePath3, width=15, height=15, verbose=FALSE))

  # Check plot has been produced
  expect_true(file.exists(savePath3))

  # Check no result messages
  expect_equal(length(result_plot$messages), 0)
})


test_that('raise errors', {
  savePath3  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.png')
  # targetFeatTable and foundPeakTable dimension mismatch
  expect_error(save_multiEIC(EICs[1:3], foundPeakTable, savePath3, width=15, height=15, verbose=TRUE), "Number of chromatograms in EICs*")
})
