context('findTargetFeatures()')

skip_if_not(FALSE, message = 'unittest refactor')
skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input and expected data
# use ko15.CDf file from the pkg faahKO
singleSpectraDataPath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
raw_data  						<- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')

# targeted features in faahKO
ROIList <- list(list("mz"=522.2, "mzmin"=522.194778, "mzmax"=522.205222, "scmin"=518, "scmax"=569, "length"=-1, "intensity"=-1),
                list("mz"=496.2, "mzmin"=496.195038, "mzmax"=496.204962, "scmin"=499, "scmax"=601, "length"=-1, "intensity"=-1),
                list("mz"=464.2, "mzmin"=464.195358, "mzmax"=464.204642, "scmin"=588, "scmax"=636, "length"=-1, "intensity"=-1),
                list("mz"=536.2, "mzmin"=536.194638, "mzmax"=536.205362, "scmin"=748, "scmax"=796, "length"=-1, "intensity"=-1))

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


test_that('default parameters, without fitGauss, no verbose', {
  # expected foundPeaks
  expected_foundPeaks <- foundPeaks_noFitGauss

  # expected messages
  expected_messages   <- c("Detecting chromatographic peaks in 4 regions of interest ...", " OK: 5 found.\n")

	# results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(raw_data, ROIList, snthresh=3, peakwidth=c(2,20), verbose=FALSE, fitGauss=FALSE))

  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)

  # Check result messages
  expect_equal(result_foundPeaks$messages, expected_messages)
})

test_that('default parameters, with fitGauss, with verbose', {
  # expected foundPeaks
  expected_foundPeaks <- foundPeaks_FitGauss

  # results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(raw_data, ROIList, snthresh=3, peakwidth=c(2,20), verbose=TRUE, fitGauss=TRUE))

  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)

  # Check result messages (3rd message from verbose with run time, cannot be matched)
  expect_equal(length(result_foundPeaks$messages), 3)
})

test_that('change snthresh', {
  # expected foundPeaks
  expected_foundPeaks     <- foundPeaks_noFitGauss
  expected_foundPeaks[4,] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

  # results
  result_foundPeaks       <- findTargetFeatures(raw_data, ROIList, snthresh=20, peakwidth=c(2,20), verbose=FALSE, fitGauss=FALSE)

  # Check result table
  expect_equal(result_foundPeaks, expected_foundPeaks)
})

test_that('change peakwidth', {
  # expected foundPeaks
  expected_foundPeaks     <- foundPeaks_noFitGauss
  expected_foundPeaks[1,] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  expected_foundPeaks[2,] <- c(TRUE, 496.2000122, 496.2000122, 496.2000122, 3387.142, 3265.075, 3476.344, 51476017.36, 49337927.84, 1128960, 85, NA, NA, NA, NA, 2, 0, 30, 567, 537, 597, 87, 222, 1, 0)
  expected_foundPeaks[3,] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  expected_foundPeaks[4,] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  expected_foundPeaks[,1] <- sapply(expected_foundPeaks[,c(1)], as.logical)

  # results
  result_foundPeaks       <- findTargetFeatures(raw_data, ROIList, snthresh=3, peakwidth=c(70,100), verbose=FALSE, fitGauss=FALSE)

  # Check result table
  expect_equal(result_foundPeaks, expected_foundPeaks)
})
