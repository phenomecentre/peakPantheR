context('integrateFIR()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input and expected data
# use ko15.CDf file from the pkg faahKO
raw_data <- MSnbase::readMSData( system.file('cdf/KO/ko15.CDF', package = "faahKO"), centroided=TRUE, mode='onDisk')

# full peakTable
full_peakTable      <- data.frame(matrix(vector(), 4, 33, dimnames=list(c(), c("cpdID", "cpdName", "found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
full_peakTable[1,]  <- c(NA, NA, TRUE, 522.2000122, 522.2000122, 522.2000122, 3344.888, 3322.979, 3379.317, 25792525.445, 25768307.538, 889280, 1840, NA, NA, NA, NA, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.023376160866574614, 0.00, NA, 11, NA, 1.4839997536872727)
full_peakTable[2,]  <- c(NA, NA, TRUE, 496.2000122, 496.2000122, 496.2000122, 3382.447, 3362.102, 3409.051, 32873727.359, 32818664.007, 1128960, 1471, NA, NA, NA, NA, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.024601030353423384, 3.13, NA, 11, NA, 2.7082909206059269)
full_peakTable[3,]  <- c(NA, NA, TRUE, 464.2000122, 464.2000122, 464.2000122, 3454.435, 3432.525, 3479.474, 10818326.613, 10818278.099, 380736, 380735, NA, NA, NA, NA, 3, 0, 5, 610, 605, 615, 24, 54, 1, 0, 0.026296922148575364, 0.00, NA, 11, NA, 1.5506880253870328)
full_peakTable[4,]  <- c(NA, NA, TRUE, 536.2000122, 536.2000122, 536.2000122, 3701.697, 3682.918, 3729.867, 8519479.783, 8460371.578, 330176, 197, NA, NA, NA, NA, 4, 0, 5, 768, 763, 773, 24, 54, 1, 0, 0.022765817240815486, 0.00, NA, 11, 1.4026668257511667, 1.7784490549015182)
full_peakTable[,2]  <- c("ID-1", "ID-2", "ID-3", "ID-4")
full_peakTable[,2]  <- c("testCpd 1", "testCpd 2, 2 peaks in box", "testCpd 3", "testCpd 4")
full_peakTable[,c(3,27)]  <- sapply(full_peakTable[,c(3,27)], as.logical)

# FIR (matching found peaks dimension, need widening mz as test data doesn't result in mz width)
input_FIR           <- full_peakTable[,c("mzmin", "mzmax", "rtmin", "rtmax")]
colnames(input_FIR) <- c("mzMin", "mzMax", "rtMin", "rtMax")
input_FIR$mzMin     <- input_FIR$mzMin - 0.0005
input_FIR$mzMax     <- input_FIR$mzMax + 0.0005



test_that('no modification, no missing, no verbose', {
  # Expected peakTable
  expected_peakTable  <- full_peakTable

  # results (output, warnings and messages)
  result_integrateFIR <- evaluate_promise(integrateFIR(rawSpec=raw_data, foundPeakTable=full_peakTable, FIR=input_FIR, verbose=TRUE))

  # Check results
  expect_equal(result_integrateFIR$result, expected_peakTable)

  # Check messages
  expect_equal(length(result_integrateFIR$messages), 1)
})

test_that('3 missing with 1 which gives no scan, verbose', {
  # Input FIR with 1 empty box (without the widened mz cpd 3 should not return any matching scan)
  input_FIR_empty               <- input_FIR
  input_FIR_empty[3,]           <- full_peakTable[3,c("mzmin", "mzmax", "rtmin", "rtmax")]
  # Input peakTable (with 2, 3, 4 not found)
  notFound_peakTable            <- full_peakTable
  notFound_peakTable[2,c(3:27)] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE)
  notFound_peakTable[3,c(3:27)] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE)
  notFound_peakTable[4,c(3:27)] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE)
  # Expected peakTable
  expected_peakTable            <- full_peakTable
  expected_peakTable[2,c(3:27)] <- c(TRUE, 496.20001, 496.19951, 496.20051, 3385.5770, 3362.102, 3409.051, 31813284.541161336, 31813284.541161336, 1128960, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE)
  expected_peakTable[3,c(3:27)] <- c(TRUE, 464.20001, 464.20001, 464.20001, 3455.99949, 3432.525, 3479.474, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE)
  expected_peakTable[4,c(3:27)] <- c(TRUE, 536.20001, 536.19951, 536.20051, 3701.6970, 3682.918, 3729.867, 8244657.8540967861, 8244657.8540967861, 330176, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TRUE)
  expected_peakTable[,c(3,27)]  <- sapply(expected_peakTable[,c(3,27)], as.logical)
  # Expected message
  expected_messages             <- c("3 features to integrate with FIR\n", "Reading data from 3 windows\n", "No scan present in the FIR # 3: rt and mz are set as the middle of the FIR box; maxo, into and intb are set to 0\n")

  # results (output, warnings and messages)
  result_integrateFIR           <- evaluate_promise(integrateFIR(rawSpec=raw_data, foundPeakTable=notFound_peakTable, FIR=input_FIR_empty, verbose=TRUE))

  # Check results
  expect_equal(result_integrateFIR$result, expected_peakTable)

  # Check messages (cannot check time on the fourth message)
  expect_equal(length(result_integrateFIR$messages), 4)
  expect_equal(result_integrateFIR$messages[1:3], expected_messages)
})

test_that('raise errors', {
  # paths to trigger errors
  wrongFIRsize    <- input_FIR[1:3,]

  # foundPeakTable and FIR dimension mismatch
  expect_error(integrateFIR(rawSpec=raw_data, foundPeakTable=full_peakTable, FIR=wrongFIRsize, verbose=TRUE), "Check input, FIR must have the same number of rows as foundPeakTable")
})


