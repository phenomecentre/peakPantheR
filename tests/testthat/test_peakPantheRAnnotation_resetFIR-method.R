context('peakPantheRAnnotation_resetFIR()')

## Test the resetting of FIR

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

## Input data
# targetFeatTable
input_targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_targetFeatTable[1,] <- c("ID-1", "Cpd 1",  3.,  1.,  4.,  5.,  2.,  6.)
input_targetFeatTable[2,] <- c("ID-2", "Cpd 2", 19., 17., 20., 21., 18., 22.)
input_targetFeatTable[,c(3:8)] <- sapply(input_targetFeatTable[,c(3:8)], as.numeric)

# uROI
input_uROI      <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_uROI[1,]  <- c( 7., 11. , 8.,  9., 12., 10.)
input_uROI[2,]  <- c(23., 27., 24., 25., 28., 26.)

# FIR
input_FIR       <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=FALSE)
input_FIR[1,]   <- c(13., 14., 15., 16.)
input_FIR[2,]   <- c(29., 30., 31., 32.)

# object
input_annotation  <- peakPantheRAnnotation(targetFeatTable = input_targetFeatTable,
                                           uROI = input_uROI, 
                                           FIR = input_FIR,
                                           uROIExist = TRUE)



test_that('reset FIR with uROI values, verbose, no verbose', {
  
  # expected
  expected_annotation   <- input_annotation
  expected_annotation@FIR[,c('rtMin','rtMax','mzMin','mzMax')] <- uROI(input_annotation)[,c('rtMin','rtMax','mzMin','mzMax')]
  expected_message      <- c("FIR will be reset with uROI values\n")
  
  # results (output, warnings and messages)
  result_reset1         <- evaluate_promise(resetFIR(input_annotation, verbose=TRUE))
  
  # Check result
  expect_equal(result_reset1$result, expected_annotation)
  
  # Check result messages
  expect_equal(length(result_reset1$messages), 1)
  expect_equal(result_reset1$messages, expected_message)
  
  ## no verbose
  result_reset2         <- evaluate_promise(resetFIR(input_annotation, verbose=FALSE))
  expect_equal(length(result_reset2$messages), 0)
})

test_that('reset FIR with ROI values, verbose, no verbose', {
  
  # input
  tmp_annotation            <- input_annotation
  tmp_annotation@uROIExist  <- FALSE
  
  # expected
  expected_annotation   <- tmp_annotation
  expected_annotation@FIR[,c('rtMin','rtMax','mzMin','mzMax')] <- ROI(input_annotation)[,c('rtMin','rtMax','mzMin','mzMax')]
  expected_message      <- c("FIR will be reset with ROI values as uROI values are not set\n")
  
  # results (output, warnings and messages)
  result_reset3         <- evaluate_promise(resetFIR(tmp_annotation, verbose=TRUE))
  
  # Check result
  expect_equal(result_reset3$result, expected_annotation)
  
  # Check result messages
  expect_equal(length(result_reset3$messages), 1)
  expect_equal(result_reset3$messages, expected_message)
  
  ## no verbose
  result_reset4         <- evaluate_promise(resetFIR(tmp_annotation, verbose=FALSE))
  expect_equal(length(result_reset4$messages), 0)
})
