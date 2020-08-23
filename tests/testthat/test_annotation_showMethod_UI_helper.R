context('annotation_showMethod_UI_helper()')

# UI specific show method that returns a named list of properties

test_that('UI show method outputs right values', {
  # Input and expected
  input_spectraPaths        <- c('./path/file1', './path/file2', './path/file3')
  input_targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
  input_targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
  input_targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
  input_targetFeatTable[,c(3:8)]  <- sapply(input_targetFeatTable[,c(3:8)], as.numeric)
  input_FIR       <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=FALSE)
  input_FIR[1,]   <- c(1., 2., 3., 4.)
  input_FIR[2,]   <- c(5., 6., 7., 8.)
  input_uROI      <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
  input_uROI[1,]  <- c(10., 11., 12., 13., 14., 15.)
  input_uROI[2,]  <- c(16., 17., 18., 19., 20., 21.)
  expected_values_default <- list(nbCompounds = 0,
                                  nbSamples = 0,
                                  uROIExist = FALSE,
                                  useUROI = FALSE,
                                  useFIR = FALSE,
                                  isAnnotated = FALSE)
  expected_values_ROIFIRInit <- list(nbCompounds = 2,
                                     nbSamples = 3,
                                     uROIExist = TRUE,
                                     useUROI = TRUE,
                                     useFIR = TRUE,
                                     isAnnotated = TRUE)

  # default values, no uROI, no use of FIR
  defaultInit     <- peakPantheRAnnotation()
  result_default  <- evaluate_promise(annotation_showMethod_UI_helper(defaultInit))
  expect_equal(result_default$result, expected_values_default)

  # multiple compounds and spectra with uROIExist and useFIR
  ROIFIRInit        <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, uROI=input_uROI, FIR=input_FIR, uROIExist=TRUE, useUROI=TRUE, useFIR=TRUE, isAnnotated=TRUE)
  result_ROIFIRInit <- evaluate_promise(annotation_showMethod_UI_helper(ROIFIRInit))
  expect_equal(result_ROIFIRInit$result, expected_values_ROIFIRInit)
})

