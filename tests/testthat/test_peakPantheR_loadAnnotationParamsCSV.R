context('peakPantheR_loadAnnotationParamsCSV()')

## Test the generation of a new object by reading parameters from CSV (simple: Spl, advanced: Adv)
# Also test prepare_basic_target_parameters() and prepare_advanced_target_parameters()

## Input data
input_CSV_adv      <- data.frame(matrix(nrow=2,ncol=21,dimnames=list(c(), c('cpdID', 'cpdName', 'X', 'ROI_rt', 'ROI_mz','ROI_rtMin', 'ROI_rtMax', 'ROI_mzMin', 'ROI_mzMax', 'X', 'uROI_rtMin', 'uROI_rtMax', 'uROI_mzMin', 'uROI_mzMax', 'uROI_rt', 'uROI_mz', 'X', 'FIR_rtMin', 'FIR_rtMax', 'FIR_mzMin', 'FIR_mzMax'))))
input_CSV_adv[1,]  <- c('ID-1', 'Cpd 1', '|', 1.,  2.,  3.,  4.,  5.,  6.,  '|', 7.,  8.,  9.,  10., 11., 12., '|', 13., 14., 15., 16.)
input_CSV_adv[2,]  <- c('ID-2', 'Cpd 2', '|', 17., 18., 19., 20., 21., 22., '|', 23., 24., 25., 26., 27., 28., '|', 29., 30., 31., 32.)
input_CSV_adv[,-c(1,2,3,10,17)]  <- sapply(input_CSV_adv[,-c(1,2,3,10,17)], as.numeric)

input_CSV_spl           <- input_CSV_adv[, c('cpdID', 'cpdName', 'ROI_rtMin', 'ROI_rt', 'ROI_rtMax', 'ROI_mzMin', 'ROI_mz', 'ROI_mzMax')]
colnames(input_CSV_spl) <- c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")

## Expected result
# targetFeatTable
input_targetFeatTable_adv     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_targetFeatTable_adv[1,] <- c("ID-1", "Cpd 1",  3.,  1.,  4.,  5.,  2.,  6.)
input_targetFeatTable_adv[2,] <- c("ID-2", "Cpd 2", 19., 17., 20., 21., 18., 22.)
input_targetFeatTable_adv[,c(3:8)] <- sapply(input_targetFeatTable_adv[,c(3:8)], as.numeric)

input_targetFeatTable_spl     <- input_targetFeatTable_adv

# uROI
input_uROI_adv      <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_uROI_adv[1,]  <- c( 7., 11. , 8.,  9., 12., 10.)
input_uROI_adv[2,]  <- c(23., 27., 24., 25., 28., 26.)

# FIR
input_FIR_adv       <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=FALSE)
input_FIR_adv[1,]   <- c(13., 14., 15., 16.)
input_FIR_adv[2,]   <- c(29., 30., 31., 32.)

# object
input_annotation_adv  <- peakPantheRAnnotation(targetFeatTable = input_targetFeatTable_adv,
                                               uROI = input_uROI_adv, 
                                               FIR = input_FIR_adv,
                                               uROIExist = TRUE)


input_annotation_spl  <- peakPantheRAnnotation(targetFeatTable = input_targetFeatTable_spl)


test_that('Spl - default with ROI, verbose, no verbose', {
  # temporary file
  savePath1           <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
  
  # save csv
  utils::write.csv(input_CSV_spl, file=savePath1, row.names=FALSE)
  
  # expected
  expected_annotation <- input_annotation_spl
  expected_message    <- c("New peakPantheRAnnotation object initialised for 2 compounds\n")
  
  # results (output, warnings and messages)
  result_load1        <- evaluate_promise(peakPantheR_loadAnnotationParamsCSV(savePath1, verbose=TRUE))
  
  # Check result
  expect_equal(result_load1$result, expected_annotation)
  
  # Check result messages
  expect_equal(length(result_load1$messages), 1)
  expect_equal(result_load1$messages, expected_message)
  
  ## no verbose
  result_load2        <- evaluate_promise(peakPantheR_loadAnnotationParamsCSV(savePath1, verbose=FALSE))
  expect_equal(length(result_load2$messages), 0)
})

test_that('Adv - default with ROI, uROI, FIR, uROIExist=TRUE, verbose, no verbose', {
  # temporary file
  savePath2           <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
  
  # save csv
  utils::write.csv(input_CSV_adv, file=savePath2, row.names=FALSE)
  
  # expected
  expected_annotation <- input_annotation_adv
  expected_message    <- c("uROIExist set to TRUE\n", "New peakPantheRAnnotation object initialised for 2 compounds\n")
  
  # results (output, warnings and messages)
  result_load1        <- evaluate_promise(peakPantheR_loadAnnotationParamsCSV(savePath2, verbose=TRUE))
  
  # Check result
  expect_equal(result_load1$result, expected_annotation)
  
  # Check result messages
  expect_equal(length(result_load1$messages), 2)
  expect_equal(result_load1$messages, expected_message)
  
  ## no verbose
  result_load2        <- evaluate_promise(peakPantheR_loadAnnotationParamsCSV(savePath2, verbose=FALSE))
  expect_equal(length(result_load2$messages), 0)
})

test_that('Adv - ROI with NA, uROI with NA, FIR with NA, uROIExist=FALSE, verbose, no verbose', {
  # NA in ROI_rt mz are allowed, NA in uROI set uROIExist=FALSE, NA in FIR is allowed
  
  # temporary file
  savePath3           <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
  
  # save csv
  tmp_CSV             <- input_CSV_adv
  tmp_CSV$ROI_rt      <- c(as.numeric(NA), 17.)
  tmp_CSV$ROI_mz      <- c(2, as.numeric(NA))
  tmp_CSV$uROI_rtMin  <- c(7., as.numeric(NA))
  tmp_CSV$FIR_mzMax   <- c(as.numeric(NA), as.numeric(NA))
  utils::write.csv(tmp_CSV, file=savePath3, row.names=FALSE)
  
  # expected
  tmp_targetFeatTable     <- input_targetFeatTable_adv
  tmp_targetFeatTable$rt  <- c(as.numeric(NA), 17.)
  tmp_targetFeatTable$mz  <- c(2, as.numeric(NA))
  tmp_uROI                <- input_uROI_adv
  tmp_uROI$rtMin          <- c(7., as.numeric(NA))
  tmp_FIR                 <- input_FIR_adv
  tmp_FIR$mzMax           <- c(as.numeric(NA), as.numeric(NA))
  expected_annotation     <- peakPantheRAnnotation(targetFeatTable=tmp_targetFeatTable, uROI=tmp_uROI, FIR=tmp_FIR, uROIExist=FALSE)
  expected_message        <- c("NA in uROI, uROIExist is set to FALSE\n", "New peakPantheRAnnotation object initialised for 2 compounds\n")
  
  # results (output, warnings and messages)
  result_load1        <- evaluate_promise(peakPantheR_loadAnnotationParamsCSV(savePath3, verbose=TRUE))
  
  # Check result
  expect_equal(result_load1$result, expected_annotation)
  
  # Check result messages
  expect_equal(length(result_load1$messages), 2)
  expect_equal(result_load1$messages, expected_message)
  
  ## no verbose
  result_load2        <- evaluate_promise(peakPantheR_loadAnnotationParamsCSV(savePath3, verbose=FALSE))
  expect_equal(length(result_load2$messages), 0)
})

test_that('raise errors - peakPantheR_loadAnnotationParamsCSV', {
  # file doesn't exist
  noFile  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
  msg1    <- paste('specified "CSVParamPath" does not exist', sep='')
  expect_error(peakPantheR_loadAnnotationParamsCSV(noFile, verbose=TRUE), msg1, fixed=TRUE)
})

test_that('prepare_basic_target_parameters', {
  # expected
  expected_targetFeatTable  <- input_targetFeatTable_spl
  expected_uROI             <- data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=FALSE)
  expected_FIR              <- data.frame(rtMin=numeric(), rtMax=numeric(), mzMin=numeric(), mzMax=numeric(), stringsAsFactors=FALSE)
  expected_uROIExist        <- FALSE

  # results (output, warnings and messages)
  result_prepare1     <- evaluate_promise(prepare_basic_target_parameters(input_CSV_spl))
  
  # Check result
  expect_equal(result_prepare1$result$targetFeatTable, expected_targetFeatTable)
  expect_equal(result_prepare1$result$uROI, expected_uROI)
  expect_equal(result_prepare1$result$FIR, expected_FIR)
  expect_equal(result_prepare1$result$uROIExist, expected_uROIExist)
  
  # Check result messages
  expect_equal(length(result_prepare1$messages), 0)
})

test_that('raise errors - prepare_basic_target_parameters', {
  # expected columns are missing
  wrongCol_spl  <- data.frame(matrix(nrow=2,ncol=2,dimnames=list(c(), c('wrongCol','notAColumn'))))
  msg2          <- paste('Columns in "CSVParamPath" must be: "cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"')
  expect_error(prepare_basic_target_parameters(wrongCol_spl), msg2, fixed=TRUE)
  
  # ROI rtMin > rtMax
  wrongROIrt_spl        <- input_CSV_spl
  wrongROIrt_spl$rtMin  <- c(5, 19)
  msg3                  <- paste('Check ROI values: "rtMin" < "rtMax" and "mzMin" < "mzMax"')
  expect_error(prepare_basic_target_parameters(wrongROIrt_spl), msg3, fixed=TRUE)
  
  # ROI mzMin > mzMax
  wrongROImz_spl        <- input_CSV_spl
  wrongROImz_spl$mzMin  <- c(5, 23)
  msg4                  <- paste('Check ROI values: "rtMin" < "rtMax" and "mzMin" < "mzMax"')
  expect_error(prepare_basic_target_parameters(wrongROImz_spl), msg4, fixed=TRUE)
  
  # ROI rtMin is NA
  wrongROIrtMin        <- input_CSV_spl
  wrongROIrtMin$rtMin  <- c(NA, 19)
  msg4                  <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_basic_target_parameters(wrongROIrtMin), msg4, fixed=TRUE)
  # ROI rtMax is NA
  wrongROIrtMax        <- input_CSV_spl
  wrongROIrtMax$rtMax  <- c(4, NA)
  msg5                  <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_basic_target_parameters(wrongROIrtMax), msg5, fixed=TRUE)
  # ROI mzMin is NA
  wrongROImzMin        <- input_CSV_spl
  wrongROImzMin$mzMin  <- c(NA, 21)
  msg6                  <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_basic_target_parameters(wrongROImzMin), msg6, fixed=TRUE)
  # ROI mzMax is NA
  wrongROImzMax        <- input_CSV_spl
  wrongROImzMax$mzMax  <- c(6, NA)
  msg7                  <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_basic_target_parameters(wrongROImzMax), msg7, fixed=TRUE)
})

test_that('prepare_advanced_target_parameters', {
  # expected
  expected_targetFeatTable  <- input_targetFeatTable_adv
  expected_uROI             <- input_uROI_adv
  expected_FIR              <- input_FIR_adv
  expected_uROIExist        <- TRUE
  expected_message          <- c("uROIExist set to TRUE\n")
  
  
  # results (output, warnings and messages)
  result_prepare1     <- evaluate_promise(prepare_advanced_target_parameters(input_CSV_adv, verbose=TRUE))
  
  # Check result
  expect_equal(result_prepare1$result$targetFeatTable, expected_targetFeatTable)
  expect_equal(result_prepare1$result$uROI, expected_uROI)
  expect_equal(result_prepare1$result$FIR, expected_FIR)
  expect_equal(result_prepare1$result$uROIExist, expected_uROIExist)
  
  # Check result messages
  expect_equal(length(result_prepare1$messages), 1)
  expect_equal(result_prepare1$messages, expected_message)
})

test_that('raise errors - prepare_advanced_target_parameters', {

  # expected columns are missing
  wrongCol_adv  <- data.frame(matrix(nrow=2,ncol=2,dimnames=list(c(), c('wrongCol','notAColumn'))))
  msg5          <- paste('Columns in "CSVParamPath" must be: "cpdID", "cpdName", "ROI_rt", "ROI_mz", "ROI_rtMin", "ROI_rtMax", "ROI_mzMin", "ROI_mzMax", "uROI_rtMin", "uROI_rtMax", "uROI_mzMin", "uROI_mzMax", "uROI_rt", "uROI_mz", "FIR_rtMin", "FIR_rtMax", "FIR_mzMin", "FIR_mzMax"')
  expect_error(prepare_advanced_target_parameters(wrongCol_adv, verbose=TRUE), msg5, fixed=TRUE)
  
  # ROI rtMin > rtMax
  wrongROIrt_adv            <- input_CSV_adv
  wrongROIrt_adv$ROI_rtMin  <- c(5, 19)
  msg6                      <- paste('Check ROI values: "rtMin" < "rtMax" and "mzMin" < "mzMax"')
  expect_error(prepare_advanced_target_parameters(wrongROIrt_adv, verbose=FALSE), msg6, fixed=TRUE)
  
  # ROI mzMin > mzMax
  wrongROImz_adv            <- input_CSV_adv
  wrongROImz_adv$ROI_mzMin  <- c(5, 23)
  msg7                      <- paste('Check ROI values: "rtMin" < "rtMax" and "mzMin" < "mzMax"')
  expect_error(prepare_advanced_target_parameters(wrongROImz_adv, verbose=FALSE), msg7, fixed=TRUE)
  
  # ROI rtMin is NA
  wrongROIrtMin             <- input_CSV_adv
  wrongROIrtMin$ROI_rtMin   <- c(NA, 19)
  msg10                     <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_advanced_target_parameters(wrongROIrtMin, verbose=FALSE), msg10, fixed=TRUE)
  # ROI rtMax is NA
  wrongROIrtMax             <- input_CSV_adv
  wrongROIrtMax$ROI_rtMax   <- c(4, NA)
  msg11                     <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_advanced_target_parameters(wrongROIrtMax, verbose=FALSE), msg11, fixed=TRUE)
  # ROI mzMin is NA
  wrongROImzMin             <- input_CSV_adv
  wrongROImzMin$ROI_mzMin   <- c(NA, 21)
  msg12                     <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_advanced_target_parameters(wrongROImzMin, verbose=FALSE), msg12, fixed=TRUE)
  # ROI mzMax is NA
  wrongROImzMax             <- input_CSV_adv
  wrongROImzMax$ROI_mzMax   <- c(6, NA)
  msg13                     <- paste('Check ROI values: "ROI$rtMin", "ROI$rtMax", "ROI$mzMin" and "ROI$mzMax" cannot be NA')
  expect_error(prepare_advanced_target_parameters(wrongROImzMax, verbose=FALSE), msg13, fixed=TRUE)

  # uROI rtMin > rtMax
  wrongUROIrt               <- input_CSV_adv
  wrongUROIrt$uROI_rtMin    <- c(7, 25)
  msg8                      <- paste('Check uROI values: "rtMin" < "rtMax" and "mzMin" < "mzMax"')
  expect_error(prepare_advanced_target_parameters(wrongUROIrt, verbose=FALSE), msg8, fixed=TRUE)
  
  # uROI mzMin < mzMax
  wrongUROImz               <- input_CSV_adv
  wrongUROImz$uROI_mzMin    <- c(12, 25)
  msg9                      <- paste('Check uROI values: "rtMin" < "rtMax" and "mzMin" < "mzMax"')
  expect_error(prepare_advanced_target_parameters(wrongUROImz, verbose=FALSE), msg9, fixed=TRUE)
})
