context('initialise_annotation_from_files_UI_helper()')

## Test the initialisation of a full annotation object from parameter files and metadata paths
# Reading parameters is managed by peakPantheR_loadAnnotationParamsCSV()
# Errors on cpd and spectra metadata are catched by peakPantherAnnotation_validObject()

## Input data
input_param      <- data.frame(matrix(nrow=2,ncol=21,dimnames=list(c(), c('cpdID', 'cpdName', 'X', 'ROI_rt', 'ROI_mz','ROI_rtMin', 'ROI_rtMax', 'ROI_mzMin', 'ROI_mzMax', 'X', 'uROI_rtMin', 'uROI_rtMax', 'uROI_mzMin', 'uROI_mzMax', 'uROI_rt', 'uROI_mz', 'X', 'FIR_rtMin', 'FIR_rtMax', 'FIR_mzMin', 'FIR_mzMax'))))
input_param[1,]  <- c('ID-1', 'Cpd 1', '|', 1.,  2.,  3.,  4.,  5.,  6.,  '|', 7.,  8.,  9.,  10., 11., 12., '|', 13., 14., 15., 16.)
input_param[2,]  <- c('ID-2', 'Cpd 2', '|', 17., 18., 19., 20., 21., 22., '|', 23., 24., 25., 26., 27., 28., '|', 29., 30., 31., 32.)
input_param[,-c(1,2,3,10,17)]  <- sapply(input_param[,-c(1,2,3,10,17)], as.numeric)

# spectraPath
input_spectraPaths    <- c('./path/file1', './path/file2', './path/file3')

# spectraMetadata
input_spectraMetadata <- data.frame(matrix(data=c('a','b','c'), nrow=3, ncol=1, dimnames=list(c(),c('testcol')), byrow=TRUE), stringsAsFactors=FALSE)

# cpdMetadata
input_cpdMetadata     <- data.frame(matrix(data=c('a','b'), nrow=2, ncol=1, dimnames=list(c(),c('testcol')), byrow=TRUE), stringsAsFactors=FALSE)

# temporary files
paramPath             <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
utils::write.csv(input_param, file=paramPath, row.names=FALSE)
spectraMetaPath       <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
utils::write.csv(input_spectraMetadata, file=spectraMetaPath, row.names=FALSE)
cpdMetaPath           <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
utils::write.csv(input_cpdMetadata, file=cpdMetaPath, row.names=FALSE)


## Expected result
# targetFeatTable
input_targetFeatTable_adv     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
input_targetFeatTable_adv[1,] <- c("ID-1", "Cpd 1",  3.,  1.,  4.,  5.,  2.,  6.)
input_targetFeatTable_adv[2,] <- c("ID-2", "Cpd 2", 19., 17., 20., 21., 18., 22.)
input_targetFeatTable_adv[,c(3:8)] <- sapply(input_targetFeatTable_adv[,c(3:8)], as.numeric)

# uROI
input_uROI_adv      <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
input_uROI_adv[1,]  <- c( 7., 11. , 8.,  9., 12., 10.)
input_uROI_adv[2,]  <- c(23., 27., 24., 25., 28., 26.)

# FIR
input_FIR_adv       <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=F)
input_FIR_adv[1,]   <- c(13., 14., 15., 16.)
input_FIR_adv[2,]   <- c(29., 30., 31., 32.)

# object
input_annotation_nometa <- peakPantheRAnnotation(targetFeatTable = input_targetFeatTable_adv,
                                                 spectraPaths = input_spectraPaths,
                                                 uROI = input_uROI_adv,
                                                 FIR = input_FIR_adv,
                                                 uROIExist = TRUE)
input_annotation_meta   <- peakPantheRAnnotation(targetFeatTable = input_targetFeatTable_adv,
                                                 spectraPaths = input_spectraPaths, 
                                                 uROI = input_uROI_adv, 
                                                 FIR = input_FIR_adv,
                                                 uROIExist = TRUE,
                                                 cpdMetadata = input_cpdMetadata,
                                                 spectraMetadata = input_spectraMetadata)

# create some metadata, add some fake paths
# make a simple import that should match the input_annotation_adv
# an import with/without metadata
# each import with/without verbose
# trigger errors on the metadata (size, path exist!)

## EITHER VERBOSE IN CREATION OR IN UPDATE, CONTROL USING VERBOSE=TRUE

test_that('parameters, spectraPaths, no metadata, verbose, no verbose', {
  # expected
  expected_annotation <- input_annotation_nometa
  expected_message    <- c("uROIExist set to TRUE\n", "New peakPantheRAnnotation object initialised for 2 compounds\n")
  
  # results (output, warnings and messages)
  result_load1        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                     cpdMetadataPath = NULL,
                                                                                     spectraMetadataPath = NULL,
                                                                                     verbose = TRUE))
  
  # Check result
  expect_equal(result_load1$result, expected_annotation)
  
  # Check result messages
  expect_equal(length(result_load1$messages), 2)
  expect_equal(result_load1$messages, expected_message)
  
  ## no verbose
  result_load2        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                     cpdMetadataPath = NULL,
                                                                                     spectraMetadataPath = NULL,
                                                                                     verbose = FALSE))
  expect_equal(length(result_load2$messages), 0)
})

test_that('parameters, spectraPaths, with metadata, verbose, no verbose', {
  # expected
  expected_annotation <- input_annotation_meta
  expected_message    <- c("uROIExist set to TRUE\n", "New peakPantheRAnnotation object initialised for 2 compounds\n")
  
  # results (output, warnings and messages)
  result_load1        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                     cpdMetadataPath = cpdMetaPath, 
                                                                                     spectraMetadataPath = spectraMetaPath, 
                                                                                     verbose = TRUE))
  
  # Check result
  expect_equal(result_load1$result, expected_annotation)
  
  # Check result messages
  expect_equal(length(result_load1$messages), 2)
  expect_equal(result_load1$messages, expected_message)
  
  ## no verbose
  result_load2        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths, 
                                                                                     cpdMetadataPath = cpdMetaPath, 
                                                                                     spectraMetadataPath = spectraMetaPath, 
                                                                                     verbose = FALSE))
  expect_equal(length(result_load2$messages), 0)
})

test_that('raise errors', {
  # loadAnnotationParamCSV() error
  # no spectraPaths
  #   maybe that's safe
  # cpdMetadataPath doesn't exist
  # cpdMetadata error (dimensions? caught by validateObject?)
  # spectraMetadataPath doesn't exist
  # spectraMetadata error (dimensions? caught by validateObject?)
  
  # file doesn't exist
  noFile  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
  msg1    <- paste('specified "CSVParamPath" does not exist', sep='')
  expect_error(peakPantheR_loadAnnotationParamsCSV(noFile, verbose=TRUE), msg1, fixed=TRUE)
})
