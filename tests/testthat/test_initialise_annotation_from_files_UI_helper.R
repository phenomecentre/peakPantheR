context('initialise_annotation_from_files_UI_helper()')

## Test the initialisation of a full annotation object from parameter files and metadata paths
# No need to check different CSV format as this is managed and tested in peakPantheR_loadAnnotationParamsCSV()
# In order to be more understandable by the user, we raise a warning for cpd and spectra metadata size (instead of catching/crashing it in peakPantherAnnotation_validObject())
# If cpd or spectraMetadata files do not exist, raise an error

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
cpdMetaPath           <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
utils::write.csv(input_cpdMetadata, file=cpdMetaPath, row.names=FALSE)


## Expected result
# targetFeatTable
input_targetFeatTable_adv     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_targetFeatTable_adv[1,] <- c("ID-1", "Cpd 1",  3.,  1.,  4.,  5.,  2.,  6.)
input_targetFeatTable_adv[2,] <- c("ID-2", "Cpd 2", 19., 17., 20., 21., 18., 22.)
input_targetFeatTable_adv[,c(3:8)] <- sapply(input_targetFeatTable_adv[,c(3:8)], as.numeric)

# uROI
input_uROI_adv      <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_uROI_adv[1,]  <- c( 7., 11. , 8.,  9., 12., 10.)
input_uROI_adv[2,]  <- c(23., 27., 24., 25., 28., 26.)

# FIR
input_FIR_adv       <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=FALSE)
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


test_that('parameters, spectraPaths, no metadata, verbose, no verbose', {
    # expected
    expected_annotation <- input_annotation_nometa
    expected_message    <- "An object of class peakPantheRAnnotation\n 2 compounds in 3 samples. \n  updated ROI exist (uROI)\n  does not use updated ROI (uROI)\n  does not use fallback integration regions (FIR)\n  is not annotated"

    # results (output, warnings and messages)
    result_load1        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                     cpdMetadataPath = NULL,
                                                                                     spectraMetadata = NULL,
                                                                                     verbose = TRUE))

    # Check result
    expect_equal(result_load1$result, expected_annotation)

    # Check result messages (in output)
    expect_equal(length(result_load1$output), 1)
    expect_equal(result_load1$output, expected_message)

    ## no verbose
    result_load2        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                     cpdMetadataPath = NULL,
                                                                                     spectraMetadata = NULL,
                                                                                     verbose = FALSE))
    expect_equal(length(result_load2$messages), 0)
    expect_equal(result_load2$output, "")
})

test_that('parameters, spectraPaths, with correct metadata, verbose, no verbose', {
    # expected
    expected_annotation <- input_annotation_meta
    expected_message    <- "An object of class peakPantheRAnnotation\n 2 compounds in 3 samples. \n  updated ROI exist (uROI)\n  does not use updated ROI (uROI)\n  does not use fallback integration regions (FIR)\n  is not annotated"

    # results (output, warnings and messages)
    result_load1        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                       cpdMetadataPath = cpdMetaPath,
                                                                                       spectraMetadata = input_spectraMetadata,
                                                                                       verbose = TRUE))

    # Check result
    expect_equal(result_load1$result, expected_annotation)

    # Check result messages
    expect_equal(length(result_load1$output), 1)
    expect_equal(result_load1$output, expected_message)

    ## no verbose
    result_load2        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                       cpdMetadataPath = cpdMetaPath,
                                                                                       spectraMetadata = input_spectraMetadata,
                                                                                       verbose = FALSE))
    expect_equal(length(result_load2$messages), 0)
    expect_equal(result_load2$output, "")
})

test_that('parameters, spectraPaths, with wrong sized metadata, verbose, no verbose', {
    # input
    wrong_spectraMetadata <- data.frame(matrix(data=c('a','b'), nrow=2, ncol=1, dimnames=list(c(),c('testcol')), byrow=TRUE), stringsAsFactors=FALSE)
    wrong_cpdMetadata     <- data.frame(matrix(data=c('a','b','c'), nrow=3, ncol=1, dimnames=list(c(),c('testcol')), byrow=TRUE), stringsAsFactors=FALSE)
    cpdMetaPath2          <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
    utils::write.csv(wrong_cpdMetadata, file=cpdMetaPath2, row.names=FALSE)

    # expected (no metadata version as meta files are rejected based on number of rows)
    expected_annotation <- input_annotation_nometa
    expected_output     <- "An object of class peakPantheRAnnotation\n 2 compounds in 3 samples. \n  updated ROI exist (uROI)\n  does not use updated ROI (uROI)\n  does not use fallback integration regions (FIR)\n  is not annotated"
    expected_message    <- c("Warning: cpdMetadata number of rows (3) does not match the number of compounds targeted (2)\n",
                           "Warning: spectraMetadata number of rows (2) does not match the number of compounds targeted (3)\n")

    # results (output, warnings and messages)
    result_load1        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                     cpdMetadataPath = cpdMetaPath2,
                                                                                     spectraMetadata = wrong_spectraMetadata,
                                                                                     verbose = TRUE))

    # Check result
    expect_equal(result_load1$result, expected_annotation)

    # Check output (show object)
    expect_equal(length(result_load1$output), 1)
    expect_equal(result_load1$output, expected_output)
    # Check message (from metadata number of rows) /!\ 3rd message is only '\n'
    expect_equal(length(result_load1$message), 3)
    expect_equal(result_load1$message[1:2], expected_message)

    ## no verbose
    result_load2        <- evaluate_promise(initialise_annotation_from_files_UI_helper(paramPath, input_spectraPaths,
                                                                                     cpdMetadataPath = cpdMetaPath2,
                                                                                     spectraMetadata = wrong_spectraMetadata,
                                                                                     verbose = FALSE))
    # Check output (show object, return only an empty value)
    expect_equal(length(result_load2$output), 1)
    expect_equal(result_load2$output, "")
    # Check message (from metadata number of rows)
    expect_equal(length(result_load2$message), 2)
    expect_equal(result_load2$message, expected_message)
})

test_that('raise errors', {
    # loadAnnotationParamCSV() error
    # no spectraPaths is safe (spectraPaths = NULL, init with 0 samples)


    noFile  <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')

    # CSVParam file doesn't exist (loadAnnotationParamCSV error)
    msg1    <- paste('specified "CSVParamPath" does not exist', sep='')
    expect_error(initialise_annotation_from_files_UI_helper(noFile, NULL, verbose=TRUE), msg1, fixed=TRUE)

    # cpdMetadata file doesn't exist
    msg2    <- paste('Error: cpdMetadata file does not exist', sep='')
    expect_error(initialise_annotation_from_files_UI_helper(paramPath, NULL, cpdMetadataPath=noFile, verbose=TRUE), msg2, fixed=TRUE)

    # spectraMetadata is not a DataFrame
    msg3    <- paste('Error: spectraMetadata is not a DataFrame', sep='')
    expect_error(initialise_annotation_from_files_UI_helper(paramPath, NULL, spectraMetadata='notADataFrame', verbose=TRUE), msg3, fixed=TRUE)

})
