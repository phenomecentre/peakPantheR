context('load_annotation_from_file_UI_helper()')


## Input data
# spectraPath
input_spectraPaths    <- c('./path/file1', './path/file2', './path/file3')

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

# correct annotation
annotationObject <- peakPantheRAnnotation(targetFeatTable = input_targetFeatTable_adv,
                                          spectraPaths = input_spectraPaths,
                                          uROI = input_uROI_adv,
                                          FIR = input_FIR_adv,
                                          uROIExist = TRUE)

# temporary files
annotationPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')
save(annotationObject, file=annotationPath, compress=TRUE)


test_that('load correct file', {
    # expected
    expected    <- annotationObject

    # results (output, warnings and messages)
    result_load <- evaluate_promise(load_annotation_from_file_UI_helper(annotationPath = annotationPath))
    # Check result
    expect_equal(result_load$result, expected)

    # Check result messages (in output)
    expect_equal(length(result_load$messages), 0)
    expect_equal(result_load$output, "")
})

test_that('raise errors', {

    noFile          <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')

    wrongNameObject <- annotationObject
    wongName_path   <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')
    save(wrongNameObject, file=wongName_path, compress=TRUE)

    annotationObject    <- 'notAPeakPantheRAnnotation'
    wongObject_path     <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')
    save(annotationObject, file=wongObject_path, compress=TRUE)

    # file doesn't exist
    msg1    <- paste('Error: annotation file does not exist', sep='')
    expect_error(load_annotation_from_file_UI_helper(annotationPath = noFile), msg1, fixed=TRUE)

    # wrong name in .RData
    msg2    <- paste("Error: annotation file must contain a `peakPantheRAnnotaiton` named 'annotationObject'", sep='')
    expect_error(load_annotation_from_file_UI_helper(annotationPath = wongName_path), msg2, fixed=TRUE)

    # not a peakPantheRAnnotation
    msg3    <- paste("Error: the variable loaded is not a `peakPantheRAnnotation`", sep='')
    expect_error(load_annotation_from_file_UI_helper(annotationPath = wongObject_path), msg3, fixed=TRUE)
})
