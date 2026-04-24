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


test_that('load RData with single peakPantheRAnnotation', {
    expected    <- annotationObject

    result_load <- evaluate_promise(load_annotation_from_file_UI_helper(
        annotationPath = annotationPath))
    expect_equal(result_load$result, expected)
    expect_equal(length(result_load$messages), 0)
    expect_equal(result_load$output, "")
})

test_that('RData object found by class regardless of variable name', {
    wrongNameObject <- annotationObject
    wrongName_path  <- tempfile(pattern="file", tmpdir=tempdir(),
        fileext='.RData')
    save(wrongNameObject, file=wrongName_path, compress=TRUE)

    result <- load_annotation_from_file_UI_helper(
        annotationPath = wrongName_path)
    expect_equal(result, annotationObject)
})

test_that('load RDS file', {
    rdsPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.rds')
    saveRDS(annotationObject, file=rdsPath)

    result <- load_annotation_from_file_UI_helper(annotationPath = rdsPath)
    expect_equal(result, annotationObject)
})

test_that('objectName selects among multiple candidates', {
    annot1 <- annotationObject
    annot2 <- annotationObject
    multiPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')
    save(annot1, annot2, file=multiPath, compress=TRUE)

    result <- load_annotation_from_file_UI_helper(annotationPath = multiPath,
        objectName = "annot2")
    expect_equal(result, annot2)
})

test_that('multiple candidates without objectName returns candidate list', {
    annot1 <- annotationObject
    annot2 <- annotationObject
    multiPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')
    save(annot1, annot2, file=multiPath, compress=TRUE)

    result <- load_annotation_from_file_UI_helper(annotationPath = multiPath)
    expect_true(is(result, "peakPantheRAnnotation_candidates"))
    expect_equal(length(result), 2)
    expect_true(all(c("annot1", "annot2") %in% names(result)))
})

test_that('raise errors', {
    noFile <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')

    notAnnot <- 'notAPeakPantheRAnnotation'
    wrongObject_path <- tempfile(pattern="file", tmpdir=tempdir(),
        fileext='.RData')
    save(notAnnot, file=wrongObject_path, compress=TRUE)

    rdsWrongPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.rds')
    saveRDS("not an annotation", file=rdsWrongPath)

    multiPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')
    annot1 <- annotationObject
    annot2 <- annotationObject
    save(annot1, annot2, file=multiPath, compress=TRUE)

    expect_error(load_annotation_from_file_UI_helper(annotationPath = noFile),
        "annotation file does not exist", fixed=TRUE)

    expect_error(load_annotation_from_file_UI_helper(
        annotationPath = wrongObject_path),
        "no `peakPantheRAnnotation` object found in the RData file",
        fixed=TRUE)

    expect_error(load_annotation_from_file_UI_helper(
        annotationPath = rdsWrongPath),
        "the RDS file does not contain a `peakPantheRAnnotation`",
        fixed=TRUE)

    expect_error(load_annotation_from_file_UI_helper(
        annotationPath = multiPath, objectName = "noSuchName"),
        "requested object 'noSuchName' is not a `peakPantheRAnnotation`",
        fixed=TRUE)
})
