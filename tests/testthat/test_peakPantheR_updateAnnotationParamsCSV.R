context('peakPantheR_updateAnnotationParamsCSV()')

skip_if_not_installed('faahKO', minimum_version = '1.18.0')
library(faahKO)

BPPARAM_serial <- BiocParallel::SerialParam()

input_spectraPaths <- c(
    system.file('cdf/KO/ko15.CDF', package = "faahKO"),
    system.file('cdf/KO/ko16.CDF', package = "faahKO"))

input_targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
    c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
    stringsAsFactors = FALSE)
input_targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390.,
    522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3670., 3701.697, 3745.,
    536.194638, 536.2, 536.205362)
input_targetFeatTable[, 3:8] <- sapply(input_targetFeatTable[, 3:8], as.numeric)


# helper: write a basic-format CSV with narrowed bounds
write_basic_csv <- function(path, tbl) {
    utils::write.csv(tbl, file = path, row.names = FALSE)
}

# helper: write an advanced-format CSV from ROI / uROI / FIR data.frames
write_advanced_csv <- function(path, roi, uroi, fir, cpdID, cpdName) {
    df <- data.frame(
        cpdID = cpdID, cpdName = cpdName,
        ROI_rt = roi$rt, ROI_mz = roi$mz,
        ROI_rtMin = roi$rtMin, ROI_rtMax = roi$rtMax,
        ROI_mzMin = roi$mzMin, ROI_mzMax = roi$mzMax,
        uROI_rtMin = uroi$rtMin, uROI_rtMax = uroi$rtMax,
        uROI_mzMin = uroi$mzMin, uROI_mzMax = uroi$mzMax,
        uROI_rt = uroi$rt, uROI_mz = uroi$mz,
        FIR_rtMin = fir$rtMin, FIR_rtMax = fir$rtMax,
        FIR_mzMin = fir$mzMin, FIR_mzMax = fir$mzMax,
        stringsAsFactors = FALSE)
    utils::write.csv(df, file = path, row.names = FALSE)
}


test_that('basic-format CSV updates uROI/FIR, leaves ROI and cache slots', {
    init <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
        targetFeatTable = input_targetFeatTable)
    annotated <- peakPantheR_parallelAnnotation(init,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE,
        verbose = FALSE)$annotation

    # narrow bounds for both compounds
    new_tbl <- input_targetFeatTable
    new_tbl$rtMin <- new_tbl$rtMin + 10
    new_tbl$rtMax <- new_tbl$rtMax - 10

    tmpCSV <- tempfile(fileext = '.csv')
    write_basic_csv(tmpCSV, new_tbl)

    updated <- peakPantheR_updateAnnotationParamsCSV(annotated, tmpCSV,
                                                        verbose = FALSE)

    # uROI and FIR overlaid; ROI left untouched (extraction envelope stable)
    expect_equal(updated@uROI$rtMin, new_tbl$rtMin)
    expect_equal(updated@uROI$rtMax, new_tbl$rtMax)
    expect_equal(updated@FIR$rtMin, new_tbl$rtMin)
    expect_equal(updated@FIR$rtMax, new_tbl$rtMax)
    expect_equal(updated@ROI$rtMin, annotated@ROI$rtMin)
    expect_equal(updated@ROI$rtMax, annotated@ROI$rtMax)
    expect_true(updated@uROIExist)

    # Cache-bearing slots untouched (identity with pre-update object)
    expect_identical(updated@dataPoints, annotated@dataPoints)
    expect_identical(updated@TIC, annotated@TIC)
    expect_identical(updated@acquisitionTime, annotated@acquisitionTime)
    expect_identical(updated@peakTables, annotated@peakTables)
    expect_identical(updated@peakFit, annotated@peakFit)
    expect_true(isAnnotated(updated))
})


test_that('basic-format CSV with target = "ROI" is the legacy path', {
    init <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
        targetFeatTable = input_targetFeatTable)

    new_tbl <- input_targetFeatTable
    new_tbl$rtMin <- new_tbl$rtMin + 5
    new_tbl$rtMax <- new_tbl$rtMax - 5
    tmpCSV <- tempfile(fileext = '.csv')
    write_basic_csv(tmpCSV, new_tbl)

    expect_message(
        updated <- peakPantheR_updateAnnotationParamsCSV(init, tmpCSV,
            target = "ROI", verbose = FALSE),
        regexp = 'deprecated')
    expect_equal(updated@ROI$rtMin, new_tbl$rtMin)
    expect_equal(updated@ROI$rtMax, new_tbl$rtMax)
})


test_that('advanced-format CSV updates ROI, uROI, FIR and uROIExist', {
    init <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
        targetFeatTable = input_targetFeatTable)

    new_roi <- data.frame(
        rtMin = c(3315, 3675), rt = c(3344.888, 3701.697),
        rtMax = c(3385, 3740),
        mzMin = c(522.1948, 536.1946), mz = c(522.2, 536.2),
        mzMax = c(522.2052, 536.2054))
    new_uroi <- data.frame(
        rtMin = c(3320, 3680), rt = c(3344.888, 3701.697),
        rtMax = c(3380, 3735),
        mzMin = c(522.1949, 536.1947), mz = c(522.2, 536.2),
        mzMax = c(522.2051, 536.2053))
    new_fir <- data.frame(
        rtMin = c(3325, 3685), rtMax = c(3375, 3730),
        mzMin = c(522.1950, 536.1948), mzMax = c(522.2050, 536.2052))

    tmpCSV <- tempfile(fileext = '.csv')
    write_advanced_csv(tmpCSV, new_roi, new_uroi, new_fir,
                        cpdID = c("ID-1", "ID-2"),
                        cpdName = c("Cpd 1", "Cpd 2"))

    updated <- peakPantheR_updateAnnotationParamsCSV(init, tmpCSV,
                                                        verbose = FALSE)

    expect_equal(updated@ROI$rtMin, new_roi$rtMin)
    expect_equal(updated@uROI$rtMax, new_uroi$rtMax)
    expect_equal(updated@FIR$mzMin, new_fir$mzMin)
    expect_true(updated@uROIExist)
})


test_that('updated annotation refits through cache (no disk reads)', {
    init <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
        targetFeatTable = input_targetFeatTable)
    annotated <- peakPantheR_parallelAnnotation(init,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE,
        verbose = FALSE)$annotation

    # narrow bounds so they remain inside the cached ROI
    new_tbl <- input_targetFeatTable
    new_tbl$rtMin <- new_tbl$rtMin + 5
    new_tbl$rtMax <- new_tbl$rtMax - 5

    tmpCSV <- tempfile(fileext = '.csv')
    write_basic_csv(tmpCSV, new_tbl)
    updated <- peakPantheR_updateAnnotationParamsCSV(annotated, tmpCSV,
                                                        verbose = FALSE)

    res <- evaluate_promise(peakPantheR_parallelAnnotation(updated,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))

    reuse_msgs <- grep("Reusing cached EIC data",
        res$messages, value = TRUE)
    read_msgs <- grep("Reading data from",
        res$messages, value = TRUE)
    expect_equal(length(reuse_msgs), 2)
    expect_equal(length(read_msgs), 0)
})


test_that('mismatched compound count raises an error', {
    init <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
        targetFeatTable = input_targetFeatTable)

    bad_tbl <- input_targetFeatTable[1, , drop = FALSE]
    tmpCSV <- tempfile(fileext = '.csv')
    write_basic_csv(tmpCSV, bad_tbl)

    expect_error(
        peakPantheR_updateAnnotationParamsCSV(init, tmpCSV, verbose = FALSE),
        regexp = 'CSV compound count')
})


test_that('mismatched cpdID order raises an error', {
    init <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
        targetFeatTable = input_targetFeatTable)

    bad_tbl <- input_targetFeatTable[c(2, 1), ]
    tmpCSV <- tempfile(fileext = '.csv')
    write_basic_csv(tmpCSV, bad_tbl)

    expect_error(
        peakPantheR_updateAnnotationParamsCSV(init, tmpCSV, verbose = FALSE),
        regexp = 'cpdID')
})


test_that('non-existent CSV path raises an error', {
    init <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
        targetFeatTable = input_targetFeatTable)

    expect_error(
        peakPantheR_updateAnnotationParamsCSV(init,
            '/nonexistent/path/file.csv', verbose = FALSE),
        regexp = 'does not exist')
})


test_that('non-peakPantheRAnnotation input raises an error', {
    tmpCSV <- tempfile(fileext = '.csv')
    write_basic_csv(tmpCSV, input_targetFeatTable)

    expect_error(
        peakPantheR_updateAnnotationParamsCSV('not an object', tmpCSV,
            verbose = FALSE),
        regexp = 'peakPantheRAnnotation')
})
