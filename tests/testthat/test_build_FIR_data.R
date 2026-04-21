context('build_FIR_data()')

skip_if_not_installed('faahKO', minimum_version = '1.18.0')
library(faahKO)

# Shared setup: real mzR/MSnbase handle and ROI EIC data
raw_data <- MSnbase::readMSData(
    system.file('cdf/KO/ko15.CDF', package = "faahKO"),
    centroided = TRUE, mode = 'onDisk')

# Two target windows with real signal (reused from other integrateFIR tests)
targetFeatTable <- data.frame(
    rtMin = c(3309.7589296586070, 3670.9201232710743),
    rt    = c(3346.8277590361445, 3704.1427831325304),
    rtMax = c(3385.4098874628098, 3740.0172511251831),
    mzMin = c(522.1995, 536.1995),
    mz    = c(522.2,    536.2),
    mzMax = c(522.2005, 536.2005),
    stringsAsFactors = FALSE)

ROIsDataPoint <- extractSignalRawData(raw_data,
    rt = targetFeatTable[, c("rtMin", "rtMax")],
    mz = targetFeatTable[, c("mzMin", "mzMax")],
    verbose = FALSE)


test_that('needsFilling empty returns list() silently', {
    FIR <- targetFeatTable[, c("mzMin", "mzMax", "rtMin", "rtMax")]
    res <- evaluate_promise(build_FIR_data(raw_data, ROIsDataPoint,
        targetFeatTable, FIR, needsFilling_idx = integer(0), verbose = TRUE))
    expect_equal(res$result, list())
    expect_equal(length(res$messages), 0)
})


test_that('all-contained: FIR subset of ROI reuses in-memory data', {
    # FIR bounds equal ROI bounds -> contained
    FIR <- targetFeatTable[, c("mzMin", "mzMax", "rtMin", "rtMax")]

    res <- evaluate_promise(build_FIR_data(raw_data, ROIsDataPoint,
        targetFeatTable, FIR, needsFilling_idx = c(1, 2), verbose = TRUE))

    # Results match what a fresh extract would produce
    fresh <- extractSignalRawData(raw_data,
        mz = data.frame(mzMin = FIR$mzMin, mzMax = FIR$mzMax),
        rt = data.frame(rtMin = FIR$rtMin, rtMax = FIR$rtMax),
        verbose = FALSE)
    expect_equal(res$result, fresh)

    # One reuse message, no extract-side messages
    expect_equal(length(res$messages), 1)
    expect_equal(res$messages[1],
        "FIR data reused from ROI for 2/2 windows\n")
})


test_that('none-contained: falls back to single batched extract', {
    # FIR wider than ROI -> not contained
    FIR <- targetFeatTable[, c("mzMin", "mzMax", "rtMin", "rtMax")]
    FIR$mzMin <- FIR$mzMin - 0.01
    FIR$mzMax <- FIR$mzMax + 0.01

    res <- evaluate_promise(build_FIR_data(raw_data, ROIsDataPoint,
        targetFeatTable, FIR, needsFilling_idx = c(1, 2), verbose = TRUE))

    fresh <- extractSignalRawData(raw_data,
        mz = data.frame(mzMin = FIR$mzMin, mzMax = FIR$mzMax),
        rt = data.frame(rtMin = FIR$rtMin, rtMax = FIR$rtMax),
        verbose = FALSE)
    expect_equal(res$result, fresh)

    # No reuse message; standard extract messages present
    reuse_msgs <- grep("reused from ROI", res$messages, value = TRUE)
    expect_equal(length(reuse_msgs), 0)
    read_msgs <- grep("Reading data from 2 windows", res$messages,
        value = TRUE)
    expect_equal(length(read_msgs), 1)
})


test_that('mixed: reuse one row, extract the other', {
    FIR <- targetFeatTable[, c("mzMin", "mzMax", "rtMin", "rtMax")]
    # row 1 contained (equal bounds), row 2 wider than ROI
    FIR$mzMin[2] <- FIR$mzMin[2] - 0.01
    FIR$mzMax[2] <- FIR$mzMax[2] + 0.01

    res <- evaluate_promise(build_FIR_data(raw_data, ROIsDataPoint,
        targetFeatTable, FIR, needsFilling_idx = c(1, 2), verbose = TRUE))

    fresh <- extractSignalRawData(raw_data,
        mz = data.frame(mzMin = FIR$mzMin, mzMax = FIR$mzMax),
        rt = data.frame(rtMin = FIR$rtMin, rtMax = FIR$rtMax),
        verbose = FALSE)
    expect_equal(res$result, fresh)

    # Reuse message reports 1/2; extract ran for the remaining window
    expect_true(any(res$messages ==
        "FIR data reused from ROI for 1/2 windows\n"))
    expect_true(any(res$messages == "Reading data from 1 windows\n"))
})
