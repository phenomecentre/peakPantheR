context('cached re-annotation')

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

init_annotation <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
    targetFeatTable = input_targetFeatTable)


test_that('second run reuses cached EIC data and skips disk reads', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    expect_true(isAnnotated(first$annotation))

    res <- evaluate_promise(peakPantheR_parallelAnnotation(first$annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))

    reuse_msgs <- grep("Reusing cached EIC data",
        res$messages, value = TRUE)
    read_msgs <- grep("Reading data from",
        res$messages, value = TRUE)
    expect_equal(length(reuse_msgs), 2)
    expect_equal(length(read_msgs), 0)

    # peakTables identical across runs
    expect_equal(peakTables(res$result$annotation),
                    peakTables(first$annotation))
})


test_that('cache miss (bounds outside cache) falls back to disk read', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)

    # Drop one cached data.frame to simulate a miss on file 1
    broken <- first$annotation
    broken@dataPoints[[1]][[1]] <- broken@dataPoints[[1]][[1]][0, , drop = FALSE]

    res <- evaluate_promise(peakPantheR_parallelAnnotation(broken,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))

    reuse_msgs <- grep("Reusing cached EIC data",
        res$messages, value = TRUE)
    read_msgs <- grep("Reading data from",
        res$messages, value = TRUE)
    # File 1 falls back to read; file 2 reuses
    expect_equal(length(reuse_msgs), 1)
    expect_gte(length(read_msgs), 1)
})


test_that('cache_bounds_ok returns FALSE for empty or misshaped cache', {
    tbl <- input_targetFeatTable
    roi <- tbl[, c("rtMin", "rtMax", "mzMin", "mzMax")]
    expect_false(cache_bounds_ok(NULL, tbl, roi))
    expect_false(cache_bounds_ok(list(), tbl, roi))
    empty_dp <- list(
        data.frame(rt = numeric(), mz = numeric(), int = numeric()),
        data.frame(rt = numeric(), mz = numeric(), int = numeric()))
    expect_false(cache_bounds_ok(empty_dp, tbl, roi))
})


test_that('cache hit when uROI narrows inside ROI; miss when it widens', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    annot <- first$annotation

    # Narrow uROI (inside cached ROI) → cache hit, no disk read
    narrow <- annot
    narrow@uROI <- data.frame(
        rtMin = ROI(annot)$rtMin + 5,
        rt    = ROI(annot)$rt,
        rtMax = ROI(annot)$rtMax - 5,
        mzMin = ROI(annot)$mzMin,
        mz    = ROI(annot)$mz,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    narrow@useUROI <- TRUE
    narrow@uROIExist <- TRUE
    res_narrow <- evaluate_promise(peakPantheR_parallelAnnotation(narrow,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))
    expect_equal(length(grep("Reading data from", res_narrow$messages)), 0)
    expect_gte(length(grep("Reusing cached EIC data",
        res_narrow$messages)), 1)

    # Widen uROI outside cached ROI → cache miss, disk read
    wide <- annot
    wide@uROI <- data.frame(
        rtMin = ROI(annot)$rtMin - 50,
        rt    = ROI(annot)$rt,
        rtMax = ROI(annot)$rtMax + 50,
        mzMin = ROI(annot)$mzMin,
        mz    = ROI(annot)$mz,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    wide@useUROI <- TRUE
    wide@uROIExist <- TRUE
    res_wide <- evaluate_promise(peakPantheR_parallelAnnotation(wide,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))
    expect_gte(length(grep("Reading data from", res_wide$messages)), 1)
})


test_that('FIR inside ROI but outside uROI is served from cache (no disk read)', {
    # First run populates @dataPoints at ROI bounds
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    annot <- first$annotation

    # Narrow uROI to a region with no peak signal so the refit records
    # found=FALSE and FIR integration kicks in.
    annot@uROI <- data.frame(
        rtMin = c(3355, 3710),
        rt    = c(3357.5, 3712.5),
        rtMax = c(3360, 3715),
        mzMin = ROI(annot)$mzMin,
        mz    = ROI(annot)$mz,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    annot@useUROI   <- TRUE
    annot@uROIExist <- TRUE

    # FIR sits within @ROI but outside the narrow uROI above. Before the fix
    # this would error in build_FIR_data (no raw_data in cache mode); after
    # the fix the wider @ROI-wide cache is used as the envelope.
    annot@FIR <- data.frame(
        rtMin = ROI(annot)$rtMin,
        rtMax = ROI(annot)$rtMax,
        mzMin = ROI(annot)$mzMin,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    annot@useFIR <- TRUE

    res <- evaluate_promise(peakPantheR_parallelAnnotation(annot,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))

    # No disk reads, cache reused, FIR filled the not-found rows.
    expect_equal(length(grep("Reading data from", res$messages)), 0)
    expect_gte(length(grep("Reusing cached EIC data", res$messages)), 1)
    pt <- peakTables(res$result$annotation)
    expect_true(all(vapply(pt, function(x) all(x$is_filled),
        FUN.VALUE = logical(1))))
})


test_that('build_FIR_data errors when raw_data NULL and FIR not contained', {
    raw_data <- MSnbase::readMSData(input_spectraPaths[1],
        centroided = TRUE, mode = 'onDisk')
    tbl <- input_targetFeatTable
    cached_dp <- extractSignalRawData(raw_data,
        rt = tbl[, c("rtMin", "rtMax")],
        mz = tbl[, c("mzMin", "mzMax")], verbose = FALSE)
    # FIR row 1 widened beyond target bounds → not contained
    FIR_bad <- data.frame(
        rtMin = c(3336.542, 3689.7),
        rtMax = c(3390.272, 3738.213),
        mzMin = c(522.1995 - 10, 536.1995),
        mzMax = c(522.2005 + 10, 536.2005))

    expect_error(
        peakPantheR:::build_FIR_data(raw_data = NULL,
            ROIsDataPoint = cached_dp, ROI = tbl,
            FIR = FIR_bad, needsFilling_idx = c(1L, 2L), verbose = FALSE),
        regexp = "Cached annotation in use")
})
