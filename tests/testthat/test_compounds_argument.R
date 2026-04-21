context('compounds argument')

skip_if_not_installed('faahKO', minimum_version = '1.18.0')
library(faahKO)

BPPARAM_serial <- BiocParallel::SerialParam()

input_spectraPaths <- c(
    system.file('cdf/KO/ko15.CDF', package = "faahKO"),
    system.file('cdf/KO/ko16.CDF', package = "faahKO"))

input_targetFeatTable <- data.frame(matrix(vector(), 3, 8, dimnames=list(c(),
    c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
    stringsAsFactors = FALSE)
input_targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390.,
    522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440.,
    496.195038, 496.2, 496.204962)
input_targetFeatTable[3,] <- c("ID-3", "Cpd 3", 3670., 3701.697, 3745.,
    536.194638, 536.2, 536.205362)
input_targetFeatTable[, 3:8] <- sapply(input_targetFeatTable[, 3:8], as.numeric)

init_annotation <- peakPantheRAnnotation(spectraPaths = input_spectraPaths,
    targetFeatTable = input_targetFeatTable)


# -----------------------------------------------------------------------------
# resolve_compound_selector

test_that('resolve_compound_selector returns NULL when compounds is NULL', {
    expect_null(peakPantheR:::resolve_compound_selector(NULL,
        c("A", "B", "C")))
})

test_that('resolve_compound_selector returns integer positions', {
    expect_equal(peakPantheR:::resolve_compound_selector(c("B", "C"),
        c("A", "B", "C")), c(2L, 3L))
})

test_that('resolve_compound_selector preserves request order', {
    expect_equal(peakPantheR:::resolve_compound_selector(c("C", "A"),
        c("A", "B", "C")), c(3L, 1L))
})

test_that('resolve_compound_selector silently deduplicates', {
    expect_equal(peakPantheR:::resolve_compound_selector(c("A", "A", "B"),
        c("A", "B", "C")), c(1L, 2L))
})

test_that('resolve_compound_selector errors on unknown cpdID', {
    expect_error(peakPantheR:::resolve_compound_selector(c("A", "ZZ"),
        c("A", "B", "C")), regexp = "unknown cpdID")
})

test_that('resolve_compound_selector errors on non-character', {
    expect_error(peakPantheR:::resolve_compound_selector(1L,
        c("A", "B", "C")), regexp = "must be a character vector")
})

test_that('resolve_compound_selector errors on empty character', {
    expect_error(peakPantheR:::resolve_compound_selector(character(0),
        c("A", "B", "C")), regexp = "at least one cpdID")
})

test_that('resolve_compound_selector errors on duplicated ids', {
    expect_error(peakPantheR:::resolve_compound_selector("A",
        c("A", "A", "B")), regexp = "cpdID must be unique")
})


# -----------------------------------------------------------------------------
# peakPantheR_parallelAnnotation: compounds merges back into full-width object

test_that('parallelAnnotation compounds refits selected rows only', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    ann <- first$annotation

    res <- peakPantheR_parallelAnnotation(ann, compounds = "ID-2",
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    refit <- res$annotation

    # Shape preserved
    expect_equal(nbCompounds(refit), nbCompounds(ann))
    expect_equal(nbSamples(refit), nbSamples(ann))
    expect_equal(cpdID(refit), cpdID(ann))
    # Unselected rows untouched
    for (s in seq_len(nbSamples(refit))) {
        expect_equal(refit@peakTables[[s]][c(1L, 3L), ],
                        ann@peakTables[[s]][c(1L, 3L), ])
        expect_equal(refit@peakFit[[s]][c(1L, 3L)],
                        ann@peakFit[[s]][c(1L, 3L)])
    }
})

test_that('parallelAnnotation compounds=NULL matches full annotation', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    same <- peakPantheR_parallelAnnotation(first$annotation, compounds = NULL,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    expect_equal(peakTables(same$annotation), peakTables(first$annotation))
})

test_that('parallelAnnotation compounds selecting all cpdIDs == no filter', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    all_ids <- cpdID(first$annotation)
    same <- peakPantheR_parallelAnnotation(first$annotation,
        compounds = all_ids, BPPARAM = BPPARAM_serial, getAcquTime = FALSE,
        verbose = FALSE)
    expect_equal(peakTables(same$annotation), peakTables(first$annotation))
})

test_that('parallelAnnotation compounds reuses cached EIC on subset', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)

    res <- evaluate_promise(peakPantheR_parallelAnnotation(first$annotation,
        compounds = "ID-1", BPPARAM = BPPARAM_serial, getAcquTime = FALSE,
        verbose = TRUE))
    expect_equal(length(grep("Reading data from", res$messages)), 0)
    expect_gte(length(grep("Reusing cached EIC data", res$messages)), 1)
})

test_that('parallelAnnotation compounds errors when not annotated', {
    expect_error(peakPantheR_parallelAnnotation(init_annotation,
        compounds = "ID-1", BPPARAM = BPPARAM_serial, getAcquTime = FALSE,
        verbose = FALSE),
        regexp = "requires @isAnnotated = TRUE")
})

test_that('parallelAnnotation compounds errors on unknown cpdID', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    expect_error(peakPantheR_parallelAnnotation(first$annotation,
        compounds = "NOT_A_CPD", BPPARAM = BPPARAM_serial,
        getAcquTime = FALSE, verbose = FALSE),
        regexp = "unknown cpdID")
})

test_that('parallelAnnotation compounds errors on non-character', {
    first <- peakPantheR_parallelAnnotation(init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    expect_error(peakPantheR_parallelAnnotation(first$annotation,
        compounds = 1L, BPPARAM = BPPARAM_serial, getAcquTime = FALSE,
        verbose = FALSE),
        regexp = "must be a character vector")
})
