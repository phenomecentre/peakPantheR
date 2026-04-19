context('peakPantheRAnnotation dataPoints<- replacement method')

# Build a minimal annotation with 3 samples x 2 compounds
build_fixture <- function() {
    targetFeatTable <- data.frame(
        cpdID = c('ID-1', 'ID-2'),
        cpdName = c('Cpd 1', 'Cpd 2'),
        rtMin = c(3310, 3280),
        rt = c(3344.888, 3385.577),
        rtMax = c(3390, 3440),
        mzMin = c(522.194778, 496.195038),
        mz = c(522.2, 496.2),
        mzMax = c(522.205222, 496.204962),
        stringsAsFactors = FALSE)
    spectraPaths <- c('./path/file1', './path/file2', './path/file3')
    peakPantheRAnnotation(spectraPaths = spectraPaths,
                          targetFeatTable = targetFeatTable)
}

well_formed_df <- function(n = 5) {
    data.frame(rt = seq_len(n) + 3300,
               mz = rep(522.2, n),
               int = seq_len(n) * 100,
               stringsAsFactors = FALSE)
}

test_that('dataPoints<- assigns well-formed list and round-trips', {
    ann <- build_fixture()
    newDP <- list(
        list(well_formed_df(), well_formed_df()),
        list(well_formed_df(), well_formed_df()),
        list(well_formed_df(), well_formed_df()))
    dataPoints(ann) <- newDP
    expect_identical(dataPoints(ann), newDP)
    expect_identical(ann@dataPoints, newDP)
})

test_that('dataPoints<- accepts all-NULL list (sample-level absence)', {
    ann <- build_fixture()
    newDP <- list(NULL, NULL, NULL)
    dataPoints(ann) <- newDP
    expect_identical(dataPoints(ann), newDP)
})

test_that('dataPoints<- rejects wrong outer length', {
    ann <- build_fixture()
    expect_error(dataPoints(ann) <- list(list(well_formed_df(),
                                              well_formed_df())),
                 'dataPoints has 1 elements \\(samples\\). Should be 3')
})

test_that('dataPoints<- rejects mixed NULL / non-NULL entries', {
    ann <- build_fixture()
    bad <- list(list(well_formed_df(), well_formed_df()), NULL, NULL)
    expect_error(dataPoints(ann) <- bad,
                 'dataPoints must all either be list of ROI data points or NULL')
})

test_that('dataPoints<- rejects wrong inner length (compound count)', {
    ann <- build_fixture()
    bad <- list(list(well_formed_df()),
                list(well_formed_df()),
                list(well_formed_df()))
    expect_error(dataPoints(ann) <- bad,
                 'dataPoints\\[\\[1\\]\\] contains, 1 dataPoints \\(compound\\)')
})

test_that('dataPoints<- rejects non-data.frame inner element', {
    ann <- build_fixture()
    bad <- list(list('notADF', well_formed_df()),
                list(well_formed_df(), well_formed_df()),
                list(well_formed_df(), well_formed_df()))
    expect_error(dataPoints(ann) <- bad,
                 'dataPoints\\[\\[1\\]\\]\\[\\[1\\]\\] must be a data.frame')
})

test_that('dataPoints<- rejects data.frame with wrong columns', {
    ann <- build_fixture()
    wrongCols <- data.frame(retention = 1:3, mz = 1:3, intensity = 1:3)
    bad <- list(list(wrongCols, well_formed_df()),
                list(well_formed_df(), well_formed_df()),
                list(well_formed_df(), well_formed_df()))
    expect_error(dataPoints(ann) <- bad,
                 "columns should be 'rt', 'mz', 'int'")
})

test_that('dataPoints<- leaves other slots untouched', {
    ann <- build_fixture()
    roiBefore <- ann@ROI
    uroiBefore <- ann@uROI
    firBefore <- ann@FIR
    peakFitBefore <- ann@peakFit
    peakTablesBefore <- ann@peakTables
    isAnnotBefore <- ann@isAnnotated
    newDP <- list(
        list(well_formed_df(), well_formed_df()),
        list(well_formed_df(), well_formed_df()),
        list(well_formed_df(), well_formed_df()))
    dataPoints(ann) <- newDP
    expect_identical(ann@ROI, roiBefore)
    expect_identical(ann@uROI, uroiBefore)
    expect_identical(ann@FIR, firBefore)
    expect_identical(ann@peakFit, peakFitBefore)
    expect_identical(ann@peakTables, peakTablesBefore)
    expect_identical(ann@isAnnotated, isAnnotBefore)
})
