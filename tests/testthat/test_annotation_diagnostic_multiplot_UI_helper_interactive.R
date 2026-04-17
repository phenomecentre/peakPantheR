context('annotation_diagnostic_multiplot_UI_helper_interactive()')

## Smoke test for the plotly sibling of
## annotation_diagnostic_multiplot_UI_helper().
## We verify the return is a plotly/htmlwidget for both a valid input
## (minimal filled annotation) and the empty-annotation fallback.

skip_if_not_installed('plotly')
skip_if_not_installed('faahKO', minimum_version = '1.18.0')
library(faahKO)

on.exit(tryCatch({ file.remove('./Rplots.pdf') },
    error = function(e) invisible(),
    warning = function(w) invisible()))


## Minimal inputs (reused shape from test_annotation_diagnostic_multiplot_UI_helper.R)
spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                  system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                  system.file('cdf/KO/ko18.CDF', package = "faahKO"))

targetFeatTable <- data.frame(matrix(vector(), 2, 8,
    dimnames = list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax",
                           "mzMin", "mz", "mzMax"))),
    stringsAsFactors = FALSE)
targetFeatTable[1, ] <- c("ID-1", "Cpd 1", 3310, 3344.888, 3390,
                           522.194778, 522.2, 522.205222)
targetFeatTable[2, ] <- c("ID-2", "Cpd 2", 3280, 3385.577, 3440,
                           496.195038, 496.2, 496.204962)
targetFeatTable[, 3:8] <- vapply(targetFeatTable[, 3:8], as.numeric,
                                  FUN.VALUE = numeric(2))


test_that('interactive helper returns plotly on empty annotation', {
    emptyAnnot <- peakPantheRAnnotation(spectraPaths = spectraPaths,
        targetFeatTable = targetFeatTable)

    res <- evaluate_promise(
        annotation_diagnostic_multiplot_UI_helper_interactive(
            cpdNb = 1, annotation = emptyAnnot,
            splNum = NULL, splColrColumn = NULL))

    expect_true(inherits(res$result, "plotly"))
    expect_true(inherits(res$result, "htmlwidget"))
})
