context('spectra_metadata_colourScheme_UI_helper()')

# UI specific function that returns a vector of spectra colours

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

# remove Rplots.pdf created by ggplot2
on.exit( tryCatch({ file.remove('./Rplots.pdf') }, error=function(e){ invisible() }, warning=function(w){ invisible() }) )


## Input data
# spectraPaths
input_spectraPaths  <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko18.CDF', package = "faahKO"))

# targetFeatTable
input_targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_targetFeatTable[,c(3:8)] <- sapply(input_targetFeatTable[,c(3:8)], as.numeric)

# cpdMetadata
input_cpdMetadata     <- data.frame(matrix(data=c('a','b',1,2), nrow=2, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)

# spectraMetadata
input_spectraMetadata <- data.frame(matrix(data=c('c','d','e',3,4,5), nrow=3, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)

# Object, fully filled
annotation            <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)


test_that('splColrColum is NULL', {
  # input
  input_annotation  <- annotation
  # expected
  expected_colr     <- NULL

  # results (output, warnings and messages)
  result_colr  <- evaluate_promise(
      spectra_metadata_colourScheme_UI_helper(annot = input_annotation,
                                              splColrColumn = NULL))

  # Check colour vector generated
  expect_equal(result_colr$result, expected_colr)
})

test_that('splColrColum is None', {
  # input
  input_annotation  <- annotation
  # expected
  expected_colr     <- NULL

  # results (output, warnings and messages)
  result_colr  <- evaluate_promise(
      spectra_metadata_colourScheme_UI_helper(annot = input_annotation,
                                              splColrColumn = 'None'))

  # Check colour vector generated
  expect_equal(result_colr$result, expected_colr)
})

test_that('splColrColum is not a spectraMetadata column name', {
  # input
  input_annotation  <- annotation
  # expected
  expected_colr     <- NULL

  # results (output, warnings and messages)
  result_colr  <- evaluate_promise(
      spectra_metadata_colourScheme_UI_helper(annot = input_annotation,
                                              splColrColumn = 'notAColumn'))

  # Check colour vector generated
  expect_equal(result_colr$result, expected_colr)
})

test_that('splColrColum is a spectraMetadata column name', {
  # input
  input_annotation  <- annotation
  # expected
  expected_colr     <- c("blue", "red", "green")

  # results (output, warnings and messages)
  result_colr  <- evaluate_promise(
      spectra_metadata_colourScheme_UI_helper(annot = input_annotation,
                                              splColrColumn = 'testcol1'))

  # Check colour vector generated
  expect_equal(result_colr$result, expected_colr)
})

test_that('12 unique colour (recycle colour palette)', {
  # input
  large_spectraPaths    <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                            system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                            system.file('cdf/KO/ko18.CDF', package = "faahKO"),
                            system.file('cdf/KO/ko19.CDF', package = "faahKO"),
                            system.file('cdf/KO/ko21.CDF', package = "faahKO"),
                            system.file('cdf/KO/ko22.CDF', package = "faahKO"),
                            system.file('cdf/WT/wt15.CDF', package = "faahKO"),
                            system.file('cdf/WT/wt16.CDF', package = "faahKO"),
                            system.file('cdf/WT/wt18.CDF', package = "faahKO"),
                            system.file('cdf/WT/wt19.CDF', package = "faahKO"),
                            system.file('cdf/WT/wt21.CDF', package = "faahKO"),
                            system.file('cdf/WT/wt22.CDF', package = "faahKO"))
  large_spectraMetadata <- data.frame(matrix(data=c(1,2,3,4,5,6,7,8,9,10,11,12), nrow=12, ncol=1, dimnames=list(c(),c('testcol1')), byrow=FALSE), stringsAsFactors=FALSE)

  largeAnnotation       <- peakPantheRAnnotation(spectraPaths=large_spectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=large_spectraMetadata)

  # expected
  expected_colr         <- c("blue", "red", "green", "orange", "purple", "seagreen",
                             "darkturquoise", "violetred", "saddlebrown", "black",
                             "blue", "red")

  # results (output, warnings and messages)
  result_colr  <- evaluate_promise(
      spectra_metadata_colourScheme_UI_helper(annot = largeAnnotation,
                                              splColrColumn = 'testcol1'))

  # Check colour vector generated
  expect_equal(result_colr$result, expected_colr)
})
