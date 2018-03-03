context('plotEICDetectedPeakwidth()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

## Input and expected data
# use ko15.CDf file from the pkg faahKO
singleSpectraDataPath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
raw_data  <- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')
EIC       <- xcms::chromatogram(raw_data, rt = c(3420., 3495.), mz = c(464.195358, 464.204642))

test_that('plot feature', {

	# result plot
  result_plot <- plotEICDetectedPeakwidth(EIC[1], cpdID=1, cpdName='testCpd 1', rt=3454.435, rtmin=3420., rtmax=3495., mzmin=464.195358, mzmax=464.204642)

  # cannot compare plot to a stored version due to some string mismatch (even if the same plot is called twice)
  # Check plot properties
  expect_is(result_plot, 'gtable')
  expect_is(result_plot, 'gTree')
  expect_is(result_plot, 'grob')
  expect_is(result_plot, 'gDesc')
  expect_equal(length(result_plot), 2)
  # number of subplots
  expect_equal(length(result_plot)[[1]], 2)
})
