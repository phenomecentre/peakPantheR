context('plotEICDetectedPeakwidth()')

skip_if_not(FALSE, message = 'unittest refactor')
skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

## Input and expected data
# use ko15.CDf file from the pkg faahKO
spectraDataPath <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                    system.file('cdf/KO/ko16.CDF', package = "faahKO"))
EIC1            <- xcms::chromatogram(MSnbase::readMSData(spectraDataPath[1], centroided=TRUE, mode='onDisk'), rt = c(3420., 3495.), mz = c(464.195358, 464.204642))
EIC2            <- xcms::chromatogram(MSnbase::readMSData(spectraDataPath[2], centroided=TRUE, mode='onDisk'), rt = c(3420., 3495.), mz = c(464.195358, 464.204642))


test_that('plot feature in 1 sample', {

	# result plot
  result_plot <- plotEICDetectedPeakwidth(list(EIC1), cpdID="ID-1", cpdName='testCpd 1', rt=3454.435, rtmin=3420., rtmax=3495., mzmin=464.195358, mzmax=464.204642, ratio=0.85, verbose=FALSE)

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

test_that('plot feature in 2 samples, change colours', {
  
  # result plot
  result_plot <- plotEICDetectedPeakwidth(list(EIC1, EIC2), cpdID="ID-1", cpdName='testCpd 1', rt=c(3454.435, 3454.435), rtmin=c(3420., 3420.), rtmax=c(3495.,3495.), mzmin=464.195358, mzmax=464.204642, sampleColour=c('blue','red'), ratio=0.85, verbose=FALSE)
  
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

test_that('colour warning, ratio replacement, 2 spectra in a Chromatograms', {
  # Chromatograms with 2 ROI
  input_EIC2 <- xcms::chromatogram(MSnbase::readMSData(spectraDataPath[2], centroided=TRUE, mode='onDisk'), rt = data.frame(rt_lower=c(3420.,3420.), rt_upper=c(3495.,3495.)), mz = data.frame(mz_lower=c(464.195358,464.195358), mz_upper=c(464.204642,464.204642)))
  # Expected message
  expected_message <- c("Warning: sampleColour length must match the number of samples; default colour used\n", "Error: ratio must be between 0 and 1, replaced by default value\n", "Warning: more than 1 spectra per Chromatograms provided! Only the first one will be considered\n")
  
  # result plot
  result_plot <- evaluate_promise(plotEICDetectedPeakwidth(list(EIC1, input_EIC2), cpdID="ID-1", cpdName='testCpd 1', rt=c(3454.435, 3454.435), rtmin=c(3420., 3420.), rtmax=c(3495.,3495.), mzmin=464.195358, mzmax=464.204642, ratio=2, sampleColour=c('blue','red','grey'), verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_plot$messages, expected_message)
  
  # cannot compare plot to a stored version due to some string mismatch (even if the same plot is called twice)
  # Check plot properties
  expect_is(result_plot$result, 'gtable')
  expect_is(result_plot$result, 'gTree')
  expect_is(result_plot$result, 'grob')
  expect_is(result_plot$result, 'gDesc')
  expect_equal(length(result_plot$result), 2)
  # number of subplots
  expect_equal(length(result_plot$result)[[1]], 2)
})

test_that('raise errors', {
  # EICs is not a list
  msg1    <- c("Error: EICs must be a list of single xcms::Chromatograms")
  expect_error(plotEICDetectedPeakwidth(EIC1, cpdID="ID-1", cpdName='testCpd 1', rt=3454.435, rtmin=3420., rtmax=3495., mzmin=464.195358, mzmax=464.204642), msg1, fixed=TRUE)
  # EICs length is wrong
  msg2    <- c("'EIC', 'rt', 'rtmin' and 'rtmax' must be the same length")
  expect_error(plotEICDetectedPeakwidth(list(EIC1, EIC2), cpdID="ID-1", cpdName='testCpd 1', rt=3454.435, rtmin=3420., rtmax=3495., mzmin=464.195358, mzmax=464.204642), msg2, fixed=TRUE)
  # rt length is wrong
  msg3    <- c("'EIC', 'rt', 'rtmin' and 'rtmax' must be the same length")
  expect_error(plotEICDetectedPeakwidth(list(EIC1), cpdID="ID-1", cpdName='testCpd 1', rt=c(3454.435,3454.435), rtmin=3420., rtmax=3495., mzmin=464.195358, mzmax=464.204642), msg3, fixed=TRUE)
  # rtmin length is wrong
  msg4    <- c("'EIC', 'rt', 'rtmin' and 'rtmax' must be the same length")
  expect_error(plotEICDetectedPeakwidth(list(EIC1), cpdID="ID-1", cpdName='testCpd 1', rt=3454.435, rtmin=c(3420.,3420), rtmax=3495., mzmin=464.195358, mzmax=464.204642), msg4, fixed=TRUE)
  # rtmax length is wrong
  msg5    <- c("'EIC', 'rt', 'rtmin' and 'rtmax' must be the same length")
  expect_error(plotEICDetectedPeakwidth(list(EIC1), cpdID="ID-1", cpdName='testCpd 1', rt=3454.435, rtmin=3420., rtmax=c(3495.,3495.), mzmin=464.195358, mzmax=464.204642), msg5, fixed=TRUE)
  # EIC is not list of Chromatograms
  msg6    <- c("Error: EICs must be a list of single xcms::Chromatograms")
  expect_error(plotEICDetectedPeakwidth(list(EIC1, EIC2[[1]]), cpdID="ID-1", cpdName='testCpd 1', rt=c(3454.435, 3454.435), rtmin=c(3420., 3420.), rtmax=c(3495.,3495.), mzmin=464.195358, mzmax=464.204642), msg6, fixed=TRUE)
})
