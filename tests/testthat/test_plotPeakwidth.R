context('plotPeakWidth()')


## Input data
apexVal <- c(1, 2, 3, 4)
minVal  <- c(0, 0, 2, 2)
maxVal  <- c(2, 4, 4, 5)

## Expected data (load saved version of plots)
path_expected_data      <- system.file("testdata/reference_plotPeakwidth.RData", package = "peakPantheR")
load(path_expected_data) # expected_3splNoCol + expected_4splWithCol + expected_plotColourWarning


test_that('plot 3 samples, no color', {
	# generate plot
  result_3splNoCol <- plotPeakwidth(apexVal[1:3], minVal[1:3], maxVal[1:3], varName='Test variable',
                                    sampleColour=NULL, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_3splNoCol))
  expect_equal(result_3splNoCol$labels$x, "Test variable")
  expect_equal(result_3splNoCol$labels$y, "y")
  expect_equal(length(result_3splNoCol), 9)
  
  # Check against reference version
  tmp_result    <- ggplot2::ggplot_build(result_3splNoCol)
  tmp_expected  <- ggplot2::ggplot_build(expected_3splNoCol)
  expect_equal(tmp_result, tmp_expected)
})

test_that('plot 4 samples with color', {
  # generate plot
  result_4splWithCol <- plotPeakwidth(apexVal, minVal, maxVal, varName='Test variable 2',
                                      sampleColour=c('blue','red','green','orange'), verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_4splWithCol))
  expect_equal(result_4splWithCol$labels$x, "Test variable 2")
  expect_equal(result_4splWithCol$labels$y, "y")
  expect_equal(length(result_4splWithCol), 9)
  
  # Check against reference version
  tmp_result    <- ggplot2::ggplot_build(result_4splWithCol)
  tmp_expected  <- ggplot2::ggplot_build(expected_4splWithCol)
  expect_equal(tmp_result, tmp_expected)
})

test_that('sampleColour length warning', {
  # Expected message
  expected_message <- c("Warning: sampleColour length must match the number of samples; default colour used\n")
  
  # generate plot
  result_plotColourWarning <- evaluate_promise(plotPeakwidth(apexVal, minVal, maxVal,
                                                             varName='Test variable 3',
                                                             sampleColour=c('blue','red','green'),
                                                             verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_plotColourWarning$messages, expected_message)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_plotColourWarning$result))
  expect_equal(result_plotColourWarning$result$labels$x, "Test variable 3")
  expect_equal(result_plotColourWarning$result$labels$y, "y")
  expect_equal(length(result_plotColourWarning$result), 9)
  
  # Check against reference version
  tmp_result    <- ggplot2::ggplot_build(result_plotColourWarning$result)
  tmp_expected  <- ggplot2::ggplot_build(expected_plotColourWarning)
  expect_equal(tmp_result, tmp_expected)
})

test_that('raise errors', {
  # apexValue length is wrong
  msg1    <- c('"apexValue", "widthMin" and "widthMax" must be the same length')
  expect_error(plotPeakwidth(apexVal[1:3], minVal, maxVal), msg1, fixed=TRUE)
  
  # widthMin length is wrong
  msg2    <- c('"apexValue", "widthMin" and "widthMax" must be the same length')
  expect_error(plotPeakwidth(apexVal, minVal[1:3], maxVal), msg2, fixed=TRUE)
  
  # widthMax length is wrong
  msg3    <- c('"apexValue", "widthMin" and "widthMax" must be the same length')
  expect_error(plotPeakwidth(apexVal, minVal, maxVal[1:3]), msg3, fixed=TRUE)
})
