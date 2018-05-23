context('plotRunOrder()')


## Input data
sampleVal <- c(1, 2, 3, 4)
acqTime   <- as.POSIXct(c("2017-07-13 21:06:14", "2017-07-14 21:06:14", "2017-07-15 21:06:14", "2017-07-16 21:06:14"))

## Expected data (load saved version of plots)
path_expected_data      <- system.file("testdata/reference_plotRunOrder.RData", package = "peakPantheR")
load(path_expected_data) # expected_runOrder3splNoCol + expected_runOrder4splWithCol + expected_runOrderPlotColourWarning


test_that('plot 3 samples, no color', {
	# generate plot
  result_runOrder3splNoCol <- plotRunOrder(sampleVal[1:3], acqTime[1:3], varName='Test variable',
                                           sampleColour=NULL, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_runOrder3splNoCol))
  expect_equal(result_runOrder3splNoCol$labels$x, "Acquisition Time")
  expect_equal(result_runOrder3splNoCol$labels$y, "Test variable")
  expect_equal(length(result_runOrder3splNoCol), 9)
  
  # Check against reference version
  tmp_result    <- ggplot2::ggplot_build(result_runOrder3splNoCol)
  tmp_expected  <- ggplot2::ggplot_build(expected_runOrder3splNoCol)
  expect_equal(tmp_result, tmp_expected)
})

test_that('plot 4 samples with color', {
  # generate plot
  result_runOrder4splWithCol <- plotRunOrder(sampleVal, acqTime, varName='Test variable 2',
                                             sampleColour=c('blue','red','green','orange'), verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_runOrder4splWithCol))
  expect_equal(result_runOrder4splWithCol$labels$x, "Acquisition Time")
  expect_equal(result_runOrder4splWithCol$labels$y, "Test variable 2")
  expect_equal(length(result_runOrder4splWithCol), 9)
  
  # Check against reference version
  tmp_result    <- ggplot2::ggplot_build(result_runOrder4splWithCol)
  tmp_expected  <- ggplot2::ggplot_build(expected_runOrder4splWithCol)
  expect_equal(tmp_result, tmp_expected)
})

test_that('sampleColour length warning', {
  # Expected message
  expected_message <- c("Warning: sampleColour length must match the number of samples; default colour used\n")
  
  # generate plot
  result_runOrderPlotColourWarning <- evaluate_promise(plotRunOrder(sampleVal, acqTime,
                                                             varName='Test variable 3',
                                                             sampleColour=c('blue','red','green'),
                                                             verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_runOrderPlotColourWarning$messages, expected_message)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_runOrderPlotColourWarning$result))
  expect_equal(result_runOrderPlotColourWarning$result$labels$x, "Acquisition Time")
  expect_equal(result_runOrderPlotColourWarning$result$labels$y, "Test variable 3")
  expect_equal(length(result_runOrderPlotColourWarning$result), 9)
  
  # Check against reference version
  tmp_result    <- ggplot2::ggplot_build(result_runOrderPlotColourWarning$result)
  tmp_expected  <- ggplot2::ggplot_build(expected_runOrderPlotColourWarning)
  expect_equal(tmp_result, tmp_expected)
})

test_that('raise errors', {
  # acquTime is not POSIXct
  msg1    <- c('Error: \"acquTime\" must be a vector of POSIXct')
  expect_error(plotRunOrder(sampleVal, 'not a POSIXct'), msg1, fixed=TRUE)
  
  # sampleValue length is wrong
  msg2    <- c('\"sampleValue\" and \"acquTime\" must be the same length')
  expect_error(plotRunOrder(sampleVal[1:3], acqTime), msg2, fixed=TRUE)
  
  # acquTime length is wrong
  msg3    <- c('\"sampleValue\" and \"acquTime\" must be the same length')
  expect_error(plotRunOrder(sampleVal, acqTime[1:3]), msg3, fixed=TRUE)
})
