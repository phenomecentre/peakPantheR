context('peakPantheR_plotPeakWidth()')

# remove Rplots.pdf created by ggplot2
on.exit( tryCatch({ file.remove('./Rplots.pdf') }, error=function(e){ invisible() }, warning=function(w){ invisible() }) )


## Input data
apexVal <- c(1, 2, 3, 4)
minVal  <- c(0, 0, 2, 2)
maxVal  <- c(2, 4, 4, 5)
acqTime <- as.POSIXct(c("2017-07-13 21:06:14", "2017-07-14 21:06:14", "2017-07-15 21:06:14", "2017-07-16 21:06:14"))



test_that('plot 3 samples, no color', {
	# generate plot
  result_3splNoColNoRot <- peakPantheR_plotPeakwidth(apexValue=apexVal[1:3], widthMin=minVal[1:3], widthMax=maxVal[1:3], 
                                                     acquTime=NULL, varName='Test variable', sampleColour=NULL,
                                                     rotateAxis=FALSE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_3splNoColNoRot))
  expect_equal(result_3splNoColNoRot$labels$x, "x")
  expect_equal(result_3splNoColNoRot$labels$y, "Test variable")
  expect_equal(length(result_3splNoColNoRot), length(ggplot2::ggplot()))
})

test_that('plot 3 samples, no color, acquisition time', {
  # generate plot
  result_3splNoColNoRotRunOrder <- peakPantheR_plotPeakwidth(apexValue=apexVal[1:3], widthMin=minVal[1:3], widthMax=maxVal[1:3], 
                                                             acquTime=acqTime[1:3], varName='Test variable 2', sampleColour=NULL,
                                                             rotateAxis=FALSE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_3splNoColNoRotRunOrder))
  expect_equal(result_3splNoColNoRotRunOrder$labels$x, "Acquisition Time")
  expect_equal(result_3splNoColNoRotRunOrder$labels$y, "Test variable 2")
  expect_equal(length(result_3splNoColNoRotRunOrder), length(ggplot2::ggplot()))
})

test_that('plot 3 samples, no color, rotate axis', {
  # generate plot
  result_3splNoColRot <- peakPantheR_plotPeakwidth(apexValue=apexVal[1:3], widthMin=minVal[1:3], widthMax=maxVal[1:3],
                                                   acquTime=NULL, varName='Test variable 3', sampleColour=NULL,
                                                   rotateAxis=TRUE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_3splNoColRot))
  expect_equal(result_3splNoColRot$labels$x, "Test variable 3")
  expect_equal(result_3splNoColRot$labels$y, "x")
  expect_equal(length(result_3splNoColRot), length(ggplot2::ggplot()))
})

test_that('plot 3 samples, no color, acquisition time, rotate axis', {
  # generate plot
  result_3splNoColRotRunOrder <- peakPantheR_plotPeakwidth(apexValue=apexVal[1:3], widthMin=minVal[1:3], widthMax=maxVal[1:3], 
                                                           acquTime=acqTime[1:3], varName='Test variable 4', sampleColour=NULL,
                                                           rotateAxis=TRUE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_3splNoColRotRunOrder))
  expect_equal(result_3splNoColRotRunOrder$labels$x, "Test variable 4")
  expect_equal(result_3splNoColRotRunOrder$labels$y, "Acquisition Time")
  expect_equal(length(result_3splNoColRotRunOrder), length(ggplot2::ggplot()))
})

test_that('plot 4 samples with color', {
  # generate plot
  result_4splWithColNoRot <- peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal, widthMax=maxVal, 
                                                       acquTime=NULL, varName='Test variable 5', sampleColour=c('blue','red','green','orange'),
                                                       rotateAxis=FALSE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_4splWithColNoRot))
  expect_equal(result_4splWithColNoRot$labels$x, "x")
  expect_equal(result_4splWithColNoRot$labels$y, "Test variable 5")
  expect_equal(length(result_4splWithColNoRot), length(ggplot2::ggplot()))
})

test_that('plot 4 samples with color, acquisition time', {
  # generate plot
  result_4splWithColNoRotRunOrder <- peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal, widthMax=maxVal,
                                                               acquTime=acqTime, varName='Test variable 6',
                                                               sampleColour=c('blue','red','green','orange'), rotateAxis=FALSE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_4splWithColNoRotRunOrder))
  expect_equal(result_4splWithColNoRotRunOrder$labels$x, "Acquisition Time")
  expect_equal(result_4splWithColNoRotRunOrder$labels$y, "Test variable 6")
  expect_equal(length(result_4splWithColNoRotRunOrder), length(ggplot2::ggplot()))
})

test_that('plot 4 samples with color, rotate axis', {
  # generate plot
  result_4splWithColRot <- peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal, widthMax=maxVal, 
                                                     acquTime=NULL, varName='Test variable 7', sampleColour=c('blue','red','green','orange'),
                                                     rotateAxis=TRUE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_4splWithColRot))
  expect_equal(result_4splWithColRot$labels$x, "Test variable 7")
  expect_equal(result_4splWithColRot$labels$y, "x")
  expect_equal(length(result_4splWithColRot), length(ggplot2::ggplot()))
})

test_that('plot 4 samples with color, acquisition time, rotate axis', {
  # generate plot
  result_4splWithColRotRunOrder <- peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal, widthMax=maxVal,
                                                             acquTime=acqTime, varName='Test variable 8',
                                                             sampleColour=c('blue','red','green','orange'), rotateAxis=FALSE, verbose=FALSE)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_4splWithColRotRunOrder))
  expect_equal(result_4splWithColRotRunOrder$labels$x, "Acquisition Time")
  expect_equal(result_4splWithColRotRunOrder$labels$y, "Test variable 8")
  expect_equal(length(result_4splWithColRotRunOrder), length(ggplot2::ggplot()))
})

test_that('sampleColour length warning, peakwidth and rotate and input order message', {
  # Expected message
  expected_message <- c("Warning: sampleColour length must match the number of samples; default colour used\n", "Peakwidth values plotted\n", "Values plotted by input order\n", "x and y axis rotated\n")
  
  # generate plot
  result_plotColourWarning <- evaluate_promise(peakPantheR_plotPeakwidth(apexVal, minVal, maxVal, acquTime=NULL,
                                                                         varName='Test variable 9', sampleColour=c('blue','red','green'),
                                                                         rotateAxis=TRUE, verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_plotColourWarning$messages, expected_message)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_plotColourWarning$result))
  expect_equal(result_plotColourWarning$result$labels$x, "Test variable 9")
  expect_equal(result_plotColourWarning$result$labels$y, "x")
  expect_equal(length(result_plotColourWarning$result), length(ggplot2::ggplot()))
})

test_that('sampleColour length warning, no peakwidth (missing widthMax) and no rotate and run order message', {
  # Expected message
  expected_message <- c("Warning: sampleColour length must match the number of samples; default colour used\n", "Values plotted by run order\n")
  
  # generate plot
  result_plotColourWarningRunOrder <- evaluate_promise(peakPantheR_plotPeakwidth(apexVal, widthMin=minVal, widthMax=NULL, acquTime=acqTime,
                                                                                 varName='Test variable 10', sampleColour=c('blue','red','green'),
                                                                                 rotateAxis=FALSE, verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_plotColourWarningRunOrder$messages, expected_message)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_plotColourWarningRunOrder$result))
  expect_equal(result_plotColourWarningRunOrder$result$labels$x, "Acquisition Time")
  expect_equal(result_plotColourWarningRunOrder$result$labels$y, "Test variable 10")
  expect_equal(length(result_plotColourWarningRunOrder$result), length(ggplot2::ggplot()))
})

test_that('missing widthMin, no peakwidth plot', {
  # Expected message
  expected_message <- c("Values plotted by input order\n")
  
  # generate plot
  result_plotNoWidthMin <- evaluate_promise(peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=NULL, widthMax=maxVal, acquTime=NULL,
                                                                      varName='Test variable 11', sampleColour=NULL,
                                                                      rotateAxis=FALSE, verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_plotNoWidthMin$messages, expected_message)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_plotNoWidthMin$result))
  expect_equal(result_plotNoWidthMin$result$labels$x, "x")
  expect_equal(result_plotNoWidthMin$result$labels$y, "Test variable 11")
  expect_equal(length(result_plotNoWidthMin$result), length(ggplot2::ggplot()))
})

test_that('missing widthMax, no peakwidth plot', {
  # Expected message
  expected_message <- c("Values plotted by input order\n")
  
  # generate plot
  result_plotNoWidthMax <- evaluate_promise(peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=NULL, widthMax=maxVal, acquTime=NULL,
                                                                      varName='Test variable 11', sampleColour=NULL,
                                                                      rotateAxis=FALSE, verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_plotNoWidthMax$messages, expected_message)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_plotNoWidthMax$result))
  expect_equal(result_plotNoWidthMax$result$labels$x, "x")
  expect_equal(result_plotNoWidthMax$result$labels$y, "Test variable 11")
  expect_equal(length(result_plotNoWidthMax$result), length(ggplot2::ggplot()))
})

test_that('NA in acquisition time, revert to input order plot', {
  # Expected message
  expected_message <- c("Warning: \"acquTime\" contains NA, run order will not be plotted\n", "Peakwidth values plotted\n", "Values plotted by input order\n" )
  
  # generate plot
  result_plotFallBackNAAcqu <- evaluate_promise(peakPantheR_plotPeakwidth(apexValue=apexVal, widthMin=minVal, widthMax=maxVal,
                                                                          acquTime=c(acqTime[1:3],NA), varName='Test variable 12',
                                                                          sampleColour=NULL, rotateAxis=FALSE, verbose=TRUE))
  
  # check messages confirming the replacements
  expect_equal(result_plotFallBackNAAcqu$messages, expected_message)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_plotFallBackNAAcqu$result))
  expect_equal(result_plotFallBackNAAcqu$result$labels$x, "x")
  expect_equal(result_plotFallBackNAAcqu$result$labels$y, "Test variable 12")
  expect_equal(length(result_plotFallBackNAAcqu$result), length(ggplot2::ggplot()))
})

test_that('raise errors', {
  # acquTime is not POSIXct
  msg1    <- c('Error: \"acquTime\" must be a vector of POSIXct')
  expect_error(peakPantheR_plotPeakwidth(apexVal=apexVal, acquTime='not a POSIXct'), msg1, fixed=TRUE)
  
  # apexValue/acquTime length is wrong
  msg2    <- c('\"apexValue\" and \"acquTime\" must be the same length')
  expect_error(peakPantheR_plotPeakwidth(apexValue=apexVal[1:3], acquTime=acqTime), msg2, fixed=TRUE)
  
  # apexValue length is wrong
  msg3    <- c('"apexValue", "widthMin" and "widthMax" must be the same length')
  expect_error(peakPantheR_plotPeakwidth(apexVal[1:3], widthMin=minVal, widthMax=maxVal), msg3, fixed=TRUE)
  
  # widthMin length is wrong
  msg4    <- c('"apexValue", "widthMin" and "widthMax" must be the same length')
  expect_error(peakPantheR_plotPeakwidth(apexVal, widthMin=minVal[1:3], widthMax=maxVal), msg4, fixed=TRUE)
  
  # widthMax length is wrong
  msg5    <- c('"apexValue", "widthMin" and "widthMax" must be the same length')
  expect_error(peakPantheR_plotPeakwidth(apexVal, widthMin=minVal, widthMax=maxVal[1:3]), msg5, fixed=TRUE)
})
