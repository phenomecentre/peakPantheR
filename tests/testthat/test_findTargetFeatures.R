context('findTargetFeatures()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input and expected data
# use ko15.CDf file from the pkg faahKO
singleSpectraDataPath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
raw_data  						<- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')

# targeted features in faahKO
input_ROI     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=F)
input_ROI[1,] 	<- c("ID-1", "testCpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_ROI[2,] 	<- c("ID-2", "testCpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_ROI[3,] 	<- c("ID-3", "testCpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
input_ROI[4,] 	<- c("ID-4", "testCpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
input_ROI[,3:8] <- sapply(input_ROI[,3:8], as.numeric)

# ROIDataPoints for each window
input_ROIsDataPoints <- extractSignalRawData(raw_data, rt=input_ROI[,c('rtMin','rtMax')], mz=input_ROI[,c('mzMin','mzMax')], verbose=F)

# found peaks
found_peakTable     <- data.frame(matrix(vector(), 4, 10, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "maxIntMeasured", "maxIntPredicted"))),stringsAsFactors=F)
found_peakTable[1,] <- c(TRUE, 3309.7589296586070, 3346.8277590361445, 3385.4098874628098, 522.194778, 522.20001220703125, 522.205222, 26133726.6811244078, 889280, 901015.80529226747)
found_peakTable[2,] <- c(TRUE, 3345.3766648628907, 3386.5288072289159, 3428.2788374983961, 496.20001220703125, 496.20001220703125, 496.20001220703125, 35472141.3330242932, 1128960, 1113576.69008227298)
found_peakTable[3,] <- c(TRUE, 3451.2075903614455, 3451.5072891566265, 3501.6697504924518, 464.195358, 464.20001220703125, 464.204642, 7498427.1583901159, 380736, 389632.13549519412)
found_peakTable[4,] <- c(TRUE, 3670.9201232710743, 3704.1427831325304, 3740.0172511251831, 536.20001220703125, 536.20001220703125, 536.20001220703125, 8626279.9788195733, 330176, 326763.87246511364)
found_peakTable[,1] <- sapply(found_peakTable[,c(1)], as.logical)

cFit1           <- list(amplitude=162404.8057918259, center=3341.888, sigma=0.078786133031045896, gamma=0.0018336101984172684, fitStatus=2, curveModel="skewedGaussian")
class(cFit1)    <- 'peakPantheR_curveFit'
cFit2           <- list(amplitude=199249.10572753669, center=3382.577, sigma=0.074904415304607966, gamma=0.0011471899372353885, fitStatus=2, curveModel="skewedGaussian")
class(cFit2)    <- 'peakPantheR_curveFit'
cFit3           <- list(amplitude=31645.961277502651, center=3451.435, sigma=0.064803553287811053, gamma=2.8557893789555022, fitStatus=2, curveModel="skewedGaussian")
class(cFit3)    <- 'peakPantheR_curveFit'
cFit4           <- list(amplitude=59193.591103772116, center=3698.697, sigma=0.082789238806238355, gamma=0.0026044299691057823, fitStatus=2, curveModel="skewedGaussian")
class(cFit4)    <- 'peakPantheR_curveFit'
found_curveFit  <- list(cFit1, cFit2, cFit3, cFit4)

foundPeaks <- list(peakTable=found_peakTable, curveFit=found_curveFit)


test_that('default parameters, skewedGaussian, guess params, sampling 250, no verbose', {
  # expected foundPeaks
  expected_foundPeaks <- foundPeaks
  
	# results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(input_ROIsDataPoints, input_ROI, curveModel='skewedGaussian', params='guess', sampling=250, verbose=FALSE))

  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)

  # Check result messages
  expect_equal(length(result_foundPeaks$messages), 0)
})

test_that('trigger fitCurve TryCatch, with verbose', {
  # no scans in window #3
  tmpDPoints          <- input_ROIsDataPoints
  tmpDPoints[[3]]     <- extractSignalRawData(raw_data, rt=c(3454.435, 3454.435), mz=c(464.2, 464.2), verbose=F)[[1]]
  
  # expected foundPeaks
  expected_foundPeaks               <- foundPeaks
  expected_foundPeaks$peakTable[3,] <- c(FALSE, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  expected_foundPeaks$peakTable[,1] <- as.logical(expected_foundPeaks$peakTable[,1])
  expected_foundPeaks$curveFit[[3]] <- NA
  
  # expected messages
  expected_messages <- c("Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Fit of ROI #3 is unsuccessful (try error)\n")
    
  # results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(tmpDPoints, input_ROI, curveModel='skewedGaussian', params='guess', sampling=250, verbose=TRUE))
  
  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)
  
  # Check result messages (no timing)
  expect_equal(length(result_foundPeaks$messages), 3)
  expect_equal(result_foundPeaks$messages[1:2], expected_messages)
})

test_that('failed fit (fitCurve status 0/5/-1), with verbose', {
  # no scans in window #3
  tmpDPoints          <- input_ROIsDataPoints
  tmpDPoints[[3]]     <- extractSignalRawData(raw_data, rt=c(3454., 3455.), mz=c(464.195358, 464.204642), verbose=F)[[1]]
  
  # expected foundPeaks
  expected_foundPeaks               <- foundPeaks
  expected_foundPeaks$peakTable[3,] <- c(FALSE, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  expected_foundPeaks$peakTable[,1] <- as.logical(expected_foundPeaks$peakTable[,1])
  expected_foundPeaks$curveFit[[3]] <- NA
  
  # expected messages
  expected_messages <- c("Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Fit of ROI #3 is unsuccessful (fit status)\n")
  
  # results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(tmpDPoints, input_ROI, curveModel='skewedGaussian', params='guess', sampling=250, verbose=TRUE))
  
  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)
  
  # Check result messages (no timing)
  expect_equal(length(result_foundPeaks$messages), 3)
  expect_equal(result_foundPeaks$messages[1:2], expected_messages)
})

test_that('mzMin mzMax cannot be calculated due to rtMin (#3) rtMax (#4) outside of ROI, default parameters, skewedGaussian, guess params, sampling 250, verbose', {
  # expected foundPeaks
  expected_foundPeaks <- foundPeaks
  # expected messages
  expected_messages   <- c("Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3\n")
  
  # results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(input_ROIsDataPoints, input_ROI, curveModel='skewedGaussian', params='guess', sampling=250, verbose=TRUE))
  
  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)
  
  # Check result messages
  expect_equal(length(result_foundPeaks$messages), 3)
  expect_equal(result_foundPeaks$messages[1:2], expected_messages)
})

# No other curveModel currently available

test_that('change params for window #3', {
  # input params
  tmp_params    <- list( init_params  = list(amplitude=1E7, center=3454.435, sigma=1, gamma=0),
                         lower_bounds = list(amplitude=0,   center=3451.435, sigma=0, gamma=-0.1),
                         upper_bounds = list(amplitude=1E9, center=3457.435, sigma=5, gamma=0.1))
  input_params  <- list('guess', 'guess', tmp_params, 'guess')
  # expected foundPeaks
  expected_foundPeaks                         <- foundPeaks
  expected_foundPeaks$peakTable[3,2:10]        <- c(3456.2450570267611, 3457.435, 3499.0868240786049, 464.195358, 464.2000122, 464.204642, 5255410.5167749533, 380736, 174353.55750364260)
  expected_foundPeaks$curveFit[[3]]$amplitude <- 36323.971046956591
  expected_foundPeaks$curveFit[[3]]$center    <- 3457.435
  expected_foundPeaks$curveFit[[3]]$sigma     <- 0.083113691800671921
  expected_foundPeaks$curveFit[[3]]$gamma     <- 0.1
  expected_foundPeaks$curveFit[[3]]$fitStatus <- 1
  # expected messages
  expected_messages   <- c("Curve fitting parameters passed as input employed\n","Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3\n")
  
  # results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(input_ROIsDataPoints, input_ROI, curveModel='skewedGaussian', params=input_params, sampling=250, verbose=TRUE))
  
  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)
  
  # Check result messages
  expect_equal(length(result_foundPeaks$messages), 4)
  expect_equal(result_foundPeaks$messages[1:3], expected_messages)
})

test_that('change sampling', {
  # expected foundPeaks
  expected_peakTable      <- data.frame(matrix(vector(), 4, 10, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "maxIntMeasured", "maxIntPredicted"))),stringsAsFactors=F)
  expected_peakTable[1,]  <- c(TRUE, 3309.6884911519801, 3346.7859591836732, 3385.4800395368861, 522.194778, 522.20001220703125, 522.205222, 26133882.5658131838, 889280, 901012.10599065467)
  expected_peakTable[2,]  <- c(TRUE, 3345.0894935650540, 3386.4953673469390, 3428.4561855932079, 496.20001220703125, 496.20001220703125, 496.20001220703125, 35474025.5332010239, 1128960, 1113574.43999634334)
  expected_peakTable[3,]  <- c(TRUE, 3450.0344897959185, 3451.5574489795918, 3501.7095764174951, 464.195358, 464.20001220703125, 464.204642, 7456470.7446634285, 380736, 389624.14409317775)
  expected_peakTable[4,]  <- c(TRUE, 3670.8488688282255, 3704.0847551020411, 3740.0915161411931, 536.20001220703125, 536.20001220703125, 536.20001220703125, 8626359.7845999431, 330176, 326758.81671814714)
  expected_peakTable[,1]  <- sapply(expected_peakTable[,c(1)], as.logical)
  expected_foundPeaks     <- list(peakTable=expected_peakTable, curveFit=found_curveFit)
  
  # results (output, warnings and messages)
  result_foundPeaks   <- evaluate_promise(findTargetFeatures(input_ROIsDataPoints, input_ROI, curveModel='skewedGaussian', params='guess', sampling=50, verbose=FALSE))
  
  # Check result table
  expect_equal(result_foundPeaks$result, expected_foundPeaks)
  
  # Check result messages
  expect_equal(length(result_foundPeaks$messages), 0)
})

test_that('raise errors', {
  # ROIsDataPoints doesn't match number of ROI
  expect_error(findTargetFeatures(input_ROIsDataPoints, input_ROI[1:3,], curveModel='skewedGaussian', params='guess'), 'Check input, number of ROIsDataPoints entries must match the number of rows of ROI', fixed=TRUE)
  
  # data points rt outside of ROI
  # rtMin side
  tmpDPoints      <- input_ROIsDataPoints
  tmpDPoints[[2]] <- extractSignalRawData(raw_data, rt=c(3000, 3440), mz=c(496.195038, 496.204962), verbose=F)[[1]]
  expect_error(findTargetFeatures(tmpDPoints, input_ROI, curveModel='skewedGaussian', params='guess'), 'Check input not all datapoints for window #2 are into the corresponding ROI (rt)', fixed=TRUE)
  # rtMax
  tmpDPoints      <- input_ROIsDataPoints
  tmpDPoints[[3]] <- extractSignalRawData(raw_data, rt=c(3420., 3600.), mz=c(464.195358, 464.204642), verbose=F)[[1]]
  expect_error(findTargetFeatures(tmpDPoints, input_ROI, curveModel='skewedGaussian', params='guess'), 'Check input not all datapoints for window #3 are into the corresponding ROI (rt)', fixed=TRUE)
  
  # data points mz outside of ROI
  # mzMin side
  tmpDPoints      <- input_ROIsDataPoints
  tmpDPoints[[2]] <- extractSignalRawData(raw_data, rt=c(3280, 3440), mz=c(494.195038, 496.204962), verbose=F)[[1]]
  expect_error(findTargetFeatures(tmpDPoints, input_ROI, curveModel='skewedGaussian', params='guess'), 'Check input not all datapoints for window #2 are into the corresponding ROI (mz)', fixed=TRUE)
  # mzMax
  tmpDPoints      <- input_ROIsDataPoints
  tmpDPoints[[3]] <- extractSignalRawData(raw_data, rt=c(3420., 3495.), mz=c(464.195358, 466.204642), verbose=F)[[1]]
  expect_error(findTargetFeatures(tmpDPoints, input_ROI, curveModel='skewedGaussian', params='guess'), 'Check input not all datapoints for window #3 are into the corresponding ROI (mz)', fixed=TRUE)
  
  # params is not character or list
  expect_error(findTargetFeatures(input_ROIsDataPoints, input_ROI, curveModel='skewedGaussian', params=5), 'Check input, "params" must be "guess" or list', fixed=TRUE)
  
  # params is character but not 'guess'
  expect_error(findTargetFeatures(input_ROIsDataPoints, input_ROI, curveModel='skewedGaussian', params='not guess'), 'Check input, "params" must be "guess" if not list', fixed=TRUE)
  
  # params is a list of wrong length
  expect_error(findTargetFeatures(input_ROIsDataPoints, input_ROI, curveModel='skewedGaussian', params=vector('list',2)), 'Check input, number of parameters must match number of rows of ROI', fixed=TRUE)
})
