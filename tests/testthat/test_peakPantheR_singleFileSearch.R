context('peakPantheR_singleFileSearch()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

# remove Rplots.pdf created by ggplot2
on.exit( tryCatch({ file.remove('./Rplots.pdf') }, error=function(e){ invisible() }, warning=function(w){ invisible() }) )


## Input and expected data
# use ko15.CDf file from the pkg faahKO
singleSpectraDataPath <- system.file('cdf/KO/ko15.CDF', package = "faahKO")
tmp_raw_data  				<- MSnbase::readMSData(singleSpectraDataPath, centroided=TRUE, mode='onDisk')

# targeted features in faahKO
input_ROI     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
input_ROI[1,] 	<- c("ID-1", "testCpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_ROI[2,] 	<- c("ID-2", "testCpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_ROI[3,] 	<- c("ID-3", "testCpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
input_ROI[4,] 	<- c("ID-4", "testCpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
input_ROI[,3:8] <- vapply(input_ROI[,3:8], as.numeric, FUN.VALUE=numeric(4))

# Expected peakTables
found_peakTable     <- data.frame(matrix(vector(), 4, 18, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "cpdID", "cpdName", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
found_peakTable[1,] <- c(TRUE, 3309.7589296586070, 3346.8277590361445, 3385.4098874628098, 522.194778, 522.20001220703125, 522.205222, 26133726.6811244078, 26071378, 889280, 901015.80529226747, 'ID-1', 'testCpd 1', FALSE, 0.023376160866574614, 1.93975903614455092, 1.0153573486330891, 1.0268238825675249)
found_peakTable[2,] <- c(TRUE, 3345.3766648628907, 3386.5288072289159, 3428.2788374983961, 496.20001220703125, 496.20001220703125, 496.20001220703125, 35472141.3330242932, 36498367, 1128960, 1113576.69008227298, 'ID-2', 'testCpd 2', FALSE, 0.024601030353423384, 0.95180722891564074, 1.0053782620427065, 1.0093180792278085)
found_peakTable[3,] <- c(TRUE, 3451.2075903614455, 3451.5072891566265, 3501.6697504924518, 464.195358, 464.20001220703125, 464.204642, 7498427.1583901159, 7608218, 380736, 389632.13549519412, 'ID-3', 'testCpd 3', FALSE, 0.026296922148575364, -2.92771084337346110, 207.6939219686769036, 380.5019028782010082)
found_peakTable[4,] <- c(TRUE, 3670.9201232710743, 3704.1427831325304, 3740.0172511251831, 536.20001220703125, 536.20001220703125, 536.20001220703125, 8626279.9788195733, 8692184, 330176, 326763.87246511364, 'ID-4', 'testCpd 4', FALSE, 0.022765817240815486, 2.44578313253032320, 1.0305289730128095, 1.0536948855480386)
found_peakTable[,c(1,14)]       <- vapply(found_peakTable[,c(1,14)], as.logical, FUN.VALUE=logical(4))
found_peakTable[,c(2:11,15:18)] <- vapply(found_peakTable[,c(2:11,15:18)], as.numeric, FUN.VALUE=numeric(4))

# found curveFit
cFit1           <- list(amplitude=162404.8057918259, center=3341.888, sigma=0.078786133031045896, gamma=0.0018336101984172684, fitStatus=2, curveModel="skewedGaussian")
class(cFit1)    <- 'peakPantheR_curveFit'
cFit2           <- list(amplitude=199249.10572753669, center=3382.577, sigma=0.074904415304607966, gamma=0.0011471899372353885, fitStatus=2, curveModel="skewedGaussian")
class(cFit2)    <- 'peakPantheR_curveFit'
cFit3           <- list(amplitude=31645.961277502651, center=3451.435, sigma=0.064803553287811053, gamma=2.8557893789555022, fitStatus=2, curveModel="skewedGaussian")
class(cFit3)    <- 'peakPantheR_curveFit'
cFit4           <- list(amplitude=59193.591103772116, center=3698.697, sigma=0.082789238806238355, gamma=0.0026044299691057823, fitStatus=2, curveModel="skewedGaussian")
class(cFit4)    <- 'peakPantheR_curveFit'
found_curveFit  <- list(cFit1, cFit2, cFit3, cFit4)

# found ROIsDataPoint
found_ROIsDataPoints <- extractSignalRawData(tmp_raw_data, rt=input_ROI[,c('rtMin','rtMax')], mz=input_ROI[,c('mzMin','mzMax')], verbose=FALSE)


test_that('no peakStatistic, no plotEICsPath, no getAcquTime, no FIR, no verbose', {
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- found_peakTable[,1:14]
  # Expected curveFit
  expected_curveFit       <- found_curveFit
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- found_ROIsDataPoints
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n")

	# results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, peakStatistic=FALSE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, verbose=FALSE))

  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)

  # Check messages (loading file)
  expect_equal(length(result_singleFileSearch$messages), 1)
  expect_equal(result_singleFileSearch$messages, expected_messages)
})

test_that('peakStatistic, no plotEICsPath, no getAcquTime, no FIR, no verbose', {
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- found_peakTable
  # Expected curveFit
  expected_curveFit       <- found_curveFit
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- found_ROIsDataPoints
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n")
  
  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, peakStatistic=TRUE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, verbose=FALSE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check messages (loading file)
  expect_equal(length(result_singleFileSearch$messages), 1)
  expect_equal(result_singleFileSearch$messages, expected_messages)
})

test_that('no peakStatistic, no plotEICsPath, with getAcquTime, no FIR, verbose', {
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- found_peakTable[,1:14]
  # Expected curveFit
  expected_curveFit       <- found_curveFit
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- found_ROIsDataPoints
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3\n")
  
  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, peakStatistic=FALSE, plotEICsPath=NA, getAcquTime=TRUE, FIR=NULL, verbose=TRUE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check messages (can't check timing)
  expect_equal(length(result_singleFileSearch$messages), 8)
  expect_equal(result_singleFileSearch$messages[c(1:3,5,6)], expected_messages)
})

test_that('no peakStatistic, with plotEICsPath, no getAcquTime, no FIR, verbose', {
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- found_peakTable[,1:14]
  # Expected curveFit
  expected_curveFit       <- found_curveFit
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- found_ROIsDataPoints
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3\n")
  
  # temporary file
  savePath                <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.png')

  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, peakStatistic=FALSE, plotEICsPath=savePath, getAcquTime=FALSE, FIR=NULL, verbose=TRUE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check plot has been produced
  expect_true(file.exists(savePath))

  # Check messages (can't check timing and save path)
  expect_equal(length(result_singleFileSearch$messages), 8)
  expect_equal(result_singleFileSearch$messages[c(1,2,4,5)], expected_messages)
})

test_that('peakStatistic, plotEICsPath, getAcquTime, FIR, verbose', {
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- found_peakTable
  # Expected curveFit
  expected_curveFit       <- found_curveFit
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- found_ROIsDataPoints
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3\n")
  
  # temporary file
  savePath2               <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.png')
  
  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, peakStatistic=TRUE, plotEICsPath=savePath2, getAcquTime=TRUE, FIR=NULL, verbose=TRUE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check plot has been produced
  expect_true(file.exists(savePath2))
  
  # Check messages (can't check timing and save path)
  expect_equal(length(result_singleFileSearch$messages), 10)
  expect_equal(result_singleFileSearch$messages[c(1:3,5,6)], expected_messages)
})

test_that('change peak fitting params with ..., no peakStatistic, no plotEICsPath, no getAcquTime, no verbose', {
  # Expected TIC
  expected_TIC                      <- 2410533091
  # Expected peakTable
  expected_peakTable                <- found_peakTable[,1:14]
  expected_peakTable[3,2:11]        <- c(3418.0076795585401, 3455.6277710843374, 3495.4734240188186, 464.195358, 464.2000122, 464.204642, 11307215.264967661, 11538988, 380736, 381327.26552768378)
  # Expected curveFit
  expected_curveFit                 <- found_curveFit
  expected_curveFit[[3]]$amplitude  <- 64246.052173667762
  expected_curveFit[[3]]$center     <- 3450
  expected_curveFit[[3]]$sigma      <- 0.07533469863886906
  expected_curveFit[[3]]$gamma      <- 0.0019238229766131536
  expected_curveFit[[3]]$fitStatus  <- 2
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- found_ROIsDataPoints
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n")
  
  # Modify fit of window #3
  tmp_params              <- list(init_params  = list(amplitude=1E5, center=3455., sigma=0.1, gamma=0),
                                  lower_bounds = list(amplitude=0,   center=3450., sigma=0,   gamma=-0.1),
                                  upper_bounds = list(amplitude=1E9, center=3460., sigma=5,   gamma=0.1))
  new_params              <- list('guess', 'guess', tmp_params, 'guess')
  
  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, peakStatistic=FALSE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, verbose=FALSE, params=new_params))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-5)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check messages (loading file)
  expect_equal(length(result_singleFileSearch$messages), 1)
  expect_equal(result_singleFileSearch$messages, expected_messages)
})

test_that('no targetFeatures on import, no peakStatistic', {
  # Empty targetFeatures
  empty_ROI   <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
  
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- data.frame(matrix(vector(), 0, 14, dimnames=list(c(), c('cpdID', 'cpdName', 'found', 'rt', 'rtMin', 'rtMax', 'mz', 'mzMin', 'mzMax', 'peakArea', 'peakAreaRaw', 'maxIntMeasured', 'maxIntPredicted', 'is_filled'))), stringsAsFactors=FALSE)
  # Expected curveFit
  expected_curveFit       <- list()
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- list()
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 0 windows\n", "No data exist for the rt provided\n", "- No target features passed in 'targetFeatTable', no integration, only TIC will be reported -\n")
  
  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, empty_ROI, peakStatistic=FALSE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, verbose=TRUE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check messages (loading file)
  expect_equal(length(result_singleFileSearch$messages), 5)
  expect_equal(result_singleFileSearch$messages[1:4], expected_messages)
})

test_that('no targetFeatures on import, peakStatistic', {
  # Empty targetFeatures
  empty_ROI   <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
  
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- data.frame(matrix(vector(), 0, 18, dimnames=list(c(), c('cpdID', 'cpdName', 'found', 'rt', 'rtMin', 'rtMax', 'mz', 'mzMin', 'mzMax', 'peakArea', 'peakAreaRaw', 'maxIntMeasured', 'maxIntPredicted', 'is_filled', 'ppm_error', 'rt_dev_sec', 'tailingFactor', 'asymmetryFactor'))), stringsAsFactors=FALSE)
  # Expected curveFit
  expected_curveFit       <- list()
  # Expected ROIsDataPoint
  expected_ROIsDataPoint  <- list()
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 0 windows\n", "No data exist for the rt provided\n", "- No target features passed in 'targetFeatTable', no integration, only TIC will be reported -\n")
  
  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, empty_ROI, peakStatistic=TRUE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, verbose=TRUE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check messages (loading file)
  expect_equal(length(result_singleFileSearch$messages), 5)
  expect_equal(result_singleFileSearch$messages[1:4], expected_messages)
})

test_that('no features found, no FIR', {
  # targetFeature that doesn't give result
  noMatch_ROI                   <- input_ROI[1,]
  noMatch_ROI[1, 6:8]           <- c(52.194778, 52.2, 52.205222)

  # Expected TIC
  expected_TIC                  <- 2410533091
  # Expected peakTable
  expected_peakTable            <- data.frame(matrix(vector(), 1, 14, dimnames=list(c(), c('found', 'rtMin', 'rt', 'rtMax', 'mzMin', 'mz', 'mzMax', 'peakArea', 'peakAreaRaw', 'maxIntMeasured', 'maxIntPredicted', 'cpdID', 'cpdName', 'is_filled'))), stringsAsFactors=FALSE)
  expected_peakTable$cpdID      <- "ID-1"
  expected_peakTable$cpdName    <- "testCpd 1"
  expected_peakTable$found      <- FALSE
  expected_peakTable$is_filled  <- FALSE
  # Expected curveFit
  expected_curveFit             <- list(NA)
  # Expected ROIsDataPoint
  expected_ROIsDataPoint        <- list(data.frame(rt=numeric(), mz=numeric(), int=numeric(), stringsAsFactors=FALSE))
  # Expected acquTime
  expected_acquTime             <- NA
  # Expected messages
  expected_messages             <- c( "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 1 windows\n", "Fit of ROI #1 is unsuccessful (try err)\n")

  # results (output, warnings and messages)
  result_singleFileSearch       <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, noMatch_ROI, peakStatistic=FALSE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, verbose=TRUE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check messages (loading file)
  expect_equal(length(result_singleFileSearch$messages), 6)
  expect_equal(result_singleFileSearch$messages[c(1,2,4)], expected_messages)
})

test_that('one feature not found (#3), use FIR', {
  # input ROI, no window #3
  tmp_ROI                             <- input_ROI
  tmp_ROI[3,c('mzMin','mz','mzMax')]  <- c(46.19, 46.2, 46.21)
  # input FIR (use previously found peaks dimension. Need to widen mz as this test data doesn't give any mz width)
  input_FIR               <- found_peakTable[,c("mzMin", "mzMax", "rtMin", "rtMax")]
  input_FIR$mzMin         <- input_FIR$mzMin - 0.0005
  input_FIR$mzMax         <- input_FIR$mzMax + 0.0005
  
  # Expected TIC
  expected_TIC            <- 2410533091
  # Expected peakTable
  expected_peakTable      <- found_peakTable[,1:14]
  expected_peakTable[3,c('rtMin','rt', 'rtMax', 'mzMin', 'mz', 'mzMax', 'peakArea', 'peakAreaRaw', 'maxIntMeasured', 'maxIntPredicted')] <- c(3451.20759036145, 3454.435, 3501.66975049245, 464.194858, 464.2, 464.205142, 7744647, 7744647, 380736, as.numeric(NA))
  expected_peakTable$found[3]     <- TRUE
  expected_peakTable$is_filled[3] <- TRUE
  # Expected curveFit
  expected_curveFit       <- found_curveFit
  expected_curveFit[[3]]  <- NA
  # Expected ROIsDataPoint
  expected_ROIsDataPoint      <- found_ROIsDataPoints
  expected_ROIsDataPoint[[3]] <- data.frame(rt=numeric(), mz=numeric(), int=numeric(), stringsAsFactors=FALSE)
  # Expected acquTime
  expected_acquTime       <- NA
  # Expected messages
  expected_messages       <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Fit of ROI #3 is unsuccessful (try err)\n", "Reading data from 1 windows\n", "1 features to integrate with FIR\n")
  
  # results (output, warnings and messages)
  result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(singleSpectraDataPath, tmp_ROI, peakStatistic=FALSE, plotEICsPath=NA, getAcquTime=FALSE, FIR=input_FIR, verbose=TRUE))
  
  # Check results
  expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
  expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
  expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
  expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
  
  # Check messages (can't check timing)
  expect_equal(length(result_singleFileSearch$messages), 11)
  expect_equal(result_singleFileSearch$messages[c(1,2,4,5,7,9)], expected_messages)
})

test_that('raise errors', {
  
  # paths to trigger errors
  fakeInputPath     <- './notAPath/test.CDF'
  fakeOutputPath    <- './notAPath/save.png'
  wrongOutputFormat <- './notAPNG.txt'
  # FIR to trigger errors
  notADataFrame     <- 'not at dataframe'
  wrongNbrows       <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("mzMin", "mzMax", "rtMin", "rtMax"))), stringsAsFactors=FALSE)
  wrongCols         <- data.frame(matrix(vector(), 4, 4, dimnames=list(c(), c("wrongCol name", "mzMax", "rtMin", "rtMax"))), stringsAsFactors=FALSE)

  # singleSpectraDataPath doesnt exist
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath=fakeInputPath, input_ROI), "Check input, file*")
  # save folder doesnt exist
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, plotEICsPath=fakeOutputPath), "Check input, plotEICsPath folder*")
  # save file is not a .png
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, plotEICsPath=wrongOutputFormat), "Check input, plotEICsPath file name*")

  # not implemented curveModel
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, curveModel='FakeModel'), "Error: \"curveModel\" must be one of: skewedGaussian, emgGaussian")
  # FIR is not a data.frame
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, FIR=notADataFrame), "Check input, FIR must be a data.frame not character")
  # FIR has wrong number of rows
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, FIR=wrongNbrows), "Check input, FIR must have the same number of rows as targetFeatTable")
  # FIR columns are wrong
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI, FIR=wrongCols), 'Check input, FIR must have "mzMin", "mzMax", "rtMin" and "rtMax" as columns')
  
  # rtMin > rtMax
  wrong_ROI_rt     <- input_ROI
  wrong_ROI_rt[2,] <- c("ID-2", "testCpd 2", 3440., 3385.577, 3280., 496.195038, 496.2, 496.204962)
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, wrong_ROI_rt), 'Check input, "rtMin" must be <= to "rtMax"')
  # mzMin > mzMax
  wrong_ROI_mz     <- input_ROI
  wrong_ROI_mz[3,] <- c("ID-3", "testCpd 3", 3420., 3454.435, 3495., 464.204642, 464.2, 464.195358)
  expect_error(peakPantheR_singleFileSearch(singleSpectraDataPath, wrong_ROI_mz), 'Check input, "mzMin" must be <= to "mzMax"')
})


## cacheROI: decouple extraction span from fit span -------------------------
# Widen input_ROI by 100 sec rt and 0.5 Da mz for these tests — still contains
# the target peak, but larger than the narrow fit window.
wide_ROI    <- input_ROI
wide_ROI[, "rtMin"] <- wide_ROI[, "rtMin"] - 100
wide_ROI[, "rtMax"] <- wide_ROI[, "rtMax"] + 100
wide_ROI[, "mzMin"] <- wide_ROI[, "mzMin"] - 0.5
wide_ROI[, "mzMax"] <- wide_ROI[, "mzMax"] + 0.5

wide_ROIsDataPoints <- extractSignalRawData(tmp_raw_data,
    rt=wide_ROI[,c("rtMin","rtMax")], mz=wide_ROI[,c("mzMin","mzMax")],
    verbose=FALSE)

test_that('cacheROI = NULL matches the single-window behaviour', {
  # sanity: NULL cacheROI (default) is identical to not passing the arg
  ev_default <- peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI,
      peakStatistic=FALSE, getAcquTime=FALSE, verbose=FALSE)
  ev_null    <- peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI,
      peakStatistic=FALSE, getAcquTime=FALSE, verbose=FALSE,
      cacheROI=NULL)
  expect_equal(ev_null$TIC, ev_default$TIC)
  expect_equal(ev_null$peakTable, ev_default$peakTable, tolerance=1e-6)
  expect_equal(ev_null$ROIsDataPoint, ev_default$ROIsDataPoint)
})

test_that('cacheROI widens stored data while fit stays on targetFeatTable', {
  # ROIsDataPoint spans the wider extraction; the peak fit still matches the
  # narrow-fit-only run (center / area within float tolerance).
  ev_narrow <- peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI,
      peakStatistic=FALSE, getAcquTime=FALSE, verbose=FALSE)
  ev_wide   <- peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI,
      peakStatistic=FALSE, getAcquTime=FALSE, verbose=FALSE,
      cacheROI=wide_ROI)

  # Stored data spans the wider envelope (row count strictly greater, rt
  # range covers the wide bounds)
  for (i in seq_len(nrow(input_ROI))) {
    expect_gt(nrow(ev_wide$ROIsDataPoint[[i]]),
              nrow(ev_narrow$ROIsDataPoint[[i]]))
    expect_gte(min(ev_wide$ROIsDataPoint[[i]]$rt), wide_ROI$rtMin[i])
    expect_lte(max(ev_wide$ROIsDataPoint[[i]]$rt), wide_ROI$rtMax[i])
  }

  # Fit result matches the narrow-only run: peak center / area / rt drift
  expect_equal(ev_wide$peakTable$rt,   ev_narrow$peakTable$rt,
               tolerance=1e-6)
  expect_equal(ev_wide$peakTable$peakArea, ev_narrow$peakTable$peakArea,
               tolerance=1e-6)
  expect_equal(ev_wide$peakTable$found, ev_narrow$peakTable$found)
})

test_that('cache mode returns full cached span; fit still uses target subset', {
  # Cache spans wide bounds; target is narrow. ROIsDataPoint returned equals
  # cachedDataPoints (identity), fit still lands on the peak.
  ev_cache <- peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI,
      peakStatistic=FALSE, getAcquTime=FALSE, verbose=FALSE,
      cacheROI=wide_ROI, cachedDataPoints=wide_ROIsDataPoints)

  # Returned data is the full cached span (row counts match cachedDataPoints)
  for (i in seq_along(wide_ROIsDataPoints)) {
    expect_equal(nrow(ev_cache$ROIsDataPoint[[i]]),
                 nrow(wide_ROIsDataPoints[[i]]))
  }

  # Fit result matches a non-cache narrow run
  ev_narrow <- peakPantheR_singleFileSearch(singleSpectraDataPath, input_ROI,
      peakStatistic=FALSE, getAcquTime=FALSE, verbose=FALSE)
  expect_equal(ev_cache$peakTable$rt,   ev_narrow$peakTable$rt,
               tolerance=1e-6)
  expect_equal(ev_cache$peakTable$peakArea, ev_narrow$peakTable$peakArea,
               tolerance=1e-6)
  expect_equal(ev_cache$peakTable$found, ev_narrow$peakTable$found)
})

## buildFIRData -------------------------------------------------------------
buildFIR_ROI <- data.frame(
    rtMin = c(3309.7589296586070, 3670.9201232710743),
    rt    = c(3346.8277590361445, 3704.1427831325304),
    rtMax = c(3385.4098874628098, 3740.0172511251831),
    mzMin = c(522.1995, 536.1995),
    mz    = c(522.2,    536.2),
    mzMax = c(522.2005, 536.2005),
    stringsAsFactors = FALSE)

buildFIR_ROIsDataPoint <- extractSignalRawData(tmp_raw_data,
    rt = buildFIR_ROI[, c("rtMin", "rtMax")],
    mz = buildFIR_ROI[, c("mzMin", "mzMax")],
    verbose = FALSE)


test_that('buildFIRData: empty needsFilling returns list() silently', {
    FIR <- buildFIR_ROI[, c("mzMin", "mzMax", "rtMin", "rtMax")]
    res <- evaluate_promise(buildFIRData(tmp_raw_data, buildFIR_ROIsDataPoint,
        buildFIR_ROI, FIR, needsFilling_idx = integer(0), verbose = TRUE))
    expect_equal(res$result, list())
    expect_equal(length(res$messages), 0)
})


test_that('buildFIRData: all-contained reuses in-memory data', {
    FIR <- buildFIR_ROI[, c("mzMin", "mzMax", "rtMin", "rtMax")]

    res <- evaluate_promise(buildFIRData(tmp_raw_data, buildFIR_ROIsDataPoint,
        buildFIR_ROI, FIR, needsFilling_idx = c(1, 2), verbose = TRUE))

    fresh <- extractSignalRawData(tmp_raw_data,
        mz = data.frame(mzMin = FIR$mzMin, mzMax = FIR$mzMax),
        rt = data.frame(rtMin = FIR$rtMin, rtMax = FIR$rtMax),
        verbose = FALSE)
    expect_equal(res$result, fresh)

    expect_equal(length(res$messages), 1)
    expect_equal(res$messages[1],
        "FIR data reused from ROI for 2/2 windows\n")
})


test_that('buildFIRData: none-contained falls back to batched extract', {
    FIR <- buildFIR_ROI[, c("mzMin", "mzMax", "rtMin", "rtMax")]
    FIR$mzMin <- FIR$mzMin - 0.01
    FIR$mzMax <- FIR$mzMax + 0.01

    res <- evaluate_promise(buildFIRData(tmp_raw_data, buildFIR_ROIsDataPoint,
        buildFIR_ROI, FIR, needsFilling_idx = c(1, 2), verbose = TRUE))

    fresh <- extractSignalRawData(tmp_raw_data,
        mz = data.frame(mzMin = FIR$mzMin, mzMax = FIR$mzMax),
        rt = data.frame(rtMin = FIR$rtMin, rtMax = FIR$rtMax),
        verbose = FALSE)
    expect_equal(res$result, fresh)

    reuse_msgs <- grep("reused from ROI", res$messages, value = TRUE)
    expect_equal(length(reuse_msgs), 0)
    read_msgs <- grep("Reading data from 2 windows", res$messages,
        value = TRUE)
    expect_equal(length(read_msgs), 1)
})


test_that('buildFIRData: mixed reuse one row, extract the other', {
    FIR <- buildFIR_ROI[, c("mzMin", "mzMax", "rtMin", "rtMax")]
    FIR$mzMin[2] <- FIR$mzMin[2] - 0.01
    FIR$mzMax[2] <- FIR$mzMax[2] + 0.01

    res <- evaluate_promise(buildFIRData(tmp_raw_data, buildFIR_ROIsDataPoint,
        buildFIR_ROI, FIR, needsFilling_idx = c(1, 2), verbose = TRUE))

    fresh <- extractSignalRawData(tmp_raw_data,
        mz = data.frame(mzMin = FIR$mzMin, mzMax = FIR$mzMax),
        rt = data.frame(rtMin = FIR$rtMin, rtMax = FIR$rtMax),
        verbose = FALSE)
    expect_equal(res$result, fresh)

    expect_true(any(res$messages ==
        "FIR data reused from ROI for 1/2 windows\n"))
    expect_true(any(res$messages == "Reading data from 1 windows\n"))
})


test_that('buildFIRData errors when raw_data NULL and FIR not contained', {
    cached_dp <- extractSignalRawData(tmp_raw_data,
        rt = input_ROI[, c("rtMin", "rtMax")],
        mz = input_ROI[, c("mzMin", "mzMax")], verbose = FALSE)
    FIR_bad <- data.frame(
        rtMin = c(3336.542, 3378.274, 3444.524, 3689.7),
        rtMax = c(3390.272, 3426.266, 3478.431, 3738.213),
        mzMin = c(522.1995 - 10, 496.1995, 464.1995, 536.1995),
        mzMax = c(522.2005 + 10, 496.2005, 464.2005, 536.2005))

    expect_error(
        peakPantheR:::buildFIRData(raw_data = NULL,
            ROIsDataPoint = cached_dp, ROI = input_ROI,
            FIR = FIR_bad, needsFilling_idx = c(1L, 2L),
            verbose = FALSE),
        regexp = "Cached annotation in use")
})
