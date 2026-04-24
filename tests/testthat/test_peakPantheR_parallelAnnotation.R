context('peakPantheR_parallelAnnotation()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

if (.Platform$OS.type == 'windows') {
    BPPARAM_parallel <- BiocParallel::SnowParam(workers = 1)
} else {
    BPPARAM_parallel <- BiocParallel::MulticoreParam(workers = 1)
}

## Input and expected data
# 3 files
input_spectraPaths          <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                                 system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                                 system.file('cdf/KO/ko18.CDF', package = "faahKO"))
## 1 missing file
input_missingSpectraPaths   <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                                 "aaa/bbb.cdf",
                                 system.file('cdf/KO/ko18.CDF', package = "faahKO"))

# 4 features
input_targetFeatTable     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
input_targetFeatTable[1,] 	<- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] 	<- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_targetFeatTable[3,] 	<- c("ID-3", "Cpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
input_targetFeatTable[4,] 	<- c("ID-4", "Cpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
input_targetFeatTable[,c(3:8)] <- sapply(input_targetFeatTable[,c(3:8)], as.numeric)

# FIR
input_FIR     	            <- data.frame(matrix(vector(), 4, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))),stringsAsFactors=FALSE)
input_FIR[1,] 	            <- c(3336.542, 3390.272, 522.1995, 522.2005)
input_FIR[2,] 	            <- c(3378.274, 3426.266, 496.1995, 496.2005)
input_FIR[3,] 	            <- c(3444.524, 3478.431, 464.1995, 464.2005)
input_FIR[4,] 	            <- c(3689.7,   3738.213, 536.1995, 536.2005)
input_FIR[,c(1:4)]          <- sapply(input_FIR[,c(1:4)], as.numeric)

# uROI (ROI are deliberately wider than the target peak but still contain it;
# uROI brings the fit window back onto the peak). ROI is the extraction
# envelope in the new fit-vs-extract split, so we keep it moderately wide
# rather than whole-file-wide — the semantics exercised are identical
# ("uROI ⊂ ROI and uROI is the integration window") but extraction /
# @dataPoints stay tractable.
input_uROI                  <- input_targetFeatTable[,c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
input_badtargetFeatTable    <- input_targetFeatTable
input_badtargetFeatTable[1, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(3200, 3500, 521, 523)
input_badtargetFeatTable[2, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(3200, 3500, 495, 497)
input_badtargetFeatTable[3, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(3350, 3600, 463, 465)
input_badtargetFeatTable[4, c("rtMin", "rtMax", "mzMin", "mzMax")] <- c(3550, 3850, 535, 537)

# cpdMetadata
input_cpdMetadata     <- data.frame(matrix(data=c('a','b','c','d',1,2,3,4), nrow=4, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)

# spectraMetadata
input_spectraMetadata <- data.frame(matrix(data=c('e','f','g',5,6,7), nrow=3, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)


# Expected peakTables
peakTable1     <- data.frame(matrix(vector(), 4, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
peakTable1[1,] <- c(TRUE, 3309.7589296586070, 3346.8277590361445, 3385.4098874628098, 522.194778, 522.20001220703125, 522.205222, 26133726.6811244078,  26071378, 889280, 901015.80529226747, FALSE, 0.023376160866574614, 1.93975903614455092, 1.0153573486330891, 1.0268238825675249)
peakTable1[2,] <- c(TRUE, 3345.3766648628907, 3386.5288072289159, 3428.2788374983961, 496.20001220703125, 496.20001220703125, 496.20001220703125, 35472141.3330242932, 36498367, 1128960, 1113576.69008227298, FALSE, 0.024601030353423384, 0.95180722891564074, 1.0053782620427065, 1.0093180792278085)
peakTable1[3,] <- c(TRUE, 3451.2075903614455, 3451.5072891566265, 3501.6697504924518, 464.195358, 464.20001220703125, 464.204642, 7498427.1583901159, 7608218, 380736, 389632.13549519412, FALSE, 0.026296922148575364, -2.92771084337346110, 207.6939219686769036, 380.5019028782010082)
peakTable1[4,] <- c(TRUE, 3670.9201232710743, 3704.1427831325304, 3740.0172511251831, 536.20001220703125, 536.20001220703125, 536.20001220703125, 8626279.9788195733, 8692184, 330176, 326763.87246511364, FALSE, 0.022765817240815486, 2.44578313253032320, 1.0305289730128095, 1.0536948855480386)
peakTable1[,c(1,12)]       <- sapply(peakTable1[,c(1,12)], as.logical)
peakTable1[,c(2:11,13:16)] <- sapply(peakTable1[,c(2:11,13:16)], as.numeric)
# 2
peakTable2     <- data.frame(matrix(vector(), 4, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
peakTable2[1,] <- c(TRUE, 3326.1063495851854, 3365.102, 3407.2726475892355, 522.194778, 522.20001220703125, 522.205222, 24545301.622835573, 23367885, 761664, 790802.2209998488, FALSE, 0.023376160866574614, 20.2139999999999, 1.0339153786516375, 1.0630802030537212)
peakTable2[2,] <- c(TRUE, 3365.0238566258713, 3405.791, 3453.4049569205681, 496.195038, 496.20001220703125, 496.204962, 37207579.286265120, 36858762, 1099264, 1098720.2929832144, FALSE, 0.024601030353423384, 20.2139999999999, 1.0839602450900523, 1.1717845972583161)
peakTable2[3,] <- c(TRUE, 3425.9772908380342, 3466.1733975903617, 3508.0320324994614, 464.195358, 464.20001220703125, 464.204642, 11512269.4488430563, 11245667, 366720, 365928.64209905855, FALSE, 0.026296922148575364, 11.738397590361728, 1.0157024674594695, 1.0275996777323413)
peakTable2[4,] <- c(TRUE, 3678.1204484629088, 3720.347, 3768.8011145239534, 536.194638, 536.20001220703125, 536.205362, 7983406.0412310315, 7383963, 220096, 229651.947643013, FALSE, 0.022765817240815486, 18.65, 1.0738577621711971, 1.1514233243540024)
peakTable2[,c(1,12)]       <- sapply(peakTable2[,c(1,12)], as.logical)
peakTable2[,c(2:11,13:16)] <- sapply(peakTable2[,c(2:11,13:16)], as.numeric)
# 3
peakTable3     <- data.frame(matrix(vector(), 4, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
peakTable3[1,] <- c(TRUE, 3333.8625894557053, 3368.233, 3407.4362838927614, 522.194778, 522.20001220703125, 522.205222, 21447174.404490683, 20364558, 758336, 765009.9805796633, FALSE, 0.023376160866574614, 23.345000000000255, 1.0609102044546637, 1.1155310457756928)
peakTable3[2,] <- c(TRUE, 3373.3998828113113, 3413.4952530120481, 3454.4490330927388, 496.195038, 496.20001220703125, 496.204962, 35659353.614476241, 34792490, 1149440, 1145857.7611069249, FALSE, 0.024601030353423384, 27.918253012047899, 1.0081407426394933, 1.0143315197994494)
peakTable3[3,] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, NA, NA, NA, NA)
peakTable3[4,] <- c(TRUE, 3672.3110625980275, 3714.088, 3761.43921706666, 536.194638, 536.20001220703125, 536.205362, 6467062.4309558524, 6360742, 196160, 189416.24807174454, FALSE, 0.022765817240815486, 12.391, 1.0656000616594850, 1.1333185588985102)
peakTable3[,c(1,12)]       <- sapply(peakTable3[,c(1,12)], as.logical)
peakTable3[,c(2:11,13:16)] <- sapply(peakTable3[,c(2:11,13:16)], as.numeric)
expected_peakTables <- list(peakTable1, peakTable2, peakTable3)

# Expected peakFit
# 1
cFit1.1         <- list(amplitude=162404.8057918259, center=3341.888, sigma=0.078786133031045896, gamma=0.0018336101984172684, fitStatus=2, curveModel="skewedGaussian")
class(cFit1.1)  <- 'peakPantheR_curveFit'
cFit1.2         <- list(amplitude=199249.10572753669, center=3382.577, sigma=0.074904415304607966, gamma=0.0011471899372353885, fitStatus=2, curveModel="skewedGaussian")
class(cFit1.2)  <- 'peakPantheR_curveFit'
cFit1.3         <- list(amplitude=31645.961277502651, center=3451.435, sigma=0.064803553287811053, gamma=2.8557893789555022, fitStatus=2, curveModel="skewedGaussian")
class(cFit1.3)  <- 'peakPantheR_curveFit'
cFit1.4         <- list(amplitude=59193.591103772116, center=3698.697, sigma=0.082789238806238355, gamma=0.0026044299691057823, fitStatus=2, curveModel="skewedGaussian")
class(cFit1.4)  <- 'peakPantheR_curveFit'
# 2
cFit2.1         <- list(amplitude=124090.83425474487, center=3359.102, sigma=0.071061541060964212, gamma=0.0018336072657203239, fitStatus=2, curveModel="skewedGaussian")
class(cFit2.1)  <- 'peakPantheR_curveFit'
cFit2.2         <- list(amplitude=151407.23415130575, center=3399.791, sigma=0.063753866057052563, gamma=0.001676782834598999, fitStatus=2, curveModel="skewedGaussian")
class(cFit2.2)  <- 'peakPantheR_curveFit'
cFit2.3         <- list(amplitude=60803.292514896151, center=3460.824, sigma=0.072633009809671245, gamma=0.0015588615168115277, fitStatus=2, curveModel="skewedGaussian")
class(cFit2.3)  <- 'peakPantheR_curveFit'
cFit2.4         <- list(amplitude=31542.623023583215, center=3714.347, sigma=0.062820857630350654, gamma=0.0015366513073237947, fitStatus=2, curveModel="skewedGaussian")
class(cFit2.4)  <- 'peakPantheR_curveFit'
# 3
cFit3.1         <- list(amplitude=122363.51256736703, center=3362.233, sigma=0.075489598945304492, gamma=0.0025160536725299734, fitStatus=2, curveModel="skewedGaussian")
class(cFit3.1)  <- 'peakPantheR_curveFit'
cFit3.2         <- list(amplitude=204749.86097918145, center=3409.182, sigma=0.075731781812843249, gamma=0.0013318670577834328, fitStatus=2, curveModel="skewedGaussian")
class(cFit3.2)  <- 'peakPantheR_curveFit'
cFit3.3         <- NA
cFit3.4         <- list(amplitude=26628.505498512375, center=3708.088, sigma=0.064131129861254479, gamma=0.0015719426982490699, fitStatus=2, curveModel="skewedGaussian")
class(cFit3.4)  <- 'peakPantheR_curveFit'
expected_peakFit <- list(list(cFit1.1, cFit1.2, cFit1.3, cFit1.4), list(cFit2.1, cFit2.2, cFit2.3, cFit2.4), list(cFit3.1, cFit3.2, cFit3.3, cFit3.4))

# Expected dataPoints (extracted at the narrow input_targetFeatTable bounds;
# @dataPoints under the new extract-at-ROI split stays at these bounds when
# ROI = uROI = input_targetFeatTable, i.e. the default-constructor case).
tmp_raw_data1  	  <- MSnbase::readMSData(input_spectraPaths[1], centroided=TRUE, mode='onDisk')
ROIDataPoints1    <- extractSignalRawData(tmp_raw_data1, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
tmp_raw_data2  	  <- MSnbase::readMSData(input_spectraPaths[2], centroided=TRUE, mode='onDisk')
ROIDataPoints2    <- extractSignalRawData(tmp_raw_data2, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
tmp_raw_data3  	  <- MSnbase::readMSData(input_spectraPaths[3], centroided=TRUE, mode='onDisk')
ROIDataPoints3    <- extractSignalRawData(tmp_raw_data3, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
expected_dataPoints <- list(ROIDataPoints1, ROIDataPoints2, ROIDataPoints3)

# Expected dataPoints when ROI = input_badtargetFeatTable (extraction at the
# moderately-wide bad ROI; used by the "uROI corrects ROI" tests below).
ROIDataPoints1_bad <- extractSignalRawData(tmp_raw_data1, rt=input_badtargetFeatTable[,c('rtMin','rtMax')], mz=input_badtargetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
ROIDataPoints3_bad <- extractSignalRawData(tmp_raw_data3, rt=input_badtargetFeatTable[,c('rtMin','rtMax')], mz=input_badtargetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
# Samples 1 and 3 are the surviving files in the input_missingSpectraPaths
# tests; sample 2 is "aaa/bbb.cdf" and never reaches @dataPoints.
expected_dataPoints_badROI <- list(ROIDataPoints1_bad, ROIDataPoints3_bad)


# if ((.Platform$OS.type != "windows") || (.Machine$sizeof.pointer == 8)) {
# test_that('3 files, 4 compounds, no uROI, no FIR, no getAcquTime, no verbose', {
#   # Object fully initialised
#   initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

#   # Expected annotation
#   expected_annotation             <- initAnnotation
#   expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
#   expected_annotation@peakTables  <- expected_peakTables
#   expected_annotation@peakFit     <- expected_peakFit
#   expected_annotation@dataPoints  <- expected_dataPoints
#   expected_annotation@isAnnotated <- TRUE
#   # Expected failures
#   tmp_status          <- NA
#   names(tmp_status)   <- 'test'
#   tmp_failures        <- !is.na(tmp_status)
#   names(tmp_failures) <- NULL
#   expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
#   # Expected message
#   expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n")

#   # results (output, warnings and messages)
#   result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, getAcquTime=FALSE, verbose=FALSE))

#   # Check results
#   expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
#   expect_equal(result_parallelAnnotation$result$failures, expected_failures)

#   # Check messages (centwave output)
#   expect_equal(length(result_parallelAnnotation$messages), 3)
#   expect_equal(result_parallelAnnotation$messages, expected_message)
#   }) }

# if ((.Platform$OS.type != "windows") || (.Machine$sizeof.pointer == 8)) {
# test_that('3 files (1 missing), 4 compounds, no uROI, no FIR, no getAcquTime, no verbose', {
#   # Object fully initialised
#   initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

#   # Expected annotation
#   expected_annotation             <- initAnnotation[c(1,3),]
#   expected_annotation@TIC         <- c(2410533091, 2332817115)
#   expected_annotation@peakTables  <- expected_peakTables[c(1,3)]
#   expected_annotation@peakFit     <- expected_peakFit[c(1,3)]
#   expected_annotation@dataPoints  <- expected_dataPoints[c(1,3)]
#   expected_annotation@isAnnotated <- TRUE
#   # Expected failures
#   tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
#   names(tmp_status)   <- 'aaa/bbb.cdf'
#   tmp_failures        <- !is.na(tmp_status)
#   names(tmp_failures) <- NULL
#   expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
#   # Expected message
#   expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n")

#   # results (output, warnings and messages)
#   result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, getAcquTime=FALSE, verbose=FALSE))

#   # Check results
#   expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-6)
#   expect_equal(result_parallelAnnotation$result$failures, expected_failures)

#   # Check messages (centwave output)
#   expect_equal(length(result_parallelAnnotation$messages), 2)
#   expect_equal(result_parallelAnnotation$messages, expected_message)
#   }) }

test_that('3 files, 4 compounds, no uROI, no FIR, no getAcquTime, no verbose, modify parameter with ... (cpd #3)', {
  # Cpd 3 is now found in 3rd file
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

  # Modify fit of window #3
  tmp_params              <- list(init_params  = list(amplitude=1E5, center=3455., sigma=0.1, gamma=0),
                                  lower_bounds = list(amplitude=0,   center=3450., sigma=0,   gamma=-0.1),
                                  upper_bounds = list(amplitude=1E9, center=3460., sigma=5,   gamma=0.1))
  new_params              <- list('guess', 'guess', tmp_params, 'guess')
  
  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  # modify peakTable
  tmp_peakTable1                  <- expected_peakTables[[1]]
  tmp_peakTable1[3,c(2:11,13:16)] <- c(3418.0076795585401, 3455.6277710843374, 3495.4734240188186, 464.195358, 464.2000122, 464.204642, 11307215.264967661, 11538988, 380736, 381327.26552768378,  0.026296922148575364, 1.1927710843374371, 1.0223856861398346, 1.0391667928715738)
  tmp_peakTable2                  <- expected_peakTables[[2]]
  tmp_peakTable2[3,c(2:11,13:16)] <- c(3433.0520428402692, 3464.1974939759039, 3518.3673814020449, 464.195358, 464.2000122, 464.204642, 11632481.39997852, 11167979, 366720, 370275.43095360085,  0.026296922148575364, 9.762493975903908, 1.3108566292516153, 1.566712445298531)
  tmp_peakTable3                  <- expected_peakTables[[3]]
  tmp_peakTable3[3,c(2:11,13:16)] <- c(3428.5236305028589, 3459.5514216867468, 3510.3107983995801, 464.195358, 464.2000122, 464.204642, 9697604.1795769241, 9501971, 319488, 321005.94361925457,  0.026296922148575364, 5.1164216867468895, 1.2652077714541707, 1.4814479150398188)
  tmp_peakTable3$found[3]         <- TRUE
  expected_annotation@peakTables  <- list(tmp_peakTable1, tmp_peakTable2, tmp_peakTable3)
  # modify curveFit
  tmp_curveFit1.3         <- list(amplitude=64246.052173667762, center=3450, sigma=0.07533469863886906, gamma=0.0019238229766131536, fitStatus=2, curveModel="skewedGaussian")
  class(tmp_curveFit1.3)  <- 'peakPantheR_curveFit'
  tmp_curveFit2.3         <- list(amplitude=31499.356902732343, center=3454.217764223421, sigma=0.052005180846722628, gamma=0.0030181085286239786, fitStatus=1, curveModel="skewedGaussian")
  class(tmp_curveFit2.3)  <- 'peakPantheR_curveFit'
  tmp_curveFit3.3         <- list(amplitude=29842.598057228814, center=3450, sigma=0.055439446898995937, gamma=0.0031338189157350213, fitStatus=2, curveModel="skewedGaussian")
  class(tmp_curveFit3.3)  <- 'peakPantheR_curveFit'
  expected_annotation@peakFit           <- expected_peakFit
  expected_annotation@peakFit[[1]][[3]] <- tmp_curveFit1.3
  expected_annotation@peakFit[[2]][[3]] <- tmp_curveFit2.3
  expected_annotation@peakFit[[3]][[3]] <- tmp_curveFit3.3
  expected_annotation@dataPoints  <- expected_dataPoints
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message (Windows only, no message in Linux MulticoreParam)
  expected_message    <- "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n\n"

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, BPPARAM=BPPARAM_parallel, getAcquTime=FALSE, verbose=FALSE, params=new_params))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  if (.Platform$OS.type == 'windows') {
    expect_equal(length(result_parallelAnnotation$messages), 1)
    expect_equal(result_parallelAnnotation$messages, expected_message)
  } else {
    expect_equal(length(result_parallelAnnotation$messages), 0)
  }
})

test_that('3 files, 4 compounds, no uROI, no FIR, no getAcquTime, no verbose, peaks not found and not replaced (cpd #3)', {
  # Cpd #3 will not give results
  noMatch_ROI3        <- input_targetFeatTable
  noMatch_ROI3[3,6:8] <- c(52.194778, 52.2, 52.205222)
  
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=noMatch_ROI3, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)
  
  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  tmp_peakTables                        <- expected_peakTables
  tmp_peakTables[[1]][3,c(2:11,13:16)]  <- as.numeric(NA)
  tmp_peakTables[[1]]$found[3]          <- FALSE
  tmp_peakTables[[2]][3,c(2:11,13:16)]  <- as.numeric(NA)
  tmp_peakTables[[2]]$found[3]          <- FALSE
  tmp_peakTables[[3]][3,c(2:11,13:16)]  <- as.numeric(NA)
  tmp_peakTables[[3]]$found[3]          <- FALSE
  expected_annotation@peakTables        <- tmp_peakTables
  tmp_peakFit                     <- expected_peakFit
  tmp_peakFit[[1]][[3]]           <- NA
  tmp_peakFit[[2]][[3]]           <- NA
  tmp_peakFit[[3]][[3]]           <- NA
  expected_annotation@peakFit     <- tmp_peakFit
  tmp_dataPoints                  <- expected_dataPoints
  tmp_dataPoints[[1]][[3]]        <- data.frame(rt=numeric(), mz=numeric(), int=numeric())
  tmp_dataPoints[[2]][[3]]        <- data.frame(rt=numeric(), mz=numeric(), int=numeric())
  tmp_dataPoints[[3]][[3]]        <- data.frame(rt=numeric(), mz=numeric(), int=numeric())
  expected_annotation@dataPoints  <- tmp_dataPoints
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message (Windows only, no message in Linux MulticoreParam)
  expected_message    <- "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n\n"
  
  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, BPPARAM=BPPARAM_parallel, getAcquTime=FALSE, verbose=FALSE))
  
  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)
  
  # Check messages (centwave output)
  if (.Platform$OS.type == 'windows') {
    expect_equal(length(result_parallelAnnotation$messages), 1)
    expect_equal(result_parallelAnnotation$messages, expected_message)
  } else {
    expect_equal(length(result_parallelAnnotation$messages), 0)
  }
})

test_that('3 files, 4 compounds, no uROI, FIR replace peaks not found (cpd #3), no getAcquTime, no verbose', {
  # Cpd #3 will not give results, FIR will replace
  noMatch_ROI3        <- input_targetFeatTable
  noMatch_ROI3[3,6:8] <- c(52.194778, 52.2, 52.205222)
  
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=noMatch_ROI3, useFIR=TRUE, FIR=input_FIR, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)
  
  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  tmp_peakTables                        <- expected_peakTables
  tmp_peakTables[[1]][3,c(2:11,13:16)]  <- c(3444.524, 3454.435, 3478.431, 464.1995, 464.20001220703125, 464.2005, 8939889, 8939889, 380736, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[1]]$is_filled[3]      <- TRUE
  tmp_peakTables[[2]][3,c(2:11,13:16)]  <- c(3444.524, 3463.824, 3478.431, 464.1995, 464.20001220703125, 464.2005, 8863585, 8863585, 366720, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[2]]$is_filled[3]      <- TRUE
  tmp_peakTables[[3]][3,c(2:11,13:16)]  <- c(3444.524, 3460.696, 3478.431, 464.1995, 464.20001220703125, 464.2005, 7414831, 7414831, 319488,  as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[3]]$found[3]      <- TRUE
  tmp_peakTables[[3]]$is_filled[3]      <- TRUE
  expected_annotation@peakTables        <- tmp_peakTables
  tmp_peakFit                     <- expected_peakFit
  tmp_peakFit[[1]][[3]]           <- NA
  tmp_peakFit[[2]][[3]]           <- NA
  tmp_peakFit[[3]][[3]]           <- NA
  expected_annotation@peakFit     <- tmp_peakFit
  tmp_dataPoints                  <- expected_dataPoints
  tmp_dataPoints[[1]][[3]]        <- data.frame(rt=numeric(), mz=numeric(), int=numeric())
  tmp_dataPoints[[2]][[3]]        <- data.frame(rt=numeric(), mz=numeric(), int=numeric())
  tmp_dataPoints[[3]][[3]]        <- data.frame(rt=numeric(), mz=numeric(), int=numeric())
  expected_annotation@dataPoints  <- tmp_dataPoints
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message (Windows only, no message in Linux MulticoreParam)
  expected_message    <- "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n\n"
  
  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, BPPARAM=BPPARAM_parallel, getAcquTime=FALSE, verbose=FALSE))
  
  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)
  
  # Check messages (centwave output)
  if (.Platform$OS.type == 'windows') {
    expect_equal(length(result_parallelAnnotation$messages), 1)
    expect_equal(result_parallelAnnotation$messages, expected_message)
  } else {
    expect_equal(length(result_parallelAnnotation$messages), 0)
  }
})

# if ((.Platform$OS.type != "windows") || (.Machine$sizeof.pointer == 8)) {
# test_that('3 files, 4 compounds, uROI, no FIR, no fitGauss, no getAcquTime, no verbose', {
#   # Object fully initialised
#   initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=input_uROI, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

#   # Expected annotation
#   expected_annotation             <- initAnnotation
#   expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
#   expected_annotation@peakTables  <- expected_peakTables
#   expected_annotation@peakFit     <- expected_peakFit
#   expected_annotation@dataPoints  <- expected_dataPoints
#   expected_annotation@isAnnotated <- TRUE
#   # Expected failures
#   tmp_status          <- NA
#   names(tmp_status)   <- 'test'
#   tmp_failures        <- !is.na(tmp_status)
#   names(tmp_failures) <- NULL
#   expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
#   # Expected message
#   expected_message    <- c("Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n")

#   # results (output, warnings and messages)
#   result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, getAcquTime=FALSE, verbose=FALSE))

#   # Check results
#   expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
#   expect_equal(result_parallelAnnotation$result$failures, expected_failures)

#   # Check messages (centwave output)
#   expect_equal(length(result_parallelAnnotation$messages), 3)
#   expect_equal(result_parallelAnnotation$messages, expected_message)
#   }) }

test_that('serial: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found (cpd #3), getAcquTime, verbose', {
  # sample 2 is missing
  # Cpd #3 will not give results
  noMatch_uROI3        <- input_uROI
  noMatch_uROI3[3,4:6] <- c(52.194778, 52.2, 52.205222)
  
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=noMatch_uROI3, useFIR=TRUE, FIR=input_FIR, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

  # Expected annotation
  expected_annotation             <- initAnnotation[c(1,3),]
  expected_annotation@TIC         <- c(2410533091, 2332817115)
  tmp_peakTables                        <- expected_peakTables[c(1,3)]
  tmp_peakTables[[1]][3,c(2:11,13:16)]  <- c(3444.524, 3454.435, 3478.431, 464.1995, 464.20001220703125, 464.2005, 8939889, 8939889, 380736, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[1]]$is_filled[3]      <- TRUE
  tmp_peakTables[[2]][3,c(2:11,13:16)]  <- c(3444.524, 3460.696, 3478.431, 464.1995, 464.20001220703125, 464.2005, 7414831, 7414831, 319488, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[2]]$found[3]          <- TRUE
  tmp_peakTables[[2]]$is_filled[3]      <- TRUE
  expected_annotation@peakTables        <- tmp_peakTables
  tmp_peakFit                     <- expected_peakFit[c(1,3)]
  tmp_peakFit[[1]][[3]]           <- NA
  tmp_peakFit[[2]][[3]]           <- NA
  expected_annotation@peakFit     <- tmp_peakFit
  # @dataPoints is now extracted at @ROI (the bad-ROI bounds) rather than
  # @uROI: cpd #3's uROI has a shifted mz so the fit fails, but the
  # extraction still returns the ROI-wide EIC for cpd #3.
  tmp_dataPoints                  <- expected_dataPoints_badROI
  expected_annotation@dataPoints  <- tmp_dataPoints
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
  names(tmp_status)   <- 'aaa/bbb.cdf'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message. FIR is now served from the wider @ROI extraction
  # ("FIR data reused from ROI ...") instead of a separate disk read
  # ("Reading data from 1 windows").
  expected_message    <- c("Processing 4 compounds in 3 samples:\n", "  uROI:\tTRUE\n", "  FIR:\tTRUE\n", "----- ko15 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Fit of ROI #3 is unsuccessful (try err)\n", "FIR data reused from ROI for 1/1 windows\n", "1 features to integrate with FIR\n", "Error file does not exist: aaa/bbb.cdf\n", "----- ko18 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #2\n", "Fit of ROI #3 is unsuccessful (try err)\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #4\n", "FIR data reused from ROI for 1/1 windows\n", "1 features to integrate with FIR\n", "----------------\n", "1 file(s) failed to process:\n         file                                  error\n1 aaa/bbb.cdf Error file does not exist: aaa/bbb.cdf\n", "Annotation object cannot be reordered by sample acquisition date\n", "----------------\n", "  1 failure(s)\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, nCores=1, getAcquTime=TRUE, verbose=TRUE))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-6)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (no timing)
  expect_equal(length(result_parallelAnnotation$messages), 38)
  expect_equal(result_parallelAnnotation$messages[c(1:7,9,10,13,14,17:21,23:26,29,30,33,34,35,36,38)], expected_message)
})

test_that('parallel: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found (cpd #3), getAcquTime, verbose', {
  # sample 2 is missing
  # Cpd #3 will not give results
  noMatch_uROI3        <- input_uROI
  noMatch_uROI3[3,4:6] <- c(52.194778, 52.2, 52.205222)
  
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=noMatch_uROI3, useFIR=TRUE, FIR=input_FIR, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)
  
  # Expected annotation
  expected_annotation             <- initAnnotation[c(1,3),]
  expected_annotation@TIC         <- c(2410533091, 2332817115)
  tmp_peakTables                        <- expected_peakTables[c(1,3)]
  tmp_peakTables[[1]][3,c(2:11,13:16)]  <- c(3444.524, 3454.435, 3478.431, 464.1995, 464.20001220703125, 464.2005, 8939889, 8939889, 380736, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[1]]$is_filled[3]      <- TRUE
  tmp_peakTables[[2]][3,c(2:11,13:16)]  <- c(3444.524, 3460.696, 3478.431, 464.1995, 464.20001220703125, 464.2005, 7414831, 7414831, 319488, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[2]]$found[3]          <- TRUE
  tmp_peakTables[[2]]$is_filled[3]      <- TRUE
  expected_annotation@peakTables        <- tmp_peakTables
  tmp_peakFit                     <- expected_peakFit[c(1,3)]
  tmp_peakFit[[1]][[3]]           <- NA
  tmp_peakFit[[2]][[3]]           <- NA
  expected_annotation@peakFit     <- tmp_peakFit
  tmp_dataPoints                  <- expected_dataPoints_badROI
  expected_annotation@dataPoints  <- tmp_dataPoints
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
  names(tmp_status)   <- 'aaa/bbb.cdf'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- c("Processing 4 compounds in 3 samples:\n", "  uROI:\tTRUE\n", "  FIR:\tTRUE\n",
                           "----------------\n" , "1 file(s) failed to process:\n         file                                  error\n1 aaa/bbb.cdf Error file does not exist: aaa/bbb.cdf\n", "Annotation object cannot be reordered by sample acquisition date\n", "----------------\n", "  1 failure(s)\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation,BPPARAM=BPPARAM_parallel, getAcquTime=TRUE, verbose=TRUE))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-6)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)
  
  # Check messages (no timing) (Windows is more verbose)
  if (.Platform$OS.type == 'windows') {
    expect_equal(length(result_parallelAnnotation$messages), 10)
    expect_equal(result_parallelAnnotation$messages[c(1:3, 5:8, 10)], expected_message)
  } else {
    expect_equal(length(result_parallelAnnotation$messages), 9)
    expect_equal(result_parallelAnnotation$messages[c(1:7, 9)], expected_message)
  }
})

test_that('serial and parallel give the same result: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found (cpd #3), getAcquTime, verbose', {
  # sample 2 is missing
  # Cpd #3 will not give results
  noMatch_uROI3        <- input_uROI
  noMatch_uROI3[3,4:6] <- c(52.194778, 52.2, 52.205222)
  
  # Object fully initialised
  initAnnotation  <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE, uROI=noMatch_uROI3, useFIR=TRUE, FIR=input_FIR, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

  # results
  result_serial   <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, nCores=1, getAcquTime=TRUE, verbose=TRUE))

  result_parallel <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, BPPARAM=BPPARAM_parallel, getAcquTime=TRUE, verbose=TRUE))

  # Check results
  expect_equal(result_serial$result, result_parallel$result, tolerance=1e-6)
})

test_that('already annotated message in verbose', {
  # use "serial: 3 files, (1 missing), 4 compounds, uROI, FIR replace peaks not found (cpd #3), getAcquTime, verbose"
  
  # sample 2 is missing
  # Cpd #3 will not give results
  noMatch_uROI3        <- input_uROI
  noMatch_uROI3[3,4:6] <- c(52.194778, 52.2, 52.205222)
  
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_missingSpectraPaths, targetFeatTable=input_badtargetFeatTable, uROIExist=TRUE, useUROI=TRUE,
                                               uROI=noMatch_uROI3, useFIR=TRUE, FIR=input_FIR, isAnnotated=TRUE, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)
  
  # Expected annotation
  expected_annotation             <- initAnnotation[c(1,3),]
  expected_annotation@TIC         <- c(2410533091, 2332817115)
  tmp_peakTables                        <- expected_peakTables[c(1,3)]
  tmp_peakTables[[1]][3,c(2:11,13:16)]  <- c(3444.524, 3454.435, 3478.431, 464.1995, 464.20001220703125, 464.2005, 8939889, 8939889, 380736, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[1]]$is_filled[3]      <- TRUE
  tmp_peakTables[[2]][3,c(2:11,13:16)]  <- c(3444.524, 3460.696, 3478.431, 464.1995, 464.20001220703125, 464.2005, 7414831, 7414831, 319488, as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  tmp_peakTables[[2]]$found[3]          <- TRUE
  tmp_peakTables[[2]]$is_filled[3]      <- TRUE
  expected_annotation@peakTables        <- tmp_peakTables
  tmp_peakFit                     <- expected_peakFit[c(1,3)]
  tmp_peakFit[[1]][[3]]           <- NA
  tmp_peakFit[[2]][[3]]           <- NA
  expected_annotation@peakFit     <- tmp_peakFit
  tmp_dataPoints                  <- expected_dataPoints_badROI
  expected_annotation@dataPoints  <- tmp_dataPoints
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- 'Error file does not exist: aaa/bbb.cdf'
  names(tmp_status)   <- 'aaa/bbb.cdf'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message. FIR is now served from the wider @ROI extraction
  # ("FIR data reused from ROI ...") instead of a separate disk read.
  expected_message    <- c("!! Data was already annotated, results will be overwritten !!\n", "Processing 4 compounds in 3 samples:\n", "  uROI:\tTRUE\n", "  FIR:\tTRUE\n", "----- ko15 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Fit of ROI #3 is unsuccessful (try err)\n", "FIR data reused from ROI for 1/1 windows\n", "1 features to integrate with FIR\n", "Error file does not exist: aaa/bbb.cdf\n", "----- ko18 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #2\n", "Fit of ROI #3 is unsuccessful (try err)\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #4\n", "FIR data reused from ROI for 1/1 windows\n", "1 features to integrate with FIR\n", "----------------\n", "1 file(s) failed to process:\n         file                                  error\n1 aaa/bbb.cdf Error file does not exist: aaa/bbb.cdf\n", "Annotation object cannot be reordered by sample acquisition date\n", "----------------\n", "  1 failure(s)\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, nCores = 1, getAcquTime=TRUE, verbose=TRUE))

    # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-6)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (no timing)
  expect_equal(length(result_parallelAnnotation$messages), 39)
  expect_equal(result_parallelAnnotation$messages[c(1:8,10,11,14,15,18:22,24:27,30,31,34,35,36,37,39)], expected_message)
})

test_that('catch file that doesnt exist, catch error processing, no file left', {
  # Object fully initialised
  wrongPaths      <- c("aaa/bbb.cdf", system.file("extdata/test_fakemzML.mzML", package = "peakPantheR"))
  initAnnotation  <- peakPantheRAnnotation(spectraPaths=wrongPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

  # Expected annotation
  expected_annotation             <- initAnnotation[c(FALSE, FALSE),]
  expected_annotation@isAnnotated <- FALSE
  # Expected message (have to remove Error file does not exist as it now returns a path)
  expected_message    <- c("Processing 4 compounds in 2 samples:\n", "  uROI:\tTRUE\n", "  FIR:\tFALSE\n", "----------------\n", "No file left in the object!\n", "Annotation object reordered by sample acquisition date\n", "----------------\n", "  2 failure(s)\n")

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, BPPARAM=BPPARAM_parallel, getAcquTime=FALSE, verbose=TRUE))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-6)
  # cannot check the failure paths
  expect_equal(dim(result_parallelAnnotation$result$failures)[1], 2)
  expect_equal(dim(result_parallelAnnotation$result$failures)[2], 2)

  # Check messages (no timing) (Windows is more verbose)
  if (.Platform$OS.type == 'windows') {
    expect_equal(length(result_parallelAnnotation$messages), 11)
    expect_equal(result_parallelAnnotation$messages[c(1:3, 5, 7:9, 11)], expected_message)
  } else {
    expect_equal(length(result_parallelAnnotation$messages), 10)
    expect_equal(result_parallelAnnotation$messages[c(1:4, 6:8, 10)], expected_message)
  }
})

test_that('curveModel emgGaussian: 3 files, 4 compounds, no uROI, no FIR, no getAcquTime, no verbose', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

  # Expected peakFit
  # 1
  cFit1.1emg         <- list(amplitude=22322950, center=3341.888, sigma=5, gamma=0.09934113, fitStatus=2, curveModel="emgGaussian")
  class(cFit1.1emg)  <- 'peakPantheR_curveFit'
  cFit1.2emg         <- list(amplitude=29181384, center=3382.577, sigma=5, gamma=0.08974434, fitStatus=2, curveModel="emgGaussian")
  class(cFit1.2emg)  <- 'peakPantheR_curveFit'
  cFit1.3emg         <- list(amplitude=9173607, center=3451.435, sigma=5, gamma=0.1071507, fitStatus=2, curveModel="emgGaussian")
  class(cFit1.3emg)  <- 'peakPantheR_curveFit'
  cFit1.4emg         <- list(amplitude=7719709, center=3698.697, sigma=5, gamma=0.1060304, fitStatus=2, curveModel="emgGaussian")
  class(cFit1.4emg)  <- 'peakPantheR_curveFit'
  # 2
  cFit2.1emg         <- list(amplitude=22094946, center=3359.102, sigma=5, gamma=0.07724317, fitStatus=2, curveModel="emgGaussian")
  class(cFit2.1emg)  <- 'peakPantheR_curveFit'
  cFit2.2emg         <- list(amplitude=34397901, center=3399.791, sigma=5, gamma=0.0651969, fitStatus=2, curveModel="emgGaussian")
  class(cFit2.2emg)  <- 'peakPantheR_curveFit'
  cFit2.3emg         <- list(amplitude=9671145, center=3460.824, sigma=5, gamma=0.08864798, fitStatus=2, curveModel="emgGaussian")
  class(cFit2.3emg)  <- 'peakPantheR_curveFit'
  cFit2.4emg         <- list(amplitude=7296689, center=3714.347, sigma=5, gamma=0.06171862, fitStatus=2, curveModel="emgGaussian")
  class(cFit2.4emg)  <- 'peakPantheR_curveFit'
  # 3
  cFit3.1emg         <- list(amplitude=20390552, center=3362.233, sigma=5, gamma=0.08186031, fitStatus=2, curveModel="emgGaussian")
  class(cFit3.1emg)  <- 'peakPantheR_curveFit'
  cFit3.2emg         <- list(amplitude=29719102, center=3409.182, sigma=5, gamma=0.09218107, fitStatus=2, curveModel="emgGaussian")
  class(cFit3.2emg)  <- 'peakPantheR_curveFit'
  cFit3.3emg         <- NA
  cFit3.4emg         <- list(amplitude=5751872, center=3708.088, sigma=5, gamma=0.06964929, fitStatus=2, curveModel="emgGaussian")
  class(cFit3.4emg)  <- 'peakPantheR_curveFit'
  expected_peakFit_emg <- list(list(cFit1.1emg, cFit1.2emg, cFit1.3emg, cFit1.4emg), list(cFit2.1emg, cFit2.2emg, cFit2.3emg, cFit2.4emg), list(cFit3.1emg, cFit3.2emg, cFit3.3emg, cFit3.4emg))

  # Expected peakTables
  peakTable1emg     <- data.frame(matrix(vector(), 4, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
  peakTable1emg[1,] <- c(TRUE, 3327.999, 3346.996, 3403.919, 522.1948, 522.2, 522.2052, 22261723, 25713232, 889280,  1057506.5, FALSE, 0.02337616, 2.108434, 1.653451, 2.064384)
  peakTable1emg[2,] <- c(TRUE, 3368.739, 3387.926, 3450.433, 496.1950, 496.2, 496.2050, 29098356, 35721108, 1128960, 1313099.4, FALSE, 0.02460103, 2.349398, 1.745483, 2.219176)
  peakTable1emg[3,] <- c(TRUE, 3437.496, 3456.351, 3509.499, 464.1954, 464.2, 464.2046, 9149178, 10905635, 380736,  450748.4,  FALSE, 0.02629692, 1.915663, 1.592171, 1.962157)
  peakTable1emg[4,] <- c(TRUE, 3684.765, 3703.637, 3757.294, 536.1946, 536.2, 536.2054, 7699066, 8612410, 330176,  377425.9,  FALSE, 0.02276582, 1.939759, 1.600693, 1.976502)
  peakTable1emg[,c(1,12)]       <- sapply(peakTable1emg[,c(1,12)], as.logical)
  peakTable1emg[,c(2:11,13:16)] <- sapply(peakTable1emg[,c(2:11,13:16)], as.numeric)
  # 2
  peakTable2emg     <- data.frame(matrix(vector(), 4, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
  peakTable2emg[1,] <- c(TRUE, 3345.357, 3364.813, 3436.703, 522.1948, 522.2, 522.2052, 22028618, 22658480, 761664,  916975.6, FALSE, 0.02337616, 19.92484, 1.898412, 2.477039)
  peakTable2emg[2,] <- c(TRUE, 3386.117, 3405.791, 3490.313, 496.1950, 496.2, 496.2050, 34288936, 35743000, 1099264, 1293794.6, FALSE, 0.02460103, 20.21400, 2.116160, 2.850792)
  peakTable2emg[3,] <- c(TRUE, 3447.002, 3466.197, 3529.422, 464.1954, 464.2, 464.2046, 9643477, 10659308, 366720,  432400.6, FALSE, 0.02629692, 11.76249, 1.757855, 2.240265)
  peakTable2emg[4,] <- c(TRUE, 3700.724, 3720.347, 3809.535, 536.1946, 536.2, 536.2054, 7273154,  7093526, 220096,  265357.4, FALSE, 0.02276582, 18.65000, 2.207992, 3.012906)
  peakTable2emg[,c(1,12)]       <- sapply(peakTable2emg[,c(1,12)], as.logical)
  peakTable2emg[,c(2:11,13:16)] <- sapply(peakTable2emg[,c(2:11,13:16)], as.numeric)
  # 3
  peakTable3emg     <- data.frame(matrix(vector(), 4, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
  peakTable3emg[1,] <- c(TRUE, 3348.458, 3367.799, 3435.888, 522.1948, 522.2, 522.2052, 20330539, 19833736, 758336,  873749.6, FALSE, 0.02337616, 22.91127, 1.837158, 2.373902)
  peakTable3emg[2,] <- c(TRUE, 3395.322, 3414.459, 3475.443, 496.1950, 496.2, 496.2050, 29635397, 32843420, 1149440, 1355916.7, FALSE, 0.02460103, 28.88211, 1.721309, 2.178978)
  peakTable3emg[3,] <- c(FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, NA, NA, NA, NA)
  peakTable3emg[4,] <- c(TRUE, 3694.401, 3714.064, 3793.313, 536.1946, 536.2, 536.2054, 5734001, 6062535, 196160,  225006.2, FALSE, 0.02276582, 12.36690, 2.015364, 2.673714)
  peakTable3emg[,c(1,12)]       <- sapply(peakTable3emg[,c(1,12)], as.logical)
  peakTable3emg[,c(2:11,13:16)] <- sapply(peakTable3emg[,c(2:11,13:16)], as.numeric)
  expected_peakTables_emg <- list(peakTable1emg, peakTable2emg, peakTable3emg)

  # Expected annotation
  expected_annotation             <- initAnnotation
  expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  expected_annotation@peakTables  <- expected_peakTables_emg
  expected_annotation@peakFit     <- expected_peakFit_emg
  expected_annotation@dataPoints  <- expected_dataPoints
  expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- NA
  names(tmp_status)   <- 'test'
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message (Windows only, no message in Linux MulticoreParam)
  expected_message    <- "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nPolarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\nFit of ROI #3 is unsuccessful (cannot determine rtMin/rtMax)\n\n"

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, BPPARAM=BPPARAM_parallel, getAcquTime=FALSE, verbose=FALSE, curveModel='emgGaussian'))

  # Check results
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (centwave output)
  if (.Platform$OS.type == 'windows') {
    expect_equal(length(result_parallelAnnotation$messages), 1)
    expect_equal(result_parallelAnnotation$messages, expected_message)
  } else {
    expect_equal(length(result_parallelAnnotation$messages), 0)
  }
})

test_that('curveModel unknown: 3 failures', {
  # Object fully initialised
  initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

  # Expected annotation
  expected_annotation                 <- peakPantheRAnnotation(targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata)
  expected_annotation@spectraMetadata <- data.frame(matrix(character(), ncol=2, byrow=FALSE, dimnames=list(c(), c('testcol1', 'testcol2'))), stringsAsFactors=FALSE)

  #expected_annotation@TIC         <- c(2410533091, 2524040155, 2332817115)
  #expected_annotation@peakTables  <- expected_peakTables
  #expected_annotation@peakFit     <- expected_peakFit
  #expected_annotation@dataPoints  <- expected_dataPoints
  #expected_annotation@isAnnotated <- TRUE
  # Expected failures
  tmp_status          <- c('Error: "curveModel" must be one of: skewedGaussian, emgGaussian','Error: "curveModel" must be one of: skewedGaussian, emgGaussian','Error: "curveModel" must be one of: skewedGaussian, emgGaussian')
  names(tmp_status)   <- input_spectraPaths
  tmp_failures        <- !is.na(tmp_status)
  names(tmp_failures) <- NULL
  expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)
  # Expected message
  expected_message    <- character(0)

  # results (output, warnings and messages)
  result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, BPPARAM=BPPARAM_parallel, getAcquTime=FALSE, verbose=FALSE, curveModel='unknown_curveModel'))

  # Check results (all failures)
  expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
  expect_equal(result_parallelAnnotation$result$failures, expected_failures)

  # Check messages (no fit, so no message)
  expect_equal(length(result_parallelAnnotation$messages), 0)
  expect_equal(result_parallelAnnotation$messages, expected_message)
})

test_that('raise errors', {
  # Object fails validation on input check
  wrongInit       <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)
  wrongInit@TIC   <- c(1, 2)
  msg1            <- paste('invalid class ', dQuote('peakPantheRAnnotation'),' object: TIC has 2 elements (samples). Should be 3', sep='')
  expect_error(peakPantheR_parallelAnnotation(wrongInit, nCores=1, getAcquTime=FALSE, verbose=FALSE), msg1, fixed=TRUE)

  # BPPARAM is not BiocParallel BiocParallelParam is not an integer
  initAnnotation2 <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)
  msg2            <- "Check input, BPPARAM must be a BiocParallel Param object"
  expect_error(peakPantheR_parallelAnnotation(initAnnotation2, nCores=1, getAcquTime=FALSE, BPPARAM='not a BiocParallelParam object', verbose=FALSE), msg2, fixed=TRUE)
  
  # nCores is < 1
  initAnnotation3 <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)
  msg3            <- "Check input, nCores must be a positive integer"
  expect_error(peakPantheR_parallelAnnotation(initAnnotation3, nCores=-10, getAcquTime=FALSE, verbose=FALSE), msg3, fixed=TRUE)
})


test_that('refit with narrowed uROI preserves @dataPoints ROI-span', {
  # Guard against the "cache decay" regression: without the extraction/fit
  # split, narrowing @uROI on a refit would also narrow @dataPoints, and
  # the next refit would see less cached data available.

  # First pass: full annotation at ROI=uROI seeded from input_targetFeatTable
  initAnnot <- peakPantheRAnnotation(spectraPaths=input_spectraPaths,
      targetFeatTable=input_targetFeatTable)
  full_res  <- peakPantheR_parallelAnnotation(initAnnot, nCores=1,
      getAcquTime=FALSE, verbose=FALSE)
  full_ann  <- full_res$annotation
  baseline_nrow_cpd1 <- nrow(full_ann@dataPoints[[1]][[1]])
  expect_gt(baseline_nrow_cpd1, 0)

  # Narrow @uROI for compound 1 by ~25% in rt, keep @ROI wide
  narrowed <- full_ann
  narrowed@uROI[1, "rtMin"] <- narrowed@uROI[1, "rtMin"] + 20
  narrowed@uROI[1, "rtMax"] <- narrowed@uROI[1, "rtMax"] - 20

  # Refit just compound 1
  refit_res <- peakPantheR_parallelAnnotation(narrowed,
      compounds = cpdID(narrowed)[1], nCores=1, getAcquTime=FALSE,
      verbose=FALSE)
  refit_ann <- refit_res$annotation

  # @dataPoints span for compound 1 is preserved across the refit
  expect_equal(nrow(refit_ann@dataPoints[[1]][[1]]), baseline_nrow_cpd1)

  # The refit DID act on the narrower fit window: the detected peak's
  # apex (rt) must stay within the new uROI bounds (the fit only
  # sees that subset of the data). The peak's *rtMin* can still fall
  # below the uROI because it's the leading edge at 0.5% apex intensity,
  # projected from the fit curve rather than clamped to the data window.
  new_uroi <- uROI(narrowed)[1, c("rtMin", "rtMax")]
  expect_gte(refit_ann@peakTables[[1]][1, "rt"], new_uroi$rtMin)
  expect_lte(refit_ann@peakTables[[1]][1, "rt"], new_uroi$rtMax)

  # And the fit output differs from the baseline (otherwise the refit was
  # a no-op and we couldn't distinguish "fix works" from "cache returned
  # identical results")
  expect_false(isTRUE(all.equal(
      refit_ann@peakTables[[1]][1, "peakArea"],
      full_ann@peakTables[[1]][1, "peakArea"],
      tolerance=1e-6)))
})


## cached re-annotation ---------------------------------------------------
BPPARAM_serial <- BiocParallel::SerialParam()

cache_spectraPaths <- input_spectraPaths[c(1, 2)]

cache_targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
    c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),
    stringsAsFactors = FALSE)
cache_targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390.,
    522.194778, 522.2, 522.205222)
cache_targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3670., 3701.697, 3745.,
    536.194638, 536.2, 536.205362)
cache_targetFeatTable[, 3:8] <- sapply(cache_targetFeatTable[, 3:8],
    as.numeric)

cache_init_annotation <- peakPantheRAnnotation(
    spectraPaths = cache_spectraPaths,
    targetFeatTable = cache_targetFeatTable)


test_that('second run reuses cached EIC data and skips disk reads', {
    first <- peakPantheR_parallelAnnotation(cache_init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    expect_true(isAnnotated(first$annotation))

    res <- evaluate_promise(peakPantheR_parallelAnnotation(first$annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))

    reuse_msgs <- grep("Reusing cached EIC data",
        res$messages, value = TRUE)
    read_msgs <- grep("Reading data from",
        res$messages, value = TRUE)
    expect_equal(length(reuse_msgs), 2)
    expect_equal(length(read_msgs), 0)

    expect_equal(peakTables(res$result$annotation),
                    peakTables(first$annotation))
})


test_that('cache miss (bounds outside cache) falls back to disk read', {
    first <- peakPantheR_parallelAnnotation(cache_init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)

    broken <- first$annotation
    broken@dataPoints[[1]][[1]] <- broken@dataPoints[[1]][[1]][0, ,
        drop = FALSE]

    res <- evaluate_promise(peakPantheR_parallelAnnotation(broken,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))

    reuse_msgs <- grep("Reusing cached EIC data",
        res$messages, value = TRUE)
    read_msgs <- grep("Reading data from",
        res$messages, value = TRUE)
    expect_equal(length(reuse_msgs), 1)
    expect_gte(length(read_msgs), 1)
})


test_that('cache_bounds_ok returns FALSE for empty or misshaped cache', {
    tbl <- cache_targetFeatTable
    expect_false(cache_bounds_ok(NULL, tbl))
    expect_false(cache_bounds_ok(list(), tbl))
    empty_dp <- list(
        data.frame(rt = numeric(), mz = numeric(), int = numeric()),
        data.frame(rt = numeric(), mz = numeric(), int = numeric()))
    expect_false(cache_bounds_ok(empty_dp, tbl))
})


test_that('narrowing or widening uROI inside ROI keeps the cache hot', {
    first <- peakPantheR_parallelAnnotation(cache_init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    annot <- first$annotation

    narrow <- annot
    narrow@uROI <- data.frame(
        rtMin = ROI(annot)$rtMin + 5,
        rt    = ROI(annot)$rt,
        rtMax = ROI(annot)$rtMax - 5,
        mzMin = ROI(annot)$mzMin,
        mz    = ROI(annot)$mz,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    narrow@useUROI <- TRUE
    narrow@uROIExist <- TRUE
    res_narrow <- evaluate_promise(peakPantheR_parallelAnnotation(narrow,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))
    expect_equal(length(grep("Reading data from",
        res_narrow$messages)), 0)
    expect_gte(length(grep("Reusing cached EIC data",
        res_narrow$messages)), 1)

    wider_uROI <- annot
    wider_uROI@uROI <- data.frame(
        rtMin = ROI(annot)$rtMin,
        rt    = ROI(annot)$rt,
        rtMax = ROI(annot)$rtMax,
        mzMin = ROI(annot)$mzMin,
        mz    = ROI(annot)$mz,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    wider_uROI@useUROI <- TRUE
    wider_uROI@uROIExist <- TRUE
    res_w <- evaluate_promise(peakPantheR_parallelAnnotation(wider_uROI,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))
    expect_equal(length(grep("Reading data from", res_w$messages)), 0)
    expect_gte(length(grep("Reusing cached EIC data",
        res_w$messages)), 1)
})


test_that('FIR inside ROI but outside uROI is served from cache', {
    first <- peakPantheR_parallelAnnotation(cache_init_annotation,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = FALSE)
    annot <- first$annotation

    annot@uROI <- data.frame(
        rtMin = c(3355, 3710),
        rt    = c(3357.5, 3712.5),
        rtMax = c(3360, 3715),
        mzMin = ROI(annot)$mzMin,
        mz    = ROI(annot)$mz,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    annot@useUROI   <- TRUE
    annot@uROIExist <- TRUE

    annot@FIR <- data.frame(
        rtMin = ROI(annot)$rtMin,
        rtMax = ROI(annot)$rtMax,
        mzMin = ROI(annot)$mzMin,
        mzMax = ROI(annot)$mzMax,
        stringsAsFactors = FALSE)
    annot@useFIR <- TRUE

    res <- evaluate_promise(peakPantheR_parallelAnnotation(annot,
        BPPARAM = BPPARAM_serial, getAcquTime = FALSE, verbose = TRUE))

    expect_equal(length(grep("Reading data from", res$messages)), 0)
    expect_gte(length(grep("Reusing cached EIC data",
        res$messages)), 1)
    pt <- peakTables(res$result$annotation)
    expect_true(all(vapply(pt, function(x) all(x$is_filled),
        FUN.VALUE = logical(1))))
})
