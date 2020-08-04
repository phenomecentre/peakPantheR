context('peakPantheRAnnotation_retentionTimeCorrection')

## Test the output of retention time correction

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

on.exit( tryCatch({ file.remove('./Rplots.pdf') }, error=function(e){ invisible() }, warning=function(w){ invisible() }) )
on.exit( tryCatch({ file.remove('./test_ggsave.png') }, error=function(e){ invisible() }, warning=function(w){ invisible() }) )

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

# FIR
input_FIR     <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=FALSE)
input_FIR[1,] <- c(1., 2., 3., 4.)
input_FIR[2,] <- c(5., 6., 7., 8.)

# uROI
input_uROI      <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
input_uROI[1,]  <- c(9., 10., 11., 12., 13., 14.)
input_uROI[2,]  <- c(15., 16., 17., 18., 19., 20.)

# TICs
input_TIC <- c(2410533091, 2524040155, 2332817115)

# cpdMetadata
input_cpdMetadata     <- data.frame(matrix(data=c('a','b',1,2), nrow=2, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)

# spectraMetadata
input_spectraMetadata <- data.frame(matrix(data=c('c','d','e',3,4,5), nrow=3, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)

# acquisitionTime
input_acquisitionTime <- c(as.character(Sys.time()), as.character(Sys.time()+900), as.character(Sys.time()+1800))

# peakTables
# 1
peakTable1     <- data.frame(matrix(vector(), 2, 15, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
peakTable1[1,] <- c(TRUE, 3309.7589296586070, 3346.8277590361445, 3385.4098874628098, 522.194778, 522.20001220703125, 522.205222, 26133726.6811244078, 889280, 901015.80529226747, FALSE, 0.023376160866574614, 1.93975903614455092, 1.0153573486330891, 1.0268238825675249)
peakTable1[2,] <- c(TRUE, 3345.3766648628907, 3386.5288072289159, 3428.2788374983961, 496.20001220703125, 496.20001220703125, 496.20001220703125, 35472141.3330242932, 1128960, 1113576.69008227298, FALSE, 0.024601030353423384, 0.95180722891564074, 1.0053782620427065, 1.0093180792278085)
peakTable1[,c(1,11)]       <- sapply(peakTable1[,c(1,11)], as.logical)
peakTable1[,c(2:10,12:15)] <- sapply(peakTable1[,c(2:10,12:15)], as.numeric)
# 2
peakTable2     <- data.frame(matrix(vector(), 2, 15, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
peakTable2[1,] <- c(TRUE, 3326.1063495851854, 3365.102, 3407.2726475892355, 522.194778, 522.20001220703125, 522.205222, 24545301.622835573, 761664, 790802.2209998488, FALSE, 0.023376160866574614, 0.2139999999999, 1.0339153786516375, 1.0630802030537212)
peakTable2[2,] <- c(TRUE, 3365.0238566258713, 3405.791, 3453.4049569205681, 496.195038, 496.20001220703125, 496.204962, 37207579.286265120, 1099264, 1098720.2929832144, FALSE, 0.024601030353423384, 20.2139999999999, 1.0839602450900523, 1.1717845972583161)
peakTable2[,c(1,11)]       <- sapply(peakTable2[,c(1,11)], as.logical)
peakTable2[,c(2:10,12:15)] <- sapply(peakTable2[,c(2:10,12:15)], as.numeric)
# 3
peakTable3     <- data.frame(matrix(vector(), 2, 15, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
peakTable3[1,] <- c(TRUE, 3333.8625894557053, 3368.233, 3407.4362838927614, 522.194778, 522.20001220703125, 522.205222, 21447174.404490683, 758336, 765009.9805796633, FALSE, 0.023376160866574614, 23.345000000000255, 1.0609102044546637, 1.1155310457756928)
peakTable3[2,] <- c(TRUE, 3373.3998828113113, 3413.4952530120481, 3454.4490330927388, 496.195038, 496.20001220703125, 496.204962, 35659353.614476241, 1149440, 1145857.7611069249, FALSE, 0.024601030353423384, 27.918253012047899, 1.0081407426394933, 1.0143315197994494)
peakTable3[,c(1,11)]       <- sapply(peakTable3[,c(1,11)], as.logical)
peakTable3[,c(2:10,12:15)] <- sapply(peakTable3[,c(2:10,12:15)], as.numeric)
input_peakTables <- list(peakTable1, peakTable2, peakTable3)

# peakFit
# 1
cFit1.1         <- list(amplitude=162404.8057918259, center=3341.888, sigma=0.078786133031045896, gamma=0.0018336101984172684, fitStatus=2, curveModel="skewedGaussian")
class(cFit1.1)  <- 'peakPantheR_curveFit'
cFit1.2         <- list(amplitude=199249.10572753669, center=3382.577, sigma=0.074904415304607966, gamma=0.0011471899372353885, fitStatus=2, curveModel="skewedGaussian")
class(cFit1.2)  <- 'peakPantheR_curveFit'
# 2
cFit2.1         <- list(amplitude=124090.83425474487, center=3359.102, sigma=0.071061541060964212, gamma=0.0018336072657203239, fitStatus=2, curveModel="skewedGaussian")
class(cFit2.1)  <- 'peakPantheR_curveFit'
cFit2.2         <- list(amplitude=151407.23415130575, center=3399.791, sigma=0.063753866057052563, gamma=0.001676782834598999, fitStatus=2, curveModel="skewedGaussian")
class(cFit2.2)  <- 'peakPantheR_curveFit'
# 3
cFit3.1         <- list(amplitude=122363.51256736703, center=3362.233, sigma=0.075489598945304492, gamma=0.0025160536725299734, fitStatus=2, curveModel="skewedGaussian")
class(cFit3.1)  <- 'peakPantheR_curveFit'
cFit3.2         <- list(amplitude=204749.86097918145, center=3409.182, sigma=0.075731781812843249, gamma=0.0013318670577834328, fitStatus=2, curveModel="skewedGaussian")
class(cFit3.2)  <- 'peakPantheR_curveFit'
input_peakFit   <- list(list(cFit1.1, cFit1.2), list(cFit2.1, cFit2.2), list(cFit3.1, cFit3.2))

# dataPoint
tmp_raw_data1  	  <- MSnbase::readMSData(input_spectraPaths[1], centroided=TRUE, mode='onDisk')
ROIDataPoints1    <- extractSignalRawData(tmp_raw_data1, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
tmp_raw_data2  	  <- MSnbase::readMSData(input_spectraPaths[2], centroided=TRUE, mode='onDisk')
ROIDataPoints2    <- extractSignalRawData(tmp_raw_data2, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
tmp_raw_data3  	  <- MSnbase::readMSData(input_spectraPaths[3], centroided=TRUE, mode='onDisk')
ROIDataPoints3    <- extractSignalRawData(tmp_raw_data3, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
input_dataPoints  <- list(ROIDataPoints1, ROIDataPoints2, ROIDataPoints3)

# Object, fully filled
filledAnnotation        <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, FIR=input_FIR, uROI=input_uROI, useFIR=TRUE, uROIExist=TRUE, useUROI=TRUE, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata, acquisitionTime=input_acquisitionTime, TIC=input_TIC, peakTables=input_peakTables, dataPoints=input_dataPoints, peakFit=input_peakFit, isAnnotated=TRUE)


test_that('retentionTimeCorrection-method', {
  # Test if object is modified correctly
  # input
  constantCorrected <- evaluate_promise(retentionTimeCorrection(filledAnnotation, rtCorrectionReference=c('ID-1'),
                                               method='constant', params=list(polynomialOrder=1), robust=T))
  filledAnnotation@useUROI <- FALSE
  # test also if ROI vs uROI are used correctly as original RT value based on useUROI
  constantCorrected_useROI <- evaluate_promise(retentionTimeCorrection(filledAnnotation, rtCorrectionReference=c('ID-1'),
                                               method='constant', params=list(polynomialOrder=1), robust=T))

  filledAnnotation@useUROI <- TRUE
  # expected ROI/uROI
  expected_uROI <- data.frame(rtMin=double(), rt=double(), rtMax=double(), mzMin=double(), mz=double(), mzMax=double(), stringsAsFactors=FALSE)
  expected_FIR <- data.frame(rtMin=double(), rtMax=double(), mzMin=double(), mzMax=double(), stringsAsFactors=FALSE)
  expected_uROI_useROI <- data.frame(rtMin=double(), rt=double(), rtMax=double(), mzMin=double(), mz=double(), mzMax=double(), stringsAsFactors=FALSE)
  expected_FIR_useROI <- data.frame(rtMin=double(), rtMax=double(), mzMin=double(), mzMax=double(), stringsAsFactors=FALSE)
  expected_uROI[1,] <- list(rtMin=-5.9995863454, rt=1.500414, rtMax=9.000414, mzMin=12, mz=13, mzMax=14)
  expected_uROI[2,] <- list(rtMi=0.0004136546, rt=7.500414, rtMax=15.000414, mzMin=18, mz=19, mzMax=20)
  expected_FIR[1,] <- list(rtMin=-5.9995863454, rtMax=9.000414, mzMin=3, mzMax=4)
  expected_FIR[2,] <- list(rtMin=0.0004136546, rtMax=15.000414, mzMin=7, mzMax=8)

  expected_uROI_useROI[1,] <- list(rtMin=3328.888, rt=3336.388, rtMax=3343.888, mzMin=12, mz=13, mzMax=14)
  expected_uROI_useROI[2,] <- list(rtMin=3369.577, rt=3377.077, rtMax=3384.577, mzMin=18, mz=19, mzMax=20)
  expected_FIR_useROI[1,] <- list(rtMin=3328.888, rtMax=3343.888, mzMin=3, mzMax=4)
  expected_FIR_useROI[2,] <- list(rtMin=3369.577, rtMax=3384.577, mzMin=7, mzMax=8)

  # verify results (output, warnings and messages)
  expect_equal(constantCorrected$result$annotation@uROI, expected_uROI, tolerance=1e-5)
  expect_equal(constantCorrected$result$annotation@FIR, expected_FIR, tolerance=1e-5)
  expect_equal(constantCorrected_useROI$result$annotation@uROI, expected_uROI_useROI, tolerance=1e-5)
  expect_equal(constantCorrected_useROI$result$annotation@FIR, expected_FIR_useROI, tolerance=1e-5)
  # Test plot
  expected_plotFrame <- data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(),
                                   isReference=character(), correctedRt=double(), predictedRtDrift=double(),
                                   stringsAsFactors=FALSE)

  expected_plotFrame[1, ] <- list(cpdID='ID-1', cpdName='Cpd 1', rt=10, rt_dev_sec=8.499586, isReference='Reference set',
                                  correctedRt=1.500414, predictedRtDrift=8.499586)
  expected_plotFrame[2, ] <- list(cpdID='ID-2', cpdName='Cpd 2', rt=16, rt_dev_sec=16.361353, isReference='External set',
                                  correctedRt=7.500414, predictedRtDrift=8.499586)
  expect_equal(constantCorrected$result$plot[[1]], expected_plotFrame, tolerance=1e-5)
  expect_equal(length(constantCorrected$result$plot), 9)
  expect_equal(constantCorrected$result$plot$labels$x, 'Retention time')
  expect_equal(constantCorrected$result$plot$labels$y, 'Retention time deviation')
  expect_is(constantCorrected$result$plot$layers[[1]]$geom, 'GeomPoint')
  expect_is(constantCorrected$result$plot$layers[[2]]$geom, 'GeomLine')
  expect_error(ggplot2::ggsave('./test_ggsave.png', constantCorrected$result$plot), NA)

})
