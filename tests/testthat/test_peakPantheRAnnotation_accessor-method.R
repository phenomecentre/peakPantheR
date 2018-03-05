context('peakPantheRAnnotation_accessor-method()')

## Test the accessors five the right values

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

## Input data
# spectraPaths
input_spectraPaths  <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko18.CDF', package = "faahKO"))

# targetFeatTable
input_targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
input_targetFeatTable[1,] <- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] <- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_targetFeatTable[,c(3:8)] <- sapply(input_targetFeatTable[,c(3:8)], as.numeric)

# FIR
input_FIR     <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=F)
input_FIR[1,] <- c(1., 2., 3., 4.)
input_FIR[2,] <- c(5., 6., 7., 8.)

# uROI
input_uROI      <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
input_uROI[1,]  <- c(9., 10., 11., 12., 13., 14.)
input_uROI[2,]  <- c(15., 16., 17., 18., 19., 20.)

# acquisitionTime
input_acquisitionTime <- c(as.character(Sys.time()), as.character(Sys.time()+900), as.character(Sys.time()+1800))

# TICs
input_TIC <- c(2410533091, 2524040155, 2332817115)

# peakTables
# 1
peakTable1      <- data.frame(matrix(vector(), 2, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable1[1,]  <- c(TRUE, 522.2, 522.2, 522.2, 3346.453, 3322.979, 3379.317, 25792525, 25768308, 889280, 1840, 0.05400866, 541.2220, 7.464513, 897391.7, 1, 0, 5, 540, 535, 545, 24, 60, 1, 0, 0.02337616, 1.565, 27.50892, 11, NA, 1.296709)
peakTable1[2,]  <- c(TRUE, 496.2, 496.2, 496.2, 3385.577, 3362.102, 3409.051, 32873727, 32818664, 1128960, 1471, 0.07199871, 566.3076, 7.788152, 1133465.7, 2, 0, 5, 564, 559, 569, 68, 98, 1, 0, 0.02460103, 0., 28.70062, 11, NA, 2.134513)
peakTable1[,1]  <- sapply(peakTable1[,1], as.logical)
# 2
peakTable2      <- data.frame(matrix(vector(), 2, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable2[1,]  <- c(TRUE, 522.2, 522.2, 522.2, 3365.232, 3340.193, 3394.966, 24052220, 24015094, 761664, 577, 0.05649760, 553.1203, 8.079863, 783682.1, 1, 0, 5, 551, 546, 556, 35, 70, 1, 0, 0.02337616, 20.344, 29.77567, 11, NA, NA)
peakTable2[2,]  <- c(TRUE, 496.2, 496.2, 496.2, 3407.486, 3382.447, 3432.525, 34958375, 34866657, 1099264, 598, 0.08422028, 579.6167, 8.534918, 1104863.3, 2, 0, 5, 577, 572, 582, 81, 113, 1, 0, 0.02460103, 21.909, 31.45268, 11, NA, NA)
peakTable2[,1]  <- sapply(peakTable2[,1], as.logical)
# 3
peakTable3      <- data.frame(matrix(vector(), 2, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
peakTable3[1,]  <- c(TRUE, 522.2, 522.2, 522.2, 3368.363, 3346.454, 3396.533, 21421981, 21114393, 758336, 4527, 0.05799548, 555.4057, 7.455757, 751440.4, 1, 0, 5, 554, 549, 559, 39, 71, 1, 0, 0.02337616, 23.475, 27.47565, 11, NA, NA)
peakTable3[2,]  <- c(TRUE, 496.2, 496.2, 496.2, 3413.747, 3390.273, 3437.221, 33874835, 33445046, 1149440, 246, 0.07273868, 583.7579, 7.956053, 1144862.9, 2, 0, 5, 582, 577, 587, 86, 116, 1, 0, 0.02460103, 28.170, 29.31939, 11, NA, NA)
peakTable3[,1]  <- sapply(peakTable3[,1], as.logical)

# EICs
# 1
file1  <- MSnbase::readMSData(input_spectraPaths[1], centroided=TRUE, mode='onDisk')
EIC1	 <- xcms::chromatogram(file1, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# 2
file2  <- MSnbase::readMSData(input_spectraPaths[2], centroided=TRUE, mode='onDisk')
EIC2	 <- xcms::chromatogram(file2, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# 3
file3  <- MSnbase::readMSData(input_spectraPaths[3], centroided=TRUE, mode='onDisk')
EIC3	 <- xcms::chromatogram(file3, rt = data.frame(rt_lower=input_targetFeatTable$rtMin, rt_upper=input_targetFeatTable$rtMax), mz = data.frame(mz_lower=input_targetFeatTable$mzMin, mz_upper=input_targetFeatTable$mzMax))
# single compound
tmp_EIC <- xcms::chromatogram(file1, rt = c(rt_lower=input_targetFeatTable$rtMin[1], rt_upper=input_targetFeatTable$rtMax[1]), mz = c(mz_lower=input_targetFeatTable$mzMin[1], mz_upper=input_targetFeatTable$mzMax[1]))


# Object, fully filled
filledAnnotation        <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, FIR=input_FIR, uROI=input_uROI, useFIR=TRUE, uROIExist=TRUE, useUROI=TRUE, acquisitionTime=input_acquisitionTime, TIC=input_TIC, peakTables=list(peakTable1, peakTable2, peakTable3), EICs=list(EIC1, EIC2, EIC3), isAnnotated=TRUE)



test_that('accessors return the correct values', {
  expected_ROI              <- input_targetFeatTable[, c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "cpdID", "cpdName")]
  expected_FIR              <- cbind.data.frame(input_FIR, cpdID=c("ID-1","ID-2"), cpdName=c("Cpd 1", "Cpd 2"), stringsAsFactors=FALSE)
  expected_uROI             <- cbind.data.frame(input_uROI, cpdID=c("ID-1","ID-2"), cpdName=c("Cpd 1", "Cpd 2"), stringsAsFactors=FALSE)
  expected_acquisitionTime  <- as.POSIXct(input_acquisitionTime)
  expected_peakTables       <- list(cbind(peakTable1, cpdID=c("ID-1","ID-2"), cpdName=c("Cpd 1","Cpd 2")), cbind(peakTable2, cpdID=c("ID-1","ID-2"), cpdName=c("Cpd 1","Cpd 2")), cbind(peakTable3, cpdID=c("ID-1","ID-2"), cpdName=c("Cpd 1","Cpd 2")))
  expected_EICs             <- list(EIC1, EIC2, EIC3)
  expected_filename         <- c("ko15", "ko16", "ko18")

  # Check accessors
  # Basic slots
  # cpdID
  expect_equal(cpdID(filledAnnotation), c("ID-1","ID-2"))
  # cpdName
  expect_equal(cpdName(filledAnnotation), c("Cpd 1", "Cpd 2"))
  # ROI
  expect_equal(ROI(filledAnnotation), expected_ROI)
  # FIR
  expect_equal(FIR(filledAnnotation), expected_FIR)
  # uROI
  expect_equal(uROI(filledAnnotation), expected_uROI)
  # filepath
  expect_equal(filepath(filledAnnotation), input_spectraPaths)
  # acquisitionTIme
  expect_equal(acquisitionTime(filledAnnotation), expected_acquisitionTime)
  # uROIExist
  expect_true(uROIExist(filledAnnotation))
  # useUROI
  expect_true(useUROI(filledAnnotation))
  # useFIR
  expect_true(useFIR(filledAnnotation))
  # TIC
  expect_equal(TIC(filledAnnotation), input_TIC)
  # peakTables
  expect_equal(peakTables(filledAnnotation), expected_peakTables)
  # EICs
  expect_equal(EICs(filledAnnotation), expected_EICs)
  # isAnnotated
  expect_true(isAnnotated(filledAnnotation))

  # nbSamples
  expect_equal(nbSamples(filledAnnotation), 3)
  # nbCompounds
  expect_equal(nbCompounds(filledAnnotation), 2)
  # filename
  expect_equal(filename(filledAnnotation), expected_filename)

  # annotationTable
  # simple value
  expected_annotationTable            <- data.frame(matrix(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), 3, 2), stringsAsFactors=F)
  rownames(expected_annotationTable)  <- input_spectraPaths
  colnames(expected_annotationTable)  <- c("Cpd 1", "Cpd 2")
  expect_equal(annotationTable(filledAnnotation, 'found'), expected_annotationTable)
  # no sample
  tmp_noSample                <- peakPantheRAnnotation()
  expected_noSample           <- data.frame(matrix(vector(), 0, 0), stringsAsFactors=F)
  rownames(expected_noSample) <- tmp_noSample@filepath
  colnames(expected_noSample) <- tmp_noSample@cpdName
  expect_equal(annotationTable(tmp_noSample, 'found'), expected_noSample)
  # no peakTables
  tmp_noPeakTables            <- filledAnnotation
  tmp_noPeakTables@peakTables <- vector("list", 3)
  expected_noPeakTables           <- data.frame(matrix(vector(), 3, 2), stringsAsFactors=F)
  rownames(expected_noPeakTables) <- tmp_noPeakTables@filepath
  colnames(expected_noPeakTables) <- tmp_noPeakTables@cpdName
  expect_equal(annotationTable(tmp_noPeakTables, 'found'), expected_noPeakTables)
  # raise error if column doesn't exist
  expect_error(annotationTable(filledAnnotation, 'notAnExistingColumn'), 'input column is not a column of peakTables', fixed=TRUE)

  ## try some more of the peakTable columns
  # mz
  expected_mz           <- data.frame(matrix(c(522.2, 522.2, 522.2, 496.2, 496.2, 496.2), 3, 2), stringsAsFactors=F)
  rownames(expected_mz) <- filledAnnotation@filepath
  colnames(expected_mz) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'mz'), expected_mz)
  # mzmin
  expected_mzmin           <- data.frame(matrix(c(522.2, 522.2, 522.2, 496.2, 496.2, 496.2), 3, 2), stringsAsFactors=F)
  rownames(expected_mzmin) <- filledAnnotation@filepath
  colnames(expected_mzmin) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'mzmin'), expected_mzmin)
  # mzmax
  expected_mzmax           <- data.frame(matrix(c(522.2, 522.2, 522.2, 496.2, 496.2, 496.2), 3, 2), stringsAsFactors=F)
  rownames(expected_mzmax) <- filledAnnotation@filepath
  colnames(expected_mzmax) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'mzmax'), expected_mzmax)
  # rt
  expected_rt           <- data.frame(matrix(c(3346.453, 3365.232, 3368.363, 3385.577, 3407.486, 3413.747), 3, 2), stringsAsFactors=F)
  rownames(expected_rt) <- filledAnnotation@filepath
  colnames(expected_rt) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'rt'), expected_rt)
  # rtmin
  expected_rtmin           <- data.frame(matrix(c(3322.979, 3340.193, 3346.454, 3362.102, 3382.447, 3390.273), 3, 2), stringsAsFactors=F)
  rownames(expected_rtmin) <- filledAnnotation@filepath
  colnames(expected_rtmin) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'rtmin'), expected_rtmin)
  # rtmax
  expected_rtmax           <- data.frame(matrix(c(3379.317, 3394.966, 3396.533, 3409.051, 3432.525, 3437.221), 3, 2), stringsAsFactors=F)
  rownames(expected_rtmax) <- filledAnnotation@filepath
  colnames(expected_rtmax) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'rtmax'), expected_rtmax)
  # intb
  expected_intb           <- data.frame(matrix(c(25768308, 24015094, 21114393, 32818664, 34866657, 33445046), 3, 2), stringsAsFactors=F)
  rownames(expected_intb) <- filledAnnotation@filepath
  colnames(expected_intb) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'intb'), expected_intb)
  # asymmetryFactor
  expected_asymmetryFactor           <- data.frame(matrix(c(1.296709, NA, NA, 2.134513, NA, NA), 3, 2), stringsAsFactors=F)
  rownames(expected_asymmetryFactor) <- filledAnnotation@filepath
  colnames(expected_asymmetryFactor) <- filledAnnotation@cpdName
  expect_equal(annotationTable(filledAnnotation, 'asymmetryFactor'), expected_asymmetryFactor)
})

