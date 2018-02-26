context('peakPantheRAnnotation_extract-method()')

## Test the extract sub-setting "[" method

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input data
# spectraPaths
input_spectraPaths  <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                         system.file('cdf/KO/ko18.CDF', package = "faahKO"))

# targetFeatTable
input_targetFeatTable     <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
input_targetFeatTable[1,] <- c(1, "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] <- c(2, "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_targetFeatTable[,c(1,3:8)] <- sapply(input_targetFeatTable[,c(1,3:8)], as.numeric)

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


## Object, fully filled
filledAnnotation        <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, FIR=input_FIR, uROI=input_uROI, useFIR=TRUE, uROIExist=TRUE, useUROI=TRUE, acquisitionTime=input_acquisitionTime, TIC=input_TIC, peakTables=list(peakTable1, peakTable2, peakTable3), EICs=list(EIC1, EIC2, EIC3))


test_that('no i j input returns an untouched object', {
  ## Expected values
  expected_ROI        <- input_targetFeatTable[, c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
  expected_peakTables <- list(peakTable1, peakTable2, peakTable3)
  expected_EICs       <- list(EIC1, EIC2, EIC3)

  # no sub-setting
  noChange <- filledAnnotation[]

  # whole object
  expect_equal(noChange, filledAnnotation)
  # cpdID
  expect_equal(noChange@cpdID, c(1, 2))
  # cpdName
  expect_equal(noChange@cpdName, c("Cpd 1", "Cpd 2"))
  # ROI
  expect_equal(noChange@ROI, expected_ROI)
  # FIR
  expect_equal(noChange@FIR, input_FIR)
  # uROI
  expect_equal(noChange@uROI, input_uROI)
  # filepath
  expect_equal(noChange@filepath, input_spectraPaths)
  # acquisitionTime
  expect_equal(noChange@acquisitionTime, input_acquisitionTime)
  # uROIExist
  expect_true(noChange@uROIExist)
  # useUROI
  expect_true(noChange@useUROI)
  # useFIR
  expect_true(noChange@useFIR)
  # TIC
  expect_equal(noChange@TIC, input_TIC)
  # peakTables
  expect_equal(noChange@peakTables, expected_peakTables)
  # EICs
  expect_equal(noChange@EICs, expected_EICs)
})

test_that('missing i, set j: multiple cpds EICs filter', {
  ## i will default to all samples, multiple cpds (all) to check one EICs case
  ## result must be same as input object

  ## Expected values
  expected_ROI        <- input_targetFeatTable[, c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
  expected_peakTables <- list(peakTable1, peakTable2, peakTable3)
  expected_EICs       <- list(EIC1, EIC2, EIC3)

  # no sub-setting
  noI <- filledAnnotation[,1:2]

  # whole object
  expect_equal(noI, filledAnnotation)
  # cpdID
  expect_equal(noI@cpdID, c(1, 2))
  # cpdName
  expect_equal(noI@cpdName, c("Cpd 1", "Cpd 2"))
  # ROI
  expect_equal(noI@ROI, expected_ROI)
  # FIR
  expect_equal(noI@FIR, input_FIR)
  # uROI
  expect_equal(noI@uROI, input_uROI)
  # filepath
  expect_equal(noI@filepath, input_spectraPaths)
  # acquisitionTime
  expect_equal(noI@acquisitionTime, input_acquisitionTime)
  # uROIExist
  expect_true(noI@uROIExist)
  # useUROI
  expect_true(noI@useUROI)
  # useFIR
  expect_true(noI@useFIR)
  # TIC
  expect_equal(noI@TIC, input_TIC)
  # peakTables
  expect_equal(noI@peakTables, expected_peakTables)
  # EICs
  expect_equal(noI@EICs, expected_EICs)
})

test_that('missing i, set single j: single cpd EICs filter', {
  ## i will default to all samples, single cpd to check other EICs case

  ## Expected values
  expected_ROI        <- input_targetFeatTable[1, c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
  expected_FIR        <- input_FIR[1,]
  expected_uROI       <- input_uROI[1,]
  expected_peakTables <- list(peakTable1[1,], peakTable2[1,], peakTable3[1,])
  expected_EICs       <- list(MSnbase::Chromatograms(data=list(EIC1[1,])), MSnbase::Chromatograms(data=list(EIC2[1,])), MSnbase::Chromatograms(data=list(EIC3[1,])))

  # no sub-setting
  singleJ <- filledAnnotation[,1]

  # cpdID
  expect_equal(singleJ@cpdID, c(1))
  # cpdName
  expect_equal(singleJ@cpdName, c("Cpd 1"))
  # ROI
  expect_equal(singleJ@ROI, expected_ROI)
  # FIR
  expect_equal(singleJ@FIR, expected_FIR)
  # uROI
  expect_equal(singleJ@uROI, expected_uROI)
  # filepath
  expect_equal(singleJ@filepath, input_spectraPaths)
  # acquisitionTime
  expect_equal(singleJ@acquisitionTime, input_acquisitionTime)
  # uROIExist
  expect_true(singleJ@uROIExist)
  # useUROI
  expect_true(singleJ@useUROI)
  # useFIR
  expect_true(singleJ@useFIR)
  # TIC
  expect_equal(singleJ@TIC, input_TIC)
  # peakTables
  expect_equal(singleJ@peakTables, expected_peakTables)
  # EICs
  expect_equal(singleJ@EICs, expected_EICs)
})

test_that('set i, missing j', {
  ## set i, j will default to all compounds

  ## Expected values
  expected_ROI              <- input_targetFeatTable[, c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
  expected_FIR              <- input_FIR
  expected_uROI             <- input_uROI
  expected_filepath         <- input_spectraPaths[1:2]
  expected_acquisitionTime  <- input_acquisitionTime[1:2]
  expected_TIC              <- input_TIC[1:2]
  expected_peakTables       <- list(peakTable1, peakTable2)
  expected_EICs             <- list(EIC1, EIC2)

  # no sub-setting
  setI <- filledAnnotation[1:2,]

  # cpdID
  expect_equal(setI@cpdID, c(1, 2))
  # cpdName
  expect_equal(setI@cpdName, c("Cpd 1", "Cpd 2"))
  # ROI
  expect_equal(setI@ROI, expected_ROI)
  # FIR
  expect_equal(setI@FIR, expected_FIR)
  # uROI
  expect_equal(setI@uROI, expected_uROI)
  # filepath
  expect_equal(setI@filepath, expected_filepath)
  # acquisitionTime
  expect_equal(setI@acquisitionTime, expected_acquisitionTime)
  # uROIExist
  expect_true(setI@uROIExist)
  # useUROI
  expect_true(setI@useUROI)
  # useFIR
  expect_true(setI@useFIR)
  # TIC
  expect_equal(setI@TIC, expected_TIC)
  # peakTables
  expect_equal(setI@peakTables, expected_peakTables)
  # EICs
  expect_equal(setI@EICs, expected_EICs)
})

test_that('set i, empty peakTables and EICs', {
  ## all EICs and all peakTables are NULL trigger a special case

  ## object with cpd and spectra set
  defaultInit_cpd_spectra   <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable)

  ## Expected values
  expected_ROI              <- input_targetFeatTable[, c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
  expected_FIR              <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))), stringsAsFactors=F)
  expected_FIR[1,]          <- sapply(expected_FIR[1,], as.numeric)
  expected_FIR[2,]          <- sapply(expected_FIR[2,], as.numeric)
  expected_uROI             <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
  expected_uROI[1,]         <- sapply(expected_uROI[1,], as.numeric)
  expected_uROI[2,]         <- sapply(expected_uROI[2,], as.numeric)
  expected_filepath         <- input_spectraPaths[1:2]
  expected_acquisitionTime  <- as.character(c(NA, NA))

  expected_peakTables <- vector("list", 2)
  expected_EICs       <- vector("list", 2)

  # no sub-setting
  setIandNULL <- defaultInit_cpd_spectra[1:2,]

  # cpdID
  expect_equal(setIandNULL@cpdID, c(1, 2))
  # cpdName
  expect_equal(setIandNULL@cpdName, c("Cpd 1", "Cpd 2"))
  # ROI
  expect_equal(setIandNULL@ROI, expected_ROI)
  # FIR
  expect_equal(setIandNULL@FIR, expected_FIR)
  # uROI
  expect_equal(setIandNULL@uROI, expected_uROI)
  # filepath
  expect_equal(setIandNULL@filepath, expected_filepath)
  # acquisitionTime
  expect_equal(setIandNULL@acquisitionTime, expected_acquisitionTime)
  # uROIExist
  expect_false(setIandNULL@uROIExist)
  # useUROI
  expect_false(setIandNULL@useUROI)
  # useFIR
  expect_false(setIandNULL@useFIR)
  # TIC
  expect_equal(setIandNULL@TIC, c(as.numeric(NA), as.numeric(NA)))
  # peakTables
  expect_equal(setIandNULL@peakTables, expected_peakTables)
  # EICs
  expect_equal(setIandNULL@EICs, expected_EICs)
})

test_that('set i and j', {
  ## set i and j

  ## Expected values
  expected_ROI              <- input_targetFeatTable[1, c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
  expected_FIR              <- input_FIR[1,]
  expected_uROI             <- input_uROI[1,]
  expected_filepath         <- input_spectraPaths[1:2]
  expected_acquisitionTime  <- input_acquisitionTime[1:2]
  expected_TIC              <- input_TIC[1:2]
  expected_peakTables       <- list(peakTable1[1,], peakTable2[1,])
  expected_EICs             <- list(MSnbase::Chromatograms(data=list(EIC1[1,])), MSnbase::Chromatograms(data=list(EIC2[1,])))

  # no sub-setting
  setIJ <- filledAnnotation[1:2,1]

  # cpdID
  expect_equal(setIJ@cpdID, c(1))
  # cpdName
  expect_equal(setIJ@cpdName, c("Cpd 1"))
  # ROI
  expect_equal(setIJ@ROI, expected_ROI)
  # FIR
  expect_equal(setIJ@FIR, expected_FIR)
  # uROI
  expect_equal(setIJ@uROI, expected_uROI)
  # filepath
  expect_equal(setIJ@filepath, expected_filepath)
  # acquisitionTime
  expect_equal(setIJ@acquisitionTime, expected_acquisitionTime)
  # uROIExist
  expect_true(setIJ@uROIExist)
  # useUROI
  expect_true(setIJ@useUROI)
  # useFIR
  expect_true(setIJ@useFIR)
  # TIC
  expect_equal(setIJ@TIC, expected_TIC)
  # peakTables
  expect_equal(setIJ@peakTables, expected_peakTables)
  # EICs
  expect_equal(setIJ@EICs, expected_EICs)
})

test_that('reorder i and j', {
  ## reorder i and j

  ## Expected values
  expected_ROI              <- input_targetFeatTable[c(2,1), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]
  expected_FIR              <- input_FIR[c(2,1),]
  expected_uROI             <- input_uROI[c(2,1),]
  expected_filepath         <- input_spectraPaths[c(3,2,1)]
  expected_acquisitionTime  <- input_acquisitionTime[c(3,2,1)]
  expected_TIC              <- input_TIC[c(3,2,1)]
  expected_peakTables       <- list(peakTable3[c(2,1),], peakTable2[c(2,1),], peakTable1[c(2,1),])
  expected_EICs             <- list(EIC3[c(2,1),], EIC2[c(2,1),], EIC1[c(2,1),])

  # no sub-setting
  reorderIJ <- filledAnnotation[3:1, c(2,1)]

  # cpdID
  expect_equal(reorderIJ@cpdID, c(2, 1))
  # cpdName
  expect_equal(reorderIJ@cpdName, c("Cpd 2", "Cpd 1"))
  # ROI
  expect_equal(reorderIJ@ROI, expected_ROI)
  # FIR
  expect_equal(reorderIJ@FIR, expected_FIR)
  # uROI
  expect_equal(reorderIJ@uROI, expected_uROI)
  # filepath
  expect_equal(reorderIJ@filepath, expected_filepath)
  # acquisitionTime
  expect_equal(reorderIJ@acquisitionTime, expected_acquisitionTime)
  # uROIExist
  expect_true(reorderIJ@uROIExist)
  # useUROI
  expect_true(reorderIJ@useUROI)
  # useFIR
  expect_true(reorderIJ@useFIR)
  # TIC
  expect_equal(reorderIJ@TIC, expected_TIC)
  # peakTables
  expect_equal(reorderIJ@peakTables, expected_peakTables)
  # EICs
  expect_equal(reorderIJ@EICs, expected_EICs)
})

test_that('raise error if i and j are out of bound', {
  # i out of bound
  msgI  <- paste('i index out of bound: maximum 3', sep='')
  expect_error(filledAnnotation[4,], msgI, fixed=TRUE)
  expect_error(filledAnnotation[10:15,], msgI, fixed=TRUE)

  # j out of bound
  msgJ  <- paste('j index out of bound: maximum 2', sep='')
  expect_error(filledAnnotation[,4], msgJ, fixed=TRUE)
  expect_error(filledAnnotation[,10:15], msgJ, fixed=TRUE)
})
