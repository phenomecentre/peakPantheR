context('peakPantheRAnnotation_class_initializeMethod()')

# Test the class definition (enforces default slot type). Detail error matching works with devtools::test() but fails with R CMD Check / devtools::check()
# Test initialize method (set default values, initialisation from spectraPaths and targetFeatTable)

test_that('initialize with default values', {
  # Expected values
  expected_slotName <- c("cpdID", "cpdName", "ROI", "FIR", "uROI", "filepath", "uROIExist", "useFIR", "TIC", "peakTables", "EICs")
  expected_ROI      <- data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=F)
  expected_FIR      <- data.frame(rtMin=numeric(), rtMax=numeric(), mzMin=numeric(), mzMax=numeric(), stringsAsFactors=F)
  expected_uROI     <- data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=F)

  # Object initialised with default values
  defaultInit <- peakPantheRAnnotation()

  # Check object
  expect_true(class(defaultInit) == "peakPantheRAnnotation")
  expect_equal(slotNames(defaultInit), expected_slotName)
  expect_true(is.numeric(defaultInit@cpdID))
  expect_equal(length(defaultInit@cpdID), 0)
  expect_true(is.character(defaultInit@cpdName))
  expect_equal(length(defaultInit@cpdName), 0)
  expect_equal(defaultInit@ROI, expected_ROI)
  expect_equal(defaultInit@FIR, expected_FIR)
  expect_equal(defaultInit@uROI, expected_uROI)
  expect_true(is.character(defaultInit@filepath))
  expect_equal(length(defaultInit@filepath), 0)
  expect_false(defaultInit@uROIExist)
  expect_false(defaultInit@useFIR)
  expect_true(is.numeric(defaultInit@TIC))
  expect_equal(length(defaultInit@TIC), 0)
  expect_true(is.list(defaultInit@peakTables))
  expect_equal(length(defaultInit@peakTables), 0)
  expect_true(is.list(defaultInit@EICs))
  expect_equal(length(defaultInit@EICs), 0)
})

test_that('slot types are set in class definition', {
  # slot cpdID is not numeric
  expect_error(peakPantheRAnnotation(cpdID='notNumeric'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "cpdID" in class "peakPantheRAnnotation": got class "character", should be or extend class "numeric"', fixed=TRUE)
  # slot cpdName is not character
  expect_error(peakPantheRAnnotation(cpdName=5))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "cpdName" in class "peakPantheRAnnotation": got class "numeric", should be or extend class "character"', fixed=TRUE)
  # slot ROI is not a data.frame
  expect_error(peakPantheRAnnotation(ROI='notADataFrame'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "ROI" in class "peakPantheRAnnotation": got class "character", should be or extend class "data.frame"', fixed=TRUE)
  # slot FIR is not a data.frame
  expect_error(peakPantheRAnnotation(FIR='notADataFrame'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "FIR" in class "peakPantheRAnnotation": got class "character", should be or extend class "data.frame"', fixed=TRUE)
  # slot uROI is not a data.frame
  expect_error(peakPantheRAnnotation(uROI='notADataFrame'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "uROI" in class "peakPantheRAnnotation": got class "character", should be or extend class "data.frame"', fixed=TRUE)
  # slot filepath is not character
  expect_error(peakPantheRAnnotation(filepath=5))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "filepath" in class "peakPantheRAnnotation": got class "numeric", should be or extend class "character"', fixed=TRUE)
  # slot uROIExist is not a logical
  expect_error(peakPantheRAnnotation(uROIExist='notALogical'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "uROIExist" in class "peakPantheRAnnotation": got class "character", should be or extend class "logical"', fixed=TRUE)
  # slot useFIR is not a logical
  expect_error(peakPantheRAnnotation(useFIR='notALogical'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "useFIR" in class "peakPantheRAnnotation": got class "character", should be or extend class "logical"', fixed=TRUE)
  # slot TIC is not numeric
  expect_error(peakPantheRAnnotation(TIC='notNumeric'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "TIC" in class "peakPantheRAnnotation": got class "character", should be or extend class "numeric"', fixed=TRUE)
  # slot peakTables is not a list
  expect_error(peakPantheRAnnotation(peakTables='notAList'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "peakTables" in class "peakPantheRAnnotation": got class "character", should be or extend class "list"', fixed=TRUE)
  # slot EICs is not a list
  expect_error(peakPantheRAnnotation(EICs='notAList'))#, 'invalid class “peakPantheRAnnotation” object: invalid object for slot "EICs" in class "peakPantheRAnnotation": got class "character", should be or extend class "list"', fixed=TRUE)
})

test_that('initialize with spectraPaths', {
  # Input and expected values
  tmp_peakTables      <- data.frame(matrix(vector(), 0, 31, dimnames=list(c(), c("found", "mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "sample", "is_filled", "ppm_error", "rt_dev_sec", "FWHM", "FWHM_ndatapoints", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=F)
  input_spectraPaths  <- c('./path/file1', './path/file2', './path/file3')
  input_TIC           <- c(1, 2, 3)
  input_peakTables    <- list(tmp_peakTables, tmp_peakTables, tmp_peakTables)
  input_EICs          <- list(list(), list(), list())
  expected_slotName   <- c("cpdID", "cpdName", "ROI", "FIR", "uROI", "filepath", "uROIExist", "useFIR", "TIC", "peakTables", "EICs")
  expected_filepath   <- c('./path/file1', './path/file2', './path/file3')
  expected_ROI        <- data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=F)
  expected_FIR        <- data.frame(rtMin=numeric(), rtMax=numeric(), mzMin=numeric(), mzMax=numeric(), stringsAsFactors=F)
  expected_uROI       <- data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=F)
  expected_TIC        <- as.numeric(c(NA, NA, NA))
  expected_peakTables <- vector('list', 3)
  expected_EICs       <- vector('list', 3)

  # Init object with spectraPaths
  spectraAnnotation <- peakPantheRAnnotation(spectraPaths=input_spectraPaths)
  # Check object values
  expect_true(class(spectraAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(spectraAnnotation), expected_slotName)
  expect_true(is.numeric(spectraAnnotation@cpdID))
  expect_equal(length(spectraAnnotation@cpdID), 0)
  expect_true(is.character(spectraAnnotation@cpdName))
  expect_equal(length(spectraAnnotation@cpdName), 0)
  expect_equal(spectraAnnotation@ROI, expected_ROI)
  expect_equal(spectraAnnotation@FIR, expected_FIR)
  expect_equal(spectraAnnotation@uROI, expected_uROI)
  expect_equal(spectraAnnotation@filepath, expected_filepath)
  expect_false(spectraAnnotation@uROIExist)
  expect_false(spectraAnnotation@useFIR)
  expect_equal(spectraAnnotation@TIC, expected_TIC)
  expect_equal(spectraAnnotation@peakTables, expected_peakTables)
  expect_equal(spectraAnnotation@EICs, expected_EICs)

  # Provide a TIC that shouldn't get replaced
  TICAnnotation <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, TIC=input_TIC)
  # Check TIC get set from input and not default with spectraPaths
  expect_true(class(TICAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(TICAnnotation), expected_slotName)
  expect_true(is.numeric(TICAnnotation@cpdID))
  expect_equal(length(TICAnnotation@cpdID), 0)
  expect_true(is.character(TICAnnotation@cpdName))
  expect_equal(length(TICAnnotation@cpdName), 0)
  expect_equal(TICAnnotation@ROI, expected_ROI)
  expect_equal(TICAnnotation@FIR, expected_FIR)
  expect_equal(TICAnnotation@uROI, expected_uROI)
  expect_equal(TICAnnotation@filepath, expected_filepath)
  expect_false(TICAnnotation@uROIExist)
  expect_false(TICAnnotation@useFIR)
  expect_equal(TICAnnotation@TIC, input_TIC)                  # change is here
  expect_equal(TICAnnotation@peakTables, expected_peakTables)
  expect_equal(TICAnnotation@EICs, expected_EICs)

  # Provide a peakTables that shouldn't get replaced
  peakTablesAnnotation <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, peakTables=input_peakTables)
  # Check peakTables get set from input and not default with spectraPaths
  expect_true(class(peakTablesAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(peakTablesAnnotation), expected_slotName)
  expect_true(is.numeric(peakTablesAnnotation@cpdID))
  expect_equal(length(peakTablesAnnotation@cpdID), 0)
  expect_true(is.character(peakTablesAnnotation@cpdName))
  expect_equal(length(peakTablesAnnotation@cpdName), 0)
  expect_equal(peakTablesAnnotation@ROI, expected_ROI)
  expect_equal(peakTablesAnnotation@FIR, expected_FIR)
  expect_equal(peakTablesAnnotation@uROI, expected_uROI)
  expect_equal(peakTablesAnnotation@filepath, expected_filepath)
  expect_false(peakTablesAnnotation@uROIExist)
  expect_false(peakTablesAnnotation@useFIR)
  expect_equal(peakTablesAnnotation@TIC, expected_TIC)
  expect_equal(peakTablesAnnotation@peakTables, input_peakTables) # change is here
  expect_equal(peakTablesAnnotation@EICs, expected_EICs)

  # Provide a EICs that shouldn't get replaced
  EICsAnnotation <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, EICs=input_EICs)
  # Check EICs get set from input and not default with spectraPaths
  expect_true(class(EICsAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(EICsAnnotation), expected_slotName)
  expect_true(is.numeric(EICsAnnotation@cpdID))
  expect_equal(length(EICsAnnotation@cpdID), 0)
  expect_true(is.character(EICsAnnotation@cpdName))
  expect_equal(length(EICsAnnotation@cpdName), 0)
  expect_equal(EICsAnnotation@ROI, expected_ROI)
  expect_equal(EICsAnnotation@FIR, expected_FIR)
  expect_equal(EICsAnnotation@uROI, expected_uROI)
  expect_equal(EICsAnnotation@filepath, expected_filepath)
  expect_false(EICsAnnotation@uROIExist)
  expect_false(EICsAnnotation@useFIR)
  expect_equal(EICsAnnotation@TIC, expected_TIC)
  expect_equal(EICsAnnotation@peakTables, expected_peakTables)
  expect_equal(EICsAnnotation@EICs, input_EICs)               # change is here

  # Trigger checks on spectraPaths input
  # spectraPaths not a character vector
  expect_error(peakPantheRAnnotation(spectraPaths=c(1,2,3)), 'specified spectraPaths is not a vector of character')
  expect_error(peakPantheRAnnotation(spectraPaths=list()), 'specified spectraPaths is not a vector of character')
})

test_that('initialize with targetFeatTable', {
  # Input and expected values
  input_targetFeatTable             <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
  input_targetFeatTable[1,]         <- c(1, "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
  input_targetFeatTable[2,]         <- c(2, "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
  input_targetFeatTable[,c(1,3:8)]  <- sapply(input_targetFeatTable[,c(1,3:8)], as.numeric)
  input_FIR       <- data.frame(rtMin=numeric(), rtMax=numeric(), mzMin=numeric(), mzMax=numeric(), stringsAsFactors=F)
  input_FIR[1,]   <- c(1., 2., 3., 4.)
  input_FIR[2,]   <- c(5., 6., 7., 8.)
  input_uROI      <- data.frame(rtMin=numeric(), rt=numeric(), rtMax=numeric(), mzMin=numeric(), mz=numeric(), mzMax=numeric(), stringsAsFactors=F)
  input_uROI[1,]  <- c(10., 11., 12., 13., 14., 15.)
  input_uROI[2,]  <- c(16., 17., 18., 19., 20., 21.)
  expected_slotName   <- c("cpdID", "cpdName", "ROI", "FIR", "uROI", "filepath", "uROIExist", "useFIR", "TIC", "peakTables", "EICs")
  expected_cpdID      <- c(1,2)
  expected_cpdName    <- c('Cpd 1', 'Cpd 2')
  expected_ROI        <- data.frame(matrix(vector(), 2, 6, dimnames=list(c(), c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=F)
  expected_ROI[1,]    <- c(3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
  expected_ROI[2,]    <- c(3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
  expected_FIR        <- data.frame(rtMin=as.numeric(rep(NA,2)), rtMax=as.numeric(rep(NA,2)), mzMin=as.numeric(rep(NA,2)), mzMax=as.numeric(rep(NA,2)), stringsAsFactors=F)
  expected_uROI       <- data.frame(rtMin=as.numeric(rep(NA,2)), rt=as.numeric(rep(NA,2)), rtMax=as.numeric(rep(NA,2)), mzMin=as.numeric(rep(NA,2)), mz=as.numeric(rep(NA,2)), mzMax=as.numeric(rep(NA,2)), stringsAsFactors=F)


  # Init object with targetFeatTable
  targetFeatTableAnnotation <- peakPantheRAnnotation(targetFeatTable=input_targetFeatTable)
  # Check object values
  expect_true(class(targetFeatTableAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(targetFeatTableAnnotation), expected_slotName)
  expect_equal(targetFeatTableAnnotation@cpdID, expected_cpdID)
  expect_equal(targetFeatTableAnnotation@cpdName, expected_cpdName)
  expect_equal(targetFeatTableAnnotation@ROI, expected_ROI)
  expect_equal(targetFeatTableAnnotation@FIR, expected_FIR)
  expect_equal(targetFeatTableAnnotation@uROI, expected_uROI)
  expect_true(is.character(targetFeatTableAnnotation@filepath))
  expect_equal(length(targetFeatTableAnnotation@filepath), 0)
  expect_false(targetFeatTableAnnotation@uROIExist)
  expect_false(targetFeatTableAnnotation@useFIR)
  expect_true(is.numeric(targetFeatTableAnnotation@TIC))
  expect_equal(length(targetFeatTableAnnotation@TIC), 0)
  expect_true(is.list(targetFeatTableAnnotation@peakTables))
  expect_equal(length(targetFeatTableAnnotation@peakTables), 0)
  expect_true(is.list(targetFeatTableAnnotation@EICs))
  expect_equal(length(targetFeatTableAnnotation@EICs), 0)

  # Provide a FIR that shouldn't get replaced, and set useFIR
  FIRAnnotation <- peakPantheRAnnotation(targetFeatTable=input_targetFeatTable, FIR=input_FIR, useFIR=TRUE)
  # check object
  expect_true(class(FIRAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(FIRAnnotation), expected_slotName)
  expect_equal(FIRAnnotation@cpdID, expected_cpdID)
  expect_equal(FIRAnnotation@cpdName, expected_cpdName)
  expect_equal(FIRAnnotation@ROI, expected_ROI)
  expect_equal(FIRAnnotation@FIR, input_FIR) # change is here
  expect_equal(FIRAnnotation@uROI, expected_uROI)
  expect_true(is.character(FIRAnnotation@filepath))
  expect_equal(length(FIRAnnotation@filepath), 0)
  expect_false(FIRAnnotation@uROIExist)
  expect_true(FIRAnnotation@useFIR) # change is here
  expect_true(is.numeric(FIRAnnotation@TIC))
  expect_equal(length(FIRAnnotation@TIC), 0)
  expect_true(is.list(FIRAnnotation@peakTables))
  expect_equal(length(FIRAnnotation@peakTables), 0)
  expect_true(is.list(FIRAnnotation@EICs))
  expect_equal(length(FIRAnnotation@EICs), 0)

  # Provide a uROI that shouldn't get replaced, and set uROIExist
  uROIAnnotation <- peakPantheRAnnotation(targetFeatTable=input_targetFeatTable, uROI=input_uROI, uROIExist=TRUE)
  # check object
  expect_true(class(uROIAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(uROIAnnotation), expected_slotName)
  expect_equal(uROIAnnotation@cpdID, expected_cpdID)
  expect_equal(uROIAnnotation@cpdName, expected_cpdName)
  expect_equal(uROIAnnotation@ROI, expected_ROI)
  expect_equal(uROIAnnotation@FIR, expected_FIR)
  expect_equal(uROIAnnotation@uROI, input_uROI) # change is here
  expect_true(is.character(uROIAnnotation@filepath))
  expect_equal(length(uROIAnnotation@filepath), 0)
  expect_true(uROIAnnotation@uROIExist) # change is here
  expect_false(uROIAnnotation@useFIR)
  expect_true(is.numeric(uROIAnnotation@TIC))
  expect_equal(length(uROIAnnotation@TIC), 0)
  expect_true(is.list(uROIAnnotation@peakTables))
  expect_equal(length(uROIAnnotation@peakTables), 0)
  expect_true(is.list(uROIAnnotation@EICs))
  expect_equal(length(uROIAnnotation@EICs), 0)

  # Force uROIExist to FALSE (despite setting to TRUE) as uROI is reset
  uROIResetAnnotation <- peakPantheRAnnotation(targetFeatTable=input_targetFeatTable, uROIExist=TRUE)
  # check object
  expect_true(class(uROIResetAnnotation) == "peakPantheRAnnotation")
  expect_equal(slotNames(uROIResetAnnotation), expected_slotName)
  expect_equal(uROIResetAnnotation@cpdID, expected_cpdID)
  expect_equal(uROIResetAnnotation@cpdName, expected_cpdName)
  expect_equal(uROIResetAnnotation@ROI, expected_ROI)
  expect_equal(uROIResetAnnotation@FIR, expected_FIR)
  expect_equal(uROIResetAnnotation@uROI, expected_uROI)
  expect_true(is.character(uROIResetAnnotation@filepath))
  expect_equal(length(uROIResetAnnotation@filepath), 0)
  expect_false(uROIResetAnnotation@uROIExist) # false as ROI is reset
  expect_false(uROIResetAnnotation@useFIR)
  expect_true(is.numeric(uROIResetAnnotation@TIC))
  expect_equal(length(uROIResetAnnotation@TIC), 0)
  expect_true(is.list(uROIResetAnnotation@peakTables))
  expect_equal(length(uROIResetAnnotation@peakTables), 0)
  expect_true(is.list(uROIResetAnnotation@EICs))
  expect_equal(length(uROIResetAnnotation@EICs), 0)


  # Trigger checks on targetFeatTable input
  # targetFeatTable is not a data.frame
  expect_error(peakPantheRAnnotation(targetFeatTable='notADataFrame'), 'specified targetFeatTable is not a data.frame')
  # targetFeatTable has wrong columns
  wrongDF1           <- input_targetFeatTable
  colnames(wrongDF1) <- c("not cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF1), 'expected columns in targetFeatTable are "cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz" and "mzMax"')
  # targetFeatTable$cpdID not numeric
  wrongDF2        <- input_targetFeatTable
  wrongDF2$cpdID  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF2), 'cpdID must be numeric')
  # targetFeatTable$cpdID not character
  wrongDF3          <- input_targetFeatTable
  wrongDF3$cpdName  <- c(5, 5)
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF3), 'cpdName must be character')
  # targetFeatTable$rtMin not numeric
  wrongDF4        <- input_targetFeatTable
  wrongDF4$rtMin  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF4), 'rtMin must be numeric')
  # targetFeatTable$rt not numeric or NA
  wrongDF5        <- input_targetFeatTable
  wrongDF5$rt     <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF5), 'rt must be numeric or NA')
  # targetFeatTable$rtMax not numeric
  wrongDF6        <- input_targetFeatTable
  wrongDF6$rtMax  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF6), 'rtMax must be numeric')
  # targetFeatTable$mzMin not numeric
  wrongDF7        <- input_targetFeatTable
  wrongDF7$mzMin  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF7), 'mzMin must be numeric')
  # targetFeatTable$mz not numeric or NA
  wrongDF8        <- input_targetFeatTable
  wrongDF8$mz     <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF8), 'mz must be numeric or NA')
  # targetFeatTable$mzMax not numeric
  wrongDF9        <- input_targetFeatTable
  wrongDF9$mzMax  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheRAnnotation(targetFeatTable=wrongDF9), 'mzMax must be numeric')
})
