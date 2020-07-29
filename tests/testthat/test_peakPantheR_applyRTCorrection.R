context('peakPantheR_applyRTCorrection()')


## Input and expected data
test_that('raises error()', {
  # Input
  tmp_targetFeatTable           <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))), stringsAsFactors=FALSE)
  tmp_targetFeatTable[1,]       <- c("ID-1", "Cpd 1", 0.5, 1, 1.5, 90, 100, 110)
  tmp_targetFeatTable[2,]       <- c("ID-2", "Cpd 2", 5, 10, 15, 900, 1000, 1100)
  tmp_targetFeatTable[,c(3:8)]  <- sapply(tmp_targetFeatTable[,c(3:8)], as.numeric)
  
  tmp_referenceTable            <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_referenceTable[1,]        <- c("Ref-1", "Ref Cpd 1", 10, 1)
  tmp_referenceTable[2,]        <- c("Ref-2", "Ref Cpd 2", 20, 1.5)
  tmp_referenceTable[,c(3:4)]   <- sapply(tmp_referenceTable[,c(3:4)], as.numeric)


  # Trigger checks on targetFeatTable input
  # targetFeatTable is not a data.frame
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable='notADataFrame', referenceTable=tmp_referenceTable), 'specified targetFeatTable is not a data.frame')
  # targetFeatTable has wrong columns
  wrongDF1           <- tmp_targetFeatTable
  colnames(wrongDF1) <- c("not cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF1, referenceTable=tmp_referenceTable), 'expected columns in targetFeatTable are "cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz" and "mzMax"')
  # targetFeatTable$cpdID not character
  wrongDF2        <- tmp_targetFeatTable
  wrongDF2$cpdID  <- c(5, 5)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF2, referenceTable=tmp_referenceTable), 'cpdID must be character')
  # targetFeatTable$cpdName not character
  wrongDF3          <- tmp_targetFeatTable
  wrongDF3$cpdName  <- c(5, 5)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF3, referenceTable=tmp_referenceTable), 'cpdName must be character')
  # targetFeatTable$rtMin not numeric
  wrongDF4        <- tmp_targetFeatTable
  wrongDF4$rtMin  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF4, referenceTable=tmp_referenceTable), 'rtMin must be numeric')
  # targetFeatTable$rt not numeric or NA
  wrongDF5        <- tmp_targetFeatTable
  wrongDF5$rt     <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF5, referenceTable=tmp_referenceTable), 'rt must be numeric or NA')
  # targetFeatTable$rtMax not numeric
  wrongDF6        <- tmp_targetFeatTable
  wrongDF6$rtMax  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF6, referenceTable=tmp_referenceTable), 'rtMax must be numeric')
  # targetFeatTable$mzMin not numeric
  wrongDF7        <- tmp_targetFeatTable
  wrongDF7$mzMin  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF7, referenceTable=tmp_referenceTable), 'mzMin must be numeric')
  # targetFeatTable$mz not numeric or NA
  wrongDF8        <- tmp_targetFeatTable
  wrongDF8$mz     <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF8, referenceTable=tmp_referenceTable), 'mz must be numeric or NA')
  # targetFeatTable$mzMax not numeric
  wrongDF9        <- tmp_targetFeatTable
  wrongDF9$mzMax  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF9, referenceTable=tmp_referenceTable), 'mzMax must be numeric')
  
  # wrong referenceTable
  # referenceTable is not a data.frame
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable='notADataFrame'), 'specified referenceTable is not a data.frame')
  # referenceTable has wrong columns
  wrongDF10           <- tmp_referenceTable
  colnames(wrongDF10) <- c("not cpdID", "cpdName", "rt", "rt_dev_sec")
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=wrongDF10), 'expected columns in referenceTable are "cpdID", "cpdName", "rt" and "rt_dev_sec"')
  # referenceTable$cpdID not character
  wrongDF11        <- tmp_referenceTable
  wrongDF11$cpdID  <- c(5, 5)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=wrongDF11), 'cpdID must be character')
  # referenceTable$cpdName not character
  wrongDF12          <- tmp_referenceTable
  wrongDF12$cpdName  <- c(5, 5)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=wrongDF12), 'cpdName must be character')
  # referenceTable$rt not numeric or NA
  wrongDF13        <- tmp_referenceTable
  wrongDF13$rt     <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=wrongDF13), 'rt must be numeric or NA')
  # referenceTable$rt_dev_sec not numeric or NA
  wrongDF14             <- tmp_referenceTable
  wrongDF14$rt_dev_sec  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=wrongDF14), 'rt_dev_sec must be numeric or NA')
  
  # unknown method
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='notKnown'), 'Error: "method" must be one of: RANSAC', fixed=TRUE)
  
  # params is not a list or character
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, params=5), 'Check input, "params" must be "guess" or list', fixed=TRUE)
  # params is character but not 'guess'
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, params='notGuess'), 'Check input, "params" must be "guess" if not list', fixed=TRUE)
  
  # PARAMS SPECIFIC CHECKS
  
})

test_that('peakPantheR_applyRTCorrection()', {
  # Input FIR with 1 empty box (without the widened mz cpd 3 should not return any matching scan)
  input_rt                    <- c()
  input_rt_dev_sec            <- c()

  # Expected model and results
  expected_corrected                 <- full_peakTable

  # results (output, warnings and messages)
  polynomial_regression <- evaluate_promise(fit_polynomial(x=input_rt, y=input_rt_dev_sec))

  # Check results
  expect_equal(result_integrateFIR$result, expected_peakTable)

  # Check messages (cannot check time on message)
  expect_equal(length(result_integrateFIR$messages), 5)
  expect_equal(result_integrateFIR$messages[c(1,2,4)], expected_messages)
})

test_that('polynomial_regression rt function', {
  # Input FIR with 1 empty box (without the widened mz cpd 3 should not return any matching scan)
  input_rt                    <- c()
  input_rt_dev_sec            <- c()

  # Expected model and results
  expected_corrected                 <- full_peakTable

  # results (output, warnings and messages)
  polynomial_regression <- evaluate_promise(fit_polynomial(x=input_rt, y=input_rt_dev_sec, robust=T))

  # Check results
  expect_equal(result_integrateFIR$result, expected_peakTable)

  # Check messages (cannot check time on message)
  expect_equal(length(result_integrateFIR$messages), 5)
  expect_equal(result_integrateFIR$messages[c(1,2,4)], expected_messages)
})

test_that('RANSAC rt correction function', {
  # Input FIR with 1 empty box (without the widened mz cpd 3 should not return any matching scan)
  input_rt                    <- c()
  input_rt_dev_sec            <- c()

  # Expected model and results
  expected_corrected                 <- full_peakTable

  # results (output, warnings and messages)
  polynomial_regression <- evaluate_promise(fit_polynomial(x=input_rt, y=input_rt_dev_sec))

  # Check results
  expect_equal(result_integrateFIR$result, expected_peakTable)

  # Check messages (cannot check time on message)
  expect_equal(length(result_integrateFIR$messages), 5)
  expect_equal(result_integrateFIR$messages[c(1,2,4)], expected_messages)
})


test_that('constant rt correction', {
  # Input FIR with 1 empty box (without the widened mz cpd 3 should not return any matching scan)
  input_rt                    <- c()
  input_rt_dev_sec            <- c()

  # Expected model and results
  expected_corrected                 <- full_peakTable

  # results (output, warnings and messages)
  polynomial_regression <- evaluate_promise(fit_polynomial(x=input_rt, y=input_rt_dev_sec))

  # Check results
  expect_equal(result_integrateFIR$result, expected_peakTable)

  # Check messages (cannot check time on message)
  expect_equal(length(result_integrateFIR$messages), 5)
  expect_equal(result_integrateFIR$messages[c(1,2,4)], expected_messages)
})
