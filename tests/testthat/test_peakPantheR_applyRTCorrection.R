context('peakPantheR_applyRTCorrection()')


## Input and expected data
test_that('raises error()', {
  # Input
  tmp_targetFeatTable           <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_targetFeatTable[1,]       <- c("ID-1", "Cpd 1", 11, 1.1)
  tmp_targetFeatTable[2,]       <- c("ID-2", "Cpd 2", 25, 3.2)
  tmp_targetFeatTable[, c(3:4)]  <- sapply(tmp_targetFeatTable[, c(3:4)], as.numeric)
  
  tmp_referenceTable            <- data.frame(matrix(vector(), 2, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_referenceTable[1,]        <- c("Ref-1", "Ref Cpd 1", 10, 1)
  tmp_referenceTable[2,]        <- c("Ref-2", "Ref Cpd 2", 20, 1.5)
  tmp_referenceTable[, c(3:4)]   <- sapply(tmp_referenceTable[, c(3:4)], as.numeric)

  # Trigger checks on targetFeatTable input
  # targetFeatTable is not a data.frame
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable='notADataFrame', referenceTable=tmp_referenceTable), 'specified targetFeatTable is not a data.frame')
  # targetFeatTable has wrong columns
  wrongDF1           <- tmp_targetFeatTable
  colnames(wrongDF1) <- c("not cpdID", "cpdName", "rt", "rt_dev_sec")
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF1, referenceTable=tmp_referenceTable), "expected columns in targetFeatTable are \"cpdID\", \"cpdName\", \"rt\", \"rt_dev_sec\"")
  # targetFeatTable$cpdID not character
  wrongDF2        <- tmp_targetFeatTable
  wrongDF2$cpdID  <- c(5, 5)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF2, referenceTable=tmp_referenceTable), 'cpdID must be character')
  # targetFeatTable$cpdName not character
  wrongDF3          <- tmp_targetFeatTable
  wrongDF3$cpdName  <- c(5, 5)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF3, referenceTable=tmp_referenceTable), 'cpdName must be character')
  # targetFeatTable$rt not numeric
  wrongDF4        <- tmp_targetFeatTable
  wrongDF4$rt  <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF4, referenceTable=tmp_referenceTable), 'rt must be numeric')
  # targetFeatTable$rt_dev_sec not numeric or NA
  wrongDF5        <- tmp_targetFeatTable
  wrongDF5$rt_dev_sec     <- c('notNumeric', 'notNumeric')
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=wrongDF5, referenceTable=tmp_referenceTable), 'rt_dev_sec must be numeric or NA')

  # wrong referenceTable
  # referenceTable is not a data.frame
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable='notADataFrame'), 'specified referenceTable is not a data.frame')
  # referenceTable has wrong columns
  wrongDF6           <- tmp_referenceTable
  colnames(wrongDF6) <- c("not cpdID", "cpdName", "rt", "rt_dev_sec")
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=wrongDF6), 'expected columns in referenceTable are "cpdID", "cpdName", "rt" and "rt_dev_sec"')
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
  
  # unknown correction method
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='notKnown'), 'Error: \"method\" must be one of: \"polynomial\", \"constant\"', fixed=TRUE)

  # params is not a list or character
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, params=5), 'Check input, "params" must be list', fixed=TRUE)
  # Robust is not logical
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, robust=3, params=list(polynomialOrder=1)), 'robust must be either TRUE or FALSE', fixed=TRUE)

  # Method <-> params SPECIFIC CHECKS
  # method = polynomial but no polynomial order
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(a=5), robust=TRUE), 'polynomialOrder must be provided in params', fixed=TRUE)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(a=5), robust=FALSE), 'polynomialOrder must be provided in params', fixed=TRUE)
  # method = polynomial but polynomial order is not valid
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder='NA'), robust=FALSE), 'polynomialOrder must be an integer and equal or greater than 1', fixed=TRUE)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=1.3), robust=FALSE), 'polynomialOrder must be an integer and equal or greater than 1', fixed=TRUE)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=1.3), robust=TRUE), 'polynomialOrder must be an integer and equal or greater than 1', fixed=TRUE)

  # method = polynomial, valid polynomial order but exceeds number of references
  expect_warning(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=4), robust=FALSE), '`polynomialOrder` is larger than the number of references passed. `polynomialOrder` will be set equal to number of reference compounds - 1', fixed=TRUE)
  expect_warning(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=4), robust=TRUE), '`polynomialOrder` is larger than the number of references passed. `polynomialOrder` will be set equal to number of reference compounds - 1', fixed=TRUE)
  # test sugestion of method=constant if ref = 1
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable[1, ], method='polynomial', params=list(polynomialOrder=1), robust=FALSE), 'No function can be fitted with a single reference. Use method=\`offset\` instead.', fixed=TRUE)
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable[1, ], method='polynomial', params=list(polynomialOrder=1), robust=TRUE), 'No function can be fitted with a single reference. Use method=\`offset\` instead.', fixed=TRUE)
  # test constant method throws error with multiple references
  expect_error(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='constant', params=list(polynomialOrder=1), robust=TRUE), "`constant` Rt correction can only use a single reference", fixed=TRUE)

})

test_that('polynomial method rt correction', {
  # Input
  tmp_targetFeatTable           <- data.frame(matrix(vector(), 5, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_targetFeatTable[1,]       <- c("ID-1", "Cpd 1", 11, 1.1)
  tmp_targetFeatTable[2,]       <- c("ID-2", "Cpd 2", 25, 1.5)
  tmp_targetFeatTable[3,]       <- c("ID-3", "Cpd 3", 35, 3.5)
  tmp_targetFeatTable[4,]       <- c("ID-4", "Cpd 4", 45, 5.5)
  tmp_targetFeatTable[5,]       <- c("ID-5", "Cpd 5", 55, 7)
  tmp_targetFeatTable[, c(3:4)]  <- sapply(tmp_targetFeatTable[, c(3:4)], as.numeric)

  tmp_referenceTable            <- data.frame(matrix(vector(), 5, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_referenceTable[1,]        <- c("Ref-1", "Ref Cpd 1", 11, 1.1)
  tmp_referenceTable[2,]        <- c("Ref-2", "Ref Cpd 2", 25, 1.5)
  tmp_referenceTable[3,]        <- c("Ref-3", "Ref Cpd 3", 35, 3.5)
  tmp_referenceTable[4,]        <- c("Ref-4", "Ref Cpd 4", 45, 5.5)
  tmp_referenceTable[5,]        <- c("Ref-5", "Ref Cpd 5", 55, 7)

  tmp_referenceTable[, c(3:4)]   <- sapply(tmp_referenceTable[, c(3:4)], as.numeric)
  # Expected model and results

  expected_corrected  <- data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)

  expected_corrected[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt=10.61282,  predictedRtDrift=0.387176)
  expected_corrected[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=1.5, isReference="External set",   correctedRt=22.60164,  predictedRtDrift=2.398363)
  expected_corrected[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt= 31.16508,  predictedRtDrift=3.834925)
  expected_corrected[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=39.72851,  predictedRtDrift=5.271487)
  expected_corrected[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.29195,  predictedRtDrift=6.708049)

  expected_corrected_dg3                 <-  data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)
  expected_corrected_dg3[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt=9.907072,  predictedRtDrift=1.092928)
  expected_corrected_dg3[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=1.5, isReference="External set",   correctedRt=23.457678,  predictedRtDrift=1.542322)
  expected_corrected_dg3[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt= 31.574063,  predictedRtDrift=3.425937)
  expected_corrected_dg3[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=39.447720,  predictedRtDrift=5.552280)
  expected_corrected_dg3[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.013466,  predictedRtDrift=6.986534)

   # results (output, warnings and messages)
  polynomial_regression_dg1 <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=1), robust=FALSE))
  polynomial_regression_dg3 <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable,
                                                                          method='polynomial', params=list(polynomialOrder=3), robust=FALSE))
  # Check results
  expect_equal(polynomial_regression_dg1$result, expected_corrected, tolerance=1e-5)
  expect_equal(polynomial_regression_dg3$result, expected_corrected_dg3, tolerance=1e-5)

  # test isReference is assigned correctely
  tmp_referenceTable            <- data.frame(matrix(vector(), 5, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_referenceTable[1,]        <- c("Ref-1", "Ref Cpd 1", 11, 1.1)
  tmp_referenceTable[2,]        <- c("ID-2", "Cpd 2", 25, 1.5)
  tmp_referenceTable[3,]        <- c("Ref-3", "Ref Cpd 3", 35, 3.5)
  tmp_referenceTable[4,]        <- c("Ref-4", "Ref Cpd 4", 45, 5.5)
  tmp_referenceTable[5,]        <- c("Ref-5", "Ref Cpd 5", 55, 7)

  tmp_referenceTable[, c(3:4)]   <- sapply(tmp_referenceTable[, c(3:4)], as.numeric)

  expected_corrected_isRef                 <-  data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)
  expected_corrected_isRef[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt=10.61282,  predictedRtDrift=0.387176)
  expected_corrected_isRef[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=1.5, isReference="Reference set",   correctedRt=22.60164,  predictedRtDrift=2.398363)
  expected_corrected_isRef[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt= 31.16508,  predictedRtDrift=3.834925)
  expected_corrected_isRef[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=39.72851,  predictedRtDrift=5.271487)
  expected_corrected_isRef[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.29195,  predictedRtDrift=6.708049)

  polynomial_regression_ref <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=1), robust=FALSE))

  expect_equal(polynomial_regression_ref$result, expected_corrected_isRef, tolerance=1e-5)
  # Check messages (cannot check time on message)
  #expect_equal(length(result_integrateFIR$messages), 5)
  #expect_equal(result_integrateFIR$messages[c(1,2,4)], expected_messages)
})

test_that('RANSAC rt correction function', {
  # Input
  tmp_targetFeatTable           <- data.frame(matrix(vector(), 5, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_targetFeatTable[1,]       <- c("ID-1", "Cpd 1", 11, 1.1)
  tmp_targetFeatTable[2,]       <- c("ID-2", "Cpd 2", 25, 1.5)
  tmp_targetFeatTable[3,]       <- c("ID-3", "Cpd 3", 35, 3.5)
  tmp_targetFeatTable[4,]       <- c("ID-4", "Cpd 4", 45, 5.5)
  tmp_targetFeatTable[5,]       <- c("ID-5", "Cpd 5", 55, 7)
  tmp_targetFeatTable[, c(3:4)]  <- sapply(tmp_targetFeatTable[, c(3:4)], as.numeric)

  tmp_referenceTable            <- data.frame(matrix(vector(), 5, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_referenceTable[1,]        <- c("Ref-1", "Ref Cpd 1", 11, 1.1)
  tmp_referenceTable[2,]        <- c("Ref-2", "Ref Cpd 2", 25, 1.5)
  tmp_referenceTable[3,]        <- c("Ref-3", "Ref Cpd 3", 35, 3.5)
  tmp_referenceTable[4,]        <- c("Ref-4", "Ref Cpd 4", 45, 5.5)
  tmp_referenceTable[5,]        <- c("Ref-5", "Ref Cpd 5", 55, 7)

  tmp_referenceTable[, c(3:4)]   <- sapply(tmp_referenceTable[, c(3:4)], as.numeric)
  # Expected model and results

  expected_corrected  <- data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)

  expected_corrected[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt=10.61282,  predictedRtDrift=0.387176)
  expected_corrected[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=1.5, isReference="External set",   correctedRt=22.60164,  predictedRtDrift=2.398363)
  expected_corrected[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt= 31.16508,  predictedRtDrift=3.834925)
  expected_corrected[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=39.72851,  predictedRtDrift=5.271487)
  expected_corrected[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.29195,  predictedRtDrift=6.708049)

  expected_corrected_dg3                 <-  data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)
  expected_corrected_dg3[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt=9.907072,  predictedRtDrift=1.092928)
  expected_corrected_dg3[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=1.5, isReference="External set",   correctedRt=23.457678,  predictedRtDrift=1.542322)
  expected_corrected_dg3[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt= 31.574063,  predictedRtDrift=3.425937)
  expected_corrected_dg3[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=39.447720,  predictedRtDrift=5.552280)
  expected_corrected_dg3[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.013466,  predictedRtDrift=6.986534)

   # results (output, warnings and messages)
  set.seed(19472)
  polynomial_ransac_dg1 <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=1), robust=TRUE))
  set.seed(390753)
  polynomial_ransac_dg3 <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='polynomial', params=list(polynomialOrder=3), robust=TRUE))
  # Check results
  expect_equal(polynomial_ransac_dg1$result, expected_corrected, tolerance=1e-5)
  expect_equal(polynomial_ransac_dg3$result, expected_corrected_dg3, tolerance=1e-5)

  # Add outlier
  tmp_referenceTable_outlier            <- data.frame(matrix(vector(), 5, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_referenceTable_outlier[1,]        <- c("Ref-1", "Ref Cpd 1", 11, 1.1)
  tmp_referenceTable_outlier[2,]        <- c("ID-2", "Cpd 2", 25, 15)
  tmp_referenceTable_outlier[3,]        <- c("Ref-3", "Ref Cpd 3", 35, 3.5)
  tmp_referenceTable_outlier[4,]        <- c("Ref-4", "Ref Cpd 4", 45, 5.5)
  tmp_referenceTable_outlier[5,]        <- c("Ref-5", "Ref Cpd 5", 55, 7)

  tmp_referenceTable_outlier[, c(3:4)]   <- sapply(tmp_referenceTable_outlier[, c(3:4)], as.numeric)
  # Modify outlier in targetFeatTable
  tmp_targetFeatTable[2,]       <- c("ID-2", "Cpd 2", 25, 15)
  tmp_targetFeatTable[, c(3:4)]   <- sapply(tmp_targetFeatTable[, c(3:4)], as.numeric)


  expected_corrected_outlier                 <-  data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)
  expected_corrected_outlier[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt=10.14133,  predictedRtDrift=0.8586692)
  expected_corrected_outlier[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=15.0, isReference="Reference set outlier", correctedRt=22.26570,  predictedRtDrift=2.7343018)
  expected_corrected_outlier[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt=30.92596,  predictedRtDrift=4.0740394)
  expected_corrected_outlier[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=39.58622,  predictedRtDrift=5.4137769)
  expected_corrected_outlier[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.24649,  predictedRtDrift=6.7535145)

  expected_corrected_outlier_dg3                 <-  data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)
  expected_corrected_outlier_dg3[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt= 9.90000,  predictedRtDrift=1.100000)
  expected_corrected_outlier_dg3[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=15.0, isReference="Reference set outlier",   correctedRt=23.25802,  predictedRtDrift=1.741979)
  expected_corrected_outlier_dg3[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt=31.50000,  predictedRtDrift=3.500000)
  expected_corrected_outlier_dg3[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=39.50000,  predictedRtDrift=5.500000)
  expected_corrected_outlier_dg3[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.00000,  predictedRtDrift=7.000000)

  # Case where outlier is not detected
  expected_corrected_outlier_nd                 <-  data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)
  expected_corrected_outlier_nd[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt= 9.90000,  predictedRtDrift=1.100000)
  expected_corrected_outlier_nd[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=15.0, isReference="Reference set",   correctedRt=10.00000,  predictedRtDrift=15.000000)
  expected_corrected_outlier_nd[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt=31.50000 ,  predictedRtDrift=3.500000)
  expected_corrected_outlier_nd[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=50.23268,  predictedRtDrift=-5.232684)
  expected_corrected_outlier_nd[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=48.00000,  predictedRtDrift=7.000000)

  set.seed(22142)
  polynomial_ransac_outlier <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable_outlier, method='polynomial', params=list(polynomialOrder=1), robust=TRUE))
  set.seed(12149)
  polynomial_ransac_outlier_dg3 <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable_outlier,
                                                                          method='polynomial', params=list(polynomialOrder=3), robust=TRUE))
  set.seed(4871414)
  polynomial_ransac_outlier_nd <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable_outlier,
                                                                          method='polynomial', params=list(polynomialOrder=3), robust=TRUE))

  expect_equal(polynomial_ransac_outlier$result, expected_corrected_outlier, tolerance=1e-5)
  expect_equal(polynomial_ransac_outlier_dg3$result, expected_corrected_outlier_dg3, tolerance=1e-5)
  expect_equal(polynomial_ransac_outlier_nd$result, expected_corrected_outlier_nd, tolerance=1e-5)

})


test_that('constant rt correction', {
  # Input
  tmp_targetFeatTable           <- data.frame(matrix(vector(), 5, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_targetFeatTable[1,]       <- c("ID-1", "Cpd 1", 11, 1.1)
  tmp_targetFeatTable[2,]       <- c("ID-2", "Cpd 2", 25, 1.5)
  tmp_targetFeatTable[3,]       <- c("ID-3", "Cpd 3", 35, 3.5)
  tmp_targetFeatTable[4,]       <- c("ID-4", "Cpd 4", 45, 5.5)
  tmp_targetFeatTable[5,]       <- c("ID-5", "Cpd 5", 55, 7)
  tmp_targetFeatTable[, c(3:4)]  <- sapply(tmp_targetFeatTable[, c(3:4)], as.numeric)

  tmp_referenceTable            <- data.frame(matrix(vector(), 1, 4, dimnames=list(c(), c("cpdID", "cpdName", "rt", "rt_dev_sec"))), stringsAsFactors=FALSE)
  tmp_referenceTable[1,]        <- c("Ref-1", "Ref Cpd 1", 11, 1.1)

  tmp_referenceTable[, c(3:4)]   <- sapply(tmp_referenceTable[, c(3:4)], as.numeric)
  # Expected model and results

  expected_corrected  <- data.frame(cpdID=character(), cpdName=character(), rt=double(), rt_dev_sec=double(), isReference=character(),
                                    correctedRt=double(), predictedRtDrift=double(), stringsAsFactors=FALSE)

  expected_corrected[1,]       <- list(cpdID="ID-1", cpdName="Cpd 1",  rt=11, rt_dev_sec=1.1, isReference="External set",   correctedRt=9.9,  predictedRtDrift=1.1)
  expected_corrected[2,]       <- list(cpdID="ID-2", cpdName="Cpd 2",  rt=25, rt_dev_sec=1.5, isReference="External set",   correctedRt=23.9,  predictedRtDrift=1.1)
  expected_corrected[3,]       <- list(cpdID="ID-3", cpdName="Cpd 3",  rt=35, rt_dev_sec=3.5, isReference="External set",  correctedRt=33.9,  predictedRtDrift=1.1)
  expected_corrected[4,]       <- list(cpdID="ID-4", cpdName="Cpd 4",  rt=45, rt_dev_sec=5.5, isReference="External set",   correctedRt=43.9,  predictedRtDrift=1.1)
  expected_corrected[5,]       <- list(cpdID="ID-5", cpdName="Cpd 5",  rt=55, rt_dev_sec=7, isReference="External set",   correctedRt=53.9,  predictedRtDrift=1.1)

  constant_correction <- evaluate_promise(peakPantheR_applyRTCorrection(targetFeatTable=tmp_targetFeatTable, referenceTable=tmp_referenceTable, method='constant', params=list(polynomialOrder=1), robust=FALSE))

  expect_equal(constant_correction$result, expected_corrected, tolerance=1e-5)
})
