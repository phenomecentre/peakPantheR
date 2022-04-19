context('peakPantheR_ROIStatistics()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)

# remove Rplots.pdf created by ggplot2
on.exit( tryCatch({ file.remove('./Rplots.pdf') }, error=function(e){ invisible() }, warning=function(w){ invisible() }) )

## Input and expected data
# 3 files
input_spectraPaths          <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                                 system.file('cdf/KO/ko16.CDF', package = "faahKO"),
                                 system.file('cdf/KO/ko18.CDF', package = "faahKO"))

# 4 features
input_targetFeatTable     	<- data.frame(matrix(vector(), 4, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
input_targetFeatTable[1,] 	<- c("ID-1", "Cpd 1", 3310., 3344.888, 3390., 522.194778, 522.2, 522.205222)
input_targetFeatTable[2,] 	<- c("ID-2", "Cpd 2", 3280., 3385.577, 3440., 496.195038, 496.2, 496.204962)
input_targetFeatTable[3,] 	<- c("ID-3", "Cpd 3", 3420., 3454.435, 3495., 464.195358, 464.2, 464.204642)
input_targetFeatTable[4,] 	<- c("ID-4", "Cpd 4", 3670., 3701.697, 3745., 536.194638, 536.2, 536.205362)
input_targetFeatTable[,c(3:8)] <- vapply(input_targetFeatTable[,c(3:8)], as.numeric, FUN.VALUE=numeric(4))



test_that('3 files, save EICS, mean IS RT, save IS fit, with sampleColour, verbose, no verbose', {
  # temp saveFolder
  saveFolder1   <- tempdir()
  # clear temp folder
  suppressWarnings(do.call(file.remove, list(list.files(saveFolder1, full.names=TRUE))))
  
  # input
  refSpecFiles  <- input_spectraPaths
  input_ROI     <- input_targetFeatTable
  input_IS_ROI  <- input_targetFeatTable
  sampleColour  <- c('blue', 'red', 'green')
  
  # Expected save paths
  expected_path_plot1 <- file.path(saveFolder1, "ID-1.png")
  expected_path_plot2 <- file.path(saveFolder1, "ID-2.png")
  expected_path_plot3 <- file.path(saveFolder1, "ID-3.png")
  expected_path_plot4 <- file.path(saveFolder1, "ID-4.png")
  expected_path_CSV   <- file.path(saveFolder1, "IS_mean_RT.csv")
  expected_path_IS_plot1 <- file.path(saveFolder1,"IS_search", "cpd_1.png")
  expected_path_IS_plot2 <- file.path(saveFolder1,"IS_search", "cpd_2.png")
  expected_path_IS_plot3 <- file.path(saveFolder1,"IS_search", "cpd_3.png")
  expected_path_IS_plot4 <- file.path(saveFolder1,"IS_search", "cpd_4.png")
  # Expected IS mean RT
  expected_meanIS     <- data.frame(matrix(nrow=4,ncol=2,dimnames=list(c(), c('X','mean_rt'))))
  expected_meanIS[1,] <- c('ID-1', 3360.054253)
  expected_meanIS[2,] <- c('ID-2', 3401.938353)
  expected_meanIS[3,] <- c('ID-3', 3458.840343)
  expected_meanIS[4,] <- c('ID-4', 3712.859261)
  expected_meanIS[,2] <- vapply(expected_meanIS[,2], as.numeric, FUN.VALUE=numeric(1))
  # Expected message
  expected_message    <- c("    4 ROI in 3 reference samples\n", "    4 IS in 3 reference samples\n", "\n-- Saving EICs for each ROI --\n","Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 4 windows\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 4 windows\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 4 windows\n", "\n-- Calculating mean RT for each IS --\n", "Processing 4 compounds in 3 samples:\n", "  uROI:\tFALSE\n", "  FIR:\tFALSE\n", "----- ko15 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #1\n", "Warning: rtMin/rtMax outside of ROI; datapoints cannot be used for mzMin/mzMax calculation, approximate mz and returning ROI$mzMin and ROI$mzMax for ROI #3\n", "Annotation object cannot be reordered by sample acquisition date\n", "----------------\n", "  0 failure(s)\n")

  
	# results (output, warnings and messages)
  result_ROIstatsV <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder1, ROI=input_ROI, IS_ROI=input_IS_ROI, sampleColour=sampleColour, nCores=1, saveISPlots=TRUE, verbose=TRUE))

  # Check saved EIC plots
  expect_true(file.exists(expected_path_plot1))
  expect_true(file.exists(expected_path_plot2))
  expect_true(file.exists(expected_path_plot3))
  expect_true(file.exists(expected_path_plot4))
  # Check saved IS fit
  expect_true(file.exists(expected_path_IS_plot1))
  expect_true(file.exists(expected_path_IS_plot2))
  expect_true(file.exists(expected_path_IS_plot3))
  expect_true(file.exists(expected_path_IS_plot4))
  # Check mean IS RT
  expect_true(file.exists(expected_path_CSV))
  saved_CSV       <- read.csv(expected_path_CSV, header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
  expect_equal(saved_CSV, expected_meanIS, tolerance=1e-4)
  
  # Check messages (no filepaths)
  expect_equal(length(result_ROIstatsV$messages), 65)
  expect_equal(result_ROIstatsV$messages[c(2, 4:7, 9:10, 12:13, 16:23, 25:26, 54:55, 57)], expected_message)
  
  # no verbose
  result_ROIstatsNoV <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder1, ROI=input_ROI, IS_ROI=input_IS_ROI, sampleColour=sampleColour, nCores=1, saveISPlots=TRUE, verbose=FALSE))
  expect_equal(length(result_ROIstatsNoV$messages), 6)
})

test_that('3 files (1 missing), save EICs, no IS RT, no IS fit, sampleColour is not of length nb spectra, verbose', {
  # message for wrong sampleColour length
  
  # temp saveFolder
  saveFolder2   <- tempdir()
  # clear temp folder
  suppressWarnings(do.call(file.remove, list(list.files(saveFolder2, full.names=TRUE))))
  
  # input
  refSpecFiles  <- c(system.file('cdf/KO/ko15.CDF', package = "faahKO"),
                     'not/a/valid/path',
                     system.file('cdf/KO/ko18.CDF', package = "faahKO"))
  input_ROI     <- input_targetFeatTable
  sampleColour  <- c('blue', 'red', 'green', 'yellow')
  
  # Expected save paths
  expected_path_plot1 <- file.path(saveFolder2, "ID-1.png")
  expected_path_plot2 <- file.path(saveFolder2, "ID-2.png")
  expected_path_plot3 <- file.path(saveFolder2, "ID-3.png")
  expected_path_plot4 <- file.path(saveFolder2, "ID-4.png")
  expected_path_CSV   <- file.path(saveFolder2, "IS_mean_RT.csv")
  # Expected message
  expected_message    <- c("Check input, \"sampleColour\" must be a vector of colour of same length as \"referenceSpectraFile\": default colour used instead\n", "No IS_ROI provided, mean RT of IS will not be calculated\n", "    4 ROI in 3 reference samples\n", "- Mean RT of IS will not be calculated\n", "\n-- Saving EICs for each ROI --\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Reading data from 4 windows\n")
  
  # results (output, warnings and messages)
  result_ROIstatsV <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder2, ROI=input_ROI, IS_ROI=NULL, sampleColour=sampleColour, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  
  # Check saved EIC plots
  expect_true(file.exists(expected_path_plot1))
  expect_true(file.exists(expected_path_plot2))
  expect_true(file.exists(expected_path_plot3))
  expect_true(file.exists(expected_path_plot4))
  
  # Check messages (no filepaths)
  expect_equal(length(result_ROIstatsV$messages), 14)
  expect_equal(result_ROIstatsV$messages[c(1:2, 4:8)], expected_message)
})

test_that('3 files, no save EICs, mean IS RT, no IS fit, without sampleColour, verbose, no verbose', {
  # temp saveFolder
  saveFolder3   <- tempdir()
  # clear temp folder
  suppressWarnings(do.call(file.remove, list(list.files(saveFolder3, full.names=TRUE))))
  
  # input
  refSpecFiles  <- input_spectraPaths
  input_ROI     <- input_targetFeatTable
  input_IS_ROI  <- input_targetFeatTable
  
  # Expected save paths
  expected_path_CSV   <- file.path(saveFolder3, "IS_mean_RT.csv")
  # Expected IS mean RT
  expected_meanIS     <- data.frame(matrix(nrow=4,ncol=2,dimnames=list(c(), c('X','mean_rt'))))
  expected_meanIS[1,] <- c('ID-1', 3360.054253)
  expected_meanIS[2,] <- c('ID-2', 3401.938353)
  expected_meanIS[3,] <- c('ID-3', 3458.840343)
  expected_meanIS[4,] <- c('ID-4', 3712.859261)
  expected_meanIS[,2] <- vapply(expected_meanIS[,2], as.numeric, FUN.VALUE=numeric(1))
  # Expected message
  expected_message    <- c("No ROI provided, EICs of ROI windows will not be saved\n", "- EICs of ROI windows will not be saved\n" , "    4 IS in 3 reference samples\n", "\n-- Calculating mean RT for each IS --\n", "Processing 4 compounds in 3 samples:\n", "  uROI:\tFALSE\n", "  FIR:\tFALSE\n", "----- ko15 -----\n", "Polarity can not be extracted from netCDF files, please set manually the polarity with the 'polarity' method.\n", "Check input, mzMLPath must be a .mzML\n", "Reading data from 4 windows\n", "----------------\n", "  0 failure(s)\n")

  
  # results (output, warnings and messages)
  result_ROIstatsV <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder3, ROI=NULL, IS_ROI=input_IS_ROI, sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  
  # Check mean IS RT
  expect_true(file.exists(expected_path_CSV))
  saved_CSV       <- read.csv(expected_path_CSV, header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
  expect_equal(saved_CSV, expected_meanIS, tolerance=1e-4)
  
  # Check messages (no filepaths)
  expect_equal(length(result_ROIstatsV$messages), 47)
  expect_equal(result_ROIstatsV$messages[c(1:2, 4:12, 44,46)], expected_message)
})

test_that('3 files, no save EICs (not a data.frame), no mean IS RT (not a data.frame), sampleColour is not character, verbose', {
  # message for ROI not a data.frame, IS_ROI not a data.frame, sampleColour not a character
  
  # temp saveFolder
  saveFolder4   <- tempdir()
  # clear temp folder
  suppressWarnings(do.call(file.remove, list(list.files(saveFolder4, full.names=TRUE))))
  
  # input
  refSpecFiles  <- input_spectraPaths
  input_ROI     <- 'notADataframe'
  input_IS_ROI  <- 'notADataframe'
  
  # Expected message
  expected_message    <- c("Check input, \"sampleColour\" must be a vector of colour of same length as \"referenceSpectraFile\": default colour used instead\n", "ROI is not a data.frame, EICs of ROI windows will not be saved\n",  "IS_ROI is not a data.frame, mean RT of IS will not be calculated\n", "- EICs of ROI windows will not be saved\n", "- Mean RT of IS will not be calculated\n")
  
  
  # results (output, warnings and messages)
  result_ROIstatsV <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder4, ROI=input_ROI, IS_ROI=input_IS_ROI, sampleColour=42, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  
  # Check messages
  expect_equal(length(result_ROIstatsV$messages), 5)
  expect_equal(result_ROIstatsV$messages, expected_message)
})

test_that('3 files, no save EICs (wrong columns), no mean IS RT (wrong columns), verbose', {
  # message for ROI and IS_ROI missing a column (cpdID, cpdName, rt, mz, rtMin, rtMax, mzMin, mzMax)
  
  # temp saveFolder
  saveFolder5   <- tempdir()
  # clear temp folder
  suppressWarnings(do.call(file.remove, list(list.files(saveFolder5, full.names=TRUE))))
  
  # input
  refSpecFiles  <- input_spectraPaths
  input_ROI     <- input_targetFeatTable
  input_IS_ROI  <- input_targetFeatTable
  # Expected message
  expected_message    <- c("ROI columns must contain \"cpdID\", \"cpdName\", \"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\" and \"mzMax\", EICs of ROI windows will not be saved\n", "IS_ROI columns must contain \"cpdID\", \"cpdName\", \"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\" and \"mzMax\", mean RT of IS will not be calculated\n", "- EICs of ROI windows will not be saved\n", "- Mean RT of IS will not be calculated\n")
  
  
  ## no cpdID
  # results (output, warnings and messages)
  result_ROIstatsNoCpdID <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-1], IS_ROI=input_IS_ROI[,-1], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoCpdID$messages), 4)
  expect_equal(result_ROIstatsNoCpdID$messages, expected_message)
  
  ## no cpdName
  # results (output, warnings and messages)
  result_ROIstatsNoCpdName <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-2], IS_ROI=input_IS_ROI[,-2], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoCpdName$messages), 4)
  expect_equal(result_ROIstatsNoCpdName$messages, expected_message)
  
  ## no rtMin
  # results (output, warnings and messages)
  result_ROIstatsNoRtMin <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-3], IS_ROI=input_IS_ROI[,-3], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoRtMin$messages), 4)
  expect_equal(result_ROIstatsNoRtMin$messages, expected_message)
  
  ## no rt
  # results (output, warnings and messages)
  result_ROIstatsNoRt <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-4], IS_ROI=input_IS_ROI[,-4], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoRt$messages), 4)
  expect_equal(result_ROIstatsNoRt$messages, expected_message)
  
  ## no rtMax
  # results (output, warnings and messages)
  result_ROIstatsNoRtMax <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-5], IS_ROI=input_IS_ROI[,-5], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoRtMax$messages), 4)
  expect_equal(result_ROIstatsNoRtMax$messages, expected_message)
  
  ## no mzMin
  # results (output, warnings and messages)
  result_ROIstatsNoMzMin <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-6], IS_ROI=input_IS_ROI[,-6], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoMzMin$messages), 4)
  expect_equal(result_ROIstatsNoMzMin$messages, expected_message)
  
  ## no mz
  # results (output, warnings and messages)
  result_ROIstatsNoMz <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-7], IS_ROI=input_IS_ROI[,-7], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoMz$messages), 4)
  expect_equal(result_ROIstatsNoMz$messages, expected_message)
  
  ## no mzMax
  # results (output, warnings and messages)
  result_ROIstatsNoMzMax <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder5, ROI=input_ROI[,-8], IS_ROI=input_IS_ROI[,-8], sampleColour=NULL, nCores=1, saveISPlots=FALSE, verbose=TRUE))
  # Check messages
  expect_equal(length(result_ROIstatsNoMzMax$messages), 4)
  expect_equal(result_ROIstatsNoMzMax$messages, expected_message)
})

test_that('parallel give the same result: 3 files, no save EICs, mean IS RT, save IS fit, verbose', {
  
  # temp saveFolder
  saveFolder6   <- tempdir()
  # clear temp folder
  suppressWarnings(do.call(file.remove, list(list.files(saveFolder6, full.names=TRUE))))
  
  # input
  refSpecFiles  <- input_spectraPaths
  input_ROI     <- input_targetFeatTable
  input_IS_ROI  <- input_targetFeatTable
  sampleColour  <- c('blue', 'red', 'green')
  
  # Expected IS mean RT
  expected_meanIS     <- data.frame(matrix(nrow=4,ncol=2,dimnames=list(c(), c('X','mean_rt'))))
  expected_meanIS[1,] <- c('ID-1', 3360.054253)
  expected_meanIS[2,] <- c('ID-2', 3401.938353)
  expected_meanIS[3,] <- c('ID-3', 3458.840343)
  expected_meanIS[4,] <- c('ID-4', 3712.859261)
  expected_meanIS[,2] <- sapply(expected_meanIS[,2], as.numeric)
  # Expected save paths
  expected_path_CSV       <- file.path(saveFolder6, "IS_mean_RT.csv")
  expected_path_IS_plot1  <- file.path(saveFolder6,"IS_search", "cpd_1.png")
  expected_path_IS_plot2  <- file.path(saveFolder6,"IS_search", "cpd_2.png")
  expected_path_IS_plot3  <- file.path(saveFolder6,"IS_search", "cpd_3.png")
  expected_path_IS_plot4  <- file.path(saveFolder6,"IS_search", "cpd_4.png")
  # Expected message
  expected_message_parallel <- c("No ROI provided, EICs of ROI windows will not be saved\n", "- EICs of ROI windows will not be saved\n", "    4 IS in 3 reference samples\n", "\n-- Calculating mean RT for each IS --\n", "Processing 4 compounds in 3 samples:\n", "  uROI:\tFALSE\n", "  FIR:\tFALSE\n", "Annotation object cannot be reordered by sample acquisition date\n", "----------------\n", "  0 failure(s)\n", "All plots saved\n")
  
  
  # results (output, warnings and messages)
  if (.Platform$OS.type == "windows") {
    BPParam <- BiocParallel::SnowParam(1)
  } else {
    BPParam <- BiocParallel::MulticoreParam(1)
  }
  result_ROIstats_parallel <- evaluate_promise(peakPantheR_ROIStatistics(refSpecFiles, saveFolder6, ROI=NULL, IS_ROI=input_IS_ROI,
                                                                         sampleColour=sampleColour, nCores=1, BPPARAM=BPParam,
                                                                         saveISPlots=TRUE, verbose=TRUE))
  
  # Check saved IS fit
  expect_true(file.exists(expected_path_IS_plot1))
  expect_true(file.exists(expected_path_IS_plot2))
  expect_true(file.exists(expected_path_IS_plot3))
  expect_true(file.exists(expected_path_IS_plot4))
  
  # Check mean IS RT
  expect_true(file.exists(expected_path_CSV))
  saved_CSV       <- read.csv(expected_path_CSV, header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)
  expect_equal(saved_CSV, expected_meanIS, tolerance=1e-4)

  # Check messages (no filepaths)
  if (.Platform$OS.type == 'windows') {
    expect_equal(length(result_ROIstats_parallel$messages), 21)
  expect_equal(result_ROIstats_parallel$messages[c(1,2,4:8, 10,11, 13,20)], expected_message_parallel)
  } else {
    expect_equal(length(result_ROIstats_parallel$messages), 20)
  expect_equal(result_ROIstats_parallel$messages[c(1,2,4:10,12,19)], expected_message_parallel)
  }
})

test_that('raise errors', {
  # temp saveFolder
  saveFolder8   <- tempdir()
  # clear temp folder
  suppressWarnings(do.call(file.remove, list(list.files(saveFolder8, full.names=TRUE))))
  refSpecFiles  <- input_spectraPaths
  input_ROI     <- input_targetFeatTable
  input_IS_ROI  <- input_targetFeatTable
  sampleColour  <- c('blue', 'red', 'green')
  
  # referenceSpectraFiles is not a character vector
  error_msg1 <- 'Check input, "referenceSpectraFiles" must be a vector of spectra paths'
  expect_error(peakPantheR_ROIStatistics(referenceSpectraFiles=42, saveFolder8, ROI=input_ROI, IS_ROI=input_IS_ROI, sampleColour=sampleColour, nCores=1, saveISPlots=TRUE, verbose=TRUE), error_msg1, fixed=TRUE)
  
  # saveFolder is not a character
  error_msg2 <- 'Check input, "saveFolder" must be a path'
  expect_error(peakPantheR_ROIStatistics(referenceSpectraFiles=refSpecFiles, saveFolder=42 , ROI=input_ROI, IS_ROI=input_IS_ROI, sampleColour=sampleColour, nCores=1, saveISPlots=TRUE, verbose=TRUE), error_msg2, fixed=TRUE)
  
  # saveFolder is not of length 1
  error_msg3 <- 'Check input, "saveFolder" must be a path'
  expect_error(peakPantheR_ROIStatistics(referenceSpectraFiles=refSpecFiles, saveFolder=c('onePath','anotherPath','morePath') , ROI=input_ROI, IS_ROI=input_IS_ROI,
                                         sampleColour=sampleColour, nCores=1, saveISPlots=TRUE, verbose=TRUE), error_msg3, fixed=TRUE)
})
