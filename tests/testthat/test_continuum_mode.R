context('Continuum/profile mode')

skip_if_not_installed('msdata',  minimum_version = '0.24.1')
library(msdata)


## Input
# use 2 continuum mode files from the msdata pkg
input_spectraPaths <- c(system.file('sciex/20171016_POOL_POS_1_105-134.mzML', package = "msdata"),
                        system.file('sciex/20171016_POOL_POS_3_105-134.mzML', package = "msdata"))

# target serine in msdata files
input_ROI     	<- data.frame(matrix(vector(), 1, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
input_ROI[1,] 	<- c("ID-1", "Serine", 175., 181., 187., 106.039871, 106.049871, 106.059871)
input_ROI[,3:8] <- vapply(input_ROI[,3:8], as.numeric, FUN.VALUE=numeric(1))


## Expected
# found peakTable
found_peakTable     <- data.frame(matrix(vector(), 1, 18, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "cpdID", "cpdName", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
found_peakTable[1,] <- c(TRUE, 179.3141, 181.368, 184.3227, 106.0404, 106.0501, 106.0593, 490160.9, 491545.8, 255343, 262945.8, 'ID-1', 'Serine', FALSE, 1.952462, 0.3680482, 1.179395, 1.32217)
found_peakTable[,c(1,14)]       <- vapply(found_peakTable[,c(1,14)], as.logical, FUN.VALUE=logical(1))
found_peakTable[,c(2:11,15:18)] <- vapply(found_peakTable[,c(2:11,15:18)], as.numeric, FUN.VALUE=numeric(1))

# found curveFit
cFit1           <- list(amplitude=446548.3, center=180.7982, sigma=0.9538973, gamma=0.7536497, fitStatus=1, curveModel="skewedGaussian")
class(cFit1)    <- 'peakPantheR_curveFit'
found_curveFit  <- list(cFit1)

# found ROIsDataPoint
tmp_raw_data            <- MSnbase::readMSData(input_spectraPaths[1], centroided = FALSE, mode='onDisk')
found_ROIsDataPoints    <- extractSignalRawData(tmp_raw_data,
                                                rt=input_ROI[,c('rtMin','rtMax')],
                                                mz=input_ROI[,c('mzMin','mzMax')],
                                                verbose=FALSE)


test_that('extractSignalRawData() continuum mode', {
  # Input
  raw_data <- MSnbase::readMSData(system.file('sciex/20171016_POOL_POS_1_105-134.mzML', package = "msdata"),
                                    centroided = FALSE, mode='onDisk')

  # Expected signal
  expected_rt  <- c(175.217, 175.217, 175.217, 175.217, 175.217, 175.217, 175.217, 175.217, 175.217, 175.496, 175.496, 175.496, 175.496, 175.496, 175.496, 175.496, 175.496, 175.496, 175.775, 175.775, 175.775, 175.775, 175.775, 175.775, 176.054, 176.054, 176.054, 176.054, 176.054, 176.054, 176.054, 176.054, 176.054, 176.054, 176.333, 176.333, 176.333, 176.333, 176.333, 176.333, 176.333, 176.333, 176.333, 176.612, 176.612, 176.612, 176.612, 176.612, 176.612, 176.612, 176.612, 176.891, 176.891, 176.891, 176.891, 176.891, 176.891, 176.891, 176.891, 176.891, 176.891, 177.171, 177.171, 177.171, 177.171, 177.171, 177.171, 177.171, 177.171, 177.171, 177.171, 177.171, 177.450, 177.450, 177.450, 177.450, 177.450, 177.450, 177.450, 177.450, 177.450, 177.729, 177.729, 177.729, 177.729, 177.729, 177.729, 177.729, 177.729, 178.008, 178.008, 178.008, 178.008, 178.008, 178.008, 178.008, 178.287, 178.287, 178.287, 178.287, 178.287, 178.287, 178.287, 178.566, 178.566, 178.566, 178.566, 178.566, 178.566, 178.566, 178.566, 178.845, 178.845, 178.845, 178.845, 178.845, 178.845, 179.124,
	179.124, 179.124, 179.124, 179.124, 179.124, 179.124, 179.403, 179.403, 179.403, 179.403, 179.403, 179.403, 179.403, 179.403, 179.682, 179.682, 179.682, 179.682, 179.682, 179.682, 179.682, 179.682, 179.682, 179.682, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 179.961, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.240, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.519, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 180.798, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.077, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.356, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635, 181.635,
	181.635, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 181.914, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.193, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.472, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 182.751, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.030, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.309, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.588, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 183.867, 184.146, 184.146, 184.146, 184.146, 184.146, 184.146, 184.146, 184.146,
	184.146, 184.146, 184.146, 184.146, 184.146, 184.146, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.425, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.704, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 184.983, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.262, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.541, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 185.820, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.099, 186.378, 186.378, 186.378, 186.378, 186.378, 186.378, 186.378, 186.378, 186.378, 186.378, 186.378, 186.378, 186.657, 186.657, 186.657, 186.657, 186.657, 186.657, 186.657, 186.657, 186.657, 186.657, 186.657, 186.936, 186.936, 186.936, 186.936, 186.936, 186.936, 186.936, 186.936)
  expected_mz  <- c(106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0593, 106.0491, 106.0506, 106.0520, 106.0535, 106.0549, 106.0564, 106.0578, 106.0593, 106.0462, 106.0476, 106.0491, 106.0506, 106.0520, 106.0535, 106.0549, 106.0564, 106.0578, 106.0593, 106.0447, 106.0462, 106.0476, 106.0491, 106.0506, 106.0520, 106.0535, 106.0549, 106.0564, 106.0578, 106.0593, 106.0447, 106.0461, 106.0476, 106.0490, 106.0505, 106.0519, 106.0534, 106.0548, 106.0592, 106.0476, 106.0490, 106.0505, 106.0519, 106.0534, 106.0548, 106.0563, 106.0592, 106.0461, 106.0476, 106.0490, 106.0505, 106.0519, 106.0534, 106.0548, 106.0461, 106.0475, 106.0490, 106.0504, 106.0519, 106.0534, 106.0548, 106.0490, 106.0504, 106.0519, 106.0534, 106.0548,
	106.0563, 106.0577, 106.0592, 106.0446, 106.0461, 106.0475, 106.0519, 106.0534, 106.0548, 106.0447, 106.0461, 106.0476, 106.0505, 106.0519, 106.0534, 106.0549, 106.0447, 106.0461, 106.0476, 106.0490, 106.0505, 106.0519, 106.0534, 106.0549, 106.0447, 106.0461, 106.0476, 106.0490, 106.0505, 106.0519, 106.0534, 106.0549, 106.0563, 106.0578, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491,
	106.0506, 106.0520, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0418, 106.0433, 106.0447, 106.0462, 106.0476, 106.0491, 106.0505, 106.0520, 106.0534, 106.0549, 106.0563, 106.0578, 106.0592, 106.0404, 106.0418, 106.0433, 106.0447, 106.0462, 106.0476, 106.0491, 106.0505, 106.0520, 106.0534, 106.0549, 106.0563, 106.0578, 106.0592, 106.0404, 106.0418, 106.0433, 106.0447, 106.0462, 106.0476, 106.0491, 106.0505, 106.0520, 106.0534, 106.0549, 106.0563, 106.0578, 106.0592, 106.0404, 106.0418, 106.0433, 106.0447, 106.0462, 106.0476, 106.0491, 106.0505, 106.0520, 106.0534, 106.0549, 106.0563, 106.0578, 106.0592, 106.0404, 106.0418, 106.0433, 106.0447, 106.0462, 106.0476, 106.0491, 106.0505, 106.0520, 106.0534, 106.0549, 106.0563, 106.0578, 106.0592, 106.0404, 106.0418, 106.0433, 106.0447, 106.0462, 106.0476, 106.0491, 106.0505, 106.0520, 106.0534, 106.0549, 106.0563, 106.0578, 106.0592, 106.0405, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0405, 106.0419, 106.0434, 106.0448, 106.0463,
	106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0405, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0405, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0405, 106.0419, 106.0434, 106.0448, 106.0463, 106.0477, 106.0492, 106.0506, 106.0521, 106.0535, 106.0550, 106.0564, 106.0579, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448,
	106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0549, 106.0564, 106.0578, 106.0593, 106.0404, 106.0419, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0549, 106.0564, 106.0578, 106.0593, 106.0433, 106.0448, 106.0462, 106.0477, 106.0491, 106.0506, 106.0520, 106.0535, 106.0549, 106.0564, 106.0578, 106.0593, 106.0445, 106.0460, 106.0474, 106.0489, 106.0504, 106.0518, 106.0533, 106.0547, 106.0562, 106.0576, 106.0591, 106.0460, 106.0474, 106.0489, 106.0504, 106.0518, 106.0533, 106.0547, 106.0562)
  expected_int <- c(0, 81, 162, 81, 0, 81, 0, 0, 81, 0, 121, 61, 182, 61, 61, 61, 0, 61, 0, 49, 0, 148, 148, 0, 0, 105, 35, 70, 35, 35, 35, 0, 0, 70, 0, 99, 66, 99, 33, 33, 132, 0, 0, 0, 81, 41, 0, 122, 0, 41, 0, 0, 38, 76, 0, 0, 76, 0, 0, 76, 0, 0, 68, 103, 137, 137, 0, 0, 34, 0, 34, 34, 0, 45, 178, 45, 89, 134, 134, 0, 0, 0, 196, 147, 49, 0, 49, 0, 0, 0, 45, 89, 89, 0, 45, 0, 0, 48, 48, 0, 0, 96, 0, 0, 342, 86, 0, 86, 0, 86, 0, 0, 203, 0, 0, 203, 0, 0, 407, 0, 0, 407, 203, 0, 0, 203, 0, 406, 0, 406, 203, 0, 0, 406, 813, 1016, 1422, 406, 203, 0, 203, 0, 0, 203, 1016, 1422, 3658, 3861, 3455, 2642, 1829, 610, 203, 203, 203, 0, 203, 203, 2642, 3861, 8535, 15241, 11989, 9957, 4877, 2845, 1422, 813, 610, 0, 203, 407, 4537, 12114, 20459, 22801, 21970, 19189, 11503, 6039, 2491, 1877, 212, 215, 0, 836, 6331, 17371, 28254, 39903, 37100, 22502, 15421, 7623, 5284, 1698, 1698, 432, 215, 1507, 6205, 23466, 40361, 51447, 47899, 32908, 19562, 9074, 5783, 3221, 1082, 0, 431, 1251, 8244, 21983, 34748, 53516, 52937, 39668, 22014, 12518, 4350, 1285, 2398, 630, 645, 1947, 6216, 21336, 43791, 50980, 54316,
    34469, 18510, 9296, 5152, 3902, 1318, 325, 331, 663, 8292, 18781, 32222, 43928, 37416, 30636, 14958, 9259, 5790, 3167, 1003, 678, 0, 798, 3974, 12074, 22410, 28576, 31588, 24399, 12025, 7157, 4847, 2485, 1633, 0, 270, 409, 2178, 8543, 18297, 23541, 16922, 13642, 8021, 6257, 1216, 1217, 687, 123, 121, 361, 1804, 5789, 11837, 9534, 10051, 8473, 4722, 1943, 601, 241, 483, 0, 198, 0, 1252, 3032, 5825, 7972, 8073, 5664, 3991, 2141, 1741, 192, 485, 83, 83, 334, 1336, 1836, 3422, 4674, 4925, 3506, 1252, 835, 501, 250, 250, 69, 69, 138, 689, 2619, 2964, 2826, 2481, 3170, 1309, 620, 138, 69, 138, 0, 164, 327, 982, 3028, 2619, 2291, 1391, 655, 900, 327, 82, 0, 171, 0, 0, 171, 938, 1536, 2389, 1536, 768, 853, 512, 171, 85, 0, 72, 0, 288, 432, 504, 1296, 1799, 1727, 1224, 648, 432, 288, 288, 144, 0, 64, 193, 129, 451, 1868, 1417, 1031, 902, 644, 258, 64, 0, 129, 0, 55, 111, 166, 610, 998, 1498, 1830, 1109, 444, 277, 166, 55, 0, 0, 44, 133, 133, 489, 622, 755, 1155, 800, 755, 355, 89, 44, 89, 36, 0, 0, 218, 508, 798, 1124, 834, 580, 544, 254, 145, 73, 36, 0, 61, 61, 121, 455, 820, 820, 577, 364, 213, 91, 91, 91, 61, 30, 30, 30, 120, 479, 419, 779, 689, 449, 210, 180, 90, 60, 0, 0, 58, 289, 405, 492, 752, 260, 174, 145, 58, 58, 0, 0, 108, 431, 538, 431, 161, 269, 0, 108, 54, 0, 0, 406, 203, 813, 406, 203, 203, 0)
  expected_signal   <- list(data.frame(rt=expected_rt, mz=expected_mz, int=expected_int))

  # results (output, warnings and messages)
  result_extractSignalRawData <- evaluate_promise(extractSignalRawData(rawSpec=raw_data, rt=c(175., 187.), mz=c(106.039871, 106.059871), msLevel=1L, verbose=TRUE))
  # Check results
  expect_equal(result_extractSignalRawData$result, expected_signal, tolerance=1e-6)
})

test_that('peakPantheR_singleFileSearch() continuum mode', {

    # peakStatistic, no plotEICsPath, no getAcquTime, no FIR, no verbose

    ## Input and expected
    # use continuum mode files from the msdata pkg
    spectraPaths    <- system.file('sciex/20171016_POOL_POS_1_105-134.mzML', package = "msdata")

    # target serine in msdata files
    input_ROI     	<- data.frame(matrix(vector(), 1, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
    input_ROI[1,] 	<- c("ID-1", "Serine", 175., 181., 187., 106.039871, 106.049871, 106.059871)
    input_ROI[,3:8] <- vapply(input_ROI[,3:8], as.numeric, FUN.VALUE=numeric(1))

    # found peakTable
    found_peakTable     <- data.frame(matrix(vector(), 1, 18, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "cpdID", "cpdName", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
    found_peakTable[1,] <- c(TRUE, 179.3141, 181.368, 184.3227, 106.0404, 106.0501, 106.0593, 490160.9, 491545.8, 255343, 262945.8, 'ID-1', 'Serine', FALSE, 1.952462, 0.3680482, 1.179395, 1.32217)
    found_peakTable[,c(1,14)]       <- vapply(found_peakTable[,c(1,14)], as.logical, FUN.VALUE=logical(1))
    found_peakTable[,c(2:11,15:18)] <- vapply(found_peakTable[,c(2:11,15:18)], as.numeric, FUN.VALUE=numeric(1))

    # found curveFit
    cFit1           <- list(amplitude=446548.3, center=180.7982, sigma=0.9538973, gamma=0.7536497, fitStatus=1, curveModel="skewedGaussian")
    class(cFit1)    <- 'peakPantheR_curveFit'
    found_curveFit  <- list(cFit1)

    # found ROIsDataPoint
    tmp_raw_data            <- MSnbase::readMSData(input_spectraPaths[1], centroided = FALSE, mode='onDisk')
    found_ROIsDataPoints    <- extractSignalRawData(tmp_raw_data,
                                                rt=input_ROI[,c('rtMin','rtMax')],
                                                mz=input_ROI[,c('mzMin','mzMax')],
                                                verbose=FALSE)


    # Expected TIC
    expected_TIC            <- 650969677
    # Expected peakTable
    expected_peakTable      <- found_peakTable
    # Expected curveFit
    expected_curveFit       <- found_curveFit
    # Expected ROIsDataPoint
    expected_ROIsDataPoint  <- found_ROIsDataPoints
    # Expected acquTime
    expected_acquTime       <- NA

    # results (output, warnings and messages) (use the first continuum file)
    result_singleFileSearch <- evaluate_promise(peakPantheR_singleFileSearch(spectraPaths,
        input_ROI, peakStatistic=TRUE, plotEICsPath=NA, getAcquTime=FALSE, FIR=NULL, centroided=FALSE, verbose=FALSE))

    # Check results
    expect_equal(result_singleFileSearch$result$TIC, expected_TIC)
    expect_equal(result_singleFileSearch$result$peakTable, expected_peakTable, tolerance=1e-6)
    expect_equal(result_singleFileSearch$result$curveFit, expected_curveFit, tolerance=1e-6)
    expect_equal(result_singleFileSearch$result$ROIsDataPoint, expected_ROIsDataPoint)
    expect_equal(result_singleFileSearch$result$acquTime, expected_acquTime)
})

if ((.Platform$OS.type != "windows") || (.Machine$sizeof.pointer == 8)) {
test_that('peakPantheR_parallelAnnotation() continuum mode', {

    # 2 files, 1 compound, no uROI, no FIR, no getAcquTime, no verbose'

    ## Input and expected
    # use 2 continuum mode files from the msdata pkg
    input_spectraPaths <- c(system.file('sciex/20171016_POOL_POS_1_105-134.mzML', package = "msdata"),
                            system.file('sciex/20171016_POOL_POS_3_105-134.mzML', package = "msdata"))

    # target serine in msdata files
    input_targetFeatTable     	<- data.frame(matrix(vector(), 1, 8, dimnames=list(c(), c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax"))),stringsAsFactors=FALSE)
    input_targetFeatTable[1,] 	<- c("ID-1", "Serine", 175., 181., 187., 106.039871, 106.049871, 106.059871)
    input_targetFeatTable[,3:8] <- vapply(input_targetFeatTable[,3:8], as.numeric, FUN.VALUE=numeric(1))

    # FIR
    input_FIR     	      <- data.frame(matrix(vector(), 4, 4, dimnames=list(c(), c("rtMin", "rtMax", "mzMin", "mzMax"))),stringsAsFactors=FALSE)
    input_FIR[1,] 	      <- c(175., 187., 106.039871, 106.059871)
    input_FIR[,c(1:4)]    <- sapply(input_FIR[,c(1:4)], as.numeric)

    # uROI (ROI are wrong, uROI are right)
    input_uROI            <- input_targetFeatTable[,c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")]

    # cpdMetadata
    input_cpdMetadata     <- data.frame(matrix(data=c('a',1), nrow=1, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)

    # spectraMetadata
    input_spectraMetadata <- data.frame(matrix(data=c('e',5), nrow=1, ncol=2, dimnames=list(c(),c('testcol1','testcol2')), byrow=FALSE), stringsAsFactors=FALSE)


    # Expected peakTables
    peakTable1     <- data.frame(matrix(vector(), 1, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
    peakTable1[1,] <- c(TRUE, 179.3141, 181.368, 184.3227, 106.0404, 106.0501, 106.0593, 490160.9, 491545.8, 255343, 262945.8, FALSE, 1.952462, 0.3680482, 1.179395, 1.32217)
    peakTable1[,c(1,12)]       <- sapply(peakTable1[,c(1,12)], as.logical)
    peakTable1[,c(2:11,13:16)] <- sapply(peakTable1[,c(2:11,13:16)], as.numeric)
    # 2
    peakTable2     <- data.frame(matrix(vector(), 1, 16, dimnames=list(c(), c("found", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax", "peakArea", "peakAreaRaw", "maxIntMeasured", "maxIntPredicted", "is_filled", "ppm_error", "rt_dev_sec", "tailingFactor", "asymmetryFactor"))),stringsAsFactors=FALSE)
    peakTable2[1,] <- c(TRUE, 179.0366, 180.9154, 183.7788, 106.0407, 106.0501, 106.0597, 461117.8, 466111.8, 265650, 262253.9, FALSE, 1.981645, -0.08462651, 1.215098, 1.386996)
    peakTable2[,c(1,12)]       <- sapply(peakTable2[,c(1,12)], as.logical)
    peakTable2[,c(2:11,13:16)] <- sapply(peakTable2[,c(2:11,13:16)], as.numeric)
    expected_peakTables <- list(peakTable1, peakTable2)

    # Expected peakFit
    # 1
    cFit1.1         <- list(amplitude=446548.3, center=180.7982, sigma=0.9538973, gamma=0.7536497, fitStatus=1, curveModel="skewedGaussian")
    class(cFit1.1)  <- 'peakPantheR_curveFit'
    # 2
    cFit2.1         <- list(amplitude=444881.5, center=180.3627, sigma=0.9816369, gamma=0.8840238, fitStatus=1, curveModel="skewedGaussian")
    class(cFit2.1)  <- 'peakPantheR_curveFit'
    expected_peakFit <- list(list(cFit1.1), list(cFit2.1))

    # found ROIsDataPoint
    tmp_raw_data1  	  <- MSnbase::readMSData(input_spectraPaths[1], centroided = FALSE, mode='onDisk')
    ROIDataPoints1    <- extractSignalRawData(tmp_raw_data1, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
    tmp_raw_data2  	  <- MSnbase::readMSData(input_spectraPaths[2], centroided = FALSE, mode='onDisk')
    ROIDataPoints2    <- extractSignalRawData(tmp_raw_data2, rt=input_targetFeatTable[,c('rtMin','rtMax')], mz=input_targetFeatTable[,c('mzMin','mzMax')], verbose=FALSE)
    expected_dataPoints <- list(ROIDataPoints1, ROIDataPoints2)


    # Object fully initialised
    initAnnotation      <- peakPantheRAnnotation(spectraPaths=input_spectraPaths, targetFeatTable=input_targetFeatTable, cpdMetadata=input_cpdMetadata, spectraMetadata=input_spectraMetadata)

    # Expected annotation
    expected_annotation             <- initAnnotation
    expected_annotation@TIC         <- c(650969677, 622957884)
    expected_annotation@peakTables  <- expected_peakTables
    expected_annotation@peakFit     <- expected_peakFit
    expected_annotation@dataPoints  <- expected_dataPoints
    expected_annotation@isAnnotated <- TRUE
    # Expected failures
    tmp_status          <- NA
    names(tmp_status)   <- 'test'
    tmp_failures        <- !is.na(tmp_status)
    names(tmp_failures) <- NULL
    expected_failures   <- data.frame(matrix(c(names(tmp_status)[tmp_failures], tmp_status[tmp_failures]), ncol=2, byrow=FALSE, dimnames=list(c(), c('file', 'error'))), stringsAsFactors=FALSE)

    # results (output, warnings and messages)
    result_parallelAnnotation <- evaluate_promise(peakPantheR_parallelAnnotation(initAnnotation, ncores=0, getAcquTime=FALSE, centroided=FALSE, verbose=FALSE))

    # Check results
    expect_equal(result_parallelAnnotation$result$annotation, expected_annotation, tolerance=1e-5)
    expect_equal(result_parallelAnnotation$result$failures, expected_failures)
}) }
