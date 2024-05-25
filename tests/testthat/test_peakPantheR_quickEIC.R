context('peakPantheR_quickEIC()')

skip_if_not_installed('faahKO',  minimum_version = '1.18.0')
library(faahKO)


## Input and expected data
# use ko15.CDf file from the pkg faahKO
raw_data_path <- system.file('cdf/KO/ko15.CDF', package = "faahKO")


test_that('default plot EIC, subset rt and mz, verbose', {
  # Expected message
  expected_messages <- 'Reading data from 1 windows\n' # other messages are from reading and timing

  # results (output, warnings and messages)
  result_peakPantheR_quickEIC <- evaluate_promise(peakPantheR_quickEIC(spectraPath=raw_data_path, rt=c(3310., 3390.), mz=c(520., 522.4)))

  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_peakPantheR_quickEIC$result))
  expect_equal(result_peakPantheR_quickEIC$result$labels$x, "Retention Time (sec)")
  expect_equal(result_peakPantheR_quickEIC$result$labels$y, "Intensity")
  expect_equal(length(result_peakPantheR_quickEIC$result), length(ggplot2::ggplot()))
  
  # Check result messages (reading and timing)
  expect_equal(length(result_peakPantheR_quickEIC$messages), 3)
  expect_equal(result_peakPantheR_quickEIC$messages[2], expected_messages)
})

test_that('default plot EIC, no verbose', {
  # results (output, warnings and messages)
  result_peakPantheR_quickEIC <- evaluate_promise(peakPantheR_quickEIC(spectraPath=raw_data_path, rt=c(3310., 3390.), mz=c(520., 522.4), verbose=FALSE))

  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_peakPantheR_quickEIC$result))
  expect_equal(result_peakPantheR_quickEIC$result$labels$x, "Retention Time (sec)")
  expect_equal(result_peakPantheR_quickEIC$result$labels$y, "Intensity")
  expect_equal(length(result_peakPantheR_quickEIC$result), length(ggplot2::ggplot()))
  
  # Check result messages (None other than MSnbase)
  expect_equal(length(result_peakPantheR_quickEIC$messages), 1)
})

test_that('valuesOnly=Raw, no verbose', {
  # Expected signal
  expected_rt  <- c(3310.459, 3310.459, 3310.459, 3312.024, 3312.024, 3312.024, 3313.589, 3313.589, 3313.589, 3315.154, 3315.154, 3315.154, 3316.719, 3316.719, 3316.719, 3318.284, 3318.284, 3318.284, 3319.849, 3319.849, 3319.849, 3321.414, 3321.414, 3321.414, 3322.979, 3322.979, 3322.979, 3324.544, 3324.544, 3324.544, 3326.109, 3326.109, 3327.673, 3327.673, 3329.238, 3329.238, 3329.238, 3330.803, 3330.803, 3332.368, 3332.368, 3333.933, 3333.933, 3335.498, 3335.498, 3337.063, 3337.063, 3338.628, 3338.628, 3340.193, 3340.193, 3341.758, 3341.758, 3343.323, 3343.323, 3344.888, 3344.888, 3346.453, 3346.453, 3348.018, 3348.018, 3348.018, 3349.583, 3349.583, 3349.583, 3351.148, 3351.148, 3351.148, 3352.713, 3352.713, 3352.713, 3354.278, 3354.278, 3355.843, 3355.843, 3357.408, 3357.408, 3358.973, 3358.973, 3360.538, 3360.538, 3362.102, 3362.102, 3363.667, 3363.667, 3365.232, 3365.232, 3366.797, 3366.797, 3368.362, 3368.362, 3369.927, 3369.927, 3369.927, 3371.492, 3371.492, 3371.492, 3373.057, 3373.057, 3373.057, 3374.622, 3374.622, 3374.622, 3376.187, 3376.187, 3376.187, 3377.752, 3377.752, 3377.752, 3379.317, 3379.317, 3380.882, 3380.882, 3382.447, 3382.447, 3384.012, 3384.012, 3385.577, 3385.577, 3385.577, 3387.142, 3387.142, 3387.142, 3388.707, 3388.707, 3388.707)
  expected_mz  <- c(520.2000122, 521.2999878, 522, 520.2000122, 521, 522.1000366, 520.2000122, 521, 522.1000366, 520.2000122, 521.1000366, 522.2000122, 520.2000122, 521.1000366, 522.2000122, 520.1000366, 521.1000366, 522.2000122, 520.1000366, 521.1000366, 522.2000122, 520.1000366, 521.1000366, 522.2000122, 520.1000366, 521.2000122, 522.2000122, 520.2000122, 521.2999878, 522.2000122, 520.2000122, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 521, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.2000122, 522.2000122, 520.2000122, 522.2000122, 520.2000122, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 520.4000244, 522.2000122, 520, 520.2999878, 522.2000122, 520, 520.2999878, 522.2000122, 520, 520.2999878, 522.2000122, 520, 522.2000122, 520.2999878, 522.2000122, 520.2999878, 522.2000122, 520.2999878, 522.2000122, 520.2999878, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 520.9000244, 522.2000122, 520, 521, 522.2000122, 520.1000366, 521.1000366, 522.2000122, 520.1000366, 521.1000366, 522.2000122, 520.2000122, 521.2000122, 522.2000122, 520.1000366, 521.2000122, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 522.2000122, 520.1000366, 521.2000122, 522.2000122, 520.1000366, 521.2000122, 522.2000122, 520.2000122, 521.2000122, 522.2000122)
  expected_int <- c(1850, 425, 879, 1864, 408, 1112, 1637, 493, 1540, 1466, 590, 2187, 1452, 668, 3534, 1655, 653, 6338, 1828, 665, 11718, 2034, 712, 21744, 2225, 747, 37872, 2485, 814, 62424, 2526, 98408, 2407, 152896, 2186, 925, 225984, 2033, 308672, 1975, 399360, 1997, 504000, 1853, 614656, 1629, 711872, 1420, 784704, 1311, 836608, 1322, 866304, 1360, 882304, 1261, 889280, 1061, 888256, 936, 902, 866816, 958, 920, 827392, 1178, 997, 777728, 1366, 1129, 727040, 1416, 678464, 1541, 629120, 1621, 578048, 1499, 524288, 1300, 471040, 1619, 416320, 2171, 360064, 2632, 302400, 3164, 249152, 3686, 202560, 3914, 1755, 161024, 3679, 1560, 123520, 3255, 1329, 93160, 3230, 1172, 71856, 3475, 1146, 58392, 3573, 1206, 51072, 3427, 48376, 3360, 49168, 3504, 53120, 3751, 62488, 3761, 1061, 78680, 3562, 1336, 102840, 3472, 1338, 134656)
  expected_signal <- data.frame(rt=expected_rt, mz=expected_mz, int=expected_int)

  # results (output, warnings and messages)
  result_peakPantheR_quickEIC <- evaluate_promise(peakPantheR_quickEIC(spectraPath=raw_data_path, rt=c(3310., 3390.), mz=c(520., 522.4), valuesOnly='Raw', verbose=FALSE))

  # Check plot properties
  expect_equal(result_peakPantheR_quickEIC$result, expected_signal)
  
  # Check result messages (None other than MSnbase)
  expect_equal(length(result_peakPantheR_quickEIC$messages), 1)
})

test_that('valuesOnly=EIC, no verbose', {
  # Expected signal
  expected_rt  <- c(3310.459, 3312.024, 3313.589, 3315.154, 3316.719, 3318.284, 3319.849, 3321.414, 3322.979, 3324.544, 3326.109, 3327.673, 3329.238, 3330.803, 3332.368, 3333.933, 3335.498, 3337.063, 3338.628, 3340.193, 3341.758, 3343.323, 3344.888, 3346.453, 3348.018, 3349.583, 3351.148, 3352.713, 3354.278, 3355.843, 3357.408, 3358.973, 3360.538, 3362.102, 3363.667, 3365.232, 3366.797, 3368.362, 3369.927, 3371.492, 3373.057, 3374.622, 3376.187, 3377.752, 3379.317, 3380.882, 3382.447, 3384.012, 3385.577, 3387.142, 3388.707)
  expected_int <- c(3154, 3384, 3670, 4243, 5654, 8646, 14211, 24490, 40844, 65723, 100934, 155303, 229095, 310705, 401335, 505997, 616509, 713501, 786124, 837919, 867626, 883664, 890541, 889317, 868654, 829270, 779903, 729535, 679880, 630661, 579669, 525787, 472340, 417939, 362235, 305032, 252316, 206246, 166693, 128759, 97744, 76258, 63013, 55851, 51803, 52528, 56624, 66239, 83502, 107738, 139466)
  expected_signal <- data.frame(rt=expected_rt, int=expected_int)

  # results (output, warnings and messages)
  result_peakPantheR_quickEIC <- evaluate_promise(peakPantheR_quickEIC(spectraPath=raw_data_path, rt=c(3310., 3390.), mz=c(520., 522.4), valuesOnly='EIC', verbose=FALSE))

  # Check plot properties
  expect_equal(result_peakPantheR_quickEIC$result, expected_signal)
  
  # Check result messages (None other than MSnbase)
  expect_equal(length(result_peakPantheR_quickEIC$messages), 1)
})
