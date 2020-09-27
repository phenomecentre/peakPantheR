context('emgGaussian utils')

## Input and expected data
# Use EIC aggregated (sum) from raw data extracted from file ko15.CDf from the pkg faahKO with rt=c(3362.102, 3409.051), mz=c(496., 498.), msLevel=1
EIC_rt  <- c(3362.102, 3363.667, 3365.232, 3366.797, 3368.362, 3369.927, 3371.492, 3373.057, 3374.622, 3376.187, 3377.752, 3379.317, 3380.882, 3382.447, 3384.012, 3385.577, 3387.142, 3388.707, 3390.272, 3391.837, 3393.402, 3394.966, 3396.531, 3398.096, 3399.661, 3401.226, 3402.791, 3404.356, 3405.921, 3407.486, 3409.051)
EIC_int <- c(51048, 81568, 138288, 233920, 376448, 557288, 753216, 938048, 1091840, 1196992, 1261056, 1308992, 1362752, 1406592, 1431360, 1432896, 1407808, 1345344, 1268480, 1198592, 1126848, 1036544, 937600, 849792, 771456, 692416, 614528, 546088, 492752, 446464, 400632)
EIC     <- data.frame(rt=EIC_rt, int=EIC_int)

test_that('gaussian_cerf(), Gaussian Inverse Error function', {
  # Input / result
  x   <- c(0., 0.0001, 0.001, 0.01, 0.1, 1., 2., 5.)
  err <- c(1.000000e+00, 9.998872e-01, 9.988716e-01, 9.887166e-01, 8.875371e-01, 1.572992e-01, 4.677735e-03, 1.537437e-12)
  # Check results
  expect_equal(gaussian_cerf(x), err, tolerance=1e-5)
})

test_that('emgGaussian_minpack.lm()', {
  # Input / result
    in_param  <- list(amplitude=161883.8, center=3341.888, sigma=0.07861783, gamma=0.001827597)
    out_yy <- c(285.1279, 284.3135, 283.5015, 282.6918, 281.8844, 281.0793, 280.2765, 279.4760, 278.6778, 277.8819, 277.0882, 276.2968, 275.5077, 274.7208, 273.9362, 273.1538, 272.3736, 271.5957, 270.8200, 270.0465, 269.2752, 268.5066, 267.7397, 266.9751, 266.2125, 265.4522, 264.6941, 263.9381, 263.1842, 262.4325, 261.6830)
    result <- emgGaussian_minpack.lm(params=in_param, xx=EIC$rt)

  # Check results
  expect_equal(result, out_yy, tolerance=1e-5)
})

test_that('emgGaussian_minpack.lm_objectiveFun()', {
  # Input / result
  in_param  <- list(amplitude=161883.8, center=3341.888, sigma=0.07861783, gamma=0.001827597)
  out_res <- c(3076.974, 3079.353, 3081.731, 3084.105, 3086.478, 3088.848, 3091.216, 3093.581, 3095.944, 3098.305, 3100.664, 3103.020, 3105.374, 3107.726, 3110.076, 3112.423, 3114.768, 3117.111, 3119.452, 3121.791, 3124.127, 3126.459, 3128.791, 3131.121, 3133.448, 3135.774, 3138.097, 3140.418, 3142.737, 3145.053, 3147.368)
  result <- emgGaussian_minpack.lm_objectiveFun(params=in_param, observed=EIC$rt, xx=EIC$rt)

  # Check results
  expect_equal(result, out_res, tolerance=1e-5)
})

test_that('emgGaussian_guess()', {
  # Input / result
  guessed_param <- list(init_params  = list(amplitude=1e+07, center=3385.577, sigma=1, gamma=1),
                        lower_bounds = list(amplitude=0,     center=3382.577, sigma=0, gamma=-0.1),
                        upper_bounds = list(amplitude=1e+09, center=3388.577, sigma=5, gamma=5))

  result        <- emgGaussian_guess(x=EIC$rt, y=EIC$int)

  # Check results
  expect_equal(result, guessed_param)
})
