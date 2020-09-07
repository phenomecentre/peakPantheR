#' @title Check if object is of class peakPantheR_curveFit
#' 
#' @description Check if object is of class peakPantheR_curveFit
#' @param x object to test
#' @return (bool) TRUE or FALSE
is.peakPantheR_curveFit <- function(x) {
    inherits(x, "peakPantheR_curveFit")
}


#' @title Curve fitting using minpack.lm
#' 
#' @description Fit different curve models using \code{minpack}. Fitting
#' parameters can be passed or guessed.
#'
#' @param x (numeric) x values (e.g. retention time)
#' @param y (numeric) y observed values (e.g. spectra intensity)
#' @param curveModel (str) name of the curve model to fit (currently
#' \code{skewedGaussian} and \code{emgGaussian})
#' @param params (list or str) either 'guess' for automated parametrisation or
#' list of initial parameters (\code{$init_params}), lower parameter bounds
#' (\code{$lower_bounds}) and upper parameter bounds (\code{$upper_bounds})
#'
#' @return A 'peakPantheR_curveFit': a list of fitted curve parameters,
#' \code{fitStatus} from \code{nls.lm$info} and curve shape name
#' \code{curveModel}. \code{fitStatus=0} unsuccessful completion: improper input
#' parameters, \code{fitStatus=1} successful completion: first convergence test
#' is successful, \code{fitStatus=2} successful completion: second convergence
#' test is successful, \code{fitStatus=3} successful completion: both
#' convergence test are successful, \code{fitStatus=4} questionable completion:
#' third convergence test is successful but should be carefully examined
#' (maximizers and saddle points might satisfy), \code{fitStatus=5} unsuccessful
#' completion: excessive number of function evaluations/iterations
#' 
#' @details
#' ## Examples cannot be computed as the function is not exported:
#' ## x is retention time, y corresponding intensity
#' input_x  <- c(3362.102, 3363.667, 3365.232, 3366.797, 3368.362, 3369.927,
#'             3371.492, 3373.057, 3374.622, 3376.187, 3377.752, 3379.317,
#'             3380.882, 3382.447, 3384.012, 3385.577, 3387.142, 3388.707,
#'             3390.272, 3391.837, 3393.402, 3394.966, 3396.531, 3398.096,
#'             3399.661, 3401.226, 3402.791, 3404.356, 3405.921, 3407.486,
#'             3409.051)
#' input_y  <- c(51048, 81568, 138288, 233920, 376448, 557288, 753216, 938048,
#'             1091840, 1196992, 1261056, 1308992, 1362752, 1406592, 1431360,
#'             1432896, 1407808, 1345344, 1268480, 1198592, 1126848, 1036544,
#'             937600, 849792, 771456, 692416, 614528, 546088, 492752,
#'             446464, 400632)
#' 
#' ## Fit
#' fitted_curve <- fitCurve(input_x, input_y, curveModel='skewedGaussian',
#'                         params='guess')
#' 
#' ## Returns the optimal fitting parameters
#' fitted_curve
#' #
#' # $amplitude
#' # [1] 275371.1
#' # 
#' # $center
#' # [1] 3382.577
#' # 
#' # $sigma
#' # [1] 0.07904697
#' # 
#' # $gamma
#' # [1] 0.001147647
#' # 
#' # $fitStatus
#' # [1] 2
#' # 
#' # $curveModel
#' # [1] 'skewedGaussian'
#' #
#' # attr(,'class')
#' # [1] 'peakPantheR_curveFit'
fitCurve <- function(x, y, curveModel = "skewedGaussian", params = "guess") {
    
    # Check inputs x and y length
    if (length(x) != length(y)) {
        stop("Error: length of \"x\" and \"y\" must match!")
    }
    # known curveModel
    known_curveModel <- c("skewedGaussian", "emgGaussian")
    if (!(curveModel %in% known_curveModel)) {
        stop(paste("Error: \"curveModel\" must be one of:", known_curveModel))
    }
    # params
    if (!(typeof(params) %in% c("list", "character"))) {
        stop("Error: \"params\" must be a list or \"guess\"")
    }

    useGuess = TRUE
    if (any(params != "guess")) {
        useGuess = FALSE
        # check init_params, lower and upper bounds are defined
        if (!all(c("init_params", "lower_bounds", "upper_bounds") %in%
            names(params))) {
            stop("Error: \"params must be a list of \"init_params\", ",
                "\"lower_bounds\" and \"upper_bounds\"") }
        # init_params is list
        if (typeof(params$init_params) != "list") {
            stop("Error: \"params$init_params\" must be a list of parameters")
        }
        # lower_bounds is list
        if (typeof(params$lower_bounds) != "list") {
            stop("Error: \"params$lower_bounds\" must be a list of parameters")
        }
        # upper_bounds is list
        if (typeof(params$upper_bounds) != "list") {
            stop("Error: \"params$upper_bounds\" must be a list of parameters")
        }
    }
    
    # Init
    fittedCurve <- list()
    
    # Run fitting skewed gaussian
    if (curveModel == "skewedGaussian") {
        fittedCurve <- fitCurve_skewedGaussian(x, y, useGuess, params,
                                                curveModel)
    }
    else if (curveModel == 'emgGaussian') {
        fittedCurve <- fitCurve_emgGaussian(x, y, useGuess, params,
                                                curveModel)
    }
    # for future curve shapes } else if () { }
    
    return(fittedCurve)
}

# fit skewedGaussian
fitCurve_skewedGaussian <- function(x, y, useGuess, params, curveModel) {
    fittedCurve <- list()

    # Guess parameters and bounds
    if (useGuess) {
        new_params <- skewedGaussian_guess(x, y)
    } else {
        new_params <- params
    }

    # ensure order of init params and bounds (init is a list, lower and
    # upper are ordered numeric vectors)
    init    <- list(amplitude = new_params$init_params$amplitude,
                    center = new_params$init_params$center,
                    sigma = new_params$init_params$sigma,
                    gamma = new_params$init_params$gamma)
    lower   <- unlist(c(new_params$lower_bounds["amplitude"],
                        new_params$lower_bounds["center"],
                        new_params$lower_bounds["sigma"],
                        new_params$lower_bounds["gamma"]))
    upper   <- unlist(c(new_params$upper_bounds["amplitude"],
                        new_params$upper_bounds["center"],
                        new_params$upper_bounds["sigma"],
                        new_params$upper_bounds["gamma"]))

    # perform fit
    resultFit <- minpack.lm::nls.lm(par = init,
                                lower = lower,
                                upper = upper,
                                fn = skewedGaussian_minpack.lm_objectiveFun,
                                observed = y, xx = x)

    # prepare output
    fittedCurve <- resultFit$par
    fittedCurve$fitStatus <- resultFit$info
    fittedCurve$curveModel <- curveModel
    class(fittedCurve) <- "peakPantheR_curveFit"

    return(fittedCurve)
}

# fit emgGaussian
fitCurve_emgGaussian <- function(x, y, useGuess, params, curveModel) {
    fittedCurve <- list()

    # Guess parameters and bounds
    if (useGuess) {
        new_params <- emgGaussian_guess(x, y)
    } else {
        new_params <- params
    }

    # ensure order of init params and bounds (init is a list, lower and
    # upper are ordered numeric vectors)
    init    <- list(amplitude = new_params$init_params$amplitude,
                    center = new_params$init_params$center,
                    sigma = new_params$init_params$sigma,
                    gamma = new_params$init_params$gamma)
    lower   <- unlist(c(new_params$lower_bounds["amplitude"],
                        new_params$lower_bounds["center"],
                        new_params$lower_bounds["sigma"],
                        new_params$lower_bounds["gamma"]))
    upper   <- unlist(c(new_params$upper_bounds["amplitude"],
                        new_params$upper_bounds["center"],
                        new_params$upper_bounds["sigma"],
                        new_params$upper_bounds["gamma"]))

    # perform fit
    resultFit <- minpack.lm::nls.lm(par = init,
                                lower = lower,
                                upper = upper,
                                fn = emgGaussian_minpack.lm_objectiveFun,
                                observed = y, xx = x)

    # prepare output
    fittedCurve <- resultFit$par
    fittedCurve$fitStatus <- resultFit$info
    fittedCurve$curveModel <- curveModel
    class(fittedCurve) <- "peakPantheR_curveFit"

    return(fittedCurve)
}



#' @title Predict curve values
#'
#' @description Evaluate fitted curve values at \code{x} data points
#'
#' @param fittedCurve (peakPantheR_curveFit) A 'peakPantheR_curveFit': a list of
#' curve fitting parameters, curve shape model \code{curveModel} and nls.lm fit
#' status \code{fitStatus}.
#' @param x (numeric) values at which to evaluate the fitted curve
#' 
#' @return  fitted curve values at x
#' 
#' @details
#' ## Examples cannot be computed as the function is not exported:
#' ## Input a fitted curve
#' fittedCurve <- list(amplitude=275371.1, center=3382.577, sigma=0.07904697,
#'                     gamma=0.001147647, fitStatus=2,
#'                     curveModel='skewedGaussian')
#' class(fittedCurve)  <- 'peakPantheR_curveFit'
#' input_x <- c(3290, 3300, 3310, 3320, 3330, 3340, 3350, 3360, 3370, 3380,
#'             3390, 3400, 3410)
#'
#' ## Predict y at each input_x
#' pred_y  <- predictCurve(fittedCurve, input_x)
#' pred_y
#' # [1] 2.347729e-08 1.282668e-05 3.475590e-03 4.676579e-01 3.129420e+01
#' # [6] 1.043341e+03 1.736915e+04 1.447754e+05 6.061808e+05 1.280037e+06
#' # [11] 1.369651e+06 7.467333e+05 2.087477e+05
predictCurve <- function(fittedCurve, x) {
    
    # Check input
    if (!is.peakPantheR_curveFit(fittedCurve)) {
        stop("Error: \"fittedCurve\" must be a peakPantheR_curveFit!")
    }
    known_curveModel <- c("skewedGaussian", "emgGaussian")
    if (!(fittedCurve$curveModel %in% known_curveModel)) {
        stop(paste("Error: \"fittedCurve$curveModel\" must be one of:",
                    known_curveModel))
    }
    
    # Select correct model
    if (fittedCurve$curveModel == "skewedGaussian") 
        {
            yy <- skewedGaussian_minpack.lm(params = fittedCurve, xx = x)
        }
    else if (fittedCurve$curveModel == 'emgGaussian') {
        yy <- emgGaussian_minpack.lm(params = fittedCurve, xx = x)
        }
    # for future curve shapes
    # else if () {}

    return(yy)
}



## -----------------------------------------------------------------------------
## Skewed Gaussian
## -----------------------------------------------------------------------------

#' @title Gaussian Error function
#'
#' @description Implementation of the gaussian error function
#' 
#' @param x (numeric) value at which to evaluate the gaussian error function
#' 
#' @return Value of the gaussian error function evaluated at x
gaussian_erf <- function(x) {
    return(2 * stats::pnorm(x * sqrt(2)) - 1)
}


#' @title Gaussian Error function
#'
#' @description Implementation of the gaussian error function
#'
#' @param x (numeric) value at which to evaluate the gaussian error function
#'
#' @return Value of the gaussian error function evaluated at x
gaussian_cerf <- function(x) {
    return(1 - (2 * stats::pnorm(x * sqrt(2)) - 1))
}


#' @title Implementation of the Skewed Gaussian peak shape for use with
#' minpack.lm
#'
#' @description Implementation of the Skewed Gaussian peak shape for use with
#' minpack.lm
#'
#' @param params (list) skewed gaussian parameters (\code{params$gamma},
#' \code{params$center}, \code{params$sigma}, \code{params$amplitude})
#' @param xx (numeric) values at which to evalute the skewed gaussian
#' 
#' @return value of the skewed gaussian evaluated at xx
skewedGaussian_minpack.lm <- function(params, xx) {
    erf_term <- 1 + gaussian_erf((params$gamma * (xx-params$center))/params$sigma *
        sqrt(2))
    yy <- (params$amplitude/(params$sigma * sqrt(2 * pi))) *
        exp(-(xx - params$center)^2/2 * params$sigma^2) * erf_term
    
    return(yy)
}

#' @title Implementation of the Exponentially Modified Gaussian (EMG) peak shape for use with
#' minpack.lm
#'
#' @description Implementation of the  Exponentially Modified Gaussian (EMG) peak shape for use with
#' minpack.lm
#'
#' @param params (list) exponentiall modified gaussian parameters (\code{params$gamma},
#' \code{params$center}, \code{params$sigma}, \code{params$amplitude})
#' @param xx (numeric) values at which to evalute the exponentially modified gaussian
#'
#' @return value of the exponentially modified gaussian evaluated at xx
emgGaussian_minpack.lm <- function(params, xx) {
    cerf_term <- skew_cerf((params$center + params$gamma * (params$sigma^2) - xx)/(params$sigma *
        sqrt(2)))
    yy <- (params$amplitude*params$gamma/2) *
        exp(params$gamma*(params$center - xx + (params$gamma * (params$sigma^2)/2))) * cerf_term

    return(yy)
}

#' @title Skewed Gaussian minpack.lm objective function
#'
#' @description Skewed Gaussian minpack.lm objective function, calculates
#' residuals using the skewed gaussian Peak Shape
#'
#' @param params (list) skewed gaussian parameters (\code{params$gamma},
#' \code{params$center}, \code{params$sigma}, \code{params$amplitude})
#' @param observed (numeric) observed y value at xx
#' @param xx (numeric) value at which to evalute the skewed gaussian
#' 
#' @return difference between observed and expected skewed gaussian value
#' evaluated at xx
skewedGaussian_minpack.lm_objectiveFun <- function(params, observed, xx) {
    return(observed - skewedGaussian_minpack.lm(params, xx))
}

#' @title Exponentially Modified Gaussian minpack.lm objective function
#'
#' @description Exponentially Modified Gaussian (EMG) minpack.lm objective function, calculates
#' residuals using the EMG Peak Shape
#'
#' @param params (list) exponentially modified gaussian parameters (\code{params$gamma},
#' \code{params$center}, \code{params$sigma}, \code{params$amplitude})
#' @param observed (numeric) observed y value at xx
#' @param xx (numeric) value at which to evalute the exponentially modified gaussian
#'
#' @return difference between observed and expected exponentially modified gaussian value
#' evaluated at xx
emgGaussian_minpack.lm_objectiveFun <- function(params, observed, xx) {
    return(observed - emgGaussian_minpack.lm(params, xx))
}

#' @title Guess function for initial skewed gaussian parameters and bounds
#'
#' @description Guess function for initial skewed gaussian parameters and
#' bounds, at the moment only checks the x position
#'
#' @param x (numeric) x values (e.g. retention time)
#' @param y (numeric) y observed values (e.g. spectra intensity)
#' 
#' @return A list of guessed starting parameters \code{list()$init_params},
#' lower \code{list()$lower_bounds} and upper bounds \code{list()$upper_bounds}
#' (\code{$gamma}, \code{$center}, \code{$sigma}, \code{$amplitude})
skewedGaussian_guess <- function(x, y) {
    # set center as x position of max y value (e.g. highest spectra intensity)
    center_guess <- x[which.max(y)]
    # init_param
    init_params <- list(amplitude=1e+07, center=center_guess, sigma=1, gamma=1)
    # lower_bounds
    lower_bounds <- list(amplitude=0, center=center_guess-3, sigma=0,gamma=-0.1)
    # upper_bounds
    upper_bounds <- list(amplitude=1e+09, center=center_guess+3, sigma=5,
        gamma = 5)
    
    return(list(init_params = init_params, lower_bounds = lower_bounds,
                upper_bounds = upper_bounds))
}

#' @title Guess function for initial exponentially modified gaussian parameters and bounds
#'
#' @description Guess function for initial exponentially modified gaussian parameters and
#' bounds, at the moment only checks the x position
#'
#' @param x (numeric) x values (e.g. retention time)
#' @param y (numeric) y observed values (e.g. spectra intensity)
#'
#' @return A list of guessed starting parameters \code{list()$init_params},
#' lower \code{list()$lower_bounds} and upper bounds \code{list()$upper_bounds}
#' (\code{$gamma}, \code{$center}, \code{$sigma}, \code{$amplitude})
emgGaussian_guess <- function(x, y) {
    # set center as x position of max y value (e.g. highest spectra intensity)
    center_guess <- x[which.max(y)]
    # init_param
    init_params <- list(amplitude=1e+07, center=center_guess, sigma=1, gamma=1)
    # lower_bounds
    lower_bounds <- list(amplitude=0, center=center_guess-3, sigma=0,gamma=-0.1)
    # upper_bounds
    upper_bounds <- list(amplitude=1e+09, center=center_guess+3, sigma=5,
        gamma = 5)

    return(list(init_params = init_params, lower_bounds = lower_bounds,
                upper_bounds = upper_bounds))
}

