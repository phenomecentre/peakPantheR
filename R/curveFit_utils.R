#' Curve fitting using minpack.lm
#' 
#' Fit different curve models using minpack. Fitting parameters can be passed or guessed.
#'
#' @param x (numeric) x values (e.g. retention time)
#' @param y (numeric) y observed values (e.g. spectra intensity)
#' @param curveModel (str) name of the curve model to fit (currently \code{skewedGaussian})
#' @param params (list or str) either list of curve fit parameters
#' @param lower (NULL or numeric vector) if not NULL, a numeric vector of lower bounds on each parameter. If NULL preset parammeters are employed
#' @param upper (NULL or numeric vector) if not NULL, a numeric vector of upper bounds on each parameter. If NULL preset parammeters are employed
#' 
#' @return A list of fitted curve parameters, \code{fitStatus} from \code{nls.lm$info} and curve shape name \code{curveModel}. \code{fitStatus=0} unsuccessful completion: improper input parameters, \code{fitStatus=1} successful completion: first convergence test is successful, \code{fitStatus=2} successful completion: second convergence test is successful, \code{fitStatus=3} successful completion: both convergence test are successful, \code{fitStatus=4} questionable completion: third convergence test is successful but should be carefully examined (maximizers and saddle points might satisfy), \code{fitStatus=5} unsuccessful completion: excessive number of function evaluations/iterations
#' 
#' @export
fitCurve <- function(x, y, curveModel='skewedGaussian', params='guess', lower=NULL, upper=NULL) {
  
  ## Check inputs
  # x and y length
  if (length(x) != length(y)) {
    stop('Error: length of "x" and "y" must match!')
  }
  # known curveModel
  known_curveModel <- c('skewedGaussian')
  if (!(curveModel %in% known_curveModel)) {
    stop(paste('Error: "curveModel" must be one of', known_curveModel))
  }
  # params
  if (!(typeof(params) %in% c('list', 'character'))) {
    stop('Error: "params" must be a list or "guess"')
  }
  useGuess = TRUE
  if (any(params != "guess")) {
    useGuess = FALSE
  }
  # lower
  if (!is.null(lower)) {
    if (typeof(lower) != 'double') {
      stop('Error: "lower" must be a NULL or numeric')
    }
  }
  # upper
  if (!is.null(upper)) {
    if (typeof(upper) != 'double') {
      stop('Error: "lower" must be a NULL or numeric')
    }
  }
  
  ## Init
  fittedCurve <- list()
  
  ## Run fitting
  # skewed gaussian
  if (curveModel == 'skewedGaussian') {
    
    # Guess init parameters
    if (useGuess) {
      init_params   <- skewedGaussian_guess(x, y)
    } else {
      init_params   <- params
    }
    
    # lower param bounds
    if (is.null(lower)) {
      lower_bounds  <- c(0, init_params$center - 3, 0, -0.1)
    } else {
      lower_bounds  <- lower
    }
    
    # upper param bounds
    if (is.null(upper)) {
      upper_bounds  <- c(1e9, init_params$center + 3, 5, 5)
    } else {
      upper_bounds  <- upper
    }
    
    # perform fit
    resultFit  <- minpack.lm::nls.lm(par=init_params, lower=lower_bounds, upper=upper_bounds, fn=skewedGaussian_minpack.lm_objectiveFun, observed=y, xx=x)
    
    # prepare output
    fittedCurve             <- resultFit$par
    fittedCurve$fitStatus   <- resultFit$info
    fittedCurve$curveModel  <- curveModel
  }
  # for future curve shapes
  #} else if () {
  #}
  
  return(fittedCurve)
}


#' Predict curve values
#'
#' Evaluate fitted curve values at \code{x} data points
#'
#' @param fittedCurve (list) a list of curve fitting parameters, curve shape model \code{curveModel} and nls.lm fit status \code{fitStatus}.
#' @param x (numeric) values at which to evaluate the fitted curve
#' 
#' @return  fitted curve values at x
#' 
#' @export
predictCurve  <- function(fittedCurve, x) {

  # Check input
  if (class(fittedCurve) != 'list') {
    stop('Error: "fittedCurve" must be a list!')
  }
  if (!("curveModel" %in% names(fittedCurve))) {
    stop('Error: "fittedCurve" must contain a "curveModel"!')
  }
  known_curveModel <- c('skewedGaussian')
  if (!(fittedCurve$curveModel %in% known_curveModel)) {
    stop(paste('Error: "fittedCurve$curveModel" must be one of', known_curveModel))
  }
  
  # Select correct model
  if (fittedCurve$curveModel == 'skewedGaussian') {
    yy <- skewedGaussian_minpack.lm(params=fittedCurve, xx=x)
  } # for future curve shapes
  #} else if () {
  #}
  
  return(yy)
}



## --------------------------------------------------------------------------------------------------
##        Skewed Gaussian
## --------------------------------------------------------------------------------------------------

#' Gaussian Error function
#'
#' Implementation of the gaussian error function
#' 
#' @param x (numeric) value at which to evaluate the gaussian error function
#' 
#' @return Value of the gaussian error function evaluated at x
erf   <- function(x){
  return(2 * stats::pnorm(x * sqrt(2)) - 1)
} 


#' Implementation of the Skewed Gaussian peak shape for use with minpack.lm
#'
#' Implementation of the Skewed Gaussian peak shape for use with minpack.lm
#'
#' @param params (list) skewed gaussian parameters (\code{params$gamma}, \code{params$center}, \code{params$sigma}, \code{params$amplitude})
#' @param xx (numeric) values at which to evalute the skewed gaussian
#' 
#' @return value of the skewed gaussian evaluated at xx
skewedGaussian_minpack.lm   <- function(params, xx) {
  erf_term  <- 1 + erf((params$gamma * (xx - params$center)) / params$sigma * sqrt(2))
  yy        <- (params$amplitude / (params$sigma * sqrt(2* pi))) * exp(-(xx - params$center)^2 / 2*params$sigma^2) * erf_term
  
  return(yy)
}


#' Skewed Gaussian minpack.lm objective function
#'
#' Skewed Gaussian minpack.lm objective function, calculates residuals using the skewed gaussian Peak Shape
#'
#' @param params (list) skewed gaussian parameters (\code{params$gamma}, \code{params$center}, \code{params$sigma}, \code{params$amplitude})
#' @param observed (numeric) observed y value at xx
#' @param xx (numeric) value at which to evalute the skewed gaussian
#' 
#' @return difference between observed and expected skewed gaussian value evaluated at xx
skewedGaussian_minpack.lm_objectiveFun  <- function(params, observed, xx) {
  return(observed - skewedGaussian_minpack.lm(params, xx))
}


#' Guess function for initial skewed gaussian parameters
#'
#' Guess function for initial skewed gaussian parameters, at the moment only checks the x position
#'
#' @param x (numeric) x values (e.g. retention time)
#' @param y (numeric) y observed values (e.g. spectra intensity)
#' 
#' @return Guessed starting parameters (\code{params$gamma}, \code{params$center}, \code{params$sigma}, \code{params$amplitude})
skewedGaussian_guess <- function(x, y) {
  # set center as x position of max y value (e.g. highest spectra intensity)
  center_guess  <- x[which.max(y)]
  param_guess   <- list(amplitude = 10000000, center=center_guess, sigma=1, gamma=1)
  
  return(param_guess)
}





