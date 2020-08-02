#' Correct targeted retention time based on reference compounds
#'
#' Correct targeted features retention time using the RT and RT deviation of
#' previously fitted compounds. The `method` and `params` are used to select and
#' parametrise the retention time correction method employed. When `robust` is set to TRUE, the RANSAC algorithm is used
#' to automatically flag outliers and robustify the correction function fitting. If `diagnostic` is
#' TRUE, RT correction diagnostic plots are returned (specific to each
#' correction method).
#'
#' @param targetFeatTable a \code{\link{data.frame}} of compounds to target as
#' rows and parameters as columns: \code{cpdID} (str), \code{cpdName} (str),
#' \code{rtMin} (float in seconds), \code{rt} (float in seconds, or \emph{NA}),
#' \code{rtMax} (float in seconds), \code{mzMin} (float), \code{mz} (float or
#' \emph{NA}), \code{mzMax} (float).
#' @param referenceTable a \code{\link{data.frame}} of reference compound
#' information as rows and properties as columns: \code{cpdID} (str),
#' \code{cpdName} (str), \code{rt} (float), \code{rt_dev_sec} (float)
#' @param method (str) name of RT correction method to use (currently
#' \code{polynomial})
#' @param params (list) list of parameters to pass to the each correction method.
#' Currently allowed inputs are \code{polynomialOrder} for \code{method='polynomial'}
#' @param robust (bool) whether to use the RANSAC algorithm to flag and ignore outliers
#' during retention time correction
#' @param verbose (bool) If TRUE message progress of RT correction
#' @param ... optional method specific parameters
#' 
#' @return a targetFeatTable with corrected RT, or diagnostic information
#' 
#' @export
peakPantheR_applyRTCorrection <- function(targetFeatTable, referenceTable,
                                        method='polynomial', params=list(polynomialOrder=3), robust=TRUE,
                                        verbose=TRUE, ...) {

    # Check inputs
    applyRTCorrection_checkInput(targetFeatTable, referenceTable, method, params, robust)

    # Init
    corrected_targetFeatTable <- targetFeatTable
    corrected_targetFeatTable$isReference <- 'External set'

    if (dim(referenceTable)[1] == 1) {
      method <- 'constant'
    }
    ## Run correction
    # Polynomial models (linear, quadratic, etc...)
    if (method == 'polynomial') {
        # Adjust the polynomialOrder if the number of references passed in insuficient
        if (dim(referenceTable)[1] <= params[['polynomialOrder']]) {
            params[['polynomialOrder']] <- dim(referenceTable)[1] - 1
        }
        # Use RANSAC?
        if (isTRUE(robust)) {
            rtCorrectionOutput <- fit_RANSAC(referenceTable$rt,
                                                referenceTable$rt_dev_sec,
                                             polynomialOrder=params[['polynomialOrder']])
          which_outlier <- corrected_targetFeatTable$cpdID %in% referenceTable$cpdID[!rtCorrectionOutput$inlier]
          which_reference <- corrected_targetFeatTable$cpdID %in% referenceTable$cpdID[rtCorrectionOutput$inlier]
          corrected_targetFeatTable$isReference[which_outlier] <- 'Reference set outlier'
          corrected_targetFeatTable$isReference[which_reference] <- 'Reference set'

        }  else {
            rtCorrectionOutput <- fit_polynomial(referenceTable$rt,
                                                referenceTable$rt_dev_sec,
                                             polynomialOrder=params[['polynomialOrder']])
             which_reference <- corrected_targetFeatTable$cpdID %in% referenceTable$cpdID
             corrected_targetFeatTable$isReference[which_reference] <- 'Reference set'
        }
        # Get the correction outputs
        correctionFunction <- rtCorrectionOutput$model
        correctedRtDrift <- predict(correctionFunction, newdata=data.frame(x=targetFeatTable$rt))
        corrected_targetFeatTable$correctedRt <- corrected_targetFeatTable$rt - correctedRtDrift
        corrected_targetFeatTable$predictedRtDrift <- correctedRtDrift

    }
    else if (method == 'constant') {
        # The predicted drift is the constant observed drift in the single reference
        corrected_targetFeatTable$correctedRt <- corrected_targetFeatTable$rt - rep(referenceTable$rt_dev_sec, dim(corrected_targetFeatTable)[1])
        corrected_targetFeatTable$predictedRtDrift <- rep(referenceTable$rt_dev_sec, dim(corrected_targetFeatTable)[1])
        which_reference <- corrected_targetFeatTable$cpdID %in% referenceTable$cpdID
        corrected_targetFeatTable$isReference[which_reference] <- 'Reference set'
    }
    # for future Rt correction algorithms, just extend the else if()

    #
    return(list(correctedRtTable=corrected_targetFeatTable))
    }
    ## TODO
    # unittest
    # @example
    # vignette, where it is integrated in the workflow

# applyRTCorrection check input
applyRTCorrection_checkInput <- function(targetFeatTable, referenceTable,
                                            method, params, robust) {
    ## Check targetFeatTable
    applyRTCorrection_checkTargetFeatTable(targetFeatTable)

    ## Check referenceTable
    # is data.frame
    if (!is(referenceTable, "data.frame")){
        stop("specified referenceTable is not a data.frame")
    }
    # required columns are present
    if (!all(c("cpdID", "cpdName", "rt", "rt_dev_sec") %in%
    colnames(referenceTable))){
        stop("expected columns in referenceTable are \"cpdID\", \"cpdName\", ",
        "\"rt\" and \"rt_dev_sec\"")
    }
    # column type
    if (dim(referenceTable)[1] != 0){
        if (!is.character(referenceTable$cpdID[1])){
            stop("referenceTable$cpdID must be character")
        }
        if (!is.character(referenceTable$cpdName[1])){
            stop("referenceTable$cpdName must be character")
        }
        if (!(is.numeric(referenceTable$rt[1]) | is.na(referenceTable$rt[1]))){
            stop("referenceTable$rt must be numeric or NA")
        }
        if (!(is.numeric(referenceTable$rt_dev_sec[1]) |
        is.na(referenceTable$rt_dev_sec[1]))){
            stop("referenceTable$rt_dev_sec must be numeric or NA") }
    }

    ## Check method
    KNOWN_CORRECTIONMETHODs <- c("polynomial", "constant")
    if (!(method %in% KNOWN_CORRECTIONMETHODs)) {
        stop('Error: \"method\" must be one of: \"polynomial\", \"constant\"')
    }

   if ((dim(referenceTable)[1] == 1) & (method != 'constant')) {
         stop('No function can be fitted with a single reference. Use method=\`offset\` instead.')
       }

    ## Check params input
    if (!is.list(params)) {
        stop('Check input, "params" must be list') }
    # Verify if parameters passed on params are adequate for chosen method
    if (is.list(params)) {
       if (method == 'polynomial') {
           if (!any(names(params) == 'polynomialOrder')) {
               stop("polynomialOrder must be provided in params") }
           else {
             if (!is(params[['polynomialOrder']], 'numeric')) {
                  stop("polynomialOrder must be an integer and equal or greater than 1") }
             else if (!isTRUE(all.equal(params[['polynomialOrder']], as.integer(params[['polynomialOrder']]))) | (params[['polynomialOrder']] < 1)) {
                  stop("polynomialOrder must be an integer and equal or greater than 1")
             }
             else if (params[['polynomialOrder']] >= dim(referenceTable)[1]) {
                 warning("`polynomialOrder` is larger than the number of references passed. `polynomialOrder` will be set equal to number of reference compounds - 1")
                 }
             }
       }

      else if (method == 'constant') {
         if (dim(referenceTable)[1] > 1) {
           stop("`constant` Rt correction can only use a single reference")
         }
       }
    }

    ## Check robust argument
    if (!is.logical(robust)) { stop("robust must be either TRUE or FALSE")}


}
# check input targetFeatTable
applyRTCorrection_checkTargetFeatTable <- function(targetFeatTable) {
    # is data.frame
    if (!is(targetFeatTable, "data.frame")){
        stop("specified targetFeatTable is not a data.frame")
    }

    # required columns are present
    if (!all(c("cpdID", "cpdName", "rt", "rt_dev_sec") %in% colnames(targetFeatTable))){
        stop("expected columns in targetFeatTable are \"cpdID\", \"cpdName\", ",
        "\"rt\", \"rt_dev_sec\"")
    }

    # column type
    if (dim(targetFeatTable)[1] != 0){
        if (!is.character(targetFeatTable$cpdID[1])){
            stop("targetFeatTable$cpdID must be character")
        }
        if (!is.character(targetFeatTable$cpdName[1])){
            stop("targetFeatTable$cpdName must be character")
        }
        if (!(is.numeric(targetFeatTable$rt[1]) |
                is.na(targetFeatTable$rt[1]))){
            stop("targetFeatTable$rt must be numeric or NA")
        }
        if (!(is.numeric(targetFeatTable$rt_dev_sec[1]) | is.na(targetFeatTable$rt_dev_sec[1]))) {
            stop("targetFeatTable$rt_dev_sec must be numeric or NA") }

    }
}

## -----------------------------------------------------------------------------
##        RANSAC
## -----------------------------------------------------------------------------
# New retention time calibration functions can be added below
# All fitting functions should have the following argument:
# of the kind x = theoretical rt, y= Deviation (Rt_{obs} - Rt{exp}}
# LOESS placeholder.
fit_LOESS <- function(x, y, ...) {

    loess_fun <- loess()

    return (loess_fun)
}
# General purpose polynomial function - can be used for linear fits
fit_polynomial <- function(x, y, polynomialOrder, returnFitted=FALSE) {

    polyModel <- lm(y ~ poly(x, degree=polynomialOrder))

    if (isTRUE(returnFitted)) {
        modelFitted <- fitted(polyModel)
        modelResiduals <- residuals(polyModel)
        return(list(model=polyModel, fitted=modelFitted, residuals=modelResiduals))
    }
    else {return(list(model=polyModel))}
}

## Port of the RANSAC regressor from scikit-learn
#
#
#
fit_RANSAC <- function(x, y, polynomialOrder=3,
                            residual_threshold=NULL,
                            loss='absolute_loss',
                            min_samples=NULL,
                            max_trials = 1000, max_skips=Inf, stop_n_inliers=Inf, stop_score=Inf,
                            stop_probability=0.99, random_state=NULL) {

    if (length(x) != length(y)) {
        stop('x and y must have the same length')
    }
    #
   if (isFALSE(all.equal(polynomialOrder, as.integer(polynomialOrder)) | (polynomialOrder >= 1))) {
       stop("`polynomialOrder` must be an integer and equal or greater than 1")
    }

    # If residual_threshold is NULL, use
    # the MAD (median absolute deviation) of the y variable as cutoff
    if (is.null(residual_threshold)) {
      residual_threshold <- median(abs(y - median(y)))
    }
    # Select the loss function
    if (loss == "absolute_loss") {
      loss_function <- loss_absolute
    }
    else if (loss == "squared_loss") {
      loss_function <- loss_squared
    }
    else {
        stop("Allowed `loss` functions are `loss_squared` and `loss_absolute`")
    }

    if (!is.null(random_state)) {
      set.seed(random_state)
    }
    # Create a data frame to use when calling "predict.lm"
    x_data <- data.frame(x)
    colnames(x_data) <- 'x'

    n_inliers_best <- 0
    score_best <- 0
    # Minimum number of samples - by default the max between polynomial function degree + 1 and half of the dataset
    # size
    if (is.null(min_samples)) {
        min_samples <- max(polynomialOrder + 1 , ceiling(0.5*length(x)))}
    # Generate array of indices for all samples to use later

    n_samples <- length(x)
    all_samples_idx <- seq(1, n_samples)

    n_trials <- 0
    n_skips_no_inliers <- 0

    # Core of the RANSAC algorithm
    while (n_trials < max_trials) {
        n_trials <- n_trials + 1

        # choose random sample set
        data_subset_idx <- sample(all_samples_idx, min_samples, replace=FALSE)
        #cat(length(data_subset_idx))
        current_x <- x[data_subset_idx]
        current_y <- y[data_subset_idx]

        fitted_model_current_iteration <- tryCatch(fit_polynomial(current_x, current_y, polynomialOrder=polynomialOrder),
        error=function(e) e)
          if(inherits(fitted_model_current_iteration, "error")){
            n_skips_no_inliers <- n_skips_no_inliers + 1
            next
        }
        # residuals of all data for current random sample model
        y_pred <- predict(fitted_model_current_iteration$model, newdata=x_data)

        residuals_subset <- loss_function(y, y_pred)

        # classify data into inliers and outliers
        inlier_mask_subset <- residuals_subset < residual_threshold
        n_inliers_subset <- sum(inlier_mask_subset)

        # less inliers -> skip current random sample
        if (n_inliers_subset < n_inliers_best) {
            n_skips_no_inliers <- n_skips_no_inliers + 1
            next
        }

        # extract inlier data set
        inlier_idxs_subset <- all_samples_idx[inlier_mask_subset]
        X_inlier_subset <- x[inlier_idxs_subset]
        y_inlier_subset <- y[inlier_idxs_subset]

        # score of inlier data set - implement
        score_subset <- summary(fit_polynomial(X_inlier_subset,
                                       y_inlier_subset, polynomialOrder=polynomialOrder)$model)$r.squared

        # same number of inliers but worse score -> skip current random
        if ((n_inliers_subset == n_inliers_best) & ( score_subset < score_best)) {
            next
        }

        # save current random sample as best sample
        n_inliers_best <- n_inliers_subset
        score_best <- score_subset
        inlier_mask_best <- inlier_mask_subset
        X_inlier_best <- X_inlier_subset
        y_inlier_best <- y_inlier_subset

        # Do we need this?? # Check all these conditions and code wrapping
        max_trials <- min(max_trials, dynamic_max_trials(n_inliers_best, n_samples, min_samples, stop_probability))

        # break if sufficient number of inliers or score is reached
        if ((n_inliers_best >= stop_n_inliers) | (score_best >= stop_score)) { break }
        }

        # if none of the iterations met the required criteria
        if (is.null(inlier_mask_best)) {
          if ((n_skips_no_inliers) > max_skips) {
            warning("Number of skipped iterations larger than `max_skips`") }
          else {
            warning(
              "RANSAC could not find a valid consensus set") }
        }
          else {
            if ((n_skips_no_inliers) > max_skips) {
                warning("RANSAC found a valid consensus set but exited
                        early due to skipping more iterations than
                        `max_skips`") }
        }

        # estimate final model using all inliers
        final_fitted_regressor <- fit_polynomial(X_inlier_best, y_inlier_best, polynomialOrder)
        inlier_mask <- inlier_mask_best
        return(list(model=final_fitted_regressor$model, inlier=inlier_mask))

}

# Absolute Loss function - part of RANSAC interpretation
loss_absolute <- function(y_true, y_pred) {
  loss_value <- abs(y_true - y_pred)
  return(loss_value)
}

# Square loss function - part of RANSAC interpretation
loss_squared <- function(y_true, y_pred) {

  loss_value <- (y_true - y_pred) ** 2
  return(loss_value)

}

# dynamic max trials - part of the RANSAC implementation
dynamic_max_trials <- function(n_inliers, n_samples, min_samples, probability) {

  epsilon <- .Machine$double.eps
  inlier_ratio <- n_inliers / n_samples

  nom <- max(epsilon, 1 - probability)
  denom <- max(epsilon, 1 - inlier_ratio ** min_samples)

  if (nom == 1) {return(0)}
  if (denom == 1) {return(Inf)}

  return(abs(ceiling(log(nom) / log(denom))))

}