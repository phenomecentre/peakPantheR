## Update ROI / uROI / FIR on an existing peakPantheRAnnotation from a CSV,
## preserving cached @dataPoints / @TIC / @acquisitionTime and previous results.
#' @title Update fit parameters from CSV on an existing annotation
#'
#' @description Overlay ROI (and optionally uROI and FIR) bounds on an existing
#' \code{peakPantheRAnnotation} object from a CSV of fit parameters, e.g. a
#' file previously produced by \code{outputAnnotationParamsCSV} and edited
#' manually or via the GUI. Unlike \code{peakPantheR_loadAnnotationParamsCSV},
#' which initialises a fresh object, this function mutates the supplied
#' annotation in place and preserves cache-bearing slots (\code{@dataPoints},
#' \code{@TIC}, \code{@acquisitionTime}) and previous results
#' (\code{@peakTables}, \code{@peakFit}). The returned object can be passed
#' directly to \code{peakPantheR_parallelAnnotation} to refit against updated
#' bounds without re-reading raw spectra from disk when bounds remain within
#' the cached ROI.
#'
#' The CSV format follows \code{peakPantheR_loadAnnotationParamsCSV}: either
#' the basic layout (\code{cpdID, cpdName, rtMin, rt, rtMax, mzMin, mz, mzMax}),
#' or the advanced layout (\code{ROI_*, uROI_*, FIR_*} columns) which updates
#' all three bound slots and \code{uROIExist}.
#'
#' A basic CSV updates the integration window (\code{@uROI}) and the fallback
#' integration window (\code{@FIR}) in lockstep, leaving the extraction
#' envelope (\code{@ROI}) untouched. This matches the cache-oriented model
#' where \code{@ROI} is the stable cache key and \code{@uROI} is the
#' user-mutable integration target. Pass \code{target = "ROI"} to restore the
#' previous behaviour (basic CSV writes to \code{@ROI}); this emits a
#' deprecation message and will be removed in a future release.
#'
#' The CSV must describe the same compounds, in the same order, as the input
#' annotation (\code{cpdID} match). Use \code{peakPantheR_loadAnnotationParamsCSV}
#' followed by \code{resetAnnotation} for structural changes.
#'
#' @param annotation (peakPantheRAnnotation) An existing annotation object.
#' @param CSVParamPath (str) Path to a CSV file of fit parameters.
#' @param target (str) For basic-layout CSVs, which slot(s) to update. Either
#' \code{"uROI"} (default; updates \code{@uROI} and \code{@FIR}) or
#' \code{"ROI"} (legacy; updates \code{@ROI} only, with a deprecation
#' message). Ignored for advanced-layout CSVs, which always update all three.
#' @param verbose (bool) If TRUE message progress.
#'
#' @return (peakPantheRAnnotation) The input object with updated bound slots.
#' Cache-bearing slots (\code{@dataPoints}, \code{@TIC},
#' \code{@acquisitionTime}) and previous results are left untouched.
#'
#' @export
peakPantheR_updateAnnotationParamsCSV <- function(annotation, CSVParamPath,
                                                    target = c("uROI", "ROI"),
                                                    verbose = TRUE) {
    target <- match.arg(target)
    if (!is(annotation, "peakPantheRAnnotation")) {
        stop('"annotation" must be a peakPantheRAnnotation object')
    }
    if (!file.exists(CSVParamPath)) {
        stop('specified "CSVParamPath" does not exist')
    }

    tmp_csv <- read.csv(CSVParamPath, header = TRUE, sep = ",", quote = "\"",
                        stringsAsFactors = FALSE)

    if ("rtMin" %in% colnames(tmp_csv)) {
        params   <- prepare_basic_target_parameters(tmp_csv)
        advanced <- FALSE
    } else {
        params   <- prepare_advanced_target_parameters(tmp_csv,
                                                        verbose = verbose)
        advanced <- TRUE
    }

    # Compound compatibility: count and cpdID order
    if (nrow(params$targetFeatTable) != nbCompounds(annotation)) {
        stop('CSV compound count (', nrow(params$targetFeatTable),
                ') does not match annotation (', nbCompounds(annotation), ')')
    }
    if (!identical(as.character(params$targetFeatTable$cpdID),
                    as.character(cpdID(annotation)))) {
        stop('CSV "cpdID" values do not match annotation (order-sensitive). ',
                'Use peakPantheR_loadAnnotationParamsCSV + resetAnnotation ',
                'for structural changes.')
    }

    # Overlay bounds. Cache-bearing slots are left untouched.
    if (advanced) {
        annotation@ROI <- params$targetFeatTable[, c("rtMin", "rt", "rtMax",
                                                        "mzMin", "mz", "mzMax")]
        annotation@uROI      <- params$uROI
        annotation@FIR       <- params$FIR
        annotation@uROIExist <- params$uROIExist
    } else if (target == "ROI") {
        message('Note: target = "ROI" is deprecated for basic-layout CSVs. ',
                'Future releases will update @uROI / @FIR by default; ',
                'pass target = "uROI" explicitly to adopt the new behaviour ',
                'or switch to an advanced-layout CSV.')
        annotation@ROI <- params$targetFeatTable[, c("rtMin", "rt", "rtMax",
                                                        "mzMin", "mz", "mzMax")]
    } else {
        new_bounds <- params$targetFeatTable[, c("rtMin", "rt", "rtMax",
                                                    "mzMin", "mz", "mzMax")]
        annotation@uROI      <- new_bounds
        annotation@FIR       <- new_bounds[, c("rtMin", "rtMax",
                                                "mzMin", "mzMax")]
        annotation@uROIExist <- TRUE
    }

    methods::validObject(annotation)

    if (verbose) {
        slots_msg <- if (advanced) 'ROI/uROI/FIR' else
            if (target == "ROI") 'ROI' else 'uROI/FIR'
        message('Annotation ', slots_msg, ' updated from CSV for ',
                nbCompounds(annotation), ' compounds')
    }
    annotation
}
