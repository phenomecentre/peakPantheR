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
#' the basic layout (\code{cpdID, cpdName, rtMin, rt, rtMax, mzMin, mz, mzMax})
#' which only updates ROI, or the advanced layout (\code{ROI_*, uROI_*, FIR_*}
#' columns) which also updates uROI, FIR and \code{uROIExist}.
#'
#' The CSV must describe the same compounds, in the same order, as the input
#' annotation (\code{cpdID} match). Use \code{peakPantheR_loadAnnotationParamsCSV}
#' followed by \code{resetAnnotation} for structural changes.
#'
#' @param annotation (peakPantheRAnnotation) An existing annotation object.
#' @param CSVParamPath (str) Path to a CSV file of fit parameters.
#' @param verbose (bool) If TRUE message progress.
#'
#' @return (peakPantheRAnnotation) The input object with updated ROI
#' (and uROI, FIR, uROIExist when the advanced format is supplied). All other
#' slots, including \code{@dataPoints}, are left untouched.
#'
#' @export
peakPantheR_updateAnnotationParamsCSV <- function(annotation, CSVParamPath,
                                                    verbose = TRUE) {
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
    annotation@ROI <- params$targetFeatTable[, c("rtMin", "rt", "rtMax",
                                                    "mzMin", "mz", "mzMax")]
    if (advanced) {
        annotation@uROI      <- params$uROI
        annotation@FIR       <- params$FIR
        annotation@uROIExist <- params$uROIExist
    }

    methods::validObject(annotation)

    if (verbose) {
        message('Annotation ROI',
                if (advanced) '/uROI/FIR' else '',
                ' updated from CSV for ', nbCompounds(annotation),
                ' compounds')
    }
    annotation
}
