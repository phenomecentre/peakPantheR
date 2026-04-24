#' UI data import helper - initialise new annotation from files
#'
#' Fully initialise a \code{peakPantheRAnnotation} using the target files path,
#' CSV parameter path and metadata.
#'
#' @param CSVParamPath (str) Path to a CSV file of fit parameters
#' @param spectraPaths (str) character vector of spectra file paths, to set
#' samples to process
#' @param cpdMetadataPath NULL or path to a csv of compound metadata, with
#' compounds as row and metadata as columns
#' @param spectraMetadata NULL or DataFrame of sample metadata, with
#' samples as row and metadata as columns
#' @param verbose (bool) If TRUE message progress
#'
#' @return (peakPantheRAnnotation) Object initialised with ROI, uROI and FIR
#' read from the CSV file
#'
#' @export
#'
#' @examples
#' ## Input data
#' input_CSV <- data.frame(matrix(nrow=2,ncol=21,dimnames=list(c(),
#'     c('cpdID', 'cpdName',
#'     'X','ROI_rt', 'ROI_mz', 'ROI_rtMin', 'ROI_rtMax','ROI_mzMin','ROI_mzMax',
#'     'X','uROI_rtMin', 'uROI_rtMax', 'uROI_mzMin', 'uROI_mzMax', 'uROI_rt',
#'     'uROI_mz', 'X', 'FIR_rtMin', 'FIR_rtMax', 'FIR_mzMin', 'FIR_mzMax'))))
#' input_CSV[1,]  <- c('ID-1', 'Cpd 1', '|', 1.,  2.,  3.,  4.,  5.,  6.,  '|',
#'                     7.,  8.,  9.,  10., 11., 12., '|', 13., 14., 15., 16.)
#' input_CSV[2,]  <- c('ID-2', 'Cpd 2', '|', 17., 18., 19., 20., 21., 22., '|',
#'                     23., 24., 25., 26., 27., 28., '|', 29., 30., 31., 32.)
#' input_CSV[,-c(1,2,3,10,17)]  <- vapply(input_CSV[,-c(1,2,3,10,17)],
#'                                             as.numeric, FUN.VALUE=numeric(2))
#'
#' input_spectraPaths    <- c('./path/file1', './path/file2', './path/file3')
#'
#' # temporary file location
#' savePath1      <- tempfile(pattern='file', tmpdir=tempdir(), fileext='.csv')
#' # save csv
#' utils::write.csv(input_CSV, file=savePath1, row.names=FALSE)
#'
#' # Load parameters from CSV
#' loadedAnnotation <- initialise_annotation_from_files_UI_helper(savePath1,
#'                                                         input_spectraPaths,
#'                                                         verbose=TRUE)
#' # An object of class peakPantheRAnnotation
#' #   2 compounds in 3 samples.
#' #   updated ROI exist (uROI)
#' #   does not use updated ROI (uROI)
#' #   does not use fallback integration regions (FIR)
#' #   is not annotated
initialise_annotation_from_files_UI_helper <- function(CSVParamPath,
                                                        spectraPaths,
                                                        cpdMetadataPath = NULL,
                                                        spectraMetadata = NULL,
                                                        verbose = TRUE) {
    # Initialise with parameters
    init_annotation <- peakPantheR_loadAnnotationParamsCSV(CSVParamPath,
                                                            verbose = FALSE)
    # Add spectraPaths
    init_annotation <- resetAnnotation(init_annotation,
                                        spectraPaths = spectraPaths,
                                        verbose = FALSE)
    # Load metadata
    cpdMetadata     <- NULL
    if (!is.null(cpdMetadataPath)) {
        if (!file.exists(cpdMetadataPath)) {
            stop('Err','or: cpdMetadata file does not exist') }
        tmp_cpdMeta <- read.csv(cpdMetadataPath, header=TRUE, sep=",",
                                quote="\"", stringsAsFactors=FALSE)
        # check nb rows match the nb of cpd
        if (dim(tmp_cpdMeta)[1] != nbCompounds(init_annotation)) {
            message('War','ning: cpdMetadata number of rows (',
                    dim(tmp_cpdMeta)[1],
                    ') does not match the number of compounds targeted (',
                    nbCompounds(init_annotation), ')')
        } else { cpdMetadata <- tmp_cpdMeta }
    }
    if (!is.null(spectraMetadata)) {
        if (!is.data.frame(spectraMetadata)) {
            stop('Err','or: spectraMetadata is not a DataFrame') }
        # check nb of rows match the nb of spectra
        if (dim(spectraMetadata)[1] != nbSamples(init_annotation)) {
            message('War','ning: spectraMetadata number of rows (',
                    dim(spectraMetadata)[1],
                    ') does not match the number of compounds targeted (',
                    nbSamples(init_annotation), ')')
            spectraMetadata <- NULL
        } # else keep the spectraMetadata value
    }

    # Update annotation with metadata
    init_annotation <- resetAnnotation(init_annotation,
                                        cpdMetadata = cpdMetadata,
                                        spectraMetadata = spectraMetadata,
                                        verbose = FALSE)
    # return success message
    if(verbose){ message(show(init_annotation)) }
    return(init_annotation)
}


#' UI data import helper - prepare file paths and metadata
#'
#' Return spectraPaths and spectraMetadata from a .csv file (if available).
#' If reading from the spectraMetadata file, the spectraPaths are taken from
#' the `filepath` column
#'
#' @param spectraPaths NULL or character vector of spectra file paths, to set
#' samples to process
#' @param spectraMetadataPath NULL or path to a csv of spectra metadata, with
#' spectra as row and metadata as columns. (spectraPaths in column `filepath`)
#'
#' @return spectraPaths (str) and spectraMetadata (DataFrame or NULL) read from
#' the CSV file
#'
#' @export
#'
#' @examples
#' ## Input data
#' # spectraPath
#' input_spectraPaths    <- c('./path/file1', './path/file2', './path/file3')
#'
#' # spectraMetadata
#' input_spectraMetadata <- data.frame(matrix(data=c(input_spectraPaths,
#'                          c('a','b','c')), nrow=3, ncol=2,
#'                          dimnames=list(c(),c('filepath', 'testcol')),
#'                          byrow=FALSE), stringsAsFactors=FALSE)
#'
#' # temporary file location
#' spectraMetaPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.csv')
#' # save csv
#' utils::write.csv(input_spectraMetadata,
#'                  file=spectraMetaPath,
#'                  row.names=FALSE)
#'
#' # load data from CSV
#' spectraPaths_and_metadata_UI_helper(spectraPaths = NULL,
#'                                      spectraMetadataPath = spectraMetaPath)
spectraPaths_and_metadata_UI_helper <- function(spectraPaths = NULL,
                                                spectraMetadataPath = NULL) {
    # None are set
    if (is.null(spectraPaths) & is.null(spectraMetadataPath)) {
        stop('Err','or: spectraPaths and spectraMetadataPath are not set')
    }

    # Only spectraPaths are set
    if (!is.null(spectraPaths) & is.null(spectraMetadataPath)) {
        out_path <- spectraPaths
        out_meta <- NULL
    }

    # spectraMetadataPath is set
    if (!is.null(spectraMetadataPath)) {

        # load file
        if (!file.exists(spectraMetadataPath)) {
                stop('Err','or: spectraMetadata file does not exist') }
        tmp_spectraMeta <- read.csv(spectraMetadataPath, header=TRUE, sep=",",
                                    quote="\"", stringsAsFactors=FALSE)

        # check 'filepath' is in columns
        if (!('filepath' %in% colnames(tmp_spectraMeta))) {
            stop("Err","or: the column 'filepath' must be present in the ",
                "spectraMetadata")
        }

        # extract filepaths
        out_path <- tmp_spectraMeta[,'filepath']
        # remove filepaths from metadata
        out_meta <- tmp_spectraMeta[ , !(names(tmp_spectraMeta) %in%
                                        c('filepath')), drop=FALSE]
    }

    return(list(spectra=out_path, meta=out_meta))
}


#' UI data import helper - check loaded annotation
#'
#' Load a .RData or .RDS file and return the peakPantheRAnnotation it contains.
#' For .RData files with multiple peakPantheRAnnotation objects, either pass
#' `objectName` to pick one or receive a list of candidates (class
#' `peakPantheRAnnotation_candidates`) for the caller to choose from.
#'
#' @param annotationPath (str) Path to a .RData or .RDS file containing one or
#' more peakPantheRAnnotation objects
#' @param objectName (str or NULL) Optional name of the object to return when
#' the file contains several candidates
#'
#' @return (peakPantheRAnnotation) Object loaded from file, or a list of
#' candidate annotations when multiple are found and `objectName` is NULL
#'
#' @export
#'
#' @examples
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 compounds
#'
#' ## Inputs
#' # spectraPaths
#' spectraPaths <- c('./path/file1', './path/file2', './path/file3')
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' annotationObject <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # save annotation to disk
#' annotPath <- tempfile(pattern="file", tmpdir=tempdir(), fileext='.RData')
#' save(annotationObject, file=annotPath, compress=TRUE)
#'
#' # Load annotation
#' load_annotation_from_file_UI_helper(annotationPath = annotPath)
#' # An object of class peakPantheRAnnotation
#' #   2 compounds in 3 samples.
#' #   updated ROI do not exist (uROI)
#' #   does not use updated ROI (uROI)
#' #   does not use fallback integration regions (FIR)
#' #   is not annotated
load_annotation_from_file_UI_helper <- function(annotationPath,
                                                 objectName = NULL) {
    if (!file.exists(annotationPath)) {
        stop('Err','or: annotation file does not exist') }

    ext <- tolower(tools::file_ext(annotationPath))

    if (ext == "rds") {
        # RDS stores exactly one object — load and validate.
        obj <- readRDS(annotationPath)
        if (!is(obj, "peakPantheRAnnotation")) {
            stop("Err","or: the RDS file does not contain a ",
                 "`peakPantheRAnnotation`")
        }
        return(obj)
    }

    # RData / rda: load into a fresh env and scan for peakPantheRAnnotations.
    e <- new.env(parent = emptyenv())
    load(annotationPath, envir = e)
    allNames <- ls(e)
    candidates <- allNames[vapply(allNames, function(n) {
        is(get(n, envir = e), "peakPantheRAnnotation")
    }, logical(1))]

    if (length(candidates) == 0L) {
        stop("Err","or: no `peakPantheRAnnotation` object found in the ",
             "RData file")
    }

    if (!is.null(objectName)) {
        if (!(objectName %in% candidates)) {
            stop("Err","or: requested object '", objectName, "' is not a ",
                 "`peakPantheRAnnotation` in the file")
        }
        return(get(objectName, envir = e))
    }

    if (length(candidates) == 1L) {
        return(get(candidates, envir = e))
    }

    # Multiple candidates: return them all so the caller can pick one.
    lst <- lapply(candidates, function(n) get(n, envir = e))
    names(lst) <- candidates
    structure(lst,
              class = c("peakPantheRAnnotation_candidates", "list"))
}


#' UI show annotation helper - list of properties
#'
#' show method specific to the UI, that returns each field in a named list to
#' ease display
#'
#' @param annotation (peakPantherAnnotation) Object to describe
#'
#' @return (list) Named list of annotation properties
#'
#' @export
#'
#' @examples
#' # Initialise an empty annotation, no uROI, no use of FIR
#' annotInit     <- peakPantheRAnnotation()
#'
#' # return properties
#' annotation_showMethod_UI_helper(annotInit)
#' # $nbCompounds
#' # [1] 0
#' #
#' # #$nbSamples
#' # [1] 0
#' #
#' # #$uROIExist
#' # [1] FALSE
#' #
#' # $useUROI
#' # [1] FALSE
#' #
#' # $useFIR
#' # [1] FALSE
#' #
#' # $isAnnotated
#' # [1] FALSE
annotation_showMethod_UI_helper <- function(annotation){
    properties <- list(nbCompounds = nbCompounds(annotation),
                        nbSamples = nbSamples(annotation),
                        uROIExist = uROIExist(annotation),
                        useUROI = useUROI(annotation),
                        useFIR = useFIR(annotation),
                        isAnnotated = isAnnotated(annotation)
    )

    return(properties)
}


#' UI show annotation helper - UI sidebar string
#'
#' Return a text description of an annotation for UI presentation
#'
#' @param annotProp (list) Named list of annotation properties as
#' created by \code{annotation_showMethod_UI_helper()}
#'
#' @return (str) Textual description of the annotation to show on UI
#'
#' @export
#'
#' @examples
#' # Input
#' properties_default <- list(nbCompounds = 0,
#'                            nbSamples = 0,
#'                            uROIExist = FALSE,
#'                            useUROI = FALSE,
#'                            useFIR = FALSE,
#'                            isAnnotated = FALSE)
#'
#' # Generate description
#' annotation_showText_UI_helper(properties_default)
#' # [[1]]
#' # [1] "Not annotated"
#' #
#' # [[2]]
#' # [1] "0 compounds"
#' #
#' # [[3]]
#' # [1] "0 samples"
#' #
#' # [[4]]
#' # [1] "updated ROI do not exist (uROI)"
#' #
#' # [[5]]
#' # [1] "does not use updated ROI (uROI)"
#' #
#' # [[6]]
#' # [1] "does not use fallback integration regions (FIR)"
annotation_showText_UI_helper <- function(annotProp){
    UI_string <- list(
        if (annotProp$isAnnotated) {'Is annotated'} else {'Not annotated'},
        paste0(annotProp$nbCompounds, ' compounds'),
        paste0(annotProp$nbSamples, ' samples'),
        if (annotProp$uROIExist) {'updated ROI exist (uROI)'
        } else {'updated ROI do not exist (uROI)'},
        if (annotProp$useUROI) {'uses updated ROI (uROI)'
        } else {'does not use updated ROI (uROI)'},
        if (annotProp$useFIR) {'uses fallback integration regions (FIR)'
        } else {'does not use fallback integration regions (FIR)'}
    )

    return(UI_string)
}

#' UI diagnostic plot helper - single feature multiplot
#'
#' Return a ggplot object of a feature diagnostic multiplot
#'
#' @param cpdNb (int) position of the feature to extract (1 to nbCpd)
#' @param annotation (peakPantheRAnnotation) Annotation object
#' @param splNum (int) NULL or number of spectra to plot, chosen randomly
#' from all spectra. If NULL or equal to the total number of spectra, plot all
#' spectra
#' @param splColrColumn (str) NULL, None or a spectraMetadata column for
#' colouring each sample
#' @param ... Additional parameters for plotting
#'
#' @return (ggplotObject) Diagnostic multiplot for a feature
#'
#' @export
#'
#' @examples
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' spectraPaths <- c('./path/file1', './path/file2', './path/file3')
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # Plot of an empty annotation
#' annotation_diagnostic_multiplot_UI_helper(cpdNb = 2,
#'                                          annotation = emptyAnnotation,
#'                                          splNum = NULL,
#'                                          splColrColumn = NULL)
#' # Warning: the object has not been annotated, return an empty diagnostic
#' # plot list
annotation_diagnostic_multiplot_UI_helper <- function(cpdNb, annotation,
                                        splNum=NULL, splColrColumn=NULL, ...) {
    # subset the compound and sample according to inputs
    tmp_annotat <- subset_annot_diag_plot_UI_helper(cpdNb, annotation, splNum)

    # convert sampleColourColumn to a colour scale to use
    sampleColour <- spectra_metadata_colourScheme_UI_helper(tmp_annotat,
                                                            splColrColumn)

    # diagnostic plots
    tmp_diagPlotList <- annotationDiagnosticPlots(tmp_annotat,
        sampleColour = sampleColour, verbose = FALSE, ...)

    # multiplot
    suppressMessages(suppressWarnings(
        tmp_multiPlot <- annotationDiagnosticMultiplot(
            tmp_diagPlotList)))

    # something to plot
    if (length(tmp_multiPlot) != 0) {
        return(tmp_multiPlot)
    # nothing to plot
    } else { return(ggplot2::ggplot() + ggplot2::theme_void()) }
}

#' @title Interactive sibling of annotation_diagnostic_multiplot_UI_helper
#'
#' @description plotly-based sibling of
#' \code{annotation_diagnostic_multiplot_UI_helper()}. Returns a single
#' interactive plotly htmlwidget for the requested compound, or an empty
#' plotly figure when there is nothing to plot.
#'
#' @param cpdNb (int) index of the compound to plot
#' @param annotation (peakPantheRAnnotation)
#' @param splNum (int or NULL) number of samples to display (default all)
#' @param splColrColumn (str or NULL) spectraMetadata column for colouring
#' @param source (str or NULL) plotly event source id (for
#'   \code{plotly::event_data})
#' @param dragmode (str or NULL) plotly dragmode, e.g. "zoom" or "select"
#' @param plotWindow (str) \code{"ROI"} (default) draws the full ROI
#'   extraction envelope on the x-axis; \code{"uROIFIR"} restricts the
#'   x-axis range to the union of the compound's uROI and FIR rt bounds
#'   (EIC data outside is still computed but the initial view zooms in).
#' @param ... passed to \code{annotationDiagnosticPlots()}
#'
#' @return A plotly htmlwidget.
#' @export
annotation_diagnostic_multiplot_UI_helper_interactive <- function(cpdNb,
    annotation, splNum = NULL, splColrColumn = NULL,
    splMode = c("sequential", "random"),
    splFilterCol = NULL, splFilterLevels = NULL,
    source = NULL, dragmode = NULL,
    plotWindow = c("ROI", "uROIFIR"), ...) {
    splMode <- match.arg(splMode)
    plotWindow <- match.arg(plotWindow)
    tmp_annotat <- subset_annot_diag_plot_UI_helper(cpdNb, annotation, splNum,
        splMode = splMode,
        splFilterCol = splFilterCol, splFilterLevels = splFilterLevels)
    sampleColour <- spectra_metadata_colourScheme_UI_helper(tmp_annotat,
        splColrColumn)
    # EIC data is always extracted over the full @ROI envelope so that
    # shrinking the x-axis to uROI/FIR is a pure view change (no refit).
    tmp_diagPlotList <- annotationDiagnosticPlots(tmp_annotat,
        sampleColour = sampleColour, verbose = FALSE,
        plotWindow = "ROI", ...)

    # pull the selected compound's  uROI / FIR rt bounds
    # to overlay as shaded rectangle
    windows <- rt_windows_for_cpd(annotation, cpdNb)

    suppressMessages(suppressWarnings(
        tmp_multiPlot <- annotationDiagnosticMultiplot_interactive(
            tmp_diagPlotList,
            windowsList = list(windows),
            source = source, dragmode = dragmode)))

    plt <- NULL
    if (length(tmp_multiPlot) != 0) {
        for (p in tmp_multiPlot) {
            if (!is.null(p)) { plt <- p; break }
        }
    }
    if (is.null(plt)) { return(plotly::plotly_empty()) }
    if (identical(plotWindow, "uROIFIR")) {
        bounds <- rt_union_bounds(windows[c("uROI", "FIR")])
        if (!is.null(bounds)) {
            if (is.null(plt$x$layout)) plt$x$layout <- list()
            if (is.null(plt$x$layout$xaxis)) plt$x$layout$xaxis <- list()
            plt$x$layout$xaxis$range <- bounds
            plt$x$layout$xaxis$autorange <- FALSE
            # Rescale y to the EIC traces visible in the zoomed window;
            # otherwise plotly keeps the full-ROI y-range and tall peaks
            # outside the window flatten the signal inside it.
            yMax <- max_eic_int_in_window(tmp_annotat, bounds)
            if (is.finite(yMax) && yMax > 0) {
                if (is.null(plt$x$layout$yaxis)) plt$x$layout$yaxis <- list()
                plt$x$layout$yaxis$range <- c(0, yMax * 1.05)
                plt$x$layout$yaxis$autorange <- FALSE
            }
        }
    }
    return(plt)
}

# Compute the union rt span across a named list of window entries
# (each NULL or list(rtMin=, rtMax=)). Returns c(min, max) or NULL.
rt_union_bounds <- function(windows) {
    mins <- c(); maxs <- c()
    for (w in windows) {
        if (is.null(w)) next
        rtMin <- suppressWarnings(as.numeric(w$rtMin))
        rtMax <- suppressWarnings(as.numeric(w$rtMax))
        if (length(rtMin) != 1 || length(rtMax) != 1) next
        if (!is.finite(rtMin) || !is.finite(rtMax) || rtMin >= rtMax) next
        mins <- c(mins, rtMin); maxs <- c(maxs, rtMax)
    }
    if (length(mins) == 0) return(NULL)
    c(min(mins), max(maxs))
}

# Max EIC intensity within [rtMin, rtMax] across the annotation's
# cached @dataPoints. Intensities are summed per-scan to mirror
# generateIonChromatogram(aggregationFunction = "sum"), which builds the
# EIC traces drawn on the diagnostic plot.
max_eic_int_in_window <- function(annotation, bounds) {
    dp <- tryCatch(dataPoints(annotation), error = function(e) NULL)
    if (is.null(dp) || length(dp) == 0) return(NA_real_)
    pts <- unlist(dp, recursive = FALSE)
    best <- -Inf
    for (df in pts) {
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) next
        if (!all(c("rt", "int") %in% colnames(df))) next
        sub <- df[df$rt >= bounds[1] & df$rt <= bounds[2], , drop = FALSE]
        if (nrow(sub) == 0) next
        summed <- tapply(sub$int, sub$rt, sum, na.rm = TRUE)
        m <- suppressWarnings(max(summed, na.rm = TRUE))
        if (is.finite(m) && m > best) best <- m
    }
    if (!is.finite(best)) return(NA_real_)
    best
}

#' @title Interactive RT correction diagnostic plot
#'
#' @description Builds a plotly scatter plot of retention time
#' vs retention time deviation, coloured by reference status,
#' with the fitted correction curve overlaid.
#'
#' @param correctedRtTable (data.frame) Output of
#'   \code{peakPantheR_applyRTCorrection} with columns
#'   \code{cpdID}, \code{cpdName}, \code{rt},
#'   \code{rt_dev_sec}, \code{isReference},
#'   \code{correctedRt}, \code{predictedRtDrift}.
#' @param source (character) plotly event source id
#'
#' @return A plotly htmlwidget
#' @export
rtCorrection_interactive_plot_UI_helper <- function(
    correctedRtTable, source = "rtCorrPlot") {
    refPalette <- c(
        "Reference set"         = "#2ca02c",
        "Reference set outlier" = "#d62728",
        "External set"          = "#1f77b4")
    ct <- correctedRtTable
    ctSorted <- ct[order(ct$rt), ]
    plt <- plotly::plot_ly(source = source)
    for (grp in intersect(names(refPalette),
                          unique(ct$isReference))) {
        sub <- ct[ct$isReference == grp, , drop = FALSE]
        plt <- plotly::add_trace(plt, data = sub,
            x = ~rt, y = ~rt_dev_sec,
            type = "scatter", mode = "markers",
            name = grp,
            marker = list(color = refPalette[[grp]],
                          size = 10),
            text = ~paste0(
                cpdName,
                "<br>rt: ", round(rt, 2),
                "<br>dev: ", round(rt_dev_sec, 2),
                "<br>corrected: ",
                round(correctedRt, 2)),
            hoverinfo = "text")
    }
    plt <- plotly::add_trace(plt, data = ctSorted,
        x = ~rt, y = ~predictedRtDrift,
        type = "scatter", mode = "lines",
        name = "Fitted correction",
        line = list(color = "black", width = 2),
        hoverinfo = "none", showlegend = TRUE)
    plt <- plotly::layout(plt,
        xaxis = list(title = "Retention time (sec)"),
        yaxis = list(
            title = "Retention time deviation (sec)"),
        legend = list(orientation = "h",
            x = 0.5, xanchor = "center", y = 1.1))
    plt
}

# Extract rt-window bounds for a single compound from the annotation's
# ROI / uROI / FIR slots. Returns a named list; entries are NULL when the
# slot does not carry valid bounds for that compound.
rt_windows_for_cpd <- function(annotation, cpdNb) {
    safeRow <- function(df, col, row) {
        if (is.null(df) || nrow(df) < row) return(NA_real_)
        if (!col %in% colnames(df)) return(NA_real_)
        suppressWarnings(as.numeric(df[row, col]))
    }
    roi  <- tryCatch(ROI(annotation),  error = function(e) NULL)
    uroi <- tryCatch(uROI(annotation), error = function(e) NULL)
    fir  <- tryCatch(FIR(annotation),  error = function(e) NULL)
    pack <- function(df) {
        rtMin <- safeRow(df, "rtMin", cpdNb)
        rtMax <- safeRow(df, "rtMax", cpdNb)
        if (!is.finite(rtMin) || !is.finite(rtMax)) return(NULL)
        rt <- safeRow(df, "rt", cpdNb)
        out <- list(rtMin = rtMin, rtMax = rtMax)
        if (is.finite(rt)) out$rt <- rt
        out
    }
    list(ROI = pack(roi), uROI = pack(uroi), FIR = pack(fir))
}

#' @title Apply an rt window to an annotation's uROI / FIR row
#'
#' @description Writes \code{rtMin}/\code{rtMax} for compound \code{cpdNb}
#' to the requested slot(s) of a \code{peakPantheRAnnotation}. Used by the
#' diagnostic-plot drag-select UI to update retention-time
#' windows. Editing \code{"uROI"} flips \code{@useUROI} (and
#' \code{@uROIExist}) to \code{TRUE}; editing \code{"FIR"} flips
#' \code{@useFIR} to \code{TRUE}, so the next refit picks up the new bounds.
#'
#' @param annotation (peakPantheRAnnotation)
#' @param cpdNb (int) 1-based compound index
#' @param rt (numeric length 2) c(rtMin, rtMax), strictly increasing and
#' finite. Values are clamped to the compound's ROI bounds so that
#' uROI/FIR never extend beyond the cached data envelope.
#' @param targets (character) subset of \code{c("uROI", "FIR")}; default
#' both. Invalid or empty targets trigger an error.
#'
#' @return The updated \code{peakPantheRAnnotation}.
#' @export
apply_rt_window_to_annotation <- function(annotation, cpdNb, rt,
    targets = c("uROI", "FIR")) {
    if (!is.numeric(rt) || length(rt) != 2L || any(!is.finite(rt))) {
        stop("`rt` must be a length-2 finite numeric vector")
    }
    if (rt[1] >= rt[2]) {
        stop("`rt[1]` (rtMin) must be strictly less than `rt[2]` (rtMax)")
    }
    cpdNb <- as.integer(cpdNb)
    nCpd <- nbCompounds(annotation)
    if (length(cpdNb) != 1L || is.na(cpdNb) || cpdNb < 1L || cpdNb > nCpd) {
        stop("`cpdNb` must be a single integer in [1, ", nCpd, "]")
    }
    if (length(targets) < 1L || !all(targets %in% c("uROI", "FIR"))) {
        stop("`targets` must be a non-empty subset of c('uROI', 'FIR')")
    }

    roi <- ROI(annotation)[cpdNb, , drop = FALSE]
    rt[1] <- max(rt[1], roi$rtMin)
    rt[2] <- min(rt[2], roi$rtMax)
    if (rt[1] >= rt[2]) return(annotation)

    if ("uROI" %in% targets) {
        annotation@uROI[cpdNb, "rtMin"] <- rt[1]
        annotation@uROI[cpdNb, "rtMax"] <- rt[2]
        # Editing the uROI slot implies the caller wants it used on the next
        # fit. Without this the refit path reads @ROI (unchanged) and the
        # edit is silently ignored; uROIExist must also be TRUE for
        # validObject() to accept useUROI = TRUE.
        annotation@uROIExist <- TRUE
        annotation@useUROI <- TRUE
    }
    if ("FIR" %in% targets) {
        annotation@FIR[cpdNb, "rtMin"] <- rt[1]
        annotation@FIR[cpdNb, "rtMax"] <- rt[2]
        annotation@useFIR <- TRUE
    }
    annotation
}

subset_annot_diag_plot_UI_helper <- function(cpdNb, annotation, splNum=NULL,
    splMode = c("sequential", "random"),
    splFilterCol = NULL, splFilterLevels = NULL) {
    splMode <- match.arg(splMode)

    # restrict the pool of samples to those whose value of
    # splFilterCol is in splFilterLevels.
    nSplAll <- nbSamples(annotation)
    eligible <- seq_len(nSplAll)
    if (!is.null(splFilterCol) && nzchar(splFilterCol) &&
        splFilterCol != "None" && !is.null(splFilterLevels)) {
        meta <- spectraMetadata(annotation)
        if (splFilterCol %in% colnames(meta)) {
            vals <- as.character(meta[[splFilterCol]])
            eligible <- which(vals %in% as.character(splFilterLevels))
        }
    }
    nEligible <- length(eligible)
    if (nEligible == 0) { eligible <- seq_len(nSplAll); nEligible <- nSplAll }

    if (is.null(splNum)) splNum <- nEligible
    if (splNum < 1) {
        splNum <- 1
        warning("Negative number of samples to show, 1 spectra will be shown!")
    }
    if (splNum > nEligible) {
        splNum <- nEligible
        warning("More samples to show than available,",
                " all spectra will be shown!")
    }

    if (splMode == "random") {
        currentSplChoice <- sort(sample(eligible, size = splNum,
                                        replace = FALSE))
    } else {
        currentSplChoice <- eligible[round(seq(1, nEligible,
                                               length.out = splNum))]
    }

    subsetAnnotation <- annotation[currentSplChoice, cpdNb]
    return(subsetAnnotation)
}

#' UI diagnostic table - fit summary
#'
#' Return a table of fit statistic (ratio of peaks found, ratio of peaks filled,
#' ppm error, RT deviation)
#'
#' @param annot (peakPantheRAnnotation) Annotation object
#'
#' @return (data.frame) Fit statistics
#'
#' @export
#'
#' @examples
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' spectraPaths <- c('./path/file1', './path/file2', './path/file3')
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # statistics of an empty annotation
#' annotation_fit_summary_UI_helper(emptyAnnotation)
#' #              Ratio peaks found (%) Ratio peaks filled (%) ppm error
#' # ID-1 - Cpd 1                    NA                      0       NaN
#' # ID-2 - Cpd 2                    NA                      0       NaN
#' #               RT deviation (s)
#' # ID-1 - Cpd 1               NaN
#' # ID-2 - Cpd 2               NaN
annotation_fit_summary_UI_helper <- function(annot) {
    summary <- data.frame(matrix(ncol = 0, nrow = nbCompounds(annot)),
                                    stringsAsFactors = FALSE)
    rownames(summary) <- paste(cpdID(annot), "-", cpdName(annot))

    # used FIR (found = not filled, filled from is_filled)
    if (annot@useFIR) {
        summary$ratio_peaks_found  <- 1 - (colSums(
            annotationTable(annot, column = "is_filled"))/nbSamples(annot))
        summary$ratio_peaks_filled <- (colSums(
            annotationTable(annot, column = "is_filled"))/nbSamples(annot))

    # didn't use FIR (found from found, None are filled)
    } else {
        summary$ratio_peaks_found  <- colSums(
            annotationTable(annot, column = "found"))/nbSamples(annot)
        summary$ratio_peaks_filled <- 0
    }

    summary$ppm_error <-
        colMeans(annotationTable(annot, column = "ppm_error"), na.rm = TRUE)
    summary$rt_dev_sec <-
        colMeans(annotationTable(annot, column = "rt_dev_sec"), na.rm = TRUE)

    # nicer format for output
    summary$ratio_peaks_found  <- round(summary$ratio_peaks_found*100,1)
    summary$ratio_peaks_filled <- round(summary$ratio_peaks_filled*100,1)
    colnames(summary) <- c('Ratio peaks found (%)','Ratio peaks filled (%)',
                            'ppm error', 'RT deviation (s)')

    return(summary)
}


#' UI export helper - spectra path and metadata
#'
#' Return a table with spectra as rows and filepath and all spectra metadata
#' columns
#'
#' @param annot (peakPantheRAnnotation) Annotation object
#'
#' @return (data.frame) Spectra paths and metadata
#'
#' @export
#'
#' @examples
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' spectraPaths <- c('./path/file1', './path/file2', './path/file3')
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # spectraMetada of an empty annotation
#' outputAnnotationSpectraMetadata_UI_helper(emptyAnnotation)
#' #       filepath
#' # 1 ./path/file1
#' # 2 ./path/file2
#' # 3 ./path/file3
outputAnnotationSpectraMetadata_UI_helper <- function(annot) {
    # collect the data
    tmp_filepath        <- filepath(annot)
    tmp_spectraMetadata <- spectraMetadata(annot)

    # generate table
    outSpecMeta <- data.frame(filepath = tmp_filepath)
    outSpecMeta <- cbind(outSpecMeta, tmp_spectraMetadata)

    return(outSpecMeta)
}


#' UI export helper - feature metadata
#'
#' Return a table with features as rows and all feature metadata as columns
#'
#' @param annot (peakPantheRAnnotation) Annotation object
#'
#' @return (data.frame) Features metadata
#'
#' @export
#'
#' @examples
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' spectraPaths <- c('./path/file1', './path/file2', './path/file3')
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # featureMetadata of an empty annotation
#' outputAnnotationFeatureMetadata_UI_helper(emptyAnnotation)
#' # data frame with 0 columns and 2 rows
outputAnnotationFeatureMetadata_UI_helper <- function(annot) {
    # collect the data
    outCpdMetadata <- cpdMetadata(annot)

    return(outCpdMetadata)
}


#' UI export plot helper - sample colour
#'
#' Return a vector of spectra colours based on a metadata column
#'
#' @param annot (peakPantheRAnnotation) Annotation object
#' @param splColrColumn (str) NULL, None or a spectraMetadata column for
#' colouring each sample
#'
#' @return (character) Vector of colours
#'
#' @export
#'
#' @examples
#' ## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
#' ## compounds
#'
#' # Paths to spectra files
#' spectraPaths <- c('./path/file1', './path/file2', './path/file3')
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
#'                         496.2, 496.204962)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(2))
#'
#' emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
#'                                         targetFeatTable=targetFeatTable)
#'
#' # colour scheme with no spectraMetadata
#' outputAnnotationFeatureMetadata_UI_helper(emptyAnnotation)
#' # NULL
spectra_metadata_colourScheme_UI_helper <- function(annot,splColrColumn=NULL) {
    # convert sampleColourColumn to a colour scale to use
    if (is.null(splColrColumn)) {
        sampleColour <- NULL
    } else if (splColrColumn=='None') { # separate for case not set yet
        sampleColour <- NULL
    } else if (!(splColrColumn %in% colnames(
        peakPantheR::spectraMetadata(annot)))) {
        sampleColour <- NULL
    } else {
        # extract metadata column of interest and unique colours needed
        tmp_meta <- peakPantheR::spectraMetadata(annot)[[splColrColumn]]
        uniq_val <- unique(tmp_meta)
        n_colr   <- length(uniq_val)

        # known colors (repeat if more groups than colours available)
        colorVect <- c("blue", "red", "green", "orange", "purple", "seagreen",
                        "darkturquoise", "violetred", "saddlebrown", "black")
        if (length(colorVect) < n_colr) {
            colorVect <- rep(colorVect, ceiling(n_colr/length(colorVect)))
        }
        # iteratively replace values by colours
        for (i in seq_len(n_colr)) {
            tmp_meta <- replace(tmp_meta, tmp_meta==uniq_val[i], colorVect[i])
        }
        sampleColour <- tmp_meta
    }

    return(sampleColour)
} # TODO: unittest