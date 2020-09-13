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
            stop('Error: cpdMetadata file does not exist') }
        tmp_cpdMeta <- read.csv(cpdMetadataPath, header=TRUE, sep=",",
                                quote="\"", stringsAsFactors=FALSE)
        # check nb rows match the nb of cpd
        if (dim(tmp_cpdMeta)[1] != nbCompounds(init_annotation)) {
            message('Warning: cpdMetadata number of rows (',dim(tmp_cpdMeta)[1],
                    ') does not match the number of compounds targeted (',
                    nbCompounds(init_annotation), ')')
        } else { cpdMetadata <- tmp_cpdMeta }
    }
    if (!is.null(spectraMetadata)) {
        if (!is.data.frame(spectraMetadata)) {
            stop('Error: spectraMetadata is not a DataFrame') }
        # check nb of rows match the nb of spectra
        if (dim(spectraMetadata)[1] != nbSamples(init_annotation)) {
            message('Warning: spectraMetadata number of rows (',
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
spectraPaths_and_metadata_UI_helper <- function(spectraPaths = NULL,
                                                spectraMetadataPath = NULL) {
    # None are set
    if (is.null(spectraPaths) & is.null(spectraMetadataPath)) {
        stop('Error: spectraPaths and spectraMetadataPath are not set')
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
                stop('Error: spectraMetadata file does not exist') }
        tmp_spectraMeta <- read.csv(spectraMetadataPath, header=TRUE, sep=",",
                                    quote="\"", stringsAsFactors=FALSE)

        # check 'filepath' is in columns
        if (!('filepath' %in% colnames(tmp_spectraMeta))) {
            stop("Error: the column 'filepath' must be present in the ",
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
#' Load a .RData file (check it exists) and that a peakPantheRAnnotation named
#' "annotationObject" is present. Returns the annotation if everything is valid
#'
#' @param annotationPath (str) Path to a RData file containing a
#' peakPantheRAnnotation names `annotationObject`
#'
#' @return (peakPantheRAnnotation) Object loaded from file
#'
load_annotation_from_file_UI_helper <- function(annotationPath) {
    # Check file exist
    if (!file.exists(annotationPath)) {
        stop('Error: annotation file does not exist') }

    # load file content
    load(annotationPath)

    # check it exist and is named correctly
    if (length(ls()[ls() == 'annotationObject']) !=1) {
        stop("Error: annotation file must contain a `peakPantheRAnnotation` ",
            "named 'annotationObject'") }
    # dummy initialisation to pass BiocCheck. In no case would the code reach
    # this section if `annotationObject` wasn't present in the environment
    if (0) {annotationObject <- NULL}

    # check it's a peakPantheRAnnotation
    if (!is(annotationObject, "peakPantheRAnnotation")) {
        stop("Error: the variable loaded is not a `peakPantheRAnnotation`") }

    return(annotationObject)
}


#' UI show annotation helper - list of properties
#'
#' show method specific to the UI, that returns each field in a named list to
#' ease display
#'
#' @param annotation (peakPantherAnnotation) Object to describe
#'
#' @return (list) Named list of annotation properties
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
annotation_showText_UI_helper <- function(annotProp){
    UI_string = list(
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
#' @param cpdNb (int) postion of the feature to extract (1 to nbCpd)
#' @param annotation (peakPantheRAnnotation) Annotation object
#' @param sampleColour (str) NULL or vector colour for each sample
#' @param ... Additional parameters for plotting
#'
#' @return (ggplotObject) Diagnostic multiplot for a feature
annotation_diagnostic_multiplot_UI_helper <- function(cpdNb, annotation,
                                sampleColour=NULL, ...) {
    message(str(cpdNb))
    message(show(annotation))
    # subset annotation to only 1 cpd
    tmp_annotation <- annotation[, cpdNb]

    # diagnostic plots
    tmp_diagPlotList <- annotationDiagnosticPlots(tmp_annotation,
        sampleColour = sampleColour, verbose = FALSE, ...)

    # multiplot
    suppressMessages(suppressWarnings(
        tmp_multiPlot <- annotationDiagnosticMultiplot(tmp_diagPlotList)))

    # something to plot
    if (length(tmp_multiPlot) != 0) {
        return(tmp_multiplot)
    # nothing to plot
    } else {
        # TODO: return NA? NULL?
        return(NULL)
    }
}
# TODO: unittest pumped from `annotationDiagnosticMultiplot`, maybe with a failed plot
