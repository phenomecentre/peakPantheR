#' UI data import helper
#'
#' Fully initialise a \code{peakPantheRAnnotation} using the target files path,
#' CSV parameter path and metadata.
#'
#' @param CSVParamPath (str) Path to a CSV file of fit parameters
#' @param spectraPaths (str) character vector of spectra file paths, to set
#' samples to process
#' @param cpdMetadataPath NULL or path to a csv of compound metadata, with
#' compounds as row and metadata as columns
#' @param spectraMetadataPath NULL or path to a csv of sample metadata, with
#' samples as row and metadata as columns
#' @param verbose (bool) If TRUE message progress
#'
#' @return (peakPantheRAnnotation) Object initialised with ROI, uROI and FIR
#' read from the CSV file
#'
initialise_annotation_from_files_UI_helper <- function(CSVParamPath,
                                                    spectraPaths,
                                                    cpdMetadataPath = NULL,
                                                    spectraMetadataPath = NULL,
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
    spectraMetadata <- NULL
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
    if (!is.null(spectraMetadataPath)) {
        if (!file.exists(spectraMetadataPath)) {
                stop('Error: spectraMetadata file does not exist') }
        tmp_spectraMeta <- read.csv(spectraMetadataPath, header=TRUE, sep=",",
                                    quote="\"", stringsAsFactors=FALSE)
        # check nb of rows match the nb of spectra
        if (dim(tmp_spectraMeta)[1] != nbSamples(init_annotation)) {
            message('Warning: spectraMetadata number of rows (',
                    dim(tmp_spectraMeta)[1],
                    ') does not match the number of compounds targeted (',
                    nbSamples(init_annotation), ')')
        } else { spectraMetadata <- tmp_spectraMeta }
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


