## Input init function
#
# get param csv path
#
# filepath
#   get a list of path from the UI
#
# metadata (no checks, just length, allowed not to be here)
#   cpd metadata
#   spl metadata
#
# 1 fun that take all the path as input, triggers loadParamCSV, add the files to
# the object, return success message
# loader sorted!



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

    # Load metadata
    cpdMetadata     <- NULL
    spectraMetadata <- NULL
    if( !is.null(cpdMetadataPath) ){
        cpdMetadata <- read.csv(cpdMetadataPath, header=TRUE, sep=",",
                                quote="\"", stringsAsFactors=FALSE)
    }
    if( !is.null(spectraMetadataPath) ){
        spectraMetadata <- read.csv(spectraMetadataPath, header=TRUE, sep=",",
                                    quote="\"", stringsAsFactors=FALSE)
    }

    # Update annotation with spectraPaths and metadata
    init_annotation <- resetAnnotation(init_annotation,
                                    spectraPaths = spectraPaths,
                                    cpdMetadata = cpdMetadata,
                                    spectraMetadata = spectraMetadata,
                                    verbose = FALSE)

    # return success message
    if(verbose){
        message(show(init_annotation))
    }

    return(init_annotation)
}

