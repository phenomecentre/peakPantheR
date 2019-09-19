#' @title Save to disk each ROI EIC and mean IS RT
#' @description Using reference samples (\code{referenceSpectraFiles}), save
#' (to \code{saveFolder}) each ROI EIC (\code{ROI}) and reports the mean apex RT
#' for all IS (\code{IS_ROI}) across samples
#' @param referenceSpectraFiles (str) A character vector of paths to the
#' reference spectra files
#' @param saveFolder (str) Path to the folder where EICs and IS mean RT
#' (\code{IS_mean_RT.csv}) will be saved
#' @param ROI (data.frame) NULL or a data.frame of Regions Of Interest (ROI)
#' with compounds as row and ROI parameters as columns: \code{rtMin} (float in
#' seconds), \code{rt} (float in seconds, or \emph{NA}), \code{rtMax} (float in
#' seconds), \code{mzMin} (float), \code{mz} (float or \emph{NA}), \code{mzMax}
#' (float) (if NULL, ROI EICs are not saved)
#' @param IS_ROI (data.frame) NULL or a data.frame of IS ROI with IS as row and
#' ROI parameters as columns: \code{rtMin} (float in seconds), \code{rt} (float
#' in seconds, or \emph{NA}), \code{rtMax} (float in seconds), \code{mzMin}
#' (float), \code{mz} (float or \emph{NA}), \code{mzMax} (float)  (if NULL IS
#' mean RT is not calculated and saved in \code{IS_mean_RT.csv})
#' @param sampleColour (str) NULL or vector colour for each sample
#' @param ncores (int) Number of cores to use to integrate IS in parallel
#' @param saveISPlots (bool) If TRUE save a diagnostic plot for each IS to
#' \code{saveFolder/IS_search} compound
#' @param verbose (bool) If TRUE message progress
#' @return None
#' @export
#' @examples
#' if(requireNamespace('faahKO')){
#' ## Initialise a peakPantheRAnnotation object with 2 samples and 1 targeted
#' ## compound
#'
#' # Paths to spectra files
#' library(faahKO)
#' spectraPaths <- c(system.file('cdf/KO/ko15.CDF', package = 'faahKO'),
#'                     system.file('cdf/KO/ko16.CDF', package = 'faahKO'))
#'
#' # targetFeatTable
#' targetFeatTable <- data.frame(matrix(vector(), 1, 8, dimnames=list(c(),
#'                     c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
#'                     'mzMax'))), stringsAsFactors=FALSE)
#' targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
#'                         522.2, 522.205222)
#' targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
#'                                     FUN.VALUE=numeric(1))
#'
#' # input
#' refSpecFiles  <- spectraPaths
#' input_ROI     <- targetFeatTable
#' input_IS_ROI  <- targetFeatTable
#' sampleColour  <- c('blue', 'red')
#'
#' # temporary saveFolder
#' saveFolder1   <- tempdir()
#'
#' # Calculate ROI statiscs
#' peakPantheR_ROIStatistics(refSpecFiles, saveFolder1, ROI=input_ROI,
#'                             IS_ROI=input_IS_ROI, sampleColour=sampleColour,
#'                             ncores=0, saveISPlots=TRUE, verbose=TRUE)
#' }
peakPantheR_ROIStatistics   <- function(referenceSpectraFiles, saveFolder,
                                        ROI = NULL, IS_ROI = NULL,
                                        sampleColour = NULL, ncores = 0,
                                        saveISPlots = TRUE, verbose = TRUE) {
    # Check and process input parameters
    resInit <- ROIStatistics_init_checks(referenceSpectraFiles, saveFolder, ROI,
                                        IS_ROI, sampleColour, verbose)
    saveFolder <- resInit$saveFolder
    sampleColour <- resInit$sampleColour
    saveEICsROI <- resInit$saveEICsROI
    calculateMeanISRT <- resInit$calculateMeanISRT
    
    # save EICs ROI
    if (saveEICsROI) {
        ROIStatistics_saveEICsROI(referenceSpectraFiles, saveFolder, ROI,
                                    sampleColour, verbose)
    }

    # calculate mean IS for each RT
    if (calculateMeanISRT) {
        ROIStatistics_calculateMeanISRT(referenceSpectraFiles, saveFolder,
                            IS_ROI, saveISPlots, ncores, sampleColour, verbose)
    }
}


# -----------------------------------------------------------------------------
# peakPantheR_ROIStatistics helper functions

# Check and process input parameters
ROIStatistics_init_checks <- function(referenceSpectraFiles, saveFolder, ROI,
                            IS_ROI, sampleColour, verbose){
    # check input reference spectra
    if (typeof(referenceSpectraFiles) != "character") {
        stop('Check input, \"referenceSpectraFiles\" must be a vector ',
                    'of spectra paths')
    }
    nbSpectra <- length(referenceSpectraFiles)

    # save folder
    if ((typeof(saveFolder) != "character") | (length(saveFolder) != 1)) {
        stop("Check input, \"saveFolder\" must be a path") }
    saveFolder <- normalizePath(saveFolder, mustWork = FALSE)
    dir.create(saveFolder, recursive = TRUE, showWarnings = FALSE)
    # sampleColour
    if (!is.null(sampleColour)) {
        if ((typeof(sampleColour) != "character") |
            (length(sampleColour) != nbSpectra)) {
            if (verbose) {
                message('Check input, \"sampleColour\" must be a vector',
                    ' of colour of same length as \"referenceSpectraFile\": ',
                    'default colour used instead') }
            sampleColour <- rep("black", nbSpectra) }
    } else { sampleColour <- rep("black", nbSpectra) }

    # Check if can save EICs for each ROI and calculate mean IS RT
    res_check <- ROIStatistics_check_ROI_meanIS(ROI, IS_ROI, verbose)
    saveEICsROI <- res_check$save; calculateMeanISRT <- res_check$calculate

    # summary
    if (verbose) {
        # save ROI EICs
        if (saveEICsROI) {
            message(paste("- EICs for each ROI windows will be saved to:",
                saveFolder))
            message(paste("    ", dim(ROI)[1], " ROI in ",
                length(referenceSpectraFiles), " reference samples", sep = ""))
        } else { message("- EICs of ROI windows will not be saved") }
        # calculate IS mean RT
        if (calculateMeanISRT) {
            message(paste("- Mean RT of IS across reference samples will be",
                            "saved to:", saveFolder))
            message(paste("    ", dim(IS_ROI)[1], " IS in ",
            length(referenceSpectraFiles), " reference samples", sep = ""))
        } else { message("- Mean RT of IS will not be calculated") }
    }
    return(list(saveFolder=saveFolder, sampleColour=sampleColour,
                saveEICsROI=saveEICsROI, calculateMeanISRT=calculateMeanISRT))
}
# Check saveEICs for each ROI and calculate mean IS RT
ROIStatistics_check_ROI_meanIS <- function(ROI, IS_ROI, verbose) {
    # save EICs for each ROI if ROI is properly defined
    saveEICsROI <- FALSE
    if (!is.null(ROI)) {
        # ROI is a data.frame
        if (is.data.frame(ROI)) {
            # ROI data.frame columns
            if (all(c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz",
                "mzMax") %in% colnames(ROI))) {
                saveEICsROI <- TRUE
            } else {
                if (verbose) {
                    message('ROI columns must contain \"cpdID\", \"cpdName\", ',
                            '\"rtMin\", \"rt\", \"rtMax\", \"mzMin\", \"mz\" ',
                            'and \"mzMax\", EICs of ROI windows will not be ',
                            'saved') }}
        } else {
            if (verbose) {
                message('ROI is not a data.frame, EICs of ROI windows ',
                                'will not be saved') }}
    } else {
        if (verbose) {
            message("No ROI provided, EICs of ROI windows will not be saved") }}

    # calculate mean IS RT if IS_ROI is properly defined
    calculateMeanISRT <- FALSE
    if (!is.null(IS_ROI)) {
        # ROI is a data.frame
        if (is.data.frame(IS_ROI)) {
            # ROI data.frame columns
            if (all(c("cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz",
                "mzMax") %in% colnames(IS_ROI))) {
                calculateMeanISRT <- TRUE
            } else {
                if (verbose) {
                    message('IS_ROI columns must contain \"cpdID\", ',
                            '\"cpdName\", \"rtMin\", \"rt\", \"rtMax\", ',
                            '\"mzMin\", \"mz\" and \"mzMax\", mean RT of IS ',
                            'will not be calculated') }}
        } else {
            if (verbose) {
                message('IS_ROI is not a data.frame, mean RT of IS will',
                                ' not be calculated') }}
    } else {
        if (verbose) {
            message("No IS_ROI provided, mean RT of IS will not be calculated")
        }}

    return(list(save=saveEICsROI, calculate=calculateMeanISRT))
}
# Save EIC for each ROI
ROIStatistics_saveEICsROI <- function(referenceSpectraFiles, saveFolder, ROI,
                                        sampleColour, verbose) {
    if (verbose) { message("\n-- Saving EICs for each ROI --") }
    nbROI <- dim(ROI)[1]
    # extract all ROIs in all files
    all_ROIs <- lapply(referenceSpectraFiles, function(x) {
        # if file doesn't exist, pass
        singleSpectraDataPath <- normalizePath(x, mustWork = FALSE)
        if (!file.exists(singleSpectraDataPath)) {
            message("File \"", singleSpectraDataPath, "\" does not exist")
            return(NULL) }
        raw_data <- MSnbase::readMSData(singleSpectraDataPath,
                                        centroided = TRUE,
                                        mode = "onDisk")
        ROIsDataPoint <- extractSignalRawData(raw_data,
                                            rt = ROI[, c("rtMin", "rtMax")],
                                            mz = ROI[, c("mzMin", "mzMax")],
                                            verbose = verbose)
        return(ROIsDataPoint) })
    # remove failures
    specToKeep <- vapply(all_ROIs, function(x) {!is.null(x)},
                        FUN.VALUE = logical(1))
    all_ROIs <- all_ROIs[specToKeep]
    sampleColour <- sampleColour[specToKeep]
    # generate and save plots
    for (i in seq(nbROI)) {
        tmp_ROI_datapoints <- unlist(lapply(all_ROIs, function(x, y) {x[y]},
                                        y = i), recursive = FALSE)
        tmp_EIC_plot <- peakPantheR_plotEICFit(
                            ROIDataPointSampleList = tmp_ROI_datapoints,
                            curveFitSampleList = NULL, rtMin = NULL,
                            rtMax = NULL, sampling = 250,
                            sampleColour = sampleColour, verbose = FALSE)
        # add box limits and apex
        tmp_EIC_plot <- tmp_EIC_plot + ggplot2::geom_vline(
            ggplot2::aes(xintercept = c(ROI[i, "rtMin"], ROI[i, "rtMax"])),
            colour = "darkgrey", linetype = "dashed")
        tmp_EIC_plot <- tmp_EIC_plot + ggplot2::geom_vline(
            ggplot2::aes(xintercept = ROI[i, "rt"]), colour = "red")
        tmp_EIC_plot <- tmp_EIC_plot +
            ggplot2::ggtitle(paste(ROI[i, "cpdID"], "-", ROI[i, "cpdName"],
                "|", ROI[i, "rtMin"], "-", ROI[i, "rtMax"], "s |",
                ROI[i, "mzMin"], "-", ROI[i, "mzMax"], "m/z"))
        # save
        saveFileName <- paste(ROI[i, "cpdID"], ".png", sep = "")
        ggplot2::ggsave(file=saveFileName, plot=tmp_EIC_plot, device="png",
            path = saveFolder, dpi=100, width=25, height=25, units="cm",
            limitsize = FALSE) }
    if (verbose) { message(paste(nbROI, "ROIs saved to", saveFolder)) }
}
# Calculate mean RT for each IS
ROIStatistics_calculateMeanISRT <- function(referenceSpectraFiles, saveFolder,
                            IS_ROI, saveISPlots, ncores, sampleColour, verbose){
    if (verbose) {
        message("\n-- Calculating mean RT for each IS --")
    }
    IS_annotation <- peakPantheRAnnotation(
        spectraPaths = referenceSpectraFiles,
        targetFeatTable = IS_ROI)
    IS_annotation_results <- peakPantheR_parallelAnnotation(IS_annotation,
        ncores = ncores, verbose = verbose)
    IS_annotation <- IS_annotation_results$annotation

    # save IS fit diagnostic plots
    if (saveISPlots) {
        outputAnnotationDiagnostic(IS_annotation,
            saveFolder = file.path(saveFolder, "IS_search"),
            savePlots = TRUE, sampleColour=sampleColour, verbose = verbose,
            ncores = ncores)
    }

    # calculate statistics
    mean_IS_rt <- data.frame(
        colMeans(annotationTable(IS_annotation, column = "rt"), na.rm=TRUE))
    colnames(mean_IS_rt) <- "mean_rt"
    # save to disk
    path_meanRT <- file.path(saveFolder, "IS_mean_RT.csv")
    utils::write.csv(mean_IS_rt, file = path_meanRT, row.names = TRUE,
                    fileEncoding = "UTF-8")
    if (verbose) {
        message("IS mean RT saved at ", path_meanRT)
    }
}
