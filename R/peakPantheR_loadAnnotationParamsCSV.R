## Init a new peakPantheRAnnotation object after loading ROI, uROI and FIR
## parameters from CSV
#' @title Load fit parameters from CSV
#' 
#' @description Initialise a new \code{peakPantheRAnnotation} object after
#' loading ROI, uROI and FIR parameters from CSV. \code{spectraPaths},
#' \code{spectraMetadata} or \code{cpdMetadata} are not initialised and will
#' need to be filled before annotation. \code{useUROI} and \code{useFIR} are set
#' to \code{FALSE} and will need to be set accordingly. \code{uROIExist} is
#' established depending on the uROI columns present in the CSV and will be set
#' to \code{TRUE} only if no \code{NA} are present
#' 
#' @param CSVParamPath (str) Path to a CSV file of fit parameters as saved by
#' \code{outputAnnotationDiagnostic}
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
#' # temporary file location
#' savePath1      <- tempfile(pattern='file', tmpdir=tempdir(), fileext='.csv')
#' # save csv
#' utils::write.csv(input_CSV, file=savePath1, row.names=FALSE)
#'
#' # Load parameters from CSV
#' loadedAnnotation <- peakPantheR_loadAnnotationParamsCSV(savePath1,
#'                                                         verbose=TRUE)
#' # uROIExist set to TRUE
#' # New peakPantheRAnnotation object initialised for 2 compounds
#' # An object of class peakPantheRAnnotation
#' #  2 compounds in 0 samples.
#' #   updated ROI exist (uROI)
#' #   does not use updated ROI (uROI)
#' #   does not use fallback integration regions (FIR)
#' #   is not annotated
peakPantheR_loadAnnotationParamsCSV <- function(CSVParamPath, verbose = TRUE) {
    # Check and load csv
    tmp_csv <- loadAnnotationParamsCSV_readCSV(CSVParamPath)

    # Prepare ROI, uROI and FIR
    tmp_targetFeatTable <- tmp_csv[, c("cpdID", "cpdName", "ROI_rtMin",
        "ROI_rt", "ROI_rtMax", "ROI_mzMin", "ROI_mz", "ROI_mzMax")]
    colnames(tmp_targetFeatTable) <- c("cpdID", "cpdName", "rtMin", "rt",
        "rtMax", "mzMin", "mz", "mzMax")
    tmp_targetFeatTable[, c(3:8)] <- vapply(tmp_targetFeatTable[, c(3:8)],
        as.numeric, FUN.VALUE = numeric(nrow(tmp_targetFeatTable)))
    tmp_uROI <- tmp_csv[, c("uROI_rtMin", "uROI_rt", "uROI_rtMax", "uROI_mzMin",
        "uROI_mz", "uROI_mzMax")]
    colnames(tmp_uROI) <- c("rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")
    tmp_uROI[, seq(1, 6)] <- vapply(tmp_uROI[, seq(1, 6)], as.numeric,
                                FUN.VALUE = numeric(nrow(tmp_targetFeatTable)))
    tmp_FIR <- tmp_csv[, c("FIR_rtMin", "FIR_rtMax", "FIR_mzMin", "FIR_mzMax")]
    colnames(tmp_FIR) <- c("rtMin", "rtMax", "mzMin", "mzMax")
    tmp_FIR[, seq(1, 4)] <- vapply(tmp_FIR[, seq(1, 4)], as.numeric,
                                FUN.VALUE = numeric(nrow(tmp_targetFeatTable)))

    # establish if uROIExist
    tmp_uROIExist <- !any(is.na(tmp_uROI[, c("rtMin", "rtMax", "mzMin",
                                            "mzMax")]))
    if (tmp_uROIExist) {
        if (verbose) { message("uROIExist set to TRUE") }
    } else {
        if (verbose) { message("NA in uROI, uROIExist is set to FALSE") }}

    # check loaded data ROI
    if (!(all(tmp_targetFeatTable$rtMin <= tmp_targetFeatTable$rtMax) &
            all(tmp_targetFeatTable$mzMin <= tmp_targetFeatTable$mzMax))) {
        stop('Check ROI values: \"rtMin\" < \"rtMax\" and \"mzMin\" ',
                    '< \"mzMax\"') }
    # uROI
    if (tmp_uROIExist) {
        if (!(all(tmp_uROI$rtMin <= tmp_uROI$rtMax) &
                all(tmp_uROI$mzMin <= tmp_uROI$mzMax))) {
            stop('Check uROI values: \"rtMin\" < \"rtMax\" and ',
                        '\"mzMin\" < \"mzMax\"') }}

    # Initialise new object
    tmp_annotation <- peakPantheRAnnotation(
                                        targetFeatTable = tmp_targetFeatTable,
                                        uROI = tmp_uROI, FIR = tmp_FIR,
                                        uROIExist = tmp_uROIExist)
    if (verbose) { message("New peakPantheRAnnotation object initialised for ",
                            nbCompounds(tmp_annotation), " compounds") }
    return(tmp_annotation)
}
# check and read csv
loadAnnotationParamsCSV_readCSV <- function(CSVParamPath) {

    # Check file exist
    if (!file.exists(CSVParamPath)) {
        stop("specified \"CSVParamPath\" does not exist")
    }

    # Read file
    tmp_csv <- read.csv(CSVParamPath, header = TRUE, sep = ",", quote = "\"",
                        stringsAsFactors = FALSE)
    # Check columns
    expected_col <- c("cpdID", "cpdName", "ROI_rt", "ROI_mz", "ROI_rtMin",
        "ROI_rtMax", "ROI_mzMin", "ROI_mzMax", "uROI_rtMin", "uROI_rtMax",
        "uROI_mzMin", "uROI_mzMax", "uROI_rt", "uROI_mz", "FIR_rtMin",
        "FIR_rtMax", "FIR_mzMin", "FIR_mzMax")
    if (!(all(expected_col %in% colnames(tmp_csv)))) {
        stop(paste0('Columns in \"CSVParamPath\" must be: \"cpdID\", ',
    '\"cpdName\", \"ROI_rt\", \"ROI_mz\", \"ROI_rtMin\", \"ROI_rtMax\", ',
    '\"ROI_mzMin\", \"ROI_mzMax\", \"uROI_rtMin\", \"uROI_rtMax\", ',
    '\"uROI_mzMin\", \"uROI_mzMax\", \"uROI_rt\", \"uROI_mz\", \"FIR_rtMin\", ',
    '\"FIR_rtMax\", \"FIR_mzMin\", \"FIR_mzMax\"'))
    }

    return(tmp_csv)
}