#' @title Extract and plot a EIC from a raw data file
#'
#' @description Simple plot of an Extracted Ion Chromatogram (EIC) from a raw 
#' data file and a provided mz and rt window. Return ggplot plot object.
#' 
#' @param spectraPath (str) Path to the raw data file to read (uses 
#' `MSnbase::readMSData()`)
#' @param rt (numeric(2) or two-column matrix) the lower and upper retention
#' time range from which the data should be extracted. If a matrix is passed,
#' each row corresponds to a different window. If not provided, the full
#' retention time range will be extracted.
#' @param mz (numeric(2) or two-column matrix) the lower and upper mass range
#' from which the data should be extracted. If a matrix is passed, each row
#' corresponds to a different window. If not provided, the full mass range will
#' be extracted.
#' @param valuesOnly (str) If 'Raw' only load the file and return a table of raw
#' extracted values (exported version of `extractSignalRawData()`). If `EIC`
#' return a table of EIC data point. Else return the EIC plot (default).
#' @param centroided (bool) Indicate to `MSnbase::readMSData()` whether the spectra
#' file is centroided or not (default to 'TRUE').
#' @param msLevel (int) The MS level at which the data should be extracted 
#' (default to MS level 1).
#' @param verbose (bool) Output proress information or not.
#' 
#' @return Grob (ggplot object) of the EIC plot, if `valuesOnly='Raw'` returns 
#' a data.frame of raw datapoints with as columns 'rt', 'mz' and 'int'. If 
#' `valuesOnly='EIC'` returns a data.frame of EIC datapoints with as columns 
#' 'rt' and 'int'.
#'
#' @export
#' 
#' @examples
#' ## Use a file form the faahKO package and plot an EIC of interest
#' library(faahKO)
#' spectraPath <- system.file('cdf/KO/ko15.CDF',package='faahKO')
#' peakPantheR_quickEIC(spectraPath,
#'                      rt = c(3290., 3410.),
#'                      mz = c(522.194778, 522.205222))
peakPantheR_quickEIC <- function(spectraPath, rt, mz, valuesOnly='Plot',
                            centroided=TRUE, msLevel=1L, verbose=TRUE) {
    # Initialise file with MSnbase
    rawSpec <- MSnbase::readMSData(spectraPath,
                                    centroided=centroided,
                                    mode='onDisk')
    # Extract signals
    dataPoints  <- data.frame(extractSignalRawData(rawSpec,
                                                    rt = rt, 
                                                    mz = mz, 
                                                    msLevel = msLevel,
                                                    verbose=verbose))
    # Raw valuesOnly
    if (valuesOnly == 'Raw') { return(dataPoints) }

    # Compute EIC
    EIC <- generateIonChromatogram(dataPoints, aggregationFunction = 'sum')

    # EIC valuesOnly
    if (valuesOnly == 'EIC') {return(EIC)}

    # Plot
    p_spec <- ggplot2::ggplot(NULL, ggplot2::aes(x),
                                environment = environment()) +
        ggplot2::theme_bw() + ggplot2::xlab("Retention Time (sec)") +
        ggplot2::ylab("Intensity") +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
        ggplot2::geom_line(data = EIC, ggplot2::aes(x = rt, y = int))
    return(p_spec)
}