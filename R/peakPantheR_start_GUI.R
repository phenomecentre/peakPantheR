#' peakPantheR Graphical User Interface
#'
#' peakPantheR Graphical User Interface (GUI) implements all the functions for
#' the parallel detection, integration and reporting of pre-defined features in
#' multiple mass spectrometry data files. To exit press \code{ESC} in the
#' command line.
#' 
#' @param browser If TRUE open the graphical user interface in a web browser
#' instead of a R window. Default is TRUE
#'
#' @return None, start GUI. To exit press \code{ESC} in the command line.
#'
#' @examples
#' print("Start graphical interface, press 'ESC' in the command line to stop")
#' # peakPantheR_start_GUI()
#'
#' @import foreach
#' @import doParallel
#' @import shiny
#' @import shinythemes
#' @import shinycssloaders
#'
#' @export
peakPantheR_start_GUI <- function(browser=TRUE) {
    appDir <- system.file("shiny-GUI", "peakPantheR-App", package="peakPantheR")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing ",
            "`peakPantheR`.", call. = FALSE)
    }
    # Either browser or interactive as defined in the IDE/RStudio
    if( browser ){
        shiny::runApp(appDir, launch.browser=TRUE)
    } else {
        shiny::runApp(appDir, launch.browser=getOption("shiny.launch.browser",
                        interactive()))
    }
}