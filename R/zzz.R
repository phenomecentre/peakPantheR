#' @import utils
.onLoad <- function(libname = find.package("peakPantheR"), pkgname = "peakPantheR"){

	# CRAN Note avoidance
	if(getRversion() >= "2.15.1") {
    utils::globalVariables(
			# data.frame column names used in ggplot (cannot use aes_string due to transformations to the column in aes())
			c("x","y")
		)
  invisible()
	}
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("\nThis is peakPantheR version", utils::packageVersion("peakPantheR"), "\n"))
}
