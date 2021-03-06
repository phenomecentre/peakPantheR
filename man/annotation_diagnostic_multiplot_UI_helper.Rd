% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GUI_utils.R
\name{annotation_diagnostic_multiplot_UI_helper}
\alias{annotation_diagnostic_multiplot_UI_helper}
\title{UI diagnostic plot helper - single feature multiplot}
\usage{
annotation_diagnostic_multiplot_UI_helper(
    cpdNb,
    annotation,
    splNum = NULL,
    splColrColumn = NULL,
    ...
)
}
\arguments{
\item{cpdNb}{(int) position of the feature to extract (1 to nbCpd)}

\item{annotation}{(peakPantheRAnnotation) Annotation object}

\item{splNum}{(int) NULL or number of spectra to plot, chosen randomly
from all spectra. If NULL or equal to the total number of spectra, plot all
spectra}

\item{splColrColumn}{(str) NULL, None or a spectraMetadata column for
colouring each sample}

\item{...}{Additional parameters for plotting}
}
\value{
(ggplotObject) Diagnostic multiplot for a feature
}
\description{
Return a ggplot object of a feature diagnostic multiplot
}
\examples{
## Initialise a peakPantheRAnnotation object with 3 samples and 2 targeted
## compounds

# Paths to spectra files
spectraPaths <- c('./path/file1', './path/file2', './path/file3')

# targetFeatTable
targetFeatTable <- data.frame(matrix(vector(), 2, 8, dimnames=list(c(),
                    c('cpdID','cpdName','rtMin','rt','rtMax','mzMin','mz',
                    'mzMax'))), stringsAsFactors=FALSE)
targetFeatTable[1,] <- c('ID-1', 'Cpd 1', 3310., 3344.888, 3390., 522.194778,
                        522.2, 522.205222)
targetFeatTable[2,] <- c('ID-2', 'Cpd 2', 3280., 3385.577, 3440., 496.195038,
                        496.2, 496.204962)
targetFeatTable[,c(3:8)] <- vapply(targetFeatTable[,c(3:8)], as.numeric,
                                    FUN.VALUE=numeric(2))

emptyAnnotation <- peakPantheRAnnotation(spectraPaths=spectraPaths,
                                        targetFeatTable=targetFeatTable)

# Plot of an empty annotation
annotation_diagnostic_multiplot_UI_helper(cpdNb = 2,
                                            annotation = emptyAnnotation,
                                            splNum = NULL,
                                            splColrColumn = NULL)
# Warning: the object has not been annotated, return an empty diagnostic
# plot list
}
