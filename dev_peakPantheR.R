## -------------------------------------------------------------------------------------------
##    dev peakPantheR - detect and integrate pre-defined features in MS files using XCMS3
## -------------------------------------------------------------------------------------------

library(devtools)  #install.packages('devtools')
library(roxygen2)  #install.packages('roxygen2')

#----------------
#working_dir = './'
#setwd(working_dir)

#devtools::create('peakPantheR')
#----------------

## Dependencies
package_dir = getwd()
setwd(package_dir)

#setRepositories(ind=1:2)
devtools::document()
devtools::check()
devtools::build()
devtools::install()


remove.packages('peakPantheR')
#------------------------------------------------
library(peakPantheR)

# Source all files for testing
setwd('./R')
lapply(list.files(pattern = "[.][rR]$", recursive = TRUE), source)
setwd('..')
