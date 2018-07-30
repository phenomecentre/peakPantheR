# -------------------------------------------------------------------------------
##    dev peakPantheR - detect and integrate pre-defined features in MS files
## -------------------------------------------------------------------------------

library(devtools)  #install.packages('devtools') / devtools::install_github("r-lib/devtools")
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
#devtools::use_testthat()
devtools::document()
devtools::test()
devtools::check()           # destroy inst/doc
devtools::build_vignettes() # recreate inst/doc
devtools::build()
devtools::install()

#devtools::use_vignette("getting-started")
#devtools::use_vignette("real-time-annotation")
#devtools::use_vignette("parallel-annotation")
#devtools::build_vignettes()


library(peakPantheR)

remove.packages('peakPantheR')
#------------------------------------------------
library(peakPantheR)

?peakPantheR_singleFileSearch

# Source all files for testing
setwd('./R')
lapply(list.files(pattern = "[.][rR]$", recursive = TRUE), source)
setwd('..')
# ------------------------------------------------
