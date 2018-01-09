
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/phenomecentre/peakPantheR.svg?branch=master)](https://travis-ci.org/phenomecentre/peakPantheR)

peakPantheR
===========

Package for *Peak Picking and ANnoTation of High resolution Experiments in R*, implemented in `R` and `Shiny`

Overview
--------

`peakPantheR` implements functions to detect, integrate and report pre-defined features in MS files using XCMS3. It is designed for:

-   Real time feature detection and integration (*realTimeAnnotation*)
-   Post-acquisition feature detection, integration and reporting (*parallelAnnotation*)

Installation
------------

Install the development version of the package directly from GitHub with:

``` r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("phenomecentre/peakPantheR")
```

If the dependencies `xcms` and `MSnbase` are not successfully installed, `Bioconductor` must be added to the default repositories with:

``` r
setRepositories(ind=1:2)
```

Usage
-----

**Real time compound integration concept:**

-   process **multiple** compounds in **one** file at a time
-   load list of expected *RT* / *m/z* regions of interest (ROI)
-   detect features in each ROI and keep the highest intensity one
-   determine peak statistics for each feature
-   return a table with all detected compounds for that file (*row: compound, col: statistic*)

**Post-acquisition compound integration concept:**

-   process **multiple** compounds in **multiple** files in **parallel**, store results in a **single** object
-   load list of expected *RT* / *m/z* ROI and list of files to process
-   initialise output object with expected ROI and file paths
-   first pass (*without peak filling*):
    -   for each file, detect features in each ROI and keep highest intensity
    -   determine peak statistics for each feature
    -   store results + EIC for each ROI
-   visual inspection of first pass results, update ROI:
    -   plot all EICs, peak apex *RT* / *m/z* & peak width evolution
    -   correct ROI (remove interfering feature, correct *RT* shift)
    -   define fallback integration regions (FIR) if no feature is detected (median *RT* / *m/z* start and end of found features)
-   initialise new output object, with updated regions of interest (uROI) and fallback integration regions (FIR)
-   second pass (*with peak filling*):
    -   for each file, detect features in each uROI and keep highest intensity
    -   determine peak statistics for each feature
    -   integrate FIR when no peaks found
    -   store results + EIC for each uROI
-   summary statistics:
    -   plot EICs, apex and peakwidth evolution
    -   compare first and second pass
-   return result object and/or table (*row: file, col: compound*)

Development, Code Structure, Documentation, Unit testing & Debugging
--------------------------------------------------------------------

Required:

-   [R](https://cran.rstudio.com/) (v3.4.3), install both 32bit and 64bit in `C:\R\R-3.4.3`
-   [RTools](https://cran.r-project.org/bin/windows/Rtools/) (v3.4), install both 32bit and 64bit, even if only using x64. *Install in `C:\RTools`*

To install and modify `peakPantheR` (see also `dev_peakPantheR.R`):

``` r
# Load devtools and roxygen2 for documentation
if(!require("devtools")) install.packages("devtools")
if(!require("roxygen2")) install.packages("roxygen2")

# Navigate to package dir
package_dir = 'path/peakPantheR'
setwd(package_dir)

# Generate documentation
devtools::document()

# Check the validity of the package (CRAN rules, should run unit tests)
devtools::check()

# Build package as .tar.gz for installation
devtools::build()

# Install and load the package on current machine
devtools::install()


# Remove package
remove.packages('peakPantheR')
#------------------------------------------------

# Load package
library(peakPantheR)
```

### Code Structure

Main functions are usually saved independently in `R\myFunction.R`, similar smaller functions can be grouped in a single file.

When calling functions from external packages, the package call must be explicit such as `pkg::fun()` for the function `fun()` in the package `pkg`.

### Documentation

Each function [documentation](http://r-pkgs.had.co.nz/man.html#man-functions) is generated using `roxygen`, which will parse [roxygen comments](http://r-pkgs.had.co.nz/man.html#roxygen-comments), add the function to the `Namespace` to make it available oustide the package (see `@export`), run the examples (see `@example`) and set links to other functions' documentation (see `@family`). To regenerate the documentation, use `devtools::document()`

### Unit testing

Unit testing is achieved using the `testthat` package. By placing all tests in `inst/tests/` and one file in `test/` running them all, unit tests will be run during checks `devtools::check()`. In short, if the function is is `fun1.R` the corresponding tests are in `test_fun1.R`, and `run_tests.R` source the required functions and find all matching tests (see [a short introduction here](https://www.r-bloggers.com/unit-testing-with-r/amp/)). For more details, see [here](https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf), [here](http://r-pkgs.had.co.nz/tests.html) and [here (pg. 35-48)](http://www.is.uni-freiburg.de/ressourcen/algorithm-design-and-software-engineering-oeffentlicher-zugriff/11_softwaretesting.pdf).

> Tests need to run relatively quickly - aim for under a minute. Place skip\_on\_cran() at the beginning of long-running tests that shouldn't be run on CRAN - they'll still be run locally, but not on CRAN.

### Debugging

Interactive debugging is available in *RStudio*, more details can be found [here (pg.20-21)](http://www.is.uni-freiburg.de/ressourcen/algorithm-design-and-software-engineering-oeffentlicher-zugriff/11_softwaretesting.pdf).

Copyright
---------

`peakPantheR` is licensed under the [GPLv3](http://choosealicense.com/licenses/gpl-3.0/)

As a summary, the GPLv3 license requires attribution, inclusion of copyright and license information, disclosure of source code and changes. Derivative work must be available under the same terms.

© National Phenome Centre (2018)
