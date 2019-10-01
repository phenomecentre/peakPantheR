

# peakPantheR <img src="man/figures/peakPantheR-logo.png" align="right" />

[![Build
Status](https://travis-ci.org/phenomecentre/peakPantheR.svg?branch=master)](https://travis-ci.org/phenomecentre/peakPantheR)
[![codecov](https://codecov.io/gh/phenomecentre/peakPantheR/branch/master/graph/badge.svg)](https://codecov.io/gh/phenomecentre/peakPantheR/branch/master)
[![DOI](https://zenodo.org/badge/116680214.svg)](https://zenodo.org/badge/latestdoi/116680214)


Package for *Peak Picking and ANnoTation of High resolution Experiments in R*, implemented in `R` and `Shiny`

## Overview

`peakPantheR` is an R/Bioconductor package that implements functions to detect, integrate and report pre-defined features in MS files (*e.g. compounds, fragments, adducts, …*). It is designed for:

  - **Real time** feature detection and integration (see [Real Time Annotation](https://phenomecentre.github.io/peakPantheR.github.io/articles/real-time-annotation.html))
      - process `multiple` compounds in `one` file at a time
  - **Post-acquisition** feature detection, integration and reporting (see [Parallel Annotation](https://phenomecentre.github.io/peakPantheR.github.io/articles/parallel-annotation.html))
      - process `multiple` compounds in `multiple` files in `parallel`, store results in a `single` object

`peakPantheR` can process LC/MS data files in *NetCDF*, *mzML*/*mzXML* and *mzData* format as data import is achieved using Bioconductor’s [`mzR`](https://bioconductor.org/packages/3.10/mzR) package.

## Installation

To install [peakPantheR](https://bioconductor.org/packages/release/bioc/html/peakPantheR.html):
``` r
install.packages("BiocManager")
BiocManager::install("peakPantheR")
```

To install the development version from GitHub:
``` r
BiocManager::install("phenomecentre/peakPantheR")
```

## Usage

Both real time and parallel compound integration require a common set of information:

  - Path(s) to `netCDF` / `mzML` MS file(s)
  - An expected region of interest (`RT` / `m/z` window) for each compound.

## Vignettes

An overview of the package and detailed information on usage are available in the following vignettes:

  - [Getting Started with peakPantheR](https://phenomecentre.github.io/peakPantheR.github.io/articles/getting-started.html)
  - [Real Time Annotation](https://phenomecentre.github.io/peakPantheR.github.io/articles/real-time-annotation.html)
  - [Parallel Annotation](https://phenomecentre.github.io/peakPantheR.github.io/articles/parallel-annotation.html)

## Copyright

`peakPantheR` is licensed under the [GPLv3](http://choosealicense.com/licenses/gpl-3.0/)

As a summary, the GPLv3 license requires attribution, inclusion of copyright and license information, disclosure of source code and changes. Derivative work must be available under the same terms.

© National Phenome Centre (2019)
