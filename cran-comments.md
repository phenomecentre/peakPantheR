## Resubmission

This a resubmission, in this version I have:

* Removed "in R" from the package title.
* Replaced "MS files" by "mass spectrometry data files" in the description.
* Set the examples to run for all exported functions if the required suggest package is available.
Thanks


## Release summary

This is the first attempted CRAN release of 'peakPantheR'. 

## Test environments

* Win-builder: R-devel, R-release, R-oldrelease
* Local Windows install: R 3.5.1
* Ubuntu 16.04 (on travis-ci): R 3.4.0, R 3.5.0
* Local Ubuntu 18.04 install: R-devel + Bioc-devel
* Local OS X install: R 3.4.3


## R CMD check results

0 errors | 0 warnings | 2 notes

There was 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
	Maintainer: 'Arnaud Wolfer <>'
	
	New submission

	Possibly mis-spelled words in DESCRIPTION:
	spectrometry (8:134)
* checking package dependencies ... NOTE
	Package suggested but not available for checking: 'faahKO'

	* This is the first submission of the package, spectrometry is correctly spelled.
	* R CMD check is successful on R-devel, R-release and R-oldrelease where 'faahKO' is present (NOTE only appear on win-builder R-release, + R-devel)
