# Contributing to `peakPantheR`

We welcome any contribution, be it bug fixes, additional features, improvements to the documentation or the code base, or suggestions.

For questions and to submit issues, please refer to the [Question and Issues section](##Questions-and-Issues).

We recommend discussing contribution and changes in a [Github issue](https://github.com/phenomecentre/peakPantheR/issues/) (particularly substantial ones) before opening a pull request.

Submissions should follow the recommended coding style, as decribed [here](##Coding-Style).

Finally, by participating in this project you agree to abide by the terms of it's [Contributor Code of Conduct](##Contributor-Code-of-Conduct).


## Questions and Issues

In case of general questions on the usage of `peakPantheR` or to raise a bug, a typo or a suggestion, please create an issue in the [project issue tracker](https://github.com/phenomecentre/peakPantheR/issues/). 

For more general questions, the [Bioconductor support forum](https://support.bioconductor.org/) will reach a broader audience (questions can also be tagged with the tag `peakPantheR`). 


## Coding Style

The underlying philosophy of the package is as follows:
* It must be accessible to non-expert users (_e.g._ documented from start to finish, reduce the number of assumptions the user must make).
* It always expects erroneous inputs and deals with it (_i.e._ sanity check on all inputs, explicit error messages).
* Advance options are always available for expert users, but reasonable default values are set.
* It must work on all 3 platforms similarly (_i.e._ Windows, Linux and OSX).
* As many core features as possible are available in the graphical user interface.

`peakPantheR` coding style is governed by the [Bioconductor style guide](https://bioconductor.org/developers/how-to/coding-style/), most notably:
* 4 spaces for indentation, no tabs.
* no lines longer than 80 characters.
* camelCaps variable names.
* functions must be no longer than 50 lines.

`peakPantheR` follows the [GitFlow](https://datasift.github.io/gitflow/IntroducingGitFlow.html) structure:
* `master` mirrors the current `upstream-master` [Bioconductor development release of peakPantheR](http://bioconductor.org/packages/devel/bioc/html/peakPantheR.html).
* the `develop` branch is used for staging of completed features
* `/feature/` branches are used for development of new functionalities or bug fixes.
* Pull Requests are used to merge `feature` branches into `develop` and `develop` into `master`.

To contribute to the project:
* Discuss changes and contributions in the issue tracker before opening a pull request, to align with on-going efforts.
* Develop the new feature or bug fix in a feature branch, as contained as possible to help with code review.
* Create a PR when the development of a feature branch is completed and ready to merge for a future release.
* Continuous integration using Github Action will ensure the PR passes checks on the current and future version of Bioconductor, as well as indicate any change in code coverage.

For a successful PR, it is expected that:
* `R CMD check` is successful (`devtools::check()` passing without Errors, Warnings or Notes).
* `BiocCheck` is successful (`BiocCheck(package_dir, nlines=Inf)` passing without Errors, Warnings or Notes). 
* The documentation and `DESCRIPTION` are up-to-date with the latest version of `Roxygen2` and `devtools`. Be attentive to `devtools::document()` that will reindent part of the documentation with 2 spaces instead of 4, resulting in Errors in `BiocCheck`.
* Code is not platform specific, the full set of features is expected to build, check and work on Linux, Windows and OSX.
* Any change is documented (man pages and/or vignette).
* New additions are covered by representative unittests (correct execution and erroneous execution), and overall test coverage does not decrease.
* the `NEWS` file is updated with a description of the changes.



## Contributor Code of Conduct

As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities.

We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, ethnicity, age, or religion.

Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct.

Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. Project maintainers who do not follow the Code of Conduct may be removed from the project team.

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or contacting one or more of the project maintainers.

This Code of Conduct is adapted from the Contributor Covenant, version 1.0.0, available at https://www.contributor-covenant.org/version/1/0/0/code-of-conduct.html