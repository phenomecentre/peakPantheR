################################################################################
#                                                                              #
#                                                                              #
#       -- peakPantheR: Peak Picking and ANnoTation of High resolution         #
#                              Experiments in R --                             #
#                                                                              #
# Arnaud M. Wolfer                                                             #
# National Phenome Centre                                                      #
# Licensed under GPLv3                                                         #
################################################################################
#
# Copyright (C) {2020}  {Arnaud M. Wolfer}
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



#' peakPantheR: A package for Peak Picking and ANnoTation of High resolution
#' Experiments
#'
#' \pkg{peakPantheR} detects, integrates and reports pre-defined features in
#' mass spectrometry data files. It enables the real time annotation of multiple
#' compounds in a single file, or the parallel annotation of multiple compounds
#' in multiple files.
#'
#' The main functions of \pkg{peakPantheR} are
#' \code{\link{peakPantheR_singleFileSearch}} for realtime annotation, and
#' \code{\link{peakPantheR_parallelAnnotation}} for parallel annotation. The
#' \code{peakPantheRAnnotation} object stores parallel annotation results, while
#' reporting functions help assess the quality of annotation and update
#' fitting parameters. Refer to the vignettes for graphical user interface and
#' command line tutorials.
#'
#' @aliases peakPantheR-package
#'
#' @docType package
#' @name peakPantheR
#'
#' @import foreach
#' @import doParallel
#'
"_PACKAGE"
#> [1] "_PACKAGE"

