# server.R

# peakPantheR-App
# Based on peakPantheR v1.9.2, R >= 4.1, shiny >= 1.0.5, bslib
# National Phenome Centre
# 05/04/2022
# Licensed under GPLv3
#
# Copyright (C) {2022}  {National Phenome Centre}
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

#require(shiny)
#require(bslib)

# NOTE:
#
# A page is 12 columns wide
#
# v2.0 with more tabs?
# plot + update uROI on screen?
# update + refit in UI?


shinyUI(fluidPage(theme = bslib::bs_theme(bootswatch = "spacelab"), title='peakPantheR',
  navbarPage(title = textOutput("peakPantheR_ver"),
    inverse = FALSE,
    collapsible = TRUE,
    windowTitle = textOutput("peakPantheR_ver"),

    # -- About Tab --
    source(file.path("ui", "ui_about.R"),  local = TRUE)$value,

    # -- Import Tab --
    source(file.path("ui", "ui_import.R"),  local = TRUE)$value,

    # -- Run Tab --
    source(file.path("ui", "ui_run.R"),  local = TRUE)$value,

    # -- Diagnostic Tab --
    source(file.path("ui", "ui_diagnostic.R"),  local = TRUE)$value,

    # -- Results Tab --
    source(file.path("ui", "ui_results.R"),  local = TRUE)$value,

    # -- Export Tab --
    source(file.path("ui", "ui_export.R"),  local = TRUE)$value

  ) # end navbar (at the top)
) # end fluidPage
) # end shinyUI
