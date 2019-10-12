# ui.R

# peakPantheR-App
# Based on peakPantheR v0.99.2, R >= 3.6.0, shiny >= 1.0.5, shinythemes >= 1.1.1
# National Phenome Centre
# 25/09/2019
# Licensed under GPLv3	
#
# Copyright (C) {2019}  {National Phenome Centre}
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
#require(shinythemes)

# NOTE:
#
# A page is 12 columns wide
#
# uiOutput('nameUI') with 'nameUI' defined in server side, allow for ui blocks to be dynamically changed
# see http://shiny.rstudio.com/gallery/dynamic-ui.html
#
# v2.0 with more tabs?
# plot + update uROI on screen?
# update + refit in UI?


shinyUI(fluidPage(theme = shinythemes::shinytheme("spacelab"), title='peakPantheR',
  navbarPage(textOutput("peakPantheR_ver"),
    inverse = TRUE,
    collapsible = TRUE,

    # -- About Tab --
    source(file.path("ui", "ui_about.R"),  local = TRUE)$value,

    # -- Import Tab --
    source(file.path("ui", "ui_import.R"),  local = TRUE)$value,

    # -- Run Tab --
    source(file.path("ui", "ui_run.R"),  local = TRUE)$value,

    # -- Diagnostic Tab --
    source(file.path("ui", "ui_diagnostic.R"),  local = TRUE)$value,

    # -- Export Tab --
    source(file.path("ui", "ui_export.R"),  local = TRUE)$value

  ) # end navbar (at the top)
) # end fluidPage
) # end shinyUI