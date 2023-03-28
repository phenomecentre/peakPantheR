# server.R

# peakPantheR-App
# Based on peakPantheR v1.3.0, R >= 4.0, shiny >= 1.0.5, bslib
# National Phenome Centre
# 18/04/2022
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


# increase upload size to 500MB
options(shiny.maxRequestSize=500*1024^2)
# define max number of parallel cores
maxCores <- parallel::detectCores()
# known curve fitting methods
knownCurveModel <- c("skewedGaussian", "emgGaussian")


shinyServer( function(input, output, session){

  # -- General Initialisation --

  # close app if tab is shut
  session$onSessionEnded(stopApp)

  # get peakPantheR version
  output$peakPantheR_ver <- renderText({ paste("peakPantheR v",packageVersion('peakPantheR'),sep="") })

  # get max number of cpu cores
  output$cpu <- renderText({ maxCores })
  
  
  # -- Import Tab --
  source(file.path("server", "server_import.R"),  local = TRUE)$value

  # -- Run Tab --
  source(file.path("server", "server_run.R"),  local = TRUE)$value

  # -- Diagnostic Tab --
  source(file.path("server", "server_diagnostic.R"),  local = TRUE)$value

  # -- Results Tab --
  source(file.path("server", "server_results.R"),  local = TRUE)$value

  # -- Export Tab --
  source(file.path("server", "server_export.R"),  local = TRUE)$value


  # force update of conditional statement triggers and "generate Input" buttons even if not yet open
  outputOptions(output, "importDone", suspendWhenHidden=FALSE)
  outputOptions(output, "AnnotationDone", suspendWhenHidden=FALSE)
})
