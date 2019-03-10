# ui.R

# peakPantheR-App
# Based on peakPantheR v1.2.3, R >= 3.4.0, shiny >= 1.0.5, shinythemes >= 1.1.1
# National Phenome Centre
# 01/03/2019
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


# v2.0 with more tabs?
# plot + update uROI on screen?
# update + refit in UI?


shinyUI(fluidPage(theme = shinythemes::shinytheme("spacelab"), title='peakPantheR',
  navbarPage(textOutput("peakPantheR_ver"),
    inverse = TRUE,
    collapsible = TRUE,
             
    
    # ABOUT - Tab panel  ------------------------------------------------------ # 
    tabPanel("About",
      includeHTML("data/about.html")
    ), # end ABOUT Tab panel
    
    
    # IMPORT - Tab panel  ------------------------------------------------------ # 
    tabPanel("Import Data",
      # from files or from modified uROI/FIR
      uiOutput("importUI")
    ), # end IMPORT Tab panel
    
    
    # RUN - Tab panel  ------------------------------------------------------ # 
    tabPanel("Run",
      # will need condition based on import successfully initialised
      uiOutput("runUI")
    ), # end RUN Tab panel
    
    # DIAGNOSTIC - Tab panel  ------------------------------------------------------ # 
    tabPanel("Diagnostic: update and plots",
      # update uROI/FIR, output plots and parameters used
      uiOutput("diangosticUI")
    ), # end DIAGNOSTIC Tab panel
    
    
    # EXPORT - Tab panel  ------------------------------------------------------ # 
    tabPanel("Export results",
      uiOutput("exportUI")
    ) # end EXPORT Tab panel
  ) # end navbar
    
) # end fluidPage
) # end shinyUI