# IMPORT - Tab panel  -------------------------------------------------------- #

tabPanel("Import Data",
  fluidRow( # top block containing both imports

    ## Init from files
    # get param csv path
    # get a list of file paths
    # cpd/spl metadata (no checks, just length, allowed not to be here)
    # returns init object!

    column(6,
      h4(HTML("Create a new <em>peakPantheRAnnotation</em>"), style="color:#3e648d;font-weight:bold"),
      wellPanel(
        #TODO: all the path boxes
      ) # end left panel (New annotation)
      #TODO: HERE LEFT GO BUTTON
    ),	# end column

    ## Init from previous object
    # load

    column(6,
      h4(HTML("Load a <em>peakPantheRAnnotation</em>"), style="color:#3e648d;font-weight:bold"),
      wellPanel(
        #TODO: only one path box
      ) # end right panel (Load annoation)
      #TODO: HERE RIGHT GO BUTTON
    )   # end column
  )     # end fluidRow

  ## BOTH
  # run validateObj() [duplicate for init from files]
  # do the show on screen!

  #TODO: HERE success band 12 col wide (green) / FAIL (red) [uiOutput()]
  #TODO: HERE a box with the Show(init) results
)
# end IMPORT Tab panel ------------------------------------------------------- #
