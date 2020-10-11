# Results - Tab panel  ---------------------------------------------------- #

tabPanel("View results",

  # Conditional UI based on import successfully initialised
  uiOutput("notAnnotForResUI"),
  conditionalPanel(
    condition = "output.AnnotationDone=='yes'",
    fluidRow(

  ## Sidebar
      column(width=2,
        # Current annotation status
        uiOutput("showAnnotStatusRes")
      ), # end Sidebar column

  ## Main panel
      column(10,
        # Overall title
        h3('Results', style="color:#3e648d;font-weight:bold"),

        # Tabs
        shiny::tabsetPanel( id="ResultTabs", type="pills",

        # Overall results - TAB
          tabPanel("Overall results",
            uiOutput("peakTableControlUI"),
            uiOutput("overallResultsUI")
          ),    # end tabPanel

        # Per feature - TAB
          tabPanel("Results per targeted feature",
            uiOutput("resPerFeatControlUI"),
            uiOutput("featureResultsUI")
          ),    # end tabPanel

        # Per sample - TAB
          tabPanel("Results per sample",
            uiOutput("resPerSplControlUI"),
            uiOutput("sampleResultsUI")
          )     # end tabPanel
        )       # end tabsetPanels
      )   # end Main panel column
    )     # end fluidRow (sidebar + menu)
  )       # end conditional panel
)
# end RESULTS Tab panel --------------------------------------------------- #

