# Results - Tab panel  ---------------------------------------------------- #

tabPanel("Integration results",

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
        shiny::tabsetPanel( id="diagnosticTabs", type="pills",

        # Overall results - TAB
          tabPanel("Overall results",
            wellPanel(
              fluidRow(
                h3('all the results!')
              ) # end fluidRow
            )   # end wellPanel
          ),    # end tabPanel

        # Per feature - TAB
          tabPanel("Values per Feature",
            wellPanel(
              fluidRow(
                h3('feature valuuues!')
              ) # end fluidRow
            )   # end wellPanel
          ),    # end tabPanel

        # Per sample - TAB
          tabPanel("Values per Sample",
            wellPanel(
              fluidRow(
                h3('sample dataaaaa!')
              ) # end fluidRow
            )   # end wellPanel
          )     # end tabPanel
        )       # end tabsetPanels

        # TODO: 3 tabs; overall, per cpd, per spl

      )   # end Main panel column
    )     # end fluidRow (sidebar + menu)
  )       # end conditional panel
)
# end RESULTS Tab panel --------------------------------------------------- #

