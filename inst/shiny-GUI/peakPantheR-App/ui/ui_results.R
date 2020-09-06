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

        # TODO: 3 tabs; overall, per cpd, per spl

        # Results options
        wellPanel(
          fluidRow(
            h5('Lorem Ipsum')
          ) # end fluidRow
        ) # end wellPanel result options
      )   # end Main panel column
    )     # end fluidRow (sidebar + menu)
  )       # end conditional panel
)
# end RESULTS Tab panel --------------------------------------------------- #

