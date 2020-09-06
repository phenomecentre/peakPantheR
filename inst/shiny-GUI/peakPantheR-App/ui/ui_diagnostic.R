# DIAGNOSTIC - Tab panel  ---------------------------------------------------- #

tabPanel("Diagnostic: update and plots",

  # Conditional UI based on import successfully initialised
  uiOutput("notAnnotForDiagUI"),
  conditionalPanel(
    condition = "output.AnnotationDone=='yes'",
    fluidRow(

  ## Sidebar
      column(width=2,
        # Current annotation status
        uiOutput("showAnnotStatusDiag")
      ), # end Sidebar column

  ## Main panel
      column(10,
        # Overall title
        h3('Diagnostic', style="color:#3e648d;font-weight:bold"),

        # Diagnostic options
        wellPanel(
          fluidRow(
            h5('Lorem Ipsum')
          ) # end fluidRow
        ) # end wellPanel diagnostic options
      )   # end Main panel column
    )     # end fluidRow (sidebar + menu)
  )       # end conditional panel
)
# end DIAGNOSTIC Tab panel --------------------------------------------------- #

