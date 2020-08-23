# RUN - Tab panel  ----------------------------------------------------------- #

tabPanel("Run",
  # Conditional UI based on import successfully initialised
  uiOutput("noImportForFitUI"),
  conditionalPanel(
    condition = "output.importDone=='yes'",
    fluidRow(

  ## Sidebar
      column(width=2,
        # Current annotation status
        uiOutput("showAnnotStatus")
      ), # end Sidebar column

  ## Main panel
      column(10,
        # Overall title
        h3('Run calculation', style="color:#3e648d;font-weight:bold"),

        # Menu
        # Message already annotated
        uiOutput("alreadyAnnotatedUI"),

        # Fit menu
        uiOutput("fitUI"),

        # Progress bar
        uiOutput("progressBarUI")

        # TODO: lay the message, menu and progress bar
      ) # end Main panel column
    ),  # end fluidRow (sidebar + menu)
    fluidRow(
      uiOutput("successAnnotationUI")
      # TODO: a success/failure message
    )   # end fluiRow (success panel)
  )     # end conditional panel
)
# end RUN Tab panel ---------------------------------------------------------- #
