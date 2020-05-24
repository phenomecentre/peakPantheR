# RUN - Tab panel  ----------------------------------------------------------- #

tabPanel("Run",
  h3('Run calculation', style="color:#3e648d;font-weight:bold"),

  # Conditional UI based on import successfully initialised
  uiOutput("noImportForFitUI"),
  conditionalPanel(
    condition = "output.importDone=='yes'",

      h2(HTML('Imported something, can run the analysis'), style='color:green;font-weight:bold')
  ) # end conditional panel
)
# end RUN Tab panel ---------------------------------------------------------- #
