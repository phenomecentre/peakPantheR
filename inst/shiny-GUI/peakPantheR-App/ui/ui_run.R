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

        # Message already annotated
        uiOutput("alreadyAnnotatedUI"),

        # Run options and trigger (full menu)
        wellPanel(
          fluidRow(

            # TODO: use uROI (only if exist!)
            # TODO: use FIR (only if exist!)
            # useUROI & useFIR
            column(5, offset=1,
              h4('Checkbox: use uROI'),
              h4('Checkbox: use FIR')
            ), # end column (useUROI, useFIR)

            # ncores cpuslider
            column(4, offset=1,
			  checkboxInput("parallelisation",
                label = p("Parallelisation", style="font-weight:bold"),
				value = FALSE
              ),
              uiOutput("cpuSlider_fit")
            )  # end column (cpu slider)
          ),   # end fluidRow run parameters
          tags$hr(),

          fluidRow(
            div(actionButton("runTrigger", label="Annotate", class="btn btn-primary btn-lg"), align="center")
          )  # end fluidRow run button
        ),   # end wellPanel (run options + trigger)

        # Progress bar
        fluidRow(
          uiOutput("progressBarUI")
        ),

        # Success (green) / fail (red) row
        fluidRow(
          uiOutput("successAnnotationUI") # error/success message
          # TODO: CHECK success/failure UI
        ) # end fluiRow (success panel)
      )   # end Main panel column
    )     # end fluidRow (sidebar + menu)
  )       # end conditional panel
)
# end RUN Tab panel ---------------------------------------------------------- #
