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
            # useUROI & useFIR
            column(5, offset=1,
              uiOutput("useUROICheckbox"), # use uROI
              uiOutput("useFIRCheckbox"),  # use FIR
            ), # end column (useUROI, useFIR)

            # ncores cpuslider
            column(4, offset=1,
			  checkboxInput("parallelisation",
                label = p("Parallelisation", style="color:#3e648d;font-weight:bold"),
				value = FALSE
              ),
              uiOutput("cpuSlider")
            )  # end column (cpu slider)
          ),   # end fluidRow run parameters
          tags$hr(),

          fluidRow(
            div(actionButton("goAnnotation", label="Annotate", class="btn btn-primary btn-lg"), align="center")
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
