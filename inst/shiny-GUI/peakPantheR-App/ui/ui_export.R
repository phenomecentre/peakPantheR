# EXPORT - Tab panel  -------------------------------------------------------- #

tabPanel("Export results",

# Conditional UI based on import successfully initialised
  uiOutput("noImportForExportUI"),
  conditionalPanel(
    condition = "output.importDone=='yes'",
    fluidRow(

  ## Sidebar
      column(width=2,
        # Current annotation status
        uiOutput("showAnnotStatusExp")
      ), # end Sidebar column

  ## Main panel
      column(10,
        h3('Export Data', style="color:#3e648d;font-weight:bold"),

        # Row export inputs
        fluidRow(
          column(4,
            wellPanel(
              h4("Save annotation as .rData:"),
              span("Create a .RData file containing the annotation results as a peakPantheRAnnotation named ",em("annotationObject")," for future importation and analysis.", style="color:#666666"),
              uiOutput("dlRData")
            )  # end wellPanel
          ),   # end column
          column(8,
            wellPanel(
              fluidRow(
                column(5,
                  h4("Save input parameters as .CSV:"),
                  span("Save the ",
                           em("fit parameters")," defining the targeted features, the ",
                           em("files to process")," defining file paths and spectra metadata, as well as the",
                           em("targeted features metadata"),
                           ". These CSV files enable the reproduction of the current annotation, but does not contain annotation results", style="color:#666666"),
                ), # end column
                column(7,
                  uiOutput("dlCSVs")
                ) # end column
              ) # end fluidRow
            )   # end wellPanel
          ),    # end column

        ),  # end fluidRow Export inputs
        tags$hr(),
        br(),

        h3("Export Annotation Results and Diagnostic Plots",style="color:#3e648d;font-weight:bold"),
        span("Automated summarisation step (diagnostic and results), only possible if Shiny is running on a local machine.", style="color:#666666"), # See ",em("Code")," to run from the command line.

        # Row export results
        fluidRow(
          column(6,
            h4("Save Diagnostic Plots:"),
            span("Plots and save to disk a diagnotic plot for each targeted feature. Each spectra can be coloured based on one of the metadata column previously defined.", style="color:#666666")
          ), # end column
          column(6,
            h4("Save Annotation Values:"),
            span("Saves the annotation results to disk in multiple ",em(".csv")," files containing the compound metadata, spectra metadata and a table for each annotation (fit) property", em("(samples as rows and compounds as columns)"), ".", style="color:#666666")
          ) # end column
        ),  # end fluidRow Export results Text

        # The result export panel only exist if annotation took place
        column(12,
          uiOutput("exportDiagResUI"),
        tags$hr()
        )   # end column export results

      )     # end Main panel column
    )       # end fluidRow (sidebar + menu)
  )         # end conditional panel
)
# end EXPORT Tab panel  ------------------------------------------------------ #
