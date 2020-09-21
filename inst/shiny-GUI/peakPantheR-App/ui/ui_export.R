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
            # TODO: export .RData
          ), # end column
          column(4,
            # TODO: export text .csv(s)
          ), # end column
          column(4,
            # TODO: export .csv(s) boxes
          ) # end column
        ),  # end fluidRow Export inputs
        tags$hr(),
        br(),

        h3("Export Annotation Results and Diagnostic Plots",style="color:#3e648d;font-weight:bold"),
        helpText("Automated summarisation step, only possible if Shiny is running on a local machine.", style="color:#666666"), # See ",em("Code")," to run from the command line.
        # TODO: fix the text for annotation result output

        # Row export results
        fluidRow(
          column(6,
            h4("Save Diagnostic Plots:"),
            helpText("Stores all P-values calculated in multiple ",em(".csv")," file. ( default",em("summary_pvalue-all.csv"),")", style="color:#666666")
          # TODO: change Export Diagnostic plots text
          ), # end column
          column(6,
            h4("Save Annotation Values:"),
            helpText("Plots and save to disk all variables with a P-value inferior to a given cut-off.", style="color:#666666")
          # TODO: change Export Annotation Values text
          ) # end column
        ),  # end fluidRow Export results Text

        # The result export panel only exist if annotation took place
        column(12,
          uiOutput("exportPvalFigUI"),
          # TODO: a result export panel only appearing if is annotated!
        tags$hr()
        )   # end column export results

      )     # end Main panel column
    )       # end fluidRow (sidebar + menu)
  )         # end conditional panel
)
# end EXPORT Tab panel  ------------------------------------------------------ #
