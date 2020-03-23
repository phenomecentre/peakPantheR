# IMPORT - Tab panel  -------------------------------------------------------- #

tabPanel("Import Data",
  fluidRow( # top block containing both imports

    ## Init from files
    # get param csv path
    # get a list of file paths
    # cpd/spl metadata (no checks, just length, allowed not to be here)
    # returns init object!
    column(7,
      h4(HTML("Create a new <em>peakPantheRAnnotation</em>"), style="color:#3e648d;font-weight:bold"),
      wellPanel(
        fluidRow(
          h5(HTML("Targeted features"), style="color:#3e648d;font-weight:bold"),
          helpText("Select a .CSV file containing the ", span(strong("fit parameters")), span(em('with targeted features as rows and target windows as columns (at minimum: "cpdID", "cpdName", "rtMin", "rt", "rtMax", "mzMin", "mz", "mzMax")')),style="color:black"),
          fileInput('CSVParamPath', 'Choose a .CSV File',
                    accept=c('text/csv','text/comma-separated-values,text/plain','.csv','.tsv')),
        tags$hr()
        ), # end fluidRow Fit params

        fluidRow(
          h5(HTML("Files to process (choose one of two)"), style="color:#3e648d;font-weight:bold"),
          column(width=6,
        h6(HTML("File path only"), style="color:#3e648d;font-weight:bold"),
            helpText("Select the ", span(strong("files to process")),style="color:black"),
            fileInput('spectraPaths', 'Choose files to process',
                      multiple=TRUE)
          ), # end left files selector column (only files)
          column(width=6,
        h6(HTML("File paths and metadata"), style="color:#3e648d;font-weight:bold"),
            helpText(" Select a .CSV file containing the ", span(strong("file paths "), em("(column `filepath`)"), strong(" and metadata")), style="color:black"),
            fileInput('spectraPathsWithMetadata', 'Choose a .CSV File',
                      accept=c('text/csv','text/comma-separated-values,text/plain','.csv','.tsv'))
          ) # end right file selector column (files + metadata)
        ), # end fluidRow File paths (either without or with metadata)

        fluidRow(
          tags$hr(),
          h5(HTML("Targeted features metadata (Optional)"), style="color:#3e648d;font-weight:bold"),
          helpText("Select a .CSV file containing the ", span(strong("targeted features metadata")), style="color:black"),
          fileInput('cpdMetadataPath', 'Choose a .CSV File',
                    accept=c('text/csv','text/comma-separated-values,text/plain','.csv','.tsv'))
        ), # end FluidRow feature metadata

        fluidRow(
          div(actionButton("triggerImportNewAnnotation", label="Import",
                           class="btn btn-primary btn-lg"), align="center")
        ) # end import button row
      )# end left panel
    ),	# end left column (New annotation)


    ## Init from previous object
    # load
    column(5,
      h4(HTML("Load a <em>peakPantheRAnnotation</em>"), style="color:#3e648d;font-weight:bold"),
      wellPanel(
        h5(HTML("Annotation"), style="color:#3e648d;font-weight:bold"),
        helpText("Select a .RData file containing a ", span(strong("peakPantheRAnnotation")), " named", span(em("annotationObject")),style="color:black"),
        fluidRow(
          fileInput('pathAnnotation', 'Choose a .RData File',
                    accept=c('application/octet-stream','.RData','.rdata'))
        ),

        fluidRow(
          div(actionButton("triggerLoadPreviousAnnotation", label="Import",
                           class="btn btn-primary btn-lg"), align="center")
        ) # end import button row
      )# end right panel
    )   # end right column (Load annotation)
  ),     # end fluidRow (top block containing both imports)


  ## Result of both imports
  fluidRow(
    # Row success / fail
    column(width=12,
      # success band 12 col wide (green) / FAIL (red)
      uiOutput("resultImportCheck")  # error/success message
    ), # end column

    # Row show annotation
    column(width=12,
      # box with the Show(init) results
      uiOutput("showImportResult")  # show() object properties
    ) # end column (row) of shown results
  )   # end fluidRow (bottom block containing both imports result)
)     # end tabPanel
# end IMPORT Tab panel ------------------------------------------------------- #
