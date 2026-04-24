# IMPORT - Tab panel  -------------------------------------------------------- #

tabPanel("Import Data",
  tabsetPanel(id = "importTabs",

    ## Create new annotation tab
    tabPanel("Create a new peakPantheRAnnotation",
      fluidRow(
        column(12,
          wellPanel(
            fluidRow(
              column(12,
                h5(HTML("Targeted features"),
                   style="color:#3e648d;font-weight:bold")),
              column(12,
                span("Select a .CSV file containing the ",
                     shiny::span(strong("fit parameters")),
                     shiny::span(em(paste0(
                       'with targeted features as rows and target ',
                       'windows as columns (at minimum: "cpdID", ',
                       '"cpdName", "rtMin", "rt", "rtMax", "mzMin",',
                       ' "mz", "mzMax")'))),
                     style="color:black")),
              column(12,
                fileInput('CSVParamPath', 'Choose a .CSV File',
                          accept=c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv','.tsv'))),
              tags$hr()
            ), # end fluidRow Fit params

            fluidRow(
              column(12,
                h5(HTML("Files to process (choose one of two)"),
                   style="color:#3e648d;font-weight:bold")
              ), #end title column
              column(6,
                h6(HTML("File path only"),
                   style="color:#3e648d;font-weight:bold"),
                span("Select the ",
                     shiny::span(strong("files to process")),
                     style="color:black"),
                fileInput('spectraPaths', 'Choose files to process',
                          multiple=TRUE)
              ), # end left files selector column (only files)
              column(6,
                h6(HTML("File paths and metadata"),
                   style="color:#3e648d;font-weight:bold"),
                span(" Select a .CSV file containing the ",
                     shiny::span(strong("file paths "),
                                 em("(column `filepath`)"),
                                 strong(" and metadata")),
                     style="color:black"),
                fileInput('spectraPathsWithMetadata',
                          'Choose a .CSV File',
                          accept=c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv','.tsv'))
              ), # end right file selector column (files + metadata)
              tags$hr()
            ), # end fluidRow File paths

            fluidRow(
              column(12,
                h5(HTML("Targeted features metadata (Optional)"),
                   style="color:#3e648d;font-weight:bold")),
              column(12,
                span("Select a .CSV file containing the ",
                     shiny::span(strong("targeted features metadata")),
                     style="color:black")),
              column(12,
                fileInput('cpdMetadataPath', 'Choose a .CSV File',
                          accept=c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv','.tsv')))
            ), # end FluidRow feature metadata

            fluidRow(
              div(actionButton("triggerImportNewAnnotation",
                               label="Import",
                               class="btn btn-primary btn-lg"),
                  align="center")
            ) # end import button row
          ) # end wellPanel
        ) # end column
      ) # end fluidRow
    ), # end tabPanel (Create new)

    ## Load previous annotation tab
    tabPanel("Load a peakPantheRAnnotation",
      fluidRow(
        column(12,
          wellPanel(
            h5(HTML("Annotation"),
               style="color:#3e648d;font-weight:bold"),
            span("Select a .RData or .RDS file containing a ",
                 shiny::span(strong("peakPantheRAnnotation")),
                 ". If the file contains multiple annotation ",
                 "objects, you will be prompted to choose one.",
                 style="color:black"),
            fluidRow(
              fileInput('pathAnnotation',
                        'Choose a .RData or .RDS File',
                        accept=c('application/octet-stream',
                                 '.RData','.rdata','.RDS','.rds'))
            ),

            fluidRow(
              div(actionButton("triggerLoadPreviousAnnotation",
                               label="Import",
                               class="btn btn-primary btn-lg"),
                  align="center")
            ) # end import button row
          ) # end wellPanel
        ) # end column
      ) # end fluidRow
    ) # end tabPanel (Load previous)

  ), # end tabsetPanel

  ## Result of both imports
  fluidRow(
    # Row success / fail
    column(width=12,
      uiOutput("resultImportCheck")
    ), # end column

    # Row show annotation
    column(width=12,
      uiOutput("showImportResult")
    ) # end column
  ) # end fluidRow (result)
) # end tabPanel (Import Data)
# end IMPORT Tab panel ------------------------------------------------------- #
