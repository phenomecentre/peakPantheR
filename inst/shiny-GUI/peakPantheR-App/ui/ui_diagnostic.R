# DIAGNOSTIC - Tab panel  ---------------------------------------------------- #

tabPanel("Diagnostic: plot & update",

  # Conditional UI based on import successfully initialised
  uiOutput("notAnnotForDiagUI"),
  conditionalPanel(
    condition = "output.AnnotationDone=='yes'",
    fluidRow(

  ## Sidebar
      column(width=2,
        shiny::tabsetPanel(id = "diagSidebarTabs", type = "pills",
          tabPanel("Status",
            uiOutput("showAnnotStatusDiag")
          ),
          tabPanel("Settings",
            uiOutput("diagSettingsUI")
          )
        )
      ), # end Sidebar column

  ## Main panel
      column(10,

        # Overall title
        h3('Diagnostic', style="color:#3e648d;font-weight:bold"),

        # Tabs
        shiny::tabsetPanel( id="diagnosticTabs", type="pills",

        # Show diagnostic plots - TAB
          tabPanel("Diagnostic plot",
            # control strip (feature/colour/samples + drag-edit inside
            # the same wellPanel, divided by an <hr>)
            uiOutput("diagPlotControlUI"),
            # plot output
            uiOutput("diagPlotResultUI")
          ),      # end tabPanel

        # RT correction - TAB
          tabPanel("RT correction",
            uiOutput("rtCorrControlUI"),
            uiOutput("rtCorrResultUI")
          ),      # end tabPanel

        # Annotation success statistics - TAB
          tabPanel("Annotation statistics",
            wellPanel(
              fluidRow(
                column(11, offset=1,
                  h4('Fitting results across samples', style="color:#3e648d;font-weight:bold"),
                  span('Summary of peaks found and Fallback Integration Regions', shiny::span(em('(FIR, if applicable)')), 'for each targeted feature across all samples:', style="color:black"),
                  HTML("<p style='color:black'>Targeted features with a low <i>ratio of peaks found</i>, a large <i>ppm error</i> or a high <i>RT deviation</i> across samples, might benefit from a correction of the targeted <code>rt</code> and <code>m/z</code> used as <code>uROI</code> for example</p>"),
                ) # end column
              )   # end fluidRow
            ),    # end wellPanel
            fluidRow(
              column(12,
                uiOutput("annotationStatisticsTable")
              )
            )     # end fluiRow (fit stat panel)
          )#,     # end tabPanel

        # Show/modify updated parameters - TAB
        #  tabPanel("Final parameters",
        #    wellPanel(
        #      fluidRow(
        #        h3('! Under construction !')
        #        # TODO: UI uROI & FIR show and tweaks
        #      ) # end fluidRow
        #    )   # end wellPanel
        #  )     # end tabPanel
        )       # end tabsetPanels
      )   # end Main panel column
    )     # end fluidRow (sidebar + menu)
  )       # end conditional panel
)
# end DIAGNOSTIC Tab panel --------------------------------------------------- #

