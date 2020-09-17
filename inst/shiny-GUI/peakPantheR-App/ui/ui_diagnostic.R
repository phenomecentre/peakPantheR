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

        # Tabs
        shiny::tabsetPanel( id="diagnosticTabs", type="pills",

        # Annotation success statistics - TAB
          tabPanel("Annotation statistics",
            wellPanel(
              fluidRow(
                column(11, offset=1,
                  h4('Fitting results across samples', style="color:#3e648d;font-weight:bold"),
                  helpText('Summary of peaks found and Fallback Integration Regions', shiny::span(em('(FIR, if applicable)')), 'for each targeted feature across all samples:', style="color:black"),
                  HTML("<p style='color:black'>Targeted features with a low <i>ratio of peaks found</i>, a large <i>ppm error</i> or a high <i>RT deviation</i> across samples, might benefit from a correction of the targeted <code>rt</code> and <code>m/z</code> used as <code>uROI</code> for example</p>"),
                ) # end column
              )   # end fluidRow
            ),    # end wellPanel
            fluidRow(
              uiOutput("annotationStatisticsTable")
            )     # end fluiRow (fit stat panel)
          ),      # end tabPanel

        # Automatic update uROI/FIR - TAB
          tabPanel("Update uROI/FIR",
            wellPanel(
              fluidRow(
                column(11, offset=1,
                  h4('Update uROI and FIR', style="color:#3e648d;font-weight:bold"),
                  helpText('Based on the fit results, updated ROI', shiny::span(em('(uROI)')), 'and fallback integration region', shiny::span(em('(FIR)')), 'can be automatically determined:', style="color:black"),
                  HTML("<ul>"),
                  HTML("<li style='color:black'><code>uROI</code> are established as the min/max (<code>rt</code> and <code>m/z</code>) of the found peaks (+/- 5% in RT)</li>"),
                  HTML("<li style='color:black'><code>FIR</code> are established as the median of found <code>rtMin</code>, <code>rtMax</code>, <code>mzMin</code>, <code>mzMax</code></li>"),
                  HTML("</ul>"),
                  div(actionButton("goDiagnosticUpdateUROIFIR", label="Update uROI/FIR",
                                   class="btn btn-primary btn-lg"), align="center")
                ) # end column
              )   # end fluidRow
            ),    # end wellPanel
            fluidRow(
              uiOutput("successUpdateDiagUI") # success message update uROI/FIR
            )   # end fluiRow (success panel)
          ),    # end tabPanel

        # Show diagnostic plots - TAB
          tabPanel("Diagnostic plot",
            # control the plot
            uiOutput("diagPlotControlUI"),
            # plot output
            uiOutput("diagPlotResultUI")
          )#,    # end tabPanel

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

