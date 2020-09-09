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

        # Update uROI/FIR - TAB
          tabPanel("Update uROI/FIR",
            wellPanel(
              fluidRow(
                h3('update!')
              ) # end fluidRow
            )   # end wellPanel
          ),    # end tabPanel

        # Show diagnostic plots - TAB
          tabPanel("Diagnostic plot",
            wellPanel(
              fluidRow(
                h3('Plots!')
              ) # end fluidRow
            )   # end wellPanel
          ),    # end tabPanel

        # Show/modify updated parameters - TAB
          tabPanel("Final parameters",
            wellPanel(
              fluidRow(
                h3('Parameters!')
              ) # end fluidRow
            )   # end wellPanel
          )     # end tabPanel
        )       # end tabsetPanels

        # TODO: 3 tabs; auto-update params, diag plots, params update

      )   # end Main panel column
    )     # end fluidRow (sidebar + menu)
  )       # end conditional panel
)
# end DIAGNOSTIC Tab panel --------------------------------------------------- #

