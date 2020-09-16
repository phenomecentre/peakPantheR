## Results Tab -----------------------------------------------------------------

# - message if no import (conditional panel)
# - status of the annotation (and failures) on the sidebar

# - [TAB] show overall results (peakTable, focus on one property at the time)
# - [TAB] show the values per compound
# - [TAB] show the values per sample

## not annotated
output$notAnnotForResUI <- renderUI ({
  if(runSuccess()=='yes') return()
  tagList(
    HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation computed</h4>Annotate a <i>peakPantheRAnnotation</i></div>")#,
    #includeHTML("data/aboutResults.html")
  )
})


## show the status of the peakPantheRAnnotation (need to be separated from the previous panel so they can be hidden)
output$showAnnotStatusRes <- renderUI({
  # Capture the annotation shown and split by line into a list
  tmp_text <- annotation_showText_UI_helper(annotation_showMethod_UI_helper(values$annotation))
  fail_text <- paste(dim(values$failures)[1], 'annotation failure(s)')
  # render the panel
  wellPanel(
    h4('Status:', style="color:#3e648d;font-weight:bold"),
    helpText(tmp_text[[1]],style="color:black"),
    helpText(tmp_text[[2]],style="color:black"),
    helpText(tmp_text[[3]],style="color:black"),
    helpText(tmp_text[[4]],style="color:black"),
    helpText(tmp_text[[5]],style="color:black"),
    helpText(tmp_text[[6]],style="color:black"),
    tags$hr(),
    if (!is.null(values$failures)) {
      helpText(fail_text,style="color:black")
    }
  )
})


## Overall results -------------------------------------------------------------
# UI to control the peakTable
peakTableCol <- c('peakArea','found','rtMin','rt','rtMax','mzMin','mz','mzMax','maxIntMeasured','maxIntPredicted','is_filled','ppm_error','rt_dev_sec','tailingFactor','asymmetryFactor')
output$peakTableControlUI <- renderUI({
  wellPanel(
    fluidRow(
      column(12, offset=1,
        h4('Key fitting properties across all targeted features and samples:', style="color:#3e648d;font-weight:bold"),
      ) # end column
    ),  # end fluidRow
    fluidRow(
      column(6, offset=1,
        selectInput("peakTableColRes", label="Property to show", choices=peakTableCol)
      ) # end column
    )   # end fluidRow
  )     # end wellPanel
})

# render the peakTable
output$table_overall <- DT::renderDataTable ({
  DT::datatable(data = peakPantheR::annotationTable(values$annotation, column=input$peakTableColRes),
                options  = list(orderClasses = TRUE),
                rownames = TRUE)
})

# UI block peakTable
output$overallResultsUI <- renderUI ({
  fluidRow(
    DT::dataTableOutput("table_overall")
  )
})


## Results by Features ---------------------------------------------------------

# TODO: results per targeted features


## Results by Samples ----------------------------------------------------------
# UI to control the results per sample
output$resPerSplControlUI <- renderUI({
  wellPanel(
    fluidRow(
      column(12, offset=1,
        h4('Fitting results per sample:', style="color:#3e648d;font-weight:bold"),
      ) # end column
    ),  # end fluidRow
    fluidRow(
      column(6, offset=1,
        selectInput("splResToShow", label="Sample", choices=values$filename)
      ) # end column
    )   # end fluidRow
  )     # end wellPanel
})

# render the results per sample (filter based on filename)
output$spl_result_table <- DT::renderDataTable ({
  tmp_spl_res           <- peakTables(values$annotation)[values$filename == input$splResToShow][[1]]
  rownames(tmp_spl_res) <- values$featNmeList
  DT::datatable(data = tmp_spl_res,
                options  = list(orderClasses = TRUE),
                rownames = TRUE)
})

# UI block sample results
output$sampleResultsUI <- renderUI ({
  fluidRow(
    DT::dataTableOutput("spl_result_table")
  )
})
