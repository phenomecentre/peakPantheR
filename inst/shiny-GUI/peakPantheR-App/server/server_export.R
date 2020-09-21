## Export Tab ------------------------------------------------------------------

# - export files to recreate input
#   - RData
#   - the different csv (with metadata)
# - export results (tables to disk)
# - export plot to disk


## not annotated
output$noImportForExportUI <- renderUI ({
  if(importSuccess()=='yes') return()
  tagList(
    HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation imported</h4>Create or load a <i>peakPantheRAnnotation</i></div>")#,
    #includeHTML("data/aboutExport.html")
  )
})


## show the status of the peakPantheRAnnotation (need to be separated from the previous panel so they can be hidden)
output$showAnnotStatusExp <- renderUI({
  # Capture the annotation shown and split by line into a list
  tmp_text  <- annotation_showText_UI_helper(annotation_showMethod_UI_helper(values$annotation))
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




