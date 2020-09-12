## Diagnostic Tab --------------------------------------------------------------

# - message if no import (conditional panel)
# - status of the annotation (and failures) on the sidebar

# - [TAB] update uROI/FIR
# - [TAB] show plots
# - [TAB] show updated parameters + modify ?

## not annotated
output$notAnnotForDiagUI <- renderUI ({
  if(runSuccess()=='yes') return()
  tagList(
    HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation computed</h4>Annotate a <i>peakPantheRAnnotation</i></div>")#,
    #includeHTML("data/aboutDiagnotic.html")
  )
})


## show the status of the peakPantheRAnnotation (need to be separated from the previous panel so they can be hidden)
output$showAnnotStatusDiag <- renderUI({
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


## Run the update to uROI/FIR
observeEvent(input$goDiagnosticUpdateUROIFIR, {
  updated_annotation  <- peakPantheR::annotationParamsDiagnostic(values$annotation, verbose=TRUE)
  values$annotation <- updated_annotation
})
# TODO: CHECK the uROI FIR diagnostic update

## Check update uROI/FIR is a success
diagSuccess <- reactive({
  # Diagnostic not triggered yet
  if(input$goDiagnosticUpdateUROIFIR == 0) {
    return('no')
  } else {
    # Diagnostic triggered, no absolute certainty of result, but uROIExist will have been set by it
    if(peakPantheR::isAnnotated(values$annotation) & peakPantheR::uROIExist(values$annotation)) {
      return('yes')
    # something is wrong
    } else {
      return('no')
    }
  }
})
# TODO: CHECK diag success message

# Success update UROI/FIR using diagnostic message
output$successUpdateDiagUI <- renderUI({
  if(diagSuccess()=='yes') {
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>uROI and FIR updated sucessfully</div>")
    )
  }
})
