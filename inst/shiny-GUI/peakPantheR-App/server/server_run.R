## Run Tab ---------------------------------------------------------------------

# no import
  output$noImportForFitUI <- renderUI ({
    if(importSuccess()=='yes') return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation imported</h4>Create or load a <i>peakPantheRAnnotation</i></div>")#,
      #includeHTML("data/aboutTSAnalysis.html")
    )
  })

# show the status of the peakPantheRAnnotation
output$showAnnotStatus <- renderUI({
  # Capture the annotation shown and split by line into a list
  tmp_text <- annotation_showText_UI_helper(annotation_showMethod_UI_helper(annotation()))
  # render the panel
  wellPanel(
    helpText(tmp_text[[1]],style="color:black"),
    helpText(tmp_text[[2]],style="color:black"),
    helpText(tmp_text[[3]],style="color:black"),
    helpText(tmp_text[[4]],style="color:black"),
    helpText(tmp_text[[5]],style="color:black"),
    helpText(tmp_text[[6]],style="color:black")
  )
})

# Message if already annotated
output$alreadyAnnotatedUI <- renderUI({
  h2('Already annotated')
})


# Fit UI
output$fitUI <- renderUI({
  h2('fit duh')
})

# Progress bar
output$progressBarUI <- renderUI({
  h2('woohoo a progress bar')
})

# Success message
output$successAnnotationUI <- renderUI({
  h2('success!')
})


# TODO: a box saying if it was already annotated, to let people know (maybe orange)
# TODO: the whole Run UI
# TODO: a progress bar?
# TODO: a success message