## Run Tab ---------------------------------------------------------------------

# no import
  output$noImportForFitUI <- renderUI ({
    if(importSuccess()=='yes') return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation imported</h4>Create or load a <i>peakPantheRAnnotation</i></div>")#,
      #includeHTML("data/aboutTSAnalysis.html")
    )
  })