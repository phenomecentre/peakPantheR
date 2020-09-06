## Import Tab ------------------------------------------------------------------

# - init from files or load an annotation (init implicitely checks validity, load doesn't)
#   in case of error in the creation/loading function, the app is stopped with an error message
# - does a valid check to return a success bar (or error bar)
# - show created/loaded annotation on screen


## Generate/Load data on trigger (store 'annotation' in a reactiveValue, depending on input button pressed)
# check input but doesn't validate inputs

# Default annotation as reactiveValue
values <- reactiveValues(annotation = NULL, failures = NULL)

# New annotation
observeEvent(input$triggerImportNewAnnotation, {
  # only if the right button is clicked
  if(input$triggerImportNewAnnotation != 0) {
    # catch the errors in the import functions and kill the app
    res_data <- tryCatch({
      # deal with spectraPaths and spectraMetadata
      spectraInfo <- spectraPaths_and_metadata_UI_helper(spectraPaths = input$spectraPaths$datapath,
                                                         spectraMetadataPath = input$spectraPathsWithMetadata$datapath)
      # create annotation
      initialise_annotation_from_files_UI_helper(CSVParamPath = input$CSVParamPath$datapath,
                                                 spectraPaths = spectraInfo$spectra,
                                                 cpdMetadataPath = input$cpdMetadataPath$datapath,
                                                 spectraMetadata = spectraInfo$meta$datapath,
                                                 verbose = FALSE)
    },
    error = function(e) {
      stopApp(e[[1]])
    })
    # valid_peakPantheRAnnotation() is part of annotation creation
    values$annotation <- res_data
  } # end if
})  # end New Annotation

# Load annotation
observeEvent(input$triggerLoadPreviousAnnotation, {
  # only if the right button is clicked
  if(input$triggerLoadPreviousAnnotation!=0) {
    # catch the errors in the import functions and kill the app
    res_data <- tryCatch({
      load_annotation_from_file_UI_helper(annotationPath = input$pathAnnotation$datapath)
    },
    error = function(e) {
      stopApp(e[[1]])
    })
    # no validation on load (cannot guarantee annotation is valid)
    values$annotation <- res_data
  } # end if
})  # end Load Annotation


## Check Import/Load are a success
# value for success
importSuccess <- reactive({
  # return 'no' until a trigger button is clicked
  if(input$triggerImportNewAnnotation == 0 & input$triggerLoadPreviousAnnotation == 0) { return('no')} # import not clicked

  isolate({
    # use a validObject as check to stop having a 'yes' in case of problems
    # if error raised during import, could have a values$annotation that exist (NULL) and validObject(NULL) is TRUE... so check it's a peakPantheRAnnotation
    if( isTRUE(validObject(values$annotation, test=TRUE)) & is(values$annotation, 'peakPantheRAnnotation') ) {
      return('yes')

    # not valid, return 'no'.
    # If it isn't a peakPantheRAnnotation, the app is killed with the error raised in the loading part (ordering is out of our control)
    # If it is an annotation, it is killed here
    } else {
      if(is(values$annotation, 'peakPantheRAnnotation')) {
        stopApp( paste('Error:', validObject(values$annotation, test=TRUE)))
      }
      return('no')
    }
  })
})


# success/failure UI
output$resultImportCheck <- renderUI({
  if(importSuccess()=='no') {
    # not imported yet
    if(input$triggerImportNewAnnotation == 0 & input$triggerLoadPreviousAnnotation == 0) {
      return()
    # import control failed
    } else {
      return(
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-danger\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Error</h4>Check input data</div>")
        )
      )
    }
  # import is successful
  } else if(importSuccess()=='yes') {
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>Input data generated</div>")
    )
  }
})


## Show imported annotation
output$showImportResult <- renderUI({
  # not imported yet or failed
  if(importSuccess()=='no') {
      if(input$triggerImportNewAnnotation == 0 & input$triggerLoadPreviousAnnotation == 0) { return() }

  # import is successful
  } else if(importSuccess()=='yes') {
    # Capture the annotation shown and split by line into a list
    tmp_text <- capture.output(show(values$annotation))
    tmp_text <- strsplit(tmp_text, '\n')
    # render the panel
    wellPanel(
      h4(HTML("Created/Imported <em>peakPantheRAnnotation</em>:"), style="color:#3e648d;font-weight:bold"),
      # preformatted text, pass the arguments as a list (shiny doesn't accept '\n' in character for print)
      do.call(tags$pre, tmp_text )
    )
  }
})


# Conditional panels control (next tab)
output$importDone <- renderText({
  if( importSuccess()=='yes' ) { 'yes' } else { 'no' }
})
