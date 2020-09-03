## Run Tab ---------------------------------------------------------------------

## no import
output$noImportForFitUI <- renderUI ({
  if(importSuccess()=='yes') return()
  tagList(
    HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation imported</h4>Create or load a <i>peakPantheRAnnotation</i></div>")#,
    #includeHTML("data/aboutTSAnalysis.html")
  )
})


## show the status of the peakPantheRAnnotation
output$showAnnotStatus <- renderUI({
  # Capture the annotation shown and split by line into a list
  tmp_text <- annotation_showText_UI_helper(annotation_showMethod_UI_helper(values$annotation))
  # render the panel
  wellPanel(
    h4('Status:', style="color:#3e648d;font-weight:bold"),
    helpText(tmp_text[[1]],style="color:black"),
    helpText(tmp_text[[2]],style="color:black"),
    helpText(tmp_text[[3]],style="color:black"),
    helpText(tmp_text[[4]],style="color:black"),
    helpText(tmp_text[[5]],style="color:black"),
    helpText(tmp_text[[6]],style="color:black")
  )
})


## Message if already annotated
output$alreadyAnnotatedUI <- renderUI({
  # only check before run is triggered
  if(input$goAnnotation == 0) {
    # warning message
    if(peakPantheR::isAnnotated(values$annotation)) {
      # annotated, message
      return(
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-warning\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Warning</h4>This dataset is already annotated, if selecting `Annotate` the data will be overwritten</div>")
        )
      )
    } else {
      # not annotated, no message
      return()
    }
  # no need for msg after run is triggered
  } else {
    return()
  }
})
# TODO: CHECK the 'Already Annotated' UI


## Run UI buttons
# Checkbox use uROI (set default to current annotation value)
output$useUROICheckbox <- renderUI({
  # if uROI does not exist, strikethrough the label
  if (peakPantheR::uROIExist(values$annotation)) {
    lbl <- p("use ", shiny::span(em("updated Regions of Interest")))
  } else {
    lbl <- p(shiny::tags$s("use "), shiny::span(em(shiny::tags$s("updated Regions of Interest"))))
  }
  # set the default value based on the annotation status
  tagList(
    h5(HTML("uROI"), style="color:#3e648d;font-weight:bold"),
    checkboxInput("useUROI",
                   label = lbl,
                    value = peakPantheR::useUROI(values$annotation))
  )
})# TODO: CHECK uROI checkbox is striked-through and activated correctly

# Checkbox use FIR (set default to current annotation value)
output$useFIRCheckbox <- renderUI({
  tagList(
    h5(HTML("FIR"), style="color:#3e648d;font-weight:bold"),
    checkboxInput("useFIR",
                  label = p("use ", shiny::span(em("Fallback Integration Regions"))),
                  value = peakPantheR::useFIR(values$annotation))
  )
}) # TODO: CHECK FIR checkbox is activated correctly [cannot do the strikethrough]

# Cpu slider appears if parallelisation is selected
output$cpuSlider <- renderUI({
  if(input$parallelisation == 0) return(NULL)
  tagList(
    sliderInput("ncores",
      label = paste("Available cores: ",maxCores,sep=""),
      min = 0, max = maxCores, value = maxCores, step=1
    )
  )
})
# correction when slider doesn't appear
ncoresInput <- reactive ({
  if( input$parallelisation != 0 ) { input$ncores }
  else { return(0) }
})


## Run the annotation (simplest setup possible)
observeEvent(input$goAnnotation, {
  ## Need to update an annotation first with useUROI and useFIR
  tmp_annotation <- values$annotation
  # proxy for useUROI (if uROI does not exist: it is always FALSE)
  if (peakPantheR::uROIExist(tmp_annotation)) { tmp_useUROI <- input$useUROI } else { tmp_useUROI <- FALSE }
  # finalise the setup of the annotation (useUROI, useFIR)
  tmp_annotation <- peakPantheR::resetAnnotation(tmp_annotation,
                                                 useUROI=tmp_useUROI,
                                                 useFIR=input$useFIR,
                                                 verbose=FALSE)
  ## Annotate!
  result <- peakPantheR_parallelAnnotation(tmp_annotation, ncores=ncoresInput(), verbose=TRUE)
  # Store the annotation and failures into the reactiveValue
  values$annotation <- result$annotation
  values$failures   <- result$failures
})
# TODO: check the resulting annotation
# TODO: !! a .rdata with proper filepaths for FaahKO !!


## Progress bar
output$progressBarUI <- renderUI({
  h2('woohoo a progress bar')
})
# TODO: a progress bar?


## Check annotation run is a success
# value for success
runSuccess <- reactive({
  # return 'no' until the trigger button is clicked
  if(input$goAnnotation == 0) { return('no')} # annotation not clicked
  return('yes')
  # TODO: CHECK the annotation results after `goAnnotation` got triggered
  # TODO: a message of # failures? ~ is it a fail if not samples left (notAnnotated it seems)
#  # return 'no' until a trigger button is clicked
#  if(input$triggerImportNewAnnotation == 0 & input$triggerLoadPreviousAnnotation == 0) { return('no')} # import not clicked
#
#  isolate({
#    # use a validObject as check to stop having a 'yes' in case of problems
#    # if error raised during import, could have an annotation() that exist (NULL) and validObject(NULL) is TRUE... so check it's a peakPantheRAnnotation
#    if( isTRUE(validObject(annotation(), test=TRUE)) & is(annotation(), 'peakPantheRAnnotation') ) {
#      return('yes')
#
#    # not valid, return 'no'.
#    # If it isn't a peakPantheRAnnotation, the app is killed with the error raised in the loading part (ordering is out of our control)
#    # If it is an annotation, it is killed here
#    } else {
#      if(is(annotation(), 'peakPantheRAnnotation')) {
#        stopApp( paste('Error:', validObject(annotation(), test=TRUE)))
#      }
#      return('no')
#    }
#  })
})


## Success message
output$successAnnotationUI <- renderUI({
  if(runSuccess()=='no') {
    # not imported yet
    if(input$goAnnotation == 0) {
      return()
    # Run failed
    } else {
      return(
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-danger\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Error</h4>Annotation run failed</div>")
        )
      )
    }
  # Run is successful
  } else if(runSuccess()=='yes') {
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>Annotation successfully finished</div>")
    )
  }
})
