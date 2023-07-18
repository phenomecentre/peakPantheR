## Run Tab ---------------------------------------------------------------------

# - message if no import (conditional panel)
# - status of the annotation (and failures) on the sidebar
# - message if already annotated (also triggered after computation)
# - useUROI, useFIR and parallelisation controls [pre-checked based on annotation properties, useUROI strikedthrough if uROI do not exist]
# - run annotation after updating useUROI useFIR (erase previous data)
# - progress indicator during computation
# - success/failure message with number of failed samples


## no import
output$noImportForFitUI <- renderUI ({
  if(importSuccess()=='yes') return()
  tagList(
    HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation imported</h4>Create or load a <i>peakPantheRAnnotation</i></div>")#,
    #includeHTML("data/aboutRunAnnotation.html")
  )
})


## show the status of the peakPantheRAnnotation
output$showAnnotStatus <- renderUI({
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


## Message if already annotated
output$alreadyAnnotatedUI <- renderUI({
  # annotated, warning message
  if(peakPantheR::isAnnotated(values$annotation)) {
    return(
      tagList(
        HTML("<div class=\"alert alert-dismissible alert-warning\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Warning</h4>This dataset is already annotated, if selecting `Annotate` the data will be overwritten</div>")
      )
    )
  # not annotated, no msg
  } else { return() }
})


## Run UI buttons
# Checkbox use uROI (set default to current annotation value)
output$useUROICheckbox <- renderUI({
  # if uROI does not exist, strikethrough the label
  if (peakPantheR::uROIExist(values$annotation)) {
    lbl <- span("use ", shiny::span(em("updated Regions of Interest")))
  } else {
    lbl <- span(shiny::tags$s("use "), shiny::span(em(shiny::tags$s("updated Regions of Interest"))))
  }
  # set the default value based on the annotation status
  tagList(
    h5(HTML("uROI"), style="color:#3e648d;font-weight:bold"),
    checkboxInput("useUROI",
                   label = lbl,
                    value = peakPantheR::useUROI(values$annotation))
  )
})

# Checkbox use FIR (set default to current annotation value)
output$useFIRCheckbox <- renderUI({
  tagList(
    h5(HTML("FIR"), style="color:#3e648d;font-weight:bold"),
    checkboxInput("useFIR",
                  label = span("use ", shiny::span(em("Fallback Integration Regions"))),
                  value = peakPantheR::useFIR(values$annotation))
  )
}) # TODO: CHECK FIR checkbox is activated correctly [cannot do the strikethrough]

# selectInput choose curveModel (set default to current annotation value)
output$curveModelSelectInput <- renderUI({
  tagList(
    selectInput("curveModel",
                  label = 'Fitting model to use (curveModel)',
                  choices = knownCurveModel,
                  selected = NULL, # will be peakPantheR::curveModel(values$annotation))
                  multiple = FALSE)
  )
})

# Cpu slider appears if parallelisation is selected
output$cpuSlider <- renderUI({
  if(input$parallelisation == 0) return(NULL)
  tagList(
    sliderInput("ncores",
      label = paste("Available cores: ",maxCores,sep=""),
      min = 1, max = maxCores, value = maxCores, step=1
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
  result <- peakPantheR_parallelAnnotation(tmp_annotation, nCores=ncoresInput(), curveModel=input$curveModel, verbose=TRUE)

  # Store the annotation and failures into the reactiveValue
  values$annotation <- result$annotation
  values$failures   <- result$failures
  # Set a list of feature name for later use
  values$featNmeList        <- paste(cpdID(values$annotation), cpdName(values$annotation), sep=' - ')
  names(values$featNmeList) <- seq_len(length(values$featNmeList))
  # Set a list of spectraMetadata columns (+ None)
  tmp_splCol                <- c(list('None'), colnames(peakPantheR::spectraMetadata(values$annotation)))
  values$spectraMetadataCol <- tmp_splCol
  # Set a list of filename (sample list)
  values$filename           <- peakPantheR::filename(values$annotation)
})


## Progress bar
output$progressBarUI <- renderUI({
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   column(10, align="center",
                          h4('Processing ...', style="color:#3e648d;font-weight:bold"),
                          shiny::img(src="annot_progress.gif", width="350px")
                   )
  )
})


## Check annotation run is a success
# value for success
runSuccess <- reactive({
  # no annotation computed yet (cover the case of no import without having to do an isAnnotated on NULL which would throw an error)
  if(is.null(values$annotation)) {
    return('no')
  }
  # annotation successful or already annotated
  if(peakPantheR::isAnnotated(values$annotation)) {
    return('yes')
  # annotation not yet run or all spectra fail (trigger is checked in successAnnotationUI
  } else {
    return('no')
  }
})

## Success message
output$successAnnotationUI <- renderUI({
  failure_text <- paste(dim(values$failures)[1], 'annotation failure(s)')
  if(runSuccess()=='no') {
    # not imported yet
    if(input$goAnnotation == 0) {
      return()
    # Run failed
    } else {
      return(
        tagList(
          HTML(paste0("<div class=\"alert alert-dismissible alert-danger\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Error</h4>Annotation run failed<br>", failure_text, "</div>"))
        )
      )
    }
  # Run is successful
  } else if(runSuccess()=='yes') {
    tagList(
      HTML(paste0("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>Annotation successfully finished<br>", failure_text, "</div>"))
    )
  }
})


# Conditional panels control (next tab)
output$AnnotationDone <- renderText({
  if( runSuccess()=='yes' ) { 'yes' } else { 'no' }
})
