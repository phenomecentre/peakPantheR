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


# -- Export input files and annotation
# export peakPantheRAnnotation as .rData
output$downloadRData <- downloadHandler(
    # filename
    filename = function() { paste0(input$saveNameRData, '.rdata') },
    # data to save
    content = function(file) {
        annotationObject <- values$annotation
        save( annotationObject, file=file, compress=TRUE)
    }
)
output$dlRData <- renderUI({
  tagList(
      textInput("saveNameRData", label = ".rData name", value="annotation_rData"),
      div(
        downloadButton('downloadRData',"Download", class="btn btn-primary btn-lg"),
        align="center"
      )
  )
})
# TODO: Check exported rData

# export inputs as CSVs
output$downloadCSVFitParam <- downloadHandler(
    # filename
    filename = function() { paste0(input$saveNameCSVFitParam, '.csv') },
    # data to save
    content = function(file) {
        write.csv(outputAnnotationParamsCSV(values$annotation, saveFolder=NULL, verbose=FALSE, noSave=TRUE), file, row.names=FALSE)
    }
)
output$downloadCSVSpecMeta <- downloadHandler(
    # filename
    filename = function() { paste0(input$saveNameCSVSpecMeta, '.csv') },
    # data to save
    content = function(file) {
        write.csv(outputAnnotationSpectraMetadata_UI_helper(values$annotation), file, row.names=FALSE)
    }
)
output$downloadCSVFeatMeta <- downloadHandler(
    # filename
    filename = function() { paste0(input$saveNameCSVFeatMeta, '.csv') },
    # data to save
    content = function(file) {
        write.csv(outputAnnotationFeatureMetadata_UI_helper(values$annotation), file, row.names=FALSE)
    }
)
output$dlCSVs <- renderUI({
  tagList(
    fluidRow(
    column(6,textInput("saveNameCSVFitParam", label = "Fit parameters", value="fit_parameters")),
    column(6,HTML("<br/>"),downloadButton('downloadCSVFitParam',label=HTML("Download<br/>Fit parameters"), class="btn btn-primary btn-sm"))
    ),
    fluidRow(
    column(6,textInput("saveNameCSVSpecMeta", label = "Files to process", value="files_to_process")),
    column(6,HTML("<br/>"),downloadButton('downloadCSVSpecMeta',label=HTML("Download<br/>Files to process"), class="btn btn-primary btn-sm"))
    ),
    fluidRow(
    column(6,textInput("saveNameCSVFeatMeta", label = "Features metadata", value="features_metadata")),
    column(6,HTML("<br/>"),downloadButton('downloadCSVFeatMeta',label=HTML("Download<br/>Features metadata"), class="btn btn-primary btn-sm"))
    )
  )
})




# -- Export results and diagnostic plots

# UI block for diagnostic and result export only exist if isAnnotated()
output$exportDiagResUI <- renderUI({
  ## not annotated
  if(runSuccess()!='yes') {
    return(
      tagList(
        HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No annotation computed</h4>Annotate a <i>peakPantheRAnnotation</i></div>")#,
      )
    )
  }

  ## is annotated only
  tagList(
    fluidRow(

    # save diagnostic and result control UI
      column(6,
        uiOutput("controlExportPlotUI")
      ),
      column(6,
        uiOutput("controlExportCSVUI")
      ),

    # successful Save UI
      column(6,
        uiOutput("successSavePlot")
      ),
      column(6,
        uiOutput("successSaveCSV")
      ),

    # save targetFolder UI
      column(12,
        div(
          textInput("targetFolderSave", label="Target Folder", value="C:/result_ppR_annotation"),
          helpText("Select a target folder with write permission", style="color:#666666;font-style:italic"),
          align="center"
        )
      ), # end column targetFolder UI

    # button save Diagnostic plots
      column(6,
        div(
          actionButton("save_plot",
            label="Save diagnostic plots",
            class = "btn btn-primary btn-lg"
          ),
          align="center"
        ) # end div
      ),  # end column button save diagnostic plots

    # button save results csv
      column(6,
        div(
          actionButton("save_resultCSV",
            label="Save results",
            class = "btn btn-primary btn-lg"
          ),
          align="center"
        ) # end div
      ) # end column button save results csv
    ) # end fluidRow
  ) # end tagList
})


# UI for diagnostic plots
output$controlExportPlotUI <- renderUI ({
  tagList(
    wellPanel(
      fluidRow(
        column(12,
          h5(HTML(paste0("Number of feature diagnostics to plot: ", nbCompounds(values$annotation))), style="color:#3e648d;font-weight:bold"),
          tags$hr()
        ),
        column(12,
          selectInput("plotMetaSplColrDiag", label="Colour by Sample Metadata", choices=values$spectraMetadataCol)
        ),
        column(12,
          checkboxInput("parallelisationDiag",
            label = p("Parallelisation", style="color:#3e648d;font-weight:bold"),
            value = FALSE
          ),
          uiOutput("cpuSliderDiag")
        )
      ) # end fluidRow
    )  # end wellPanel real Fig ui
  ) # end tagList
})
# Cpu slider appears if parallelisation is selected
output$cpuSliderDiag <- renderUI({
  if(input$parallelisationDiag == 0) return(NULL)
  tagList(
    sliderInput("ncoresDiag", label = paste("Available cores: ",maxCores,sep=""), min = 0, max = maxCores, value = maxCores, step=1)
  )
})
# correction when slider doesn't appear
ncoresInputPlot <- reactive ({
  if( input$parallelisationDiag != 0 ) { input$ncoresDiag }
  else { return(0) }
})

# UI for results CSV
output$controlExportCSVUI <- renderUI ({
  tagList(
    wellPanel(
      fluidRow(
        column(6,
            textInput("projectName", label="Project name", value="ppR-project")
        ), 	# end column
        column(6,
          p("_summary.csv"),
          p("_cpdMetadata.csv"),
          p("_spectraMetadata.csv"),
          p("_found.csv"),
          p("_peakArea.csv"),
          p("_ppm_error.csv"),
          p("...")
        )
      ) # end fluidRow
    )  # end wellPanel
  ) # end tagList
})


## Save diagnostic plots
savePlotOnTrigger <- eventReactive( input$save_plot, {
  outputAnnotationDiagnostic(values$annotation,
                             saveFolder = input$targetFolderSave,
                             savePlots = TRUE,
                             sampleColour = spectra_metadata_colourScheme_UI_helper(values$annotation, input$plotMetaSplColrDiag),
                             ncores = ncoresInputPlot(),
                             verbose = TRUE)
  return(list('done'))
})
# TODO: trigger plots
## Save result csv
saveResOnTrigger <- eventReactive( input$save_resultCSV, {
  outputAnnotationResult(values$annotation,
                         saveFolder = input$targetFolderSave,
                         annotationName = input$projectName,
                         verbose = TRUE)
  return(list('done'))
})
# TODO: trigger results

# If save Plot is successful
output$successSavePlot <- renderUI({
  if( !is.null(savePlotOnTrigger()) ){
    if( is.list(savePlotOnTrigger()) ){
      return(
        HTML("<div class=\"alert alert-dismissible alert-success\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Success</h4><p>Plots saved to disk.</p></div>")
      )
    }
  }
  return(NULL)
}) # TODO: Check message on success plot
# If save Plot is successful
output$successSaveCSV <- renderUI({
  if( !is.null(saveResOnTrigger()) ){
    if( is.list(saveResOnTrigger()) ){
      return(
        HTML("<div class=\"alert alert-dismissible alert-success\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Success</h4><p>Results saved to disk.</p></div>")
      )
    }
  }
  return(NULL)
}) # TODO: Check message on success CSV
