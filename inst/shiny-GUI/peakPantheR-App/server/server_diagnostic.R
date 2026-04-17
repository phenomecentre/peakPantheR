## Diagnostic Tab --------------------------------------------------------------

# Pre-register the "diagEIC" source for plotly events so that
# observeEvent(plotly::event_data(..., source="diagEIC")), which is built
# at server startup, before the plot first renders, doesn't warn and
# discard events. event_data() gates on session$userData$plotlyShinyEventIDs
# containing "<source>-<event>" entries; plotly::event_register() populates
# this registry lazily at render time, which races with observer creation.
local({
  # Format required by plotly::event_data() is "<event>-<source>"
  # (see plotly:::event_data: eventID <- paste(event, source, sep = "-"))
  ids <- session$userData$plotlyShinyEventIDs
  session$userData$plotlyShinyEventIDs <- unique(c(ids,
    "plotly_selected-diagEIC",
    "plotly_selecting-diagEIC",
    "plotly_relayout-diagEIC",
    "plotly_brushing-diagEIC",
    "plotly_brushed-diagEIC",
    "plotly_click-diagEIC"))
})


# - message if no import (conditional panel)
# - status of the annotation (and failures) on the sidebar

# - [TAB] fitting summary (%tage found, %tage filled)
# - [TAB] automatic update uROI/FIR
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
  tmp_text  <- peakPantheR::annotation_showText_UI_helper(peakPantheR::annotation_showMethod_UI_helper(values$annotation))
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
    },
    # Hotkeys only apply (and only make sense) when the Diagnostic plot
    # sub-tab is active. Conditional on input$diagnosticTabs.
    if (identical(input$diagnosticTabs, "Diagnostic plot")) {
      tagList(
        tags$hr(),
        h5("Hotkeys",
           style = "color:#3e648d;font-weight:bold;margin-bottom:4px;"),
        tags$ul(style = "padding-left: 18px; margin-bottom: 0; color: black;",
          tags$li(tags$kbd("a"), " / ", tags$kbd("d"),
                  " — previous / next compound"),
          tags$li(tags$kbd("s"), " — toggle drag-select")
        )
      )
    }
  )
})


## Annotation fitting statistics------------------------------------------------
# render annotation fit statistic table
output$table_fit_stat <- DT::renderDT ({
  DT::datatable(data = annotation_fit_summary_UI_helper(values$annotation),
                options  = list(orderClasses = TRUE),
                rownames = TRUE)
})

# UI block annotation fit statistic table
output$annotationStatisticsTable <- renderUI ({
  fluidRow(
    column(width = 12, offset = 0,
      DT::DTOutput("table_fit_stat")
    )
  )
})


## Automatic update uROI/FIR ---------------------------------------------------
## Run the update to uROI/FIR
observeEvent(input$goDiagnosticUpdateUROIFIR, {
  updated_annotation  <- peakPantheR::annotationParamsDiagnostic(values$annotation, verbose=TRUE)
  values$annotation <- updated_annotation
})

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

# Success update UROI/FIR using diagnostic message
output$successUpdateDiagUI <- renderUI({
  if(diagSuccess()=='yes') {
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>uROI and FIR updated sucessfully</div>")
    )
  }
})


## Sidebar Settings tab --------------------------------------------------------
output$diagSettingsUI <- renderUI({
  wellPanel(
    h4("Settings", style = "color:#3e648d;font-weight:bold"),
    numericInput("plotHeightDiag", label = "Plot Height",
                 value = isolate(if (is.null(input$plotHeightDiag)) 400
                                 else input$plotHeightDiag),
                 min = 0, step = 1)
  )
})


## Plot Diagnostic -------------------------------------------------------------
# Step through compounds by a +/-1 offset in the selectInput choices list.
stepDiagFeature <- function(step) {
  choices <- unname(values$featNmeList)
  if (is.null(choices) || length(choices) == 0) return()
  cur <- isolate(input$plotFeatDiag)
  idx <- match(cur, choices)
  if (is.na(idx)) idx <- 1L
  newIdx <- idx + step
  if (newIdx < 1L || newIdx > length(choices)) return()
  updateSelectInput(session, "plotFeatDiag", selected = choices[newIdx])
}
observeEvent(input$prevFeatDiag, { stepDiagFeature(-1L) })
observeEvent(input$nextFeatDiag, { stepDiagFeature( 1L) })
observeEvent(input$diagKey, {
  k <- input$diagKey$key
  if (is.null(k)) return()
  k <- tolower(k)
  if (identical(k, "a")) stepDiagFeature(-1L)
  if (identical(k, "d")) stepDiagFeature( 1L)
  if (identical(k, "s")) {
    # toggle drag-select, matching the Enable/Disable button's behaviour
    values$dragArmed <- !isTRUE(values$dragArmed)
    newMode <- if (isTRUE(values$dragArmed)) "select" else "zoom"
    plotly::plotlyProxyInvoke(
      plotly::plotlyProxy("diagPlot", session),
      "relayout", list(dragmode = newMode))
  }
})

# UI to control the diagnostic plot
output$diagPlotControlUI <- renderUI({
  wellPanel(
    fluidRow(
      column(3, offset=0,
        selectInput("plotFeatDiag", label = "Feature",
                    choices = unname(values$featNmeList),
                    selected = isolate(input$plotFeatDiag)),
        div(style = "margin-top: -10px;",
          actionButton("prevFeatDiag", label = NULL,
                       icon = icon("chevron-left"),
                       class = "btn btn-default btn-sm",
                       title = "Previous compound (a)"),
          actionButton("nextFeatDiag", label = NULL,
                       icon = icon("chevron-right"),
                       class = "btn btn-default btn-sm",
                       title = "Next compound (d)")
        ),
        # Listener is installed once (idempotent via window flag) — inside
        # renderUI it would re-register on every re-render and stack.
        # Uses capture phase + stopImmediatePropagation so Bootstrap's
        # keydown.bs.tab handler never runs.
        tags$script(HTML(
          "if (!window.__peakPantheRDiagKeysBound) {",
          "  window.__peakPantheRDiagKeysBound = true;",
          "  document.addEventListener('keydown', function(e) {",
          "    var k = (e.key || '').toLowerCase();",
          "    if (k !== 'a' && k !== 'd' && k !== 's') return;",
          "    if (e.ctrlKey || e.metaKey || e.altKey) return;",
          "    var tag = (e.target.tagName || '').toUpperCase();",
          "    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;",
          "    if (e.target.isContentEditable) return;",
          "    e.preventDefault();",
          "    e.stopPropagation();",
          "    e.stopImmediatePropagation();",
          "    Shiny.setInputValue('diagKey',",
          "      {key: k, t: Date.now()}, {priority: 'event'});",
          "  }, true);",
          "}"))
      ), # end column
      column(3, offset=0,
        selectInput("plotMetaSplColr", label="Colour by Sample Metadata", choices=values$spectraMetadataCol),
        # Discrete-factor level filter (populated reactively by the
        # plotMetaSplColr observer further down; hidden when the colour
        # column is numeric / None / has too many unique values).
        uiOutput("plotMetaSplLevelsUI")
      ), # end column
      column(3, offset=0,
        numericInput("plotSplNumber", label = paste0("Number of Samples to show (out of ",
                                                     nbSamples(values$annotation), ")"),
                     value = nbSamples(values$annotation),
                     min = 1,
                     max = nbSamples(values$annotation),
                     step = 1),
        radioButtons("plotSplMode", label = NULL,
                     choices = c("Sequential" = "sequential",
                                 "Random" = "random"),
                     selected = isolate({
                       prev <- input$plotSplMode
                       if (is.null(prev)) "sequential" else prev
                     }),
                     inline = TRUE)
      )# end column
    ),  # end fluidRow
    # Drag-edit controls live inside the same wellPanel so the whole
    # control strip reads as one block (thin divider between the two rows).
    tags$hr(style = "margin: 10px 0;"),
    uiOutput("diagDragEditInner")
  )     # end wellPanel
})

# Discrete-factor filter: if the selected colour column is a factor or a
# character/logical with a small number of unique values, expose a
# pickerInput-style dropdown of levels that restricts which samples are
# plotted. For numeric columns (or high-cardinality) the UI is hidden.
diagLevelsInfo <- reactive({
  col <- input$plotMetaSplColr
  if (is.null(col) || col == "None" || col == "") return(NULL)
  if (is.null(values$annotation)) return(NULL)
  meta <- peakPantheR::spectraMetadata(values$annotation)
  if (!(col %in% colnames(meta))) return(NULL)
  v <- meta[[col]]
  # Treat as discrete: factor, character, logical, or numeric with <= 12 lvls
  isDiscrete <- is.factor(v) || is.character(v) || is.logical(v) ||
    (length(unique(v)) <= 12L)
  if (!isDiscrete) return(NULL)
  lvls <- if (is.factor(v)) levels(v) else sort(unique(as.character(v)))
  lvls <- lvls[!is.na(lvls)]
  if (length(lvls) == 0) return(NULL)
  list(col = col, levels = lvls)
})

# Widget tiering thresholds for the level-filter control.
.LEVELS_CHECKBOX_MAX <- 15L   # <= -> checkboxGroupInput
.LEVELS_SELECTIZE_MAX <- 200L # <= -> selectizeInput(multiple = TRUE)
                              # >   -> filter disabled (info msg only)

output$plotMetaSplLevelsUI <- renderUI({
  info <- diagLevelsInfo()
  if (is.null(info)) return(NULL)
  lvls <- info$levels
  n <- length(lvls)
  # Reuse previous selection when it is still valid (column unchanged),
  # otherwise default to all levels selected.
  sel <- isolate({
    prev <- input$plotMetaSplLevels
    if (is.null(prev) || !all(prev %in% lvls)) lvls else prev
  })
  if (n <= .LEVELS_CHECKBOX_MAX) {
    checkboxGroupInput("plotMetaSplLevels",
                       label = paste0("Restrict to levels (", n, ")"),
                       choices = lvls, selected = sel, inline = TRUE)
  } else if (n <= .LEVELS_SELECTIZE_MAX) {
    selectizeInput("plotMetaSplLevels",
                   label = paste0("Restrict to levels (", n,
                                  " — type to filter)"),
                   choices = lvls, selected = sel, multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  } else {
    # too many levels to offer a per-level filter usefully
    tagList(
      tags$label(paste0("Level filter disabled (", n, " levels)")),
      helpText(paste("This metadata column has too many unique values to",
                     "filter individually. All samples are plotted —",
                     "choose a lower-cardinality column to restrict them."))
    )
  }
})

# plot feature diagnostic (interactive plotly)
# NOTE: dragmode is not a reactive dep here as toggling drag-select must not
# trigger a full plot re-render.
# The arm/disarm observer switches dragmode via plotlyProxy instead.
output$diagPlot <- plotly::renderPlotly({
  # find the cpdNb corresponding to the cpdID + cpdName shown
  info <- diagLevelsInfo()
  splFilterCol <- if (is.null(info)) NULL else info$col
  splFilterLevels <- if (is.null(info)) NULL else input$plotMetaSplLevels
  plt <- peakPantheR::annotation_diagnostic_multiplot_UI_helper_interactive(
    cpdNb = as.numeric(names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]),
    annotation = values$annotation,
    splNum = input$plotSplNumber,
    splColrColumn = input$plotMetaSplColr,
    splMode = if (is.null(input$plotSplMode)) "sequential" else input$plotSplMode,
    splFilterCol = splFilterCol,
    splFilterLevels = splFilterLevels,
    source = "diagEIC",
    dragmode = isolate(if (isTRUE(values$dragArmed)) "select" else "zoom"))
  # Re-register in the render context so plotly's session-scoped source
  # registry (consulted by event_data) is populated at render time.
  if (!is.null(plt)) {
    plt$x$source <- "diagEIC"
    plt <- plotly::event_register(plt, "plotly_selected")
    plt <- plotly::event_register(plt, "plotly_selecting")
  }
  plt
})

# drag-select edit controls: toggle + target picker grouped on the left,
# last-edit read-out + Reset on the right. Edits auto-apply on commit.
# Renders *inside* the diagPlotControlUI wellPanel
# so the full control strip reads as one visual block.
output$diagDragEditInner <- renderUI({
  if (is.null(input$plotFeatDiag) ||
      !peakPantheR::isAnnotated(values$annotation) ||
      (as.numeric(names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]) == 0)) {
    return(NULL)
  }
  bkp <- values$rtBackup
  cpdNbCur <- suppressWarnings(as.numeric(
    names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]))
  rtTxt <- if (is.null(bkp) || !identical(bkp$cpdNb, cpdNbCur)) {
    "(none — enable drag-select [s] and drag a rectangle on the plot)"
  } else {
    sprintf("Last applied: rt=[%.3f, %.3f] to %s",
            bkp$applied[1], bkp$applied[2],
            paste(bkp$targets, collapse = " + "))
  }
  armLabel <- if (isTRUE(values$dragArmed)) "Disable drag-select (s)" else "Enable drag-select (s)"
  armClass <- if (isTRUE(values$dragArmed)) "btn btn-warning" else "btn btn-default"
  canReset <- !is.null(bkp) && identical(bkp$cpdNb, cpdNbCur)
  fluidRow(
    column(5, offset = 0,
      div(style = "display: flex; align-items: center; gap: 16px;",
        actionButton("armDragSelect", label = armLabel, class = armClass),
        # wrap checkbox label+inputs so vertical rhythm matches the button
        div(style = "margin-top: -6px;",
          checkboxGroupInput("applyTargets", label = "Apply to",
                             choices = c("uROI", "FIR"),
                             selected = isolate({
                               prev <- input$applyTargets
                               if (is.null(prev)) c("uROI", "FIR") else prev
                             }),
                             inline = TRUE))
      )
    ),
    column(5, offset = 0,
      tags$label("Status"),
      div(rtTxt, style = "font-family: monospace; padding: 2px 0;")
    ),
    column(2, offset = 0,
      div(style = "display: flex; align-items: center; height: 100%;",
        actionButton("resetDragRtWindow", label = "Reset last edit",
                     class = if (canReset) "btn btn-default" else "btn btn-default disabled",
                     title = "Revert this compound's uROI/FIR to the values before the last drag-select")
      )
    )
  )
})

# toggle drag-select mode; update dragmode client-side via plotlyProxy
# so the plot is not re-rendered.
observeEvent(input$armDragSelect, {
  values$dragArmed <- !isTRUE(values$dragArmed)
  newMode <- if (isTRUE(values$dragArmed)) "select" else "zoom"
  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy("diagPlot", session),
    "relayout", list(dragmode = newMode))
})

# Commit a freshly-drawn selection: back up the current uROI/FIR rows for
# the active compound, write the new rt window to the selected targets,
# then disarm and clear plotly's selection overlay.
commitDragSelection <- function(xmin, xmax) {
  if (!is.finite(xmin) || !is.finite(xmax) || xmin >= xmax) return()
  if (is.null(input$plotFeatDiag)) return()
  cpdNb <- suppressWarnings(as.numeric(
    names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]))
  if (length(cpdNb) != 1L || is.na(cpdNb) || cpdNb < 1) return()
  targets <- input$applyTargets
  if (length(targets) < 1L) {
    showNotification("Tick at least one target (uROI or FIR) before dragging.",
                     type = "warning")
    return()
  }
  # Backup before modifying so Reset can restore
  values$rtBackup <- list(
    cpdNb = cpdNb,
    targets = targets,
    applied = c(xmin, xmax),
    uROIrow = peakPantheR::uROI(values$annotation)[cpdNb, , drop = FALSE],
    FIRrow  = peakPantheR::FIR(values$annotation)[cpdNb, , drop = FALSE])
  values$annotation <- peakPantheR::apply_rt_window_to_annotation(
    annotation = values$annotation,
    cpdNb = cpdNb, rt = c(xmin, xmax), targets = targets)
  values$dragArmed <- FALSE
  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy("diagPlot", session),
    "relayout", list(dragmode = "zoom", selections = list()))
  showNotification(sprintf("Applied rt=[%.3f, %.3f] to %s",
                           xmin, xmax, paste(targets, collapse = " + ")),
                   type = "message")
}

# plotly_selected selects x axes when points fall inside the drag rectangle.
observeEvent(plotly::event_data("plotly_selected", source = "diagEIC"), {
  sel <- plotly::event_data("plotly_selected", source = "diagEIC")
  if (is.null(sel)) return()
  n <- NROW(sel)
  if (is.na(n) || n < 1L) return()
  if (!"x" %in% names(sel)) return()
  xs <- suppressWarnings(as.numeric(sel$x))
  xs <- xs[is.finite(xs)]
  if (length(xs) < 2L) return()
  commitDragSelection(min(xs), max(xs))
})

# plotly_relayout carries the exact rectangle via `selections` data.frame
# (modern persistent-selections API).
observeEvent(plotly::event_data("plotly_relayout", source = "diagEIC"), {
  d <- plotly::event_data("plotly_relayout", source = "diagEIC")
  if (is.null(d)) return()
  sels <- d$selections
  if (is.null(sels)) return()
  if (is.data.frame(sels) && nrow(sels) >= 1L &&
      all(c("x0", "x1") %in% names(sels))) {
    x0 <- suppressWarnings(as.numeric(sels$x0[1]))
    x1 <- suppressWarnings(as.numeric(sels$x1[1]))
    if (all(is.finite(c(x0, x1)))) {
      commitDragSelection(min(x0, x1), max(x0, x1))
    }
  }
})

# Reset last edit for the currently-displayed compound.
observeEvent(input$resetDragRtWindow, {
  bkp <- values$rtBackup
  if (is.null(bkp)) return()
  cpdNbCur <- suppressWarnings(as.numeric(
    names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]))
  if (!identical(bkp$cpdNb, cpdNbCur)) {
    showNotification("Backup is for a different compound.", type = "warning")
    return()
  }
  ann <- values$annotation
  # Restore the full uROI + FIR rows stashed pre-edit
  ann@uROI[bkp$cpdNb, ] <- bkp$uROIrow
  ann@FIR[bkp$cpdNb, ]  <- bkp$FIRrow
  values$annotation <- ann
  values$rtBackup <- NULL
  showNotification("Reverted uROI/FIR to pre-edit values.", type = "message")
})

# UI block with plot feature diagnostic
output$diagPlotResultUI <- renderUI ({
  # must be annotated and a feature selected
  if (is.null(input$plotFeatDiag)) {
    h5('Nothing to plot!')
  } else if (peakPantheR::isAnnotated(values$annotation) & (as.numeric(names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]) != 0) ){
    div(style = "padding-top: 20px; padding-bottom: 20px;",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("diagPlot", height=input$plotHeightDiag)
      )
    )
  } else {
    h5('Nothing to plot!')
  }
})