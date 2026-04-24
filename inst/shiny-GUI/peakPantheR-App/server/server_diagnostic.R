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
    "plotly_click-diagEIC",
    "plotly_click-rtCorrPlot"))
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
  fail_text <- paste(NROW(values$failures), 'annotation failure(s)')
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
    # Update uROI/FIR section and hotkeys only make sense when the
    # Diagnostic plot sub-tab is active. Conditional on input$diagnosticTabs.
    if (identical(input$diagnosticTabs, "Diagnostic plot")) {
      tagList(
        tags$hr(),
        h5("Update uROI and FIR",
           style = "color:#3e648d;font-weight:bold;margin-bottom:4px;"),
        span("Based on the fit results, updated ROI ",
             shiny::span(em("(uROI)")), " and fallback integration region ",
             shiny::span(em("(FIR)")),
             " can be automatically determined:",
             style = "color:black; font-size:small;"),
        tags$ul(style = "padding-left:18px; color:black; font-size:small;",
          tags$li(tags$code("uROI"),
                  " are established as the min/max (",
                  tags$code("rt"), " and ", tags$code("m/z"),
                  ") of the found peaks (+/- 5% in RT)"),
          tags$li(tags$code("FIR"),
                  " are established as the median of found ",
                  tags$code("rtMin"), ", ", tags$code("rtMax"), ", ",
                  tags$code("mzMin"), ", ", tags$code("mzMax"))
        ),
        actionButton("goDiagnosticUpdateUROIFIR", label = "Update uROI/FIR",
                     class = "btn btn-primary btn-sm btn-block"),
        uiOutput("successUpdateDiagUI"),
        tags$hr(),
        h5("Hotkeys",
           style = "color:#3e648d;font-weight:bold;margin-bottom:4px;"),
        tags$ul(style = "padding-left: 18px; margin-bottom: 0; color: black;",
          tags$li(tags$kbd("a"), " / ", tags$kbd("d"),
                  " — previous / next compound"),
          tags$li(tags$kbd("s"), " — toggle drag-select"),
          tags$li(tags$kbd("space"), " — refit current compound"),
          tags$li(tags$kbd("r"), " — suggest uROI/FIR for current compound"),
          tags$li(tags$kbd("t"), " — set FIR = uROI for current compound"),
          tags$li(tags$kbd("tab"), " — cycle drag target (both / uROI / FIR)")
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
  ann <- values$annotation
  # annotationParamsDiagnostic skips uROI when @uROIExist is TRUE and
  # skips FIR when @useFIR is TRUE.  Reset both so it always recalculates.
  ann@uROIExist <- FALSE
  ann@useFIR    <- FALSE
  updated_annotation <- peakPantheR::annotationParamsDiagnostic(ann,
                                                                verbose = TRUE)
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


## Suggest uROI/FIR for a single compound -------------------------------------
observeEvent(input$suggestUroiFirCpd, {
  values$suggestTrigger <- isolate(values$suggestTrigger) + 1L
})
observeEvent(values$suggestTrigger, ignoreInit = TRUE, {
  if (is.null(input$plotFeatDiag)) return()
  if (!peakPantheR::isAnnotated(values$annotation)) {
    showNotification("Run a full annotation first.", type = "warning")
    return()
  }
  cpdNb <- suppressWarnings(as.numeric(
    names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]))
  if (length(cpdNb) != 1L || is.na(cpdNb) || cpdNb < 1) return()
  tmpAnn <- values$annotation
  tmpAnn@uROIExist <- FALSE
  tmpAnn@useFIR    <- FALSE
  suggested <- peakPantheR::annotationParamsDiagnostic(tmpAnn,
                                                       verbose = FALSE)
  values$rtBackup <- list(
    cpdNb   = cpdNb,
    targets = c("uROI", "FIR"),
    applied = c(NA, NA),
    uROIrow = peakPantheR::uROI(values$annotation)[cpdNb, , drop = FALSE],
    FIRrow  = peakPantheR::FIR(values$annotation)[cpdNb, , drop = FALSE])
  ann <- values$annotation
  ann@uROI[cpdNb, ] <- peakPantheR::uROI(suggested)[cpdNb, ]
  ann@FIR[cpdNb, ]  <- peakPantheR::FIR(suggested)[cpdNb, ]
  ann@uROIExist <- TRUE
  values$annotation <- ann
  showNotification(
    sprintf("Suggested uROI/FIR applied for %s (use 'Reset last edit' to undo)",
            peakPantheR::cpdName(values$annotation)[cpdNb]),
    type = "message")
})

## Copy uROI to FIR for the current compound ----------------------------------
observeEvent(input$firEqualsUroi, {
  values$firEqualsUroiTrigger <- isolate(values$firEqualsUroiTrigger) + 1L
})
observeEvent(values$firEqualsUroiTrigger, ignoreInit = TRUE, {
  if (is.null(input$plotFeatDiag)) return()
  if (!peakPantheR::isAnnotated(values$annotation)) {
    showNotification("Run a full annotation first.", type = "warning")
    return()
  }
  cpdNb <- suppressWarnings(as.numeric(
    names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]))
  if (length(cpdNb) != 1L || is.na(cpdNb) || cpdNb < 1) return()
  ann <- values$annotation
  values$rtBackup <- list(
    cpdNb   = cpdNb,
    targets = "FIR",
    applied = c(NA, NA),
    uROIrow = peakPantheR::uROI(ann)[cpdNb, , drop = FALSE],
    FIRrow  = peakPantheR::FIR(ann)[cpdNb, , drop = FALSE])
  uROIrow <- peakPantheR::uROI(ann)[cpdNb, , drop = FALSE]
  firCols <- names(ann@FIR)
  ann@FIR[cpdNb, ] <- uROIrow[, firCols, drop = FALSE]
  values$annotation <- ann
  showNotification(
    sprintf("FIR set to uROI for %s (use 'Reset last edit' to undo)",
            peakPantheR::cpdName(ann)[cpdNb]),
    type = "message")
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
  n <- length(choices)
  newIdx <- ((idx + step - 1L) %% n) + 1L
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
    values$dragArmed <- !isTRUE(values$dragArmed)
    newMode <- if (isTRUE(values$dragArmed)) "select" else "zoom"
    plotly::plotlyProxyInvoke(
      plotly::plotlyProxy("diagPlot", session),
      "relayout", list(dragmode = newMode))
  }
  if (identical(k, " "))
    values$refitTrigger <- isolate(values$refitTrigger) + 1L
  if (identical(k, "r"))
    values$suggestTrigger <- isolate(values$suggestTrigger) + 1L
  if (identical(k, "t"))
    values$firEqualsUroiTrigger <- isolate(values$firEqualsUroiTrigger) + 1L
  if (identical(k, "tab")) {
    cur <- isolate(input$applyTargets)
    nxt <- if (setequal(cur, c("uROI", "FIR"))) "uROI"
           else if (identical(cur, "uROI"))      "FIR"
           else                                  c("uROI", "FIR")
    updateCheckboxGroupInput(session, "applyTargets", selected = nxt)
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
          "    if (k !== 'a' && k !== 'd' && k !== 's' && k !== 'r' && k !== 't' && k !== ' ' && k !== 'tab') return;",
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
        selectInput("plotMetaSplColr", label="Colour by Sample Metadata",
                    choices=values$spectraMetadataCol,
                    selected = isolate({
                      prev <- input$plotMetaSplColr
                      if (is.null(prev) || !(prev %in% values$spectraMetadataCol))
                        NULL else prev
                    })),
        # Discrete-factor level filter (populated reactively by the
        # plotMetaSplColr observer further down; hidden when the colour
        # column is numeric / None / has too many unique values).
        uiOutput("plotMetaSplLevelsUI")
      ), # end column
      column(3, offset=0,
        numericInput("plotSplNumber",
                     label = paste0("Number of Samples to show (out of ",
                                    nbSamples(values$annotation), ")"),
                     # Default to min(50, n) on first render to avoid
                     # blasting 400+ EICs into the plot; preserve the
                     # previous value when the control strip re-renders
                     # (e.g. after a refit / drag / suggest / FIR=uROI).
                     value = isolate({
                       n <- nbSamples(values$annotation)
                       prev <- input$plotSplNumber
                       if (!is.null(prev) && is.finite(prev) &&
                           prev >= 1 && prev <= n) prev
                       else min(50L, n)
                     }),
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
      ),# end column
      column(3, offset=0,
        radioButtons("plotWindowDiag", label = "X-axis range",
                     choices = c("Full ROI" = "ROI",
                                 "uROI/FIR only" = "uROIFIR"),
                     selected = isolate({
                       prev <- input$plotWindowDiag
                       if (is.null(prev)) "ROI" else prev
                     }),
                     inline = FALSE)
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
    dragmode = isolate(if (isTRUE(values$dragArmed)) "select" else "zoom"),
    plotWindow = if (is.null(input$plotWindowDiag)) "ROI"
                 else input$plotWindowDiag)
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
  tagList(
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
      column(3, offset = 0,
        tags$label("Status"),
        div(rtTxt, style = "font-family: monospace; padding: 2px 0;")
      ),
      column(4, offset = 0,
        div(style = "display: flex; align-items: center; gap: 6px; height: 100%;",
          actionButton("resetDragRtWindow", label = "Reset last edit",
                       class = if (canReset) "btn btn-default btn-sm" else "btn btn-default btn-sm disabled",
                       title = "Revert this compound's uROI/FIR to the values before the last drag-select"),
          actionButton("suggestUroiFirCpd",
                       label = "Suggest uROI/FIR",
                       icon = icon("lightbulb"),
                       class = "btn btn-info btn-sm",
                       title = paste("Compute suggested uROI and FIR for this",
                                     "compound based on found peaks across",
                                     "all samples")),
          actionButton("firEqualsUroi",
                       label = "FIR = uROI",
                       icon = icon("clone"),
                       class = "btn btn-default btn-sm",
                       title = paste("Copy this compound's uROI values",
                                     "into FIR")),
          actionButton("refitCurrentCpd",
                       label = "Refit compound",
                       icon = icon("rotate"),
                       class = "btn btn-primary btn-sm",
                       title = paste("Rerun the peak fit for the compound",
                                     "shown above, across all samples,",
                                     "reusing cached EIC data where possible"))
        )
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
  # Clamp to ROI bounds; skip silently if nothing overlaps
  roi <- peakPantheR::ROI(values$annotation)[cpdNb, , drop = FALSE]
  clampedMin <- max(xmin, roi$rtMin)
  clampedMax <- min(xmax, roi$rtMax)
  if (clampedMin >= clampedMax) return()

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
  clippedNote <- if (xmin != clampedMin || xmax != clampedMax)
    " (clipped to ROI)" else ""
  showNotification(sprintf("Applied rt=[%.3f, %.3f]%s to %s",
                           clampedMin, clampedMax, clippedNote,
                           paste(targets, collapse = " + ")),
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

# Refit just the currently-displayed compound. Dispatches to the
# compound-subset path in peakPantheR_parallelAnnotation(), which reuses
# cached EIC data in @dataPoints via build_parallelAnnotation_cache() and
# overwrites only that compound's rows in @peakTables / @peakFit /
# @dataPoints; all other compounds' results are preserved. Updating
# values$annotation invalidates output$diagPlot, so the plot re-renders
# with the new fit automatically.
observeEvent(input$refitCurrentCpd, {
  values$refitTrigger <- isolate(values$refitTrigger) + 1L
})
observeEvent(values$refitTrigger, ignoreInit = TRUE, {
  if (is.null(input$plotFeatDiag)) return()
  if (!peakPantheR::isAnnotated(values$annotation)) {
    showNotification("Run a full annotation before refitting a single compound.",
                     type = "warning")
    return()
  }
  cpdNbCur <- suppressWarnings(as.numeric(
    names(values$featNmeList)[values$featNmeList == input$plotFeatDiag]))
  if (length(cpdNbCur) != 1L || is.na(cpdNbCur) || cpdNbCur < 1) {
    showNotification("No compound selected.", type = "warning")
    return()
  }
  cpdIDstr <- peakPantheR::cpdID(values$annotation)[cpdNbCur]
  cm <- input$curveModel
  if (is.null(cm)) {
      fits <- peakPantheR::peakFit(values$annotation)
      for (s in fits) { for (f in s) {
          if (is.list(f) && !is.null(f$curveModel)) { cm <- f$curveModel; break }
      }; if (!is.null(cm)) break }
  }
  if (is.null(cm)) cm <- "skewedGaussian"
  res <- tryCatch({
    withProgress(
      message = sprintf("Refitting %s ...", cpdIDstr),
      value = NULL, {
        peakPantheR::peakPantheR_parallelAnnotation(
          values$annotation, compounds = cpdIDstr,
          nCores = 1, getAcquTime = FALSE,
          curveModel = cm, verbose = FALSE)
      })
  }, error = function(e) {
    showNotification(sprintf("Refit failed: %s", conditionMessage(e)),
                     type = "error", duration = 10)
    NULL
  })
  # Helper: clear the compound's fit results so the old (wrong-peak)
  # data does not persist when the refit could not produce new results.
  clearCompoundFit <- function(ann, cpdIdx) {
    nSpl <- peakPantheR::nbSamples(ann)
    for (s in seq_len(nSpl)) {
      pt <- ann@peakTables[[s]]
      if (!is.null(pt) && nrow(pt) >= cpdIdx) {
        pt$found[cpdIdx] <- FALSE
        numCols <- c("rt", "rtMin", "rtMax", "mz", "mzMin", "mzMax",
                      "peakArea", "peakAreaRaw", "maxIntMeasured",
                      "maxIntPredicted", "ppm_error", "rt_dev_sec",
                      "tailingFactor", "asymmetryFactor")
        for (col in intersect(numCols, names(pt)))
          pt[[col]][cpdIdx] <- NA_real_
        if ("is_filled" %in% names(pt))
          pt$is_filled[cpdIdx] <- FALSE
        ann@peakTables[[s]] <- pt
      }
      if (!is.null(ann@peakFit[[s]]) &&
          length(ann@peakFit[[s]]) >= cpdIdx)
        ann@peakFit[[s]][[cpdIdx]] <- NA
    }
    ann
  }
  if (is.null(res) || peakPantheR::nbSamples(res$annotation) == 0L) {
    values$annotation <- clearCompoundFit(values$annotation, cpdNbCur)
    showNotification(
      sprintf("Refit of %s failed; old fit cleared. uROI/FIR preserved.",
              cpdIDstr),
      type = "warning", duration = 10)
    return()
  }
  values$annotation <- res$annotation
  nFail <- if (is.null(res$failures)) 0L else NROW(res$failures)
  if (nFail > 0L) {
    values$failures <- if (is.null(values$failures)) res$failures
                       else rbind(values$failures, res$failures)
  }
  showNotification(sprintf("Refitted %s across %d sample(s) (%d failure(s))",
                           cpdIDstr,
                           peakPantheR::nbSamples(res$annotation),
                           nFail),
                   type = if (nFail == 0L) "message" else "warning")
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


## RT Correction Tab -----------------------------------------------------------

# Control panel: method, polynomial order, RANSAC, references, window width
output$rtCorrControlUI <- renderUI({
  if (!peakPantheR::isAnnotated(values$annotation)) {
    return(wellPanel(
      helpText("Run an annotation first to enable RT correction.",
               style = "color:black")))
  }
  wellPanel(
    fluidRow(
      column(3, offset = 0,
        selectInput("rtCorrMethod", label = "Correction method",
                    choices = c("polynomial", "constant"),
                    selected = isolate({
                      prev <- input$rtCorrMethod
                      if (is.null(prev)) "polynomial" else prev
                    })),
        conditionalPanel(
          condition = "input.rtCorrMethod == 'polynomial'",
          numericInput("rtCorrPolyOrder",
                       label = "Polynomial order",
                       value = isolate({
                         prev <- input$rtCorrPolyOrder
                         if (is.null(prev)) 2L else prev
                       }),
                       min = 1, step = 1)),
        # Reference compound is mandatory for the constant-shift method:
        # applyRTCorrection applies the reference's rt_dev_sec as a single
        # offset, so the reference table must contain exactly one compound.
        conditionalPanel(
          condition = "input.rtCorrMethod == 'constant'",
          uiOutput("rtCorrRefSelectUI"))
      ),
      column(3, offset = 0,
        checkboxInput("rtCorrRobust", label = "Use RANSAC (robust)",
                      value = isolate({
                        prev <- input$rtCorrRobust
                        if (is.null(prev)) FALSE else prev
                      })),
        numericInput("rtCorrWindowWidth",
                     label = "RT window width (sec)",
                     value = isolate({
                       prev <- input$rtCorrWindowWidth
                       if (is.null(prev)) 15 else prev
                     }),
                     min = 1, step = 1)
      ),
      column(3, offset = 0,
        div(style = paste("display:flex; flex-direction:column;",
                          "gap:8px; padding-top:24px;"),
          actionButton("goRtCorrection",
                       label = "Run RT correction",
                       icon = icon("chart-line"),
                       class = "btn btn-primary"),
          actionButton("applyRtCorrection",
                       label = "Apply to annotation",
                       icon = icon("check"),
                       class = if (is.null(values$rtCorrResult))
                         "btn btn-success disabled"
                       else "btn btn-success")
        )
      )
    )
  )
})

# Reference compound picker (only surfaces for the constant method).
# Choices are labelled "cpdID — cpdName" but the value stored is the
# cpdID, which is what rtCorrection_checkInput() expects.
output$rtCorrRefSelectUI <- renderUI({
  if (!peakPantheR::isAnnotated(values$annotation)) return(NULL)
  ids <- peakPantheR::cpdID(values$annotation)
  nms <- peakPantheR::cpdName(values$annotation)
  labels <- ifelse(nzchar(nms), paste0(ids, " — ", nms), ids)
  choices <- stats::setNames(as.list(ids), labels)
  sel <- isolate({
    prev <- input$rtCorrRefs
    if (!is.null(prev) && prev %in% ids) prev else character(0)
  })
  selectizeInput("rtCorrRefs",
                 label = "Reference compound (required)",
                 choices = c("Select a reference" = "", choices),
                 selected = sel,
                 multiple = FALSE,
                 options = list(placeholder = "Pick one compound"))
})

# Run the RT correction
observeEvent(input$goRtCorrection, {
  ann <- values$annotation
  if (!peakPantheR::isAnnotated(ann)) {
    showNotification("Annotation required first.", type = "warning")
    return()
  }
  method <- input$rtCorrMethod
  refs   <- NULL
  if (identical(method, "constant")) {
    refs <- input$rtCorrRefs
    if (is.null(refs) || length(refs) == 0L || !nzchar(refs)) {
      showNotification(paste("The constant-shift method requires a",
                             "single reference compound. Pick one from",
                             "the reference dropdown."),
                       type = "warning", duration = 8)
      return()
    }
  }
  robust <- isTRUE(input$rtCorrRobust)
  rtWW   <- input$rtCorrWindowWidth
  polyOrd <- if (method == "polynomial") {
    input$rtCorrPolyOrder
  } else 1L
  params <- if (method == "polynomial") {
    list(polynomialOrder = polyOrd)
  } else list()

  tryCatch(withProgress(
    message = "Running RT correction...", value = NULL, {
      refs <- peakPantheR:::rtCorrection_checkInput(
        ann, refs, rtWW)
      featInfo <- peakPantheR:::rtCorrection_prepareTargetFeatTable(
        ann, refs)
      targetFeatTable <- featInfo$targetFeatTable
      referenceTable  <- featInfo$referenceTable
      referenceTable  <- referenceTable[
        !is.na(referenceTable$rt_dev_sec), , drop = FALSE]
      if (identical(method, "constant") && nrow(referenceTable) != 1L) {
        stop("the selected reference has no valid rt deviation; ",
             "pick another compound")
      }
      correctedRtTable <- peakPantheR:::peakPantheR_applyRTCorrection(
        targetFeatTable[
          !is.na(targetFeatTable$rt_dev_sec), ,
          drop = FALSE],
        referenceTable, method, params, robust)
      values$rtCorrResult <- list(
        correctedRtTable = correctedRtTable,
        targetFeatTable  = targetFeatTable,
        method = method, params = params,
        robust = robust, rtWindowWidth = rtWW)
      nOutlier <- sum(
        correctedRtTable$isReference == "Reference set outlier")
      showNotification(sprintf(
        "RT correction fitted (%d compounds, %d outliers)",
        nrow(correctedRtTable), nOutlier),
        type = "message")
    }),
    error = function(e) {
      showNotification(
        sprintf("RT correction failed: %s",
                conditionMessage(e)),
        type = "error", duration = 10)
    })
})

# Results panel: plot + summary
output$rtCorrResultUI <- renderUI({
  res <- values$rtCorrResult
  if (is.null(res)) return(NULL)
  ct <- res$correctedRtTable
  nRef <- sum(ct$isReference == "Reference set")
  nOut <- sum(ct$isReference == "Reference set outlier")
  nExt <- sum(ct$isReference == "External set")
  methodTxt <- if (res$method == "polynomial") {
    paste0("polynomial (order ",
           res$params$polynomialOrder, ")")
  } else "constant"
  if (res$robust) methodTxt <- paste0("RANSAC + ", methodTxt)
  tagList(
    div(style = "padding-top: 20px; padding-bottom: 20px;",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("rtCorrPlot", height = 450)
      )
    ),
    wellPanel(
      fluidRow(
        column(12,
          helpText(sprintf(
            paste("Method: %s | References: %d |",
                  "Outliers: %d | External: %d |",
                  "Window width: %.1f sec"),
            methodTxt, nRef, nOut, nExt,
            res$rtWindowWidth),
            style = "color:black"))
      )
    )
  )
})

# Render the interactive RT correction plot
output$rtCorrPlot <- plotly::renderPlotly({
  res <- values$rtCorrResult
  if (is.null(res)) return(plotly::plotly_empty())
  peakPantheR::rtCorrection_interactive_plot_UI_helper(
    res$correctedRtTable, source = "rtCorrPlot")
})

# Apply RT correction to the annotation
observeEvent(input$applyRtCorrection, {
  res <- values$rtCorrResult
  if (is.null(res)) {
    showNotification("Run RT correction first.",
                     type = "warning")
    return()
  }
  ann <- values$annotation
  ct  <- res$correctedRtTable
  tft <- res$targetFeatTable
  rtWW <- res$rtWindowWidth
  # tft rows align positionally with annotation rows at the time the
  # correction was computed. If the annotation has been modified since
  # (e.g. a refit reshaped @cpdID), the stored result no longer maps onto
  # @uROI / @FIR rows and must be re-computed.
  if (!identical(tft$cpdID, peakPantheR::cpdID(ann))) {
    showNotification(
      paste("Annotation has changed since RT correction was computed;",
            "re-run the correction before applying."),
      type = "error", duration = 10)
    return()
  }
  mask <- !is.na(tft$rt_dev_sec)
  ann <- peakPantheR:::rtCorrection_prepareAnnotation(
    ann, tft, rtWW)
  ann@uROI[mask, "rt"]    <- ct$correctedRt
  ann@uROI[mask, "rtMin"] <- ct$correctedRt - (rtWW / 2)
  ann@uROI[mask, "rtMax"] <- ct$correctedRt + (rtWW / 2)
  ann@FIR[mask, "rtMin"]  <- ct$correctedRt - (rtWW / 2)
  ann@FIR[mask, "rtMax"]  <- ct$correctedRt + (rtWW / 2)
  ann@uROIExist <- TRUE
  ann@useUROI   <- TRUE
  ann@useFIR    <- TRUE
  values$annotation <- ann
  showNotification(
    sprintf("RT correction applied to %d compounds.",
            sum(mask)),
    type = "message")
})