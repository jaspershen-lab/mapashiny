# ── Multi-Omics Step: Results & Report ────────────────────────────────────────

#' @noRd
mod_mo_report_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Multi-Omics  •  Step 7",
    .title       = "Results & Report",
    .subtitle    = paste0(
      "Download your multi-omics results as an HTML report or export the ",
      "integrated modules as a table."
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      mapa_card(
        "Export Options",
        tags$p(
          class = "text-muted small mb-1",
          "Upload the .rda object from clustering (or LLM annotation)",
          "to populate the report."
        ),
        fileInput(
          ns("upload_modules"), NULL,
          accept      = ".rda",
          buttonLabel = "Browse…",
          placeholder = "No file selected"
        ),
        tags$hr(class = "my-2"),
        tags$p(class = "text-muted small",
               "Generate a self-contained HTML report."),
        tags$hr(class = "my-2"),

        actionButton(
          ns("btn_generate"), "Generate Report",
          class = "btn-step-next w-100",
          icon  = icon("file-code")
        ),
        tags$br(), tags$br(),
        uiOutput(ns("download_ui")),

        div(class = "mt-3",
          actionButton(ns("btn_back"),
                       label = tagList(tags$i(class = "fas fa-arrow-left me-1"), "Back"),
                       class = "btn-step-back")
        )
      ),

      mapa_card(
        "Report Preview",
        uiOutput(ns("report_preview"))
      )
    )
  )
}

#' @noRd
mod_mo_report_server <- function(id, annotated_modules, so_data, mo_data, mode,
                                 go_back) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    future::plan(future::sequential)

    report_path <- reactiveVal(NULL)
    report_dir  <- reactiveVal(NULL)

    # ── Upload module result ─────────────────────────────────────────
    observeEvent(input$upload_modules, {
      req(input$upload_modules$datapath)
      tmp <- new.env()
      load(input$upload_modules$datapath, envir = tmp)
      nms <- ls(tmp)
      if (length(nms) == 1) {
        annotated_modules(get(nms[1], envir = tmp))
        report_path(NULL)
        report_dir(NULL)
      } else {
        shinyalert::shinyalert(
          title = "Invalid file content",
          text  = "The .rda file must contain exactly one object.",
          type  = "error", html = TRUE,
          confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── Generate report on button click ─────────────────────────────
    observeEvent(input$btn_generate, {
      obj <- annotated_modules()

      if (is.null(obj)) {
        shinyalert::shinyalert(
          title = "No data available",
          text  = paste0("Complete the previous steps or upload",
                         " an .rda file on the left."),
          type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return()
      }

      tmp_dir <- tempfile(pattern = "mapa_mo_report_")
      dir.create(tmp_dir, recursive = TRUE)

      alert_id <- shinyalert::shinyalert(
        title = "Generating report…",
        text  = tags$div(
          style = "text-align:center;",
          tags$p("This may take a moment.", class = "text-muted small"),
          tags$img(src = "www/spinner.gif", width = "50px", height = "50px",
                   style = "margin-top:10px;")
        ),
        type = "", showConfirmButton = FALSE, showCancelButton = FALSE,
        timer = 0, closeOnEsc = FALSE, closeOnClickOutside = FALSE,
        html = TRUE
      )

      promises::future_promise({
        mapa::report_functional_module(object = obj, path = tmp_dir)
        file.path(tmp_dir, "MAPA_multiomics_report", "index.html")
      }) |>
        promises::then(
          function(html_file) {
            shinyalert::closeAlert(id = alert_id)
            if (is.na(html_file) || !file.exists(html_file)) {
              shinyalert::shinyalert(
                title = "Report generation failed",
                text  = "The HTML report file could not be found.",
                type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
              )
            } else {
              report_path(html_file)
              report_dir(tmp_dir)
            }
          },
          function(err) {
            shinyalert::closeAlert(id = alert_id)
            shinyalert::shinyalert(
              title = "Report generation failed",
              text  = conditionMessage(err),
              type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
            )
          }
        )

      NULL
    })

    # ── Show download button only after report is ready ──────────────
    output$download_ui <- renderUI({
      req(report_path())
      downloadButton(ns("download_report"), "Download Report (.zip)",
                     class = "btn btn-mo w-100")
    })

    # ── Render report in iframe ──────────────────────────────────────
    output$report_preview <- renderUI({
      path <- report_path()
      if (is.null(path)) {
        tags$p(class = "text-muted small mt-2",
               "Upload an .rda file and click Generate Report to preview here.")
      } else {
        content <- paste(readLines(path, warn = FALSE), collapse = "\n")
        tags$iframe(
          srcdoc = content,
          style  = "width:100%; height:800px; border:none;"
        )
      }
    })

    # ── Download the full report folder as a zip archive ────────────
    output$download_report <- downloadHandler(
      filename = function() paste0("mapa_mo_report_", Sys.Date(), ".zip"),
      content  = function(file) {
        dir <- req(report_dir())
        owd <- setwd(dir)
        on.exit(setwd(owd), add = TRUE)
        utils::zip(file, files = "MAPA_multiomics_report")
      }
    )

    observeEvent(input$btn_back, go_back())
  })
}
