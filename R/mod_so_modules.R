# ── Step SO-4: Module Identification (Single-Omics) ──────────────────────────

#' @noRd
mod_so_modules_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Single-Omics  •  Step 4",
    .title       = "Module Identification",
    .subtitle    = "Cluster enriched pathways into functional modules.",

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      mapa_card(
        "Clustering Parameters",
        uiOutput(ns("carryover_ui")),
        optional_upload_caption("Step 3 · Pathway Similarity Computation"),
        fileInput(ns("upload_similarity"),
                  "Upload similarity result (.rda)",
                  accept = ".rda"),
        tags$hr(class = "my-2"),
        numericInput(ns("sim_cutoff"), "Similarity cutoff",
                     value = 0.5, min = 0.1, max = 0.95, step = 0.05),
        selectInput(ns("cluster_method"), "Clustering method",
                    choices = c(
                      "Louvain"               = "louvain",
                      "Walktrap"              = "walktrap",
                      "Infomap"               = "infomap",
                      "Edge betweenness"      = "edge_betweenness",
                      "Fast greedy"           = "fast_greedy",
                      "Label propagation"     = "label_prop",
                      "Leading eigenvector"   = "leading_eigen",
                      "Optimal"               = "optimal",
                      "Binary cut"            = "binary_cut",
                      "Hierarchical – Ward.D"    = "h_ward.D",
                      "Hierarchical – Ward.D2"   = "h_ward.D2",
                      "Hierarchical – Single"    = "h_single",
                      "Hierarchical – Complete"  = "h_complete",
                      "Hierarchical – Average"   = "h_average",
                      "Hierarchical – McQuitty"  = "h_mcquitty",
                      "Hierarchical – Median"    = "h_median",
                      "Hierarchical – Centroid"  = "h_centroid"
                    ),
                    selected = "louvain"),
        checkboxInput(ns("assess_quality"), "Assess clustering quality", value = FALSE),
        step_nav_buttons(ns, next_label = "Identify Modules",
                         next_arrow = FALSE, show_code = TRUE),
        uiOutput(ns("proceed_ui"))
      ),

      mapa_card(
        "Module Results",
        uiOutput(ns("dl_rda_top")),
        uiOutput(ns("module_status")),
        tags$br(),
        uiOutput(ns("result_tabs_ui"))
      )
    )
  )
}

#' @noRd
mod_so_modules_server <- function(id, so_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    module_result <- reactiveVal(NULL)
    assess_result <- reactiveVal(NULL)
    module_plot   <- reactiveVal(NULL)
    module_plot_d <- reactiveVal(NULL)
    module_code   <- reactiveVal(NULL)

    # ── Disable Run button until similarity result is present ─────────
    observe({
      if (is.null(so_data$similarity_result)) shinyjs::disable("btn_next")
      else                                     shinyjs::enable("btn_next")
    })

    # ── Carry-over banner (is Step 3's result already in the session?) ─
    output$carryover_ui <- renderUI({
      carryover_status(
        !is.null(so_data$similarity_result),
        "Step 3 · Pathway Similarity Computation",
        ready_detail = "The similarity result was carried over from the previous step."
      )
    })

    # ── .rda download button above the status banner ──────────────────
    output$dl_rda_top <- renderUI({
      req(module_result())
      downloadButton(
        ns("dl_module_object_top"),
        "Functional modules (.rda)",
        class = "btn-mapa-dl mb-2"
      )
    })

    output$dl_module_object_top <- downloadHandler(
      filename = "enriched_functional_module.rda",
      content  = function(file) {
        enriched_functional_module <- module_result()
        save(enriched_functional_module, file = file)
      }
    )

    # ── Show reproducible R code ──────────────────────────────────────
    observeEvent(input$btn_code, {
      if (is.null(module_code())) {
        shinyalert::shinyalert(
          title = "No code yet",
          text  = "Run module identification first to view the reproducible code.",
          type  = "warning", confirmButtonCol = "#dd4b39"
        )
        return()
      }
      show_code_modal(module_code())
    })

    # ── Upload similarity result ─────────────────────────────────────
    observeEvent(input$upload_similarity, {
      req(input$upload_similarity$datapath)
      tmp <- new.env()
      load(input$upload_similarity$datapath, envir = tmp)
      nms <- ls(tmp)
      if (length(nms) == 1) {
        so_data$similarity_result <- get(nms[1], envir = tmp)
      } else {
        shinyalert::shinyalert(
          title = "Invalid file content",
          text  = "The .rda file must contain exactly one object.",
          type  = "error", html = TRUE,
          confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── Status banner ────────────────────────────────────────────────
    output$module_status <- renderUI({
      res <- module_result()
      if (is.null(res))
        return(status_alert(
          "Configure parameters and click Identify Modules.", "info"
        ))
      n <- tryCatch(
        length(unique(
          res@merged_module$functional_module_result$module
        )),
        error = function(e) NA_integer_
      )
      if (is.na(n))
        status_alert(
          "Modules identified — check the tabs for results.", "success"
        )
      else
        status_alert(paste0(n, " functional modules identified."), "success")
    })

    # ── Result tabs ──────────────────────────────────────────────────
    output$result_tabs_ui <- renderUI({
      req(module_result())
      tab_panels <- list(
        bslib::nav_panel(
          "Table",
          DT::DTOutput(ns("module_table")),
          tags$div(
            class = "mt-2",
            downloadButton(ns("dl_module_table"), "Download (.csv)",
                           class = "btn-mapa-dl")
          )
        ),
        bslib::nav_panel(
          "Network Plot",
          uiOutput(ns("network_plot_controls")),
          plotOutput(ns("module_plot"), width = "100%", height = "650px"),
          tags$div(
            class = "mt-2",
            downloadButton(ns("dl_module_plot"), "Download (.pdf)",
                           class = "btn-mapa-dl")
          )
        ),
        bslib::nav_panel(
          "R Object",
          verbatimTextOutput(ns("module_object"))
        )
      )

      if (!is.null(assess_result())) {
        tab_panels <- c(tab_panels, list(
          bslib::nav_panel(
            "Module Size",
            plotOutput(ns("size_plot"), height = "600px"),
            tags$div(
              class = "mt-2 d-flex gap-2 align-items-center",
              numericInput(ns("size_w"), "Width",
                           value = 8, min = 4, max = 30, width = "100px"),
              numericInput(ns("size_h"), "Height",
                           value = 6, min = 4, max = 30, width = "100px"),
              downloadButton(ns("dl_size_plot"), "Download (.pdf)",
                             class = "btn-mapa-dl")
            )
          ),
          bslib::nav_panel(
            "Silhouette Scores",
            plotOutput(ns("eval_plot"), height = "500px"),
            tags$div(
              class = "mt-2 d-flex gap-2 align-items-center",
              numericInput(ns("eval_w"), "Width",
                           value = 8, min = 4, max = 30, width = "100px"),
              numericInput(ns("eval_h"), "Height",
                           value = 6, min = 4, max = 30, width = "100px"),
              downloadButton(ns("dl_eval_plot"), "Download (.pdf)",
                             class = "btn-mapa-dl")
            )
          ),
          bslib::nav_panel(
            "Quality Metrics",
            DT::DTOutput(ns("quality_table")),
            tags$div(
              class = "mt-2",
              downloadButton(ns("dl_quality_table"), "Download (.csv)",
                             class = "btn-mapa-dl")
            )
          )
        ))
      }

      do.call(bslib::navset_tab, c(tab_panels, list(id = ns("result_tabs"))))
    })

    # ── Network plot controls ────────────────────────────────────────
    output$network_plot_controls <- renderUI({
      bslib::layout_columns(
        col_widths = c(3, 2, 3, 4),
        fill = FALSE,
        numericInput(ns("degree_cutoff"), "Degree cutoff",
                     value = 1, min = 0, max = 1000),
        tags$div(
          class = "mt-4 pt-1",
          checkboxInput(ns("plot_text"), "Labels", FALSE)
        ),
        tags$div(
          class = "mt-4 pt-1",
          checkboxInput(ns("plot_text_all"), "All labels", FALSE)
        ),
        tags$div(
          class = "mt-4 pt-1",
          actionButton(ns("btn_gen_plot"), "Generate plot",
                       class = "btn btn-sm btn-outline-primary w-100")
        )
      )
    })

    # ── Proceed button (shown after success) ─────────────────────────
    output$proceed_ui <- renderUI({
      req(module_result())
      actionButton(
        ns("btn_proceed"),
        tagList("Proceed to Next Step",
                tags$i(class = "fas fa-arrow-right ms-1")),
        class = "btn-step-next w-100 mt-2"
      )
    })

    observeEvent(input$btn_proceed, go_next())

    # ── Run Module Identification ────────────────────────────────────
    observeEvent(input$btn_next, {
      req(so_data$similarity_result)

      alert_id <- shinyalert::shinyalert(
        title = "Identifying modules...",
        text  = tags$div(
          style = "text-align:center;",
          tags$p("This may take several minutes.", class = "text-muted small"),
          tags$img(src   = "www/spinner.gif",
                   width = "50px", height = "50px",
                   style = "margin-top:10px;")
        ),
        type                = "",
        showConfirmButton   = FALSE,
        showCancelButton    = FALSE,
        timer               = 0,
        closeOnEsc          = FALSE,
        closeOnClickOutside = FALSE,
        html                = TRUE
      )

      result <- tryCatch({
        mapa::get_functional_modules(
          object         = so_data$similarity_result,
          sim.cutoff     = input$sim_cutoff,
          cluster_method = input$cluster_method,
          save_to_local  = FALSE
        )
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          title = "Module identification failed",
          text  = e$message,
          type  = "error",
          html  = TRUE,
          confirmButtonCol = "#dd4b39"
        )
        NULL
      })

      shinyalert::closeAlert(id = alert_id)

      if (!is.null(result)) {
        module_result(result)
        module_plot(NULL)
        module_plot_d(NULL)
        so_data$functional_modules <- result
        module_code(sprintf(
          "enriched_functional_module <- mapa::get_functional_modules(\n  object = similarity_result,\n  sim.cutoff = %s,\n  cluster_method = \"%s\",\n  save_to_local = FALSE\n)",
          input$sim_cutoff, input$cluster_method))

        if (isTRUE(input$assess_quality)) {
          assess_id <- shinyalert::shinyalert(
            title = "Assessing clustering quality...",
            text  = tags$div(
              style = "text-align:center;",
              tags$img(src   = "www/spinner.gif",
                       width = "50px", height = "50px",
                       style = "margin-top:10px;")
            ),
            type                = "",
            showConfirmButton   = FALSE,
            showCancelButton    = FALSE,
            timer               = 0,
            closeOnEsc          = FALSE,
            closeOnClickOutside = FALSE,
            html                = TRUE
          )

          aq <- tryCatch({
            mapa::assess_clustering_quality(object = result)
          }, error = function(e) {
            shinyalert::closeAlert(id = assess_id)
            shinyalert::shinyalert(
              title = "Quality assessment failed",
              text  = e$message,
              type  = "error",
              html  = TRUE,
              confirmButtonCol = "#dd4b39"
            )
            NULL
          })

          shinyalert::closeAlert(id = assess_id)
          assess_result(aq)
        }

        shinyalert::shinyalert(
          title = "Module identification complete",
          text  = "Explore results in the tabs on the right.",
          type  = "success",
          html  = TRUE,
          confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── Generate network plot ────────────────────────────────────────
    observeEvent(input$btn_gen_plot, {
      req(module_result())

      plot_id <- shinyalert::shinyalert(
        title = "Generating plot...",
        text  = tags$div(
          style = "text-align:center;",
          tags$img(src   = "www/spinner.gif",
                   width = "50px", height = "50px",
                   style = "margin-top:10px;")
        ),
        type                = "",
        showConfirmButton   = FALSE,
        showCancelButton    = FALSE,
        timer               = 0,
        closeOnEsc          = FALSE,
        closeOnClickOutside = FALSE,
        html                = TRUE
      )

      tryCatch({
        p <- mapa::plot_similarity_network(
          object        = module_result(),
          level         = "functional_module",
          degree_cutoff = input$degree_cutoff,
          text          = isTRUE(input$plot_text),
          text_all      = isTRUE(input$plot_text_all)
        ) + ggplot2::theme(aspect.ratio = 1)

        module_plot(p)
        module_plot_d(p + ggplot2::guides(fill = "none"))

        shinyalert::closeAlert(id = plot_id)
      }, error = function(e) {
        shinyalert::closeAlert(id = plot_id)
        shinyalert::shinyalert(
          title = "Plot generation failed",
          text  = e$message,
          type  = "error",
          html  = TRUE,
          confirmButtonCol = "#dd4b39"
        )
      })
    })

    # ── Render outputs ───────────────────────────────────────────────
    output$module_table <- DT::renderDT({
      req(module_result())
      df <- tryCatch(
        module_result()@merged_module$functional_module_result,
        error = function(e) NULL
      )
      req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    output$module_object <- renderText({
      req(module_result())
      obj <- module_result()
      out <- c(capture.output(obj, type = "message"),
               capture.output(obj, type = "output"))
      paste(out, collapse = "\n")
    })

    output$module_plot <- renderPlot({
      req(module_plot_d())
      module_plot_d()
    }, res = 96)

    output$size_plot <- renderPlot({
      req(assess_result())
      assess_result()$size_plot
    }, res = 96)

    output$eval_plot <- renderPlot({
      req(assess_result())
      assess_result()$evaluation_plot +
        ggplot2::guides(fill = "none")
    }, res = 96)

    output$quality_table <- DT::renderDT({
      req(assess_result())
      df <- tryCatch(assess_result()$quality_metrics, error = function(e) NULL)
      req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # ── Download handlers ────────────────────────────────────────────
    output$dl_module_table <- downloadHandler(
      filename = "functional_module_result.csv",
      content  = function(file) {
        df <- module_result()@merged_module$functional_module_result
        write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_module_plot <- downloadHandler(
      filename = "functional_module_network.pdf",
      content  = function(file) {
        p <- module_plot()
        req(p)
        ggplot2::ggsave(file, plot = p, width = 10, height = 10)
      }
    )

    output$dl_size_plot <- downloadHandler(
      filename = "module_size_plot.pdf",
      content  = function(file) {
        req(assess_result())
        ggplot2::ggsave(file,
                        plot   = assess_result()$size_plot,
                        width  = input$size_w,
                        height = input$size_h)
      }
    )

    output$dl_eval_plot <- downloadHandler(
      filename = "silhouette_plot.pdf",
      content  = function(file) {
        req(assess_result())
        ggplot2::ggsave(file,
                        plot   = assess_result()$evaluation_plot,
                        width  = input$eval_w,
                        height = input$eval_h)
      }
    )

    output$dl_quality_table <- downloadHandler(
      filename = "clustering_quality_metrics.csv",
      content  = function(file) {
        write.csv(assess_result()$quality_metrics, file, row.names = FALSE)
      }
    )

    observeEvent(input$btn_back, go_back())
  })
}
