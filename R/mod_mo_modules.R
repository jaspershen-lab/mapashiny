# ── Step MO-3: Module Identification (Multi-Omics) ───────────────────────────

#' @noRd
mod_mo_modules_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Multi-Omics  •  Step 3",
    .title       = "Module Identification",
    .subtitle    = "Cluster genes, metabolites and pathways together into integrated functional modules.",

    # ── Upload card (full width, above parameters) ──
    mapa_card(
      "Upload Network Results",
      status_alert(
        "Upload network outputs (.rda) saved from Step 3 to skip re-building the network.",
        "info"
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        gap = "1rem",
        fileInput(ns("upload_mnet"),
                  "Network object (.rda)",
                  accept = ".rda", buttonLabel = "Browse…"),
        fileInput(ns("upload_sim"),
                  "Similarity matrix (.rda)",
                  accept = ".rda", buttonLabel = "Browse…")
      )
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      tagList(
        mapa_card(
          "Clustering Parameters",
          sliderInput(ns("sim_cutoff"), "Similarity cutoff",
                      min = 0.1, max = 0.9, value = 0.45, step = 0.05),
          selectInput(ns("cluster_method"), "Clustering method",
                      choices = c(
                        # Graph-based
                        "Louvain"          = "louvain",
                        "Walktrap"         = "walktrap",
                        "Infomap"          = "infomap",
                        "Edge betweenness" = "edge_betweenness",
                        "Fast greedy"      = "fast_greedy",
                        "Label propagation" = "label_prop",
                        "Leading eigenvector" = "leading_eigen",
                        "Optimal"          = "optimal",
                        # Partition
                        "Binary cut"       = "binary_cut",
                        # Hierarchical
                        "Hierarchical – Ward.D"    = "h_ward.D",
                        "Hierarchical – Ward.D2"   = "h_ward.D2",
                        "Hierarchical – Single"    = "h_single",
                        "Hierarchical – Complete"  = "h_complete",
                        "Hierarchical – Average"   = "h_average",
                        "Hierarchical – McQuitty"  = "h_mcquitty",
                        "Hierarchical – Median"    = "h_median",
                        "Hierarchical – Centroid"  = "h_centroid"
                      ),
                      selected = "louvain")
        ),
        div(
          class = "mt-2",
          step_nav_buttons(ns,
                           next_label = "Identify Modules",
                           next_arrow = FALSE, show_code = TRUE),
          uiOutput(ns("proceed_ui"))
        )
      ),

      mapa_card(
        "Module Summary",
        uiOutput(ns("download_modules_ui")),
        uiOutput(ns("module_status")),
        tags$br(),
        DT::DTOutput(ns("module_table"))
      )
    )
  )
}

#' @noRd
mod_mo_modules_server <- function(id, mo_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    module_result   <- reactiveVal(NULL)
    mo_module_code  <- reactiveVal(NULL)

    # ── Disable Run button until both network files are present ───────
    observe({
      if (is.null(mo_data$mnet_obj) || is.null(mo_data$sim_matrix))
        shinyjs::disable("btn_next")
      else
        shinyjs::enable("btn_next")
    })

    # ── Show reproducible R code ──────────────────────────────────────
    observeEvent(input$btn_code, {
      if (is.null(mo_module_code())) {
        shinyalert::shinyalert(
          title = "No code yet",
          text  = "Identify modules first to view the reproducible code.",
          type  = "warning", confirmButtonCol = "#dd4b39"
        )
        return()
      }
      show_code_modal(mo_module_code())
    })

    # ── Auto-load network files on upload ─────────────────────────────
    .load_rda <- function(path) {
      env <- new.env(parent = emptyenv())
      load(path, envir = env)
      get(ls(env)[1], envir = env)
    }

    observeEvent(input$upload_mnet, {
      req(input$upload_mnet)
      mo_data$mnet_obj <- .load_rda(input$upload_mnet$datapath)
    })

    observeEvent(input$upload_sim, {
      req(input$upload_sim)
      mo_data$sim_matrix <- .load_rda(input$upload_sim$datapath)
    })

    # ── Status banner ─────────────────────────────────────────────────
    output$module_status <- renderUI({
      if (is.null(module_result())) {
        status_alert(
          "Configure parameters and click Identify Modules.", "info"
        )
      } else {
        n <- nrow(module_result()$functional_module_result)
        status_alert(
          paste0(n, " multi-omics functional modules identified."), "success"
        )
      }
    })

    # ── Download handlers ─────────────────────────────────────────────
    output$download_modules_ui <- renderUI({
      req(module_result())
      tagList(
        tags$hr(class = "my-2"),
        div(class = "d-flex gap-2",
          downloadButton(
            ns("download_modules"),
            "Multi-omics functional modules (.rda)",
            class = "btn-mapa-dl mb-2"
          ),
          downloadButton(
            ns("download_modules_csv"),
            "Multi-omics functional summary (.csv)",
            class = "btn-mapa-dl mb-2"
          )
        )
      )
    })

    output$download_modules <- downloadHandler(
      filename = function() paste0("mo_modules_", Sys.Date(), ".rda"),
      content  = function(file) {
        mo_modules <- module_result()
        save(mo_modules, file = file)
      }
    )

    output$download_modules_csv <- downloadHandler(
      filename = function() paste0("mo_modules_summary_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(module_result()$functional_module_result,
                         file, row.names = FALSE)
      }
    )

    # ── Proceed button (appears after successful clustering) ──────────
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

    # ── Run clustering ────────────────────────────────────────────────
    observeEvent(input$btn_next, {
      req(mo_data$mnet_obj, mo_data$sim_matrix)

      shinyalert::shinyalert(
        title = "Identifying functional modules...",
        text  = tags$div(
          style = "text-align: center;",
          tags$img(src = "www/spinner.gif", width = "50px", height = "50px",
                   style = "margin-top: 20px;")
        ),
        type                = "",
        showConfirmButton   = FALSE,
        showCancelButton    = FALSE,
        timer               = 0,
        closeOnEsc          = FALSE,
        closeOnClickOutside = FALSE,
        html                = TRUE
      )

      result <- tryCatch(
        mapa::merge_multi_omics_nodes(
          object         = mo_data$mnet_obj,
          sim_matrix     = mo_data$sim_matrix,
          sim_cutoff     = input$sim_cutoff,
          cluster_method = input$cluster_method
        ),
        error = function(e) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "Module identification failed", text = e$message,
            type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          NULL
        }
      )
      if (is.null(result)) return()

      shinyalert::closeAlert()

      mo_data$mo_modules <- result
      module_result(result)
      mo_module_code(sprintf(
        "mo_modules <- mapa::merge_multi_omics_nodes(\n  object = mnet_obj,\n  sim_matrix = sim_matrix,\n  sim_cutoff = %s,\n  cluster_method = \"%s\"\n)",
        input$sim_cutoff, input$cluster_method))
    })

    observeEvent(input$btn_back, go_back())

    # ── Module summary table ──────────────────────────────────────────
    output$module_table <- DT::renderDT({
      req(module_result())
      df <- module_result()$functional_module_result
      DT::datatable(df,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })
  })
}
