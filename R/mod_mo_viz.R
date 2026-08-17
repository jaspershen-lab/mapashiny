# ── Multi-Omics Step: Visualization ──────────────────────────────────────────

#' @noRd
mod_mo_viz_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Multi-Omics  •  Step 6",
    .title       = "Visualization",
    .subtitle    = "Explore multi-omics functional modules through networks.",

    shinyjs::useShinyjs(),

    mapa_card(
      "Module Result",
      uiOutput(ns("carryover_ui")),
      optional_upload_caption("Step 5 · LLM Module Annotation"),
      tags$p(
        class = "text-muted small mb-2",
        "Uploading the .rda object from multi-omics clustering (or LLM annotation)",
        "populates the plots below."
      ),
      fileInput(
        ns("upload_modules"), NULL,
        accept      = ".rda",
        buttonLabel = "Browse…",
        placeholder = "No file selected"
      )
    ),

    bslib::navset_card_tab(

      # ── Tab 1: Module Info ──────────────────────────────────────────
      bslib::nav_panel(
        "Module Info",
        div(
          class = "pt-3",
          bslib::layout_columns(
            col_widths = c(3, 9),
            div(
              selectInput(ns("mo_info_module_id"), "Module ID", choices = NULL),
              bslib::layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("mo_info_node_size"), "Node size", value = 5, min = 1, max = 20),
                numericInput(ns("mo_info_label_size"), "Label size", value = 3, min = 1, max = 10)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                checkboxInput(ns("mo_info_show_rwr_edge"), "Show diffusion edges", value = FALSE),
                checkboxInput(ns("mo_info_show_labels"), "Show labels", value = TRUE)
              ),
              checkboxInput(ns("mo_info_llm_text"), "LLM text", value = FALSE),
              tags$strong("Metabolite diff metric colors"),
              bslib::layout_columns(
                col_widths = c(6, 6),
                shinyWidgets::colorPickr(ns("mo_info_met_color_low"), "Low",
                                         selected = "#71b7ed", theme = "monolith", width = "100%"),
                shinyWidgets::colorPickr(ns("mo_info_met_color_high"), "High",
                                         selected = "#f57c6e", theme = "monolith", width = "100%")
              ),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("mo_info_type"), "Type", choices = c("pdf", "png", "jpeg")),
                numericInput(ns("mo_info_width"), "Width", value = 10, min = 4, max = 30),
                numericInput(ns("mo_info_height"), "Height", value = 8, min = 4, max = 30)
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("btn_generate_mo_info"), "Generate",
                             class = "btn btn-mo flex-fill"),
                downloadButton(ns("download_mo_info"), "Download",
                               class = "btn btn-mo flex-fill")
              )
            ),
            div(
              class = "scrollable-container",
              plotOutput(ns("mo_info_plot"), width = "100%", height = "700px")
            )
          )
        )
      ),

      # ── Tab 2: Similarity Network ───────────────────────────────────
      bslib::nav_panel(
        "Similarity Network",
        div(
          class = "pt-3",
          bslib::layout_columns(
            col_widths = c(3, 9),
            div(
              selectInput(ns("mo_sim_module_id"), "Filter Module IDs",
                          choices = NULL, multiple = TRUE, selected = NULL),
              tags$p(class = "text-muted small mb-2",
                     "Leave empty to show all modules."),
              bslib::layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("mo_sim_node_size"), "Node size", value = 4, min = 1, max = 20),
                numericInput(ns("mo_sim_label_size"), "Label size", value = 3, min = 1, max = 10)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                checkboxInput(ns("mo_sim_show_rwr_edge"), "Show diffusion edges", value = TRUE),
                checkboxInput(ns("mo_sim_show_labels"), "Show module labels", value = TRUE)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                checkboxInput(ns("mo_sim_use_llm_label"), "Use LLM label", value = TRUE),
                checkboxInput(ns("mo_sim_show_node_labels"), "Show node labels", value = FALSE)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("mo_sim_module_circle_radius"), "Circle radius",
                             value = 10, min = 1, max = 50),
                numericInput(ns("mo_sim_module_scale_factor"), "Scale factor",
                             value = 1.2, min = 0.5, max = 5, step = 0.1)
              ),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("mo_sim_type"), "Type", choices = c("pdf", "png", "jpeg")),
                numericInput(ns("mo_sim_width"), "Width", value = 10, min = 4, max = 30),
                numericInput(ns("mo_sim_height"), "Height", value = 10, min = 4, max = 30)
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("btn_generate_mo_sim"), "Generate",
                             class = "btn btn-mo flex-fill"),
                downloadButton(ns("download_mo_sim"), "Download",
                               class = "btn btn-mo flex-fill")
              )
            ),
            div(
              class = "scrollable-container",
              plotOutput(ns("mo_sim_plot"), width = "100%", height = "700px")
            )
          )
        )
      )
    ),

    step_nav_buttons(ns)
  )
}

#' @noRd
mod_mo_viz_server <- function(id, annotated_modules, mode, go_next, go_back, mo_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Carry-over banner (are annotated modules already in the session?) ─
    output$carryover_ui <- renderUI({
      carryover_status(
        !is.null(annotated_modules()),
        "Step 5 · LLM Module Annotation",
        ready_detail = "Annotated modules were carried over from the previous step.",
        upload_hint  = paste0(
          "Run the earlier steps first, or upload the .rda saved from ",
          "multi-omics clustering (or LLM annotation) to plot it here."
        )
      )
    })

    # ── Upload module result ─────────────────────────────────────────
    observeEvent(input$upload_modules, {
      req(input$upload_modules$datapath)
      tmp <- new.env()
      load(input$upload_modules$datapath, envir = tmp)
      nms <- ls(tmp)
      if (length(nms) == 1) {
        annotated_modules(get(nms[1], envir = tmp))
      } else {
        shinyalert::shinyalert(
          title = "Invalid file content",
          text  = "The .rda file must contain exactly one object.",
          type  = "error", html = TRUE,
          confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── Populate module ID selects and toggle LLM controls ───────────
    observe({
      req(annotated_modules())
      obj <- annotated_modules()
      if (!is.list(obj) || !"functional_module_result" %in% names(obj)) return()

      # Use full module list from mo_data when available (normal workflow).
      # isolate() prevents a reactive dependency on mo_data so this observer
      # only re-fires when annotated_modules() changes, not on every mo_data
      # write (which would clear the choices for the upload-directly case).
      full_modules <- isolate(if (!is.null(mo_data)) mo_data$mo_modules else NULL)
      src_obj <- if (!is.null(full_modules) &&
                     "functional_module_result" %in% names(full_modules)) full_modules else obj
      df <- src_obj$functional_module_result
      df <- df[stringr::str_order(df$module, numeric = TRUE), ]

      choices <- setNames(
        df$module,
        paste0(df$module,
               " (n=", df$module_content_number,
               ", omics=", df$multi_omics_num, ")")
      )

      updateSelectInput(session, "mo_sim_module_id",  choices = choices)
      updateSelectInput(session, "mo_info_module_id", choices = choices, selected = choices[[1]])

      has_llm <- "llm_module_name" %in% colnames(obj$functional_module_result) &&
        any(!is.na(obj$functional_module_result$llm_module_name) &
              nzchar(obj$functional_module_result$llm_module_name))

      for (cb_id in c("mo_info_llm_text", "mo_sim_use_llm_label")) {
        if (!has_llm) {
          updateCheckboxInput(session, cb_id, value = FALSE)
          shinyjs::disable(cb_id)
        } else {
          shinyjs::enable(cb_id)
        }
      }
    })

    # ══════════════════════════════════════════════════════════════════
    # ── Tab 1: Similarity Network ─────────────────────────────────────
    # ══════════════════════════════════════════════════════════════════
    mo_sim_rv <- reactiveVal(NULL)

    observeEvent(input$btn_generate_mo_sim, {
      req(annotated_modules())
      obj <- annotated_modules()

      alert_id <- shinyalert::shinyalert(
        title = "Generating similarity network",
        text  = tags$div(style = "text-align: center;", "This may take a moment…",
                         tags$div(tags$img(src = "www/spinner.gif", width = "50px"),
                                  style = "margin-top: 15px;")),
        type = "", showConfirmButton = FALSE, showCancelButton = FALSE,
        timer = 0, closeOnEsc = FALSE, closeOnClickOutside = FALSE, html = TRUE
      )

      tryCatch({
        sel_mods <- if (length(input$mo_sim_module_id) > 0) input$mo_sim_module_id else NULL

        p <- mapa::plot_multi_omics_similarity_network(
          object               = obj,
          module_id            = sel_mods,
          node_size            = input$mo_sim_node_size,
          label_size           = input$mo_sim_label_size,
          show_rwr_edge        = input$mo_sim_show_rwr_edge,
          show_labels          = input$mo_sim_show_labels,
          use_llm_label        = input$mo_sim_use_llm_label,
          show_node_labels     = input$mo_sim_show_node_labels,
          module_circle_radius = input$mo_sim_module_circle_radius,
          module_scale_factor  = input$mo_sim_module_scale_factor
        )
        mo_sim_rv(p)
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          text = paste("Error:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })

      shinyalert::closeAlert(id = alert_id)
    })

    output$mo_sim_plot <- renderPlot({ req(mo_sim_rv()); mo_sim_rv() }, res = 96)

    observe({
      if (is.null(mo_sim_rv())) shinyjs::disable("download_mo_sim")
      else shinyjs::enable("download_mo_sim")
    })

    output$download_mo_sim <- downloadHandler(
      filename = function() paste0("multi_omics_similarity_network.", input$mo_sim_type),
      content  = function(file) {
        ggplot2::ggsave(file, plot = mo_sim_rv(),
                        width = input$mo_sim_width, height = input$mo_sim_height)
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # ── Tab 2: Module Info ─────────────────────────────────────────────
    # ══════════════════════════════════════════════════════════════════
    mo_info_rv <- reactiveVal(NULL)

    observeEvent(input$btn_generate_mo_info, {
      req(annotated_modules(), input$mo_info_module_id)
      obj <- annotated_modules()

      alert_id <- shinyalert::shinyalert(
        title = "Generating module information",
        text  = tags$div(style = "text-align: center;", "This may take a moment…",
                         tags$div(tags$img(src = "www/spinner.gif", width = "50px"),
                                  style = "margin-top: 15px;")),
        type = "", showConfirmButton = FALSE, showCancelButton = FALSE,
        timer = 0, closeOnEsc = FALSE, closeOnClickOutside = FALSE, html = TRUE
      )

      tryCatch({
        p <- mapa::plot_module_info(
          object            = obj,
          module_id         = input$mo_info_module_id,
          node_size         = input$mo_info_node_size,
          label_size        = input$mo_info_label_size,
          show_rwr_edge     = input$mo_info_show_rwr_edge,
          show_labels       = input$mo_info_show_labels,
          llm_text          = input$mo_info_llm_text,
          metabolite_colors = c(input$mo_info_met_color_low, input$mo_info_met_color_high)
        )
        mo_info_rv(p)
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          text = paste("Error:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })

      shinyalert::closeAlert(id = alert_id)
    })

    output$mo_info_plot <- renderPlot({ req(mo_info_rv()); mo_info_rv() }, res = 96)

    observe({
      if (is.null(mo_info_rv())) shinyjs::disable("download_mo_info")
      else shinyjs::enable("download_mo_info")
    })

    output$download_mo_info <- downloadHandler(
      filename = function() {
        paste0("multi_omics_module_info_", input$mo_info_module_id, ".", input$mo_info_type)
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = mo_info_rv(),
                        width = input$mo_info_width, height = input$mo_info_height)
      }
    )

    # ── Navigation ───────────────────────────────────────────────────
    observeEvent(input$btn_next, go_next())
    observeEvent(input$btn_back, go_back())
  })
}
