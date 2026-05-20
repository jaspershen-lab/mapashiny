# ── Single-Omics Step: Visualization ─────────────────────────────────────────

#' @noRd
mod_so_viz_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Single-Omics  •  Step 6",
    .title       = "Visualization",
    .subtitle    = "Explore functional modules through plots and networks.",

    shinyjs::useShinyjs(),

    mapa_card(
      "Upload Module Result",
      tags$p(
        class = "text-muted small mb-2",
        "Upload the .rda object from clustering (or LLM annotation)",
        "to populate the plots below."
      ),
      fileInput(
        ns("upload_modules"), NULL,
        accept      = ".rda",
        buttonLabel = "Browse…",
        placeholder = "No file selected"
      )
    ),

    bslib::navset_card_tab(

      # ── Tab 1: Barplot ──────────────────────────────────────────────
      bslib::nav_panel(
        "Barplot",
        div(
          class = "pt-3",
          bslib::layout_columns(
            col_widths = c(3, 9),
            div(
              selectInput(ns("barplot_level"), "Level",
                          choices = c("Functional module" = "functional_module",
                                      "Module" = "module",
                                      "Pathway" = "pathway"),
                          selected = "pathway"),
              bslib::layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("barplot_top_n"), "Top N", value = 5, min = 1, max = 1000),
                selectInput(ns("barplot_line_type"), "Line type",
                            choices = c("Straight" = "straight", "Meteor" = "meteor"))
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("barplot_y_label_width"), "Y label width", value = 50, min = 20, max = 100),
                numericInput(ns("barplot_count_cutoff"), "Count cutoff", value = 5, min = 1, max = 1000)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                numericInput(ns("barplot_p_adjust_cutoff"), "P-adjust cutoff", value = 0.05, min = 0, max = 0.5),
                selectInput(ns("barplot_x_axis_name"), "X axis name", choices = NULL)
              ),
              div(
                style = "display: flex; flex-direction: column;",
                tags$label("LLM text", `for` = ns("barplot_llm_text")),
                checkboxInput(ns("barplot_llm_text"), "", value = FALSE)
              ),
              # Gene database color panel
              shinyjs::hidden(
                div(id = ns("db_color_panel_gene"),
                    selectInput(ns("gene_barplot_database"), "Database",
                                choices = c("GO" = "go", "KEGG" = "kegg", "Reactome" = "reactome"),
                                multiple = TRUE, selected = NULL),
                    tags$strong("Database colors"),
                    bslib::layout_columns(
                      col_widths = c(4, 4, 4),
                      shinyWidgets::colorPickr(ns("barplot_go_color"), "GO",
                                               selected = "#eeca40", theme = "monolith", width = "100%"),
                      shinyWidgets::colorPickr(ns("barplot_kegg_color"), "KEGG",
                                               selected = "#fd7541", theme = "monolith", width = "100%"),
                      shinyWidgets::colorPickr(ns("barplot_reactome_color"), "Reactome",
                                               selected = "#23b9c7", theme = "monolith", width = "100%")
                    )
                )
              ),
              # Metabolite database color panel
              shinyjs::hidden(
                div(id = ns("db_color_panel_metabolite"),
                    selectInput(ns("met_barplot_database"), "Database",
                                choices = c("SMPDB" = "hmdb", "KEGG" = "metkegg"),
                                multiple = TRUE, selected = NULL),
                    tags$strong("Database colors"),
                    bslib::layout_columns(
                      col_widths = c(6, 6),
                      shinyWidgets::colorPickr(ns("barplot_hmdb_color"), "SMPDB",
                                               selected = "#7998ad", theme = "monolith", width = "100%"),
                      shinyWidgets::colorPickr(ns("barplot_metkegg_color"), "KEGG",
                                               selected = "#fd7541", theme = "monolith", width = "100%")
                    )
                )
              ),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("barplot_type"), "Type", choices = c("pdf", "png", "jpeg")),
                numericInput(ns("barplot_width"), "Width", value = 7, min = 4, max = 20),
                numericInput(ns("barplot_height"), "Height", value = 7, min = 4, max = 20)
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("btn_generate_barplot"), "Generate",
                             class = "btn btn-primary flex-fill"),
                downloadButton(ns("download_barplot"), "Download",
                               class = "btn btn-primary flex-fill")
              )
            ),
            plotOutput(ns("barplot_plot"), height = "500px")
          )
        )
      ),

      # ── Tab 2: Module Similarity Network ───────────────────────────
      bslib::nav_panel(
        "Similarity Network",
        div(
          class = "pt-3",
          bslib::layout_columns(
            col_widths = c(3, 9),
            div(
              numericInput(ns("sim_network_degree_cutoff"), "Degree cutoff",
                           value = 1, min = 0, max = 1000),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                checkboxInput(ns("sim_network_text"), "Text", value = FALSE),
                checkboxInput(ns("sim_network_llm_text"), "LLM text", value = FALSE),
                checkboxInput(ns("sim_network_text_all"), "Text all", value = FALSE)
              ),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("sim_network_type"), "Type", choices = c("pdf", "png", "jpeg")),
                numericInput(ns("sim_network_width"), "Width", value = 7, min = 4, max = 20),
                numericInput(ns("sim_network_height"), "Height", value = 7, min = 4, max = 20)
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("btn_generate_sim_network"), "Generate",
                             class = "btn btn-primary flex-fill"),
                downloadButton(ns("download_sim_network"), "Download",
                               class = "btn btn-primary flex-fill")
              )
            ),
            div(
              class = "scrollable-container",
              plotOutput(ns("sim_network_plot"), width = "100%", height = "600px")
            )
          )
        )
      ),

      # ── Tab 3: Module Information ───────────────────────────────────
      bslib::nav_panel(
        "Module Info",
        div(
          class = "pt-3",
          bslib::layout_columns(
            col_widths = c(3, 9),
            div(
              bslib::layout_columns(
                col_widths = c(8, 4),
                selectInput(ns("module_info_module_id"), "Module ID", choices = NULL),
                div(
                  style = "display: flex; flex-direction: column;",
                  tags$label("LLM text", `for` = ns("module_info_llm_text")),
                  checkboxInput(ns("module_info_llm_text"), "", value = FALSE)
                )
              ),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("module_info_type"), "Type", choices = c("pdf", "png", "jpeg")),
                numericInput(ns("module_info_width"), "Width", value = 7, min = 4, max = 30),
                numericInput(ns("module_info_height"), "Height", value = 21, min = 4, max = 30)
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("btn_generate_module_info"), "Generate",
                             class = "btn btn-primary flex-fill"),
                downloadButton(ns("download_module_info"), "Download",
                               class = "btn btn-primary flex-fill")
              )
            ),
            div(
              div(style = "width: 100%; height: 400px; overflow: auto; border: 1px solid #ccc;",
                  plotOutput(ns("module_info_plot1"), width = "100%", height = "600px")),
              plotOutput(ns("module_info_plot2")),
              plotOutput(ns("module_info_plot3"))
            )
          )
        )
      ),

      # ── Tab 4: Relationship Network ─────────────────────────────────
      bslib::nav_panel(
        "Relationship Network",
        div(
          class = "pt-3",
          bslib::layout_columns(
            col_widths = c(3, 9),
            div(
              bslib::layout_columns(
                col_widths = c(6, 6),
                checkboxInput(ns("rel_net_circular"), "Circular layout", value = FALSE),
                checkboxInput(ns("rel_net_llm_text"), "LLM text", value = TRUE)
              ),
              selectInput(ns("rel_net_module_id"), "Module ID",
                          choices = NULL, multiple = TRUE, selected = NULL),
              tags$strong("Levels Included"),
              bslib::layout_columns(
                col_widths = c(3, 3, 3, 3),
                checkboxInput(ns("rel_net_include_fm"), "Functional module", value = TRUE),
                checkboxInput(ns("rel_net_include_modules"), "Modules", value = TRUE),
                checkboxInput(ns("rel_net_include_pathways"), "Pathways", value = TRUE),
                checkboxInput(ns("rel_net_include_molecules"), "Molecules", value = TRUE)
              ),
              tags$strong("Colors"),
              bslib::layout_columns(
                col_widths = c(3, 3, 3, 3),
                shinyWidgets::colorPickr(ns("rel_net_fm_color"), "Functional module",
                                         selected = "#DD4124FF", theme = "monolith", width = "100%"),
                shinyWidgets::colorPickr(ns("rel_net_module_color"), "Module",
                                         selected = "#0F7BA2FF", theme = "monolith", width = "100%"),
                shinyWidgets::colorPickr(ns("rel_net_pathway_color"), "Pathway",
                                         selected = "#43B284FF", theme = "monolith", width = "100%"),
                shinyWidgets::colorPickr(ns("rel_net_molecule_color"), "Molecule",
                                         selected = "#FAB255FF", theme = "monolith", width = "100%")
              ),
              tags$strong("Text"),
              bslib::layout_columns(
                col_widths = c(3, 3, 3, 3),
                checkboxInput(ns("rel_net_fm_text"), "Functional module", value = TRUE),
                checkboxInput(ns("rel_net_module_text"), "Module", value = TRUE),
                checkboxInput(ns("rel_net_pathway_text"), "Pathway", value = TRUE),
                checkboxInput(ns("rel_net_molecule_text"), "Molecules", value = TRUE)
              ),
              tags$strong("Text size"),
              bslib::layout_columns(
                col_widths = c(3, 3, 3, 3),
                numericInput(ns("rel_net_fm_text_size"), "Functional module", value = 3, min = 0.3, max = 10),
                numericInput(ns("rel_net_module_text_size"), "Module", value = 3, min = 0.3, max = 10),
                numericInput(ns("rel_net_pathway_text_size"), "Pathway", value = 3, min = 0.3, max = 10),
                numericInput(ns("rel_net_molecule_text_size"), "Molecule", value = 3, min = 0.3, max = 10)
              ),
              tags$strong("Position limits"),
              sliderInput(ns("rel_net_fm_position"), "Functional module",
                          min = 0, max = 1, value = c(0, 1)),
              sliderInput(ns("rel_net_module_position"), "Module",
                          min = 0, max = 1, value = c(0, 1)),
              sliderInput(ns("rel_net_pathway_position"), "Pathway",
                          min = 0, max = 1, value = c(0, 1)),
              sliderInput(ns("rel_net_molecule_position"), "Molecule",
                          min = 0, max = 1, value = c(0, 1)),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("rel_net_type"), "Type", choices = c("pdf", "png", "jpeg")),
                numericInput(ns("rel_net_width"), "Width", value = 21, min = 4, max = 30),
                numericInput(ns("rel_net_height"), "Height", value = 7, min = 4, max = 20)
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("btn_generate_rel_net"), "Generate",
                             class = "btn btn-primary flex-fill"),
                downloadButton(ns("download_rel_net"), "Download",
                               class = "btn btn-primary flex-fill")
              )
            ),
            div(
              class = "scrollable-container",
              plotOutput(ns("rel_net_plot"), height = "800px", width = "800px")
            )
          )
        )
      ),

      # ── Tab 5: Module-Expression Heatmap ────────────────────────────
      bslib::nav_panel(
        "Expression Heatmap",
        div(
          class = "pt-3",
          bslib::layout_columns(
            col_widths = c(3, 9),
            div(
              fileInput(ns("upload_expression_data"),
                        "Upload expression data (.csv, .xlsx, .rda)",
                        accept = c(".csv", ".xlsx", ".rda")),
              bslib::layout_columns(
                col_widths = c(5, 7),
                selectInput(ns("heatmap_level"), "Level",
                            choices = c("Pathway" = "pathway", "Molecule" = "molecule"),
                            selected = "pathway"),
                selectInput(ns("heatmap_module_id"), "Module ID",
                            choices = NULL, multiple = TRUE, selected = NULL)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                checkboxInput(ns("heatmap_scale"), "Scale expression data", value = TRUE),
                checkboxInput(ns("heatmap_wordcloud"), "Word cloud", value = TRUE)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                checkboxInput(ns("heatmap_cluster_rows"), "Cluster rows", value = FALSE),
                checkboxInput(ns("heatmap_show_cluster_tree"), "Show cluster tree", value = TRUE)
              ),
              checkboxInput(ns("heatmap_llm_text"), "LLM text", value = FALSE),
              tags$strong("Colors"),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                shinyWidgets::colorPickr(ns("heatmap_fm_color"), "Functional module",
                                         selected = "#DD4124FF", theme = "monolith", width = "100%"),
                shinyWidgets::colorPickr(ns("heatmap_pathway_color"), "Pathway",
                                         selected = "#43B284FF", theme = "monolith", width = "100%"),
                shinyWidgets::colorPickr(ns("heatmap_molecule_color"), "Molecule",
                                         selected = "#FAB255FF", theme = "monolith", width = "100%")
              ),
              tags$strong("Position limits"),
              sliderInput(ns("heatmap_fm_position"), "Functional module",
                          min = 0, max = 1, value = c(0.2, 0.8)),
              sliderInput(ns("heatmap_pathway_position"), "Pathway",
                          min = 0, max = 1, value = c(0.1, 0.9)),
              sliderInput(ns("heatmap_molecule_position"), "Molecule",
                          min = 0, max = 1, value = c(0, 1)),
              tags$strong("Layout ratios"),
              bslib::layout_columns(
                col_widths = c(6, 6),
                sliderInput(ns("heatmap_height_ratio_1"), "Heatmap spacing",
                            min = 0.1, max = 10, value = 1, step = 0.1),
                sliderInput(ns("heatmap_height_ratio_2"), "Heatmap main",
                            min = 10, max = 200, value = 100, step = 10)
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                sliderInput(ns("heatmap_network_ratio_1"), "Network main",
                            min = 1, max = 50, value = 10, step = 1),
                sliderInput(ns("heatmap_network_ratio_2"), "Network spacing",
                            min = 1, max = 20, value = 5, step = 1)
              ),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                selectInput(ns("heatmap_type"), "Type", choices = c("pdf", "png", "jpeg")),
                numericInput(ns("heatmap_width"), "Width", value = 12, min = 4, max = 30),
                numericInput(ns("heatmap_height"), "Height", value = 8, min = 4, max = 20)
              ),
              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("btn_generate_heatmap"), "Generate",
                             class = "btn btn-primary flex-fill"),
                downloadButton(ns("download_heatmap"), "Download",
                               class = "btn btn-primary flex-fill")
              )
            ),
            div(
              class = "scrollable-container",
              plotOutput(ns("heatmap_plot"), height = "800px", width = "800px")
            )
          )
        )
      )
    ),

    step_nav_buttons(ns)
  )
}

#' @noRd
mod_so_viz_server <- function(id, annotated_modules, mode, go_next, go_back) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    # ── Derive query type from loaded object ─────────────────────────
    query_type <- reactive({
      req(annotated_modules())
      obj <- annotated_modules()
      req(methods::is(obj, "functional_module"))
      if ("enrich_pathway" %in% names(obj@process_info)) {
        obj@process_info$enrich_pathway@parameter$query_type
      } else {
        obj@process_info$do_gsea@parameter$query_type
      }
    })

    # ── React to loaded data: update selects and toggle panels ───────
    observe({
      req(annotated_modules())
      obj <- annotated_modules()
      if (!methods::is(obj, "functional_module")) return()

      qt <- query_type()
      db_choices <- c("GO" = "go", "KEGG" = "kegg", "Reactome" = "reactome",
                      "SMPDB" = "hmdb", "KEGG" = "metkegg")

      shinyjs::toggleElement(id = "db_color_panel_gene",       condition = qt == "gene")
      shinyjs::toggleElement(id = "db_color_panel_metabolite", condition = qt == "metabolite")

      if (qt == "gene") {
        if ("enrich_pathway" %in% names(obj@process_info)) {
          x_choices <- c("qscore", "RichFactor", "FoldEnrichment")
          avail_db  <- obj@process_info$enrich_pathway@parameter$database
        } else {
          x_choices <- "NES"
          avail_db  <- obj@process_info$do_gsea@parameter$database
        }
        updateSelectInput(session, "gene_barplot_database",
                          choices  = db_choices[db_choices %in% avail_db],
                          selected = avail_db)
      } else if (qt == "metabolite") {
        x_choices <- "qscore"
        avail_db  <- obj@process_info$enrich_pathway@parameter$database
        updateSelectInput(session, "met_barplot_database",
                          choices  = db_choices[db_choices %in% avail_db],
                          selected = avail_db)
      }
      updateSelectInput(session, "barplot_x_axis_name", choices = x_choices)


      no_module_data <- (
        (qt == "gene"       && length(c(obj@merged_pathway_go,
                                        obj@merged_pathway_kegg,
                                        obj@merged_pathway_reactome)) == 0) ||
        (qt == "metabolite" && length(c(obj@merged_pathway_hmdb,
                                        obj@merged_pathway_metkegg)) == 0)
      )

      if (no_module_data) {
        updateSelectInput(session, "barplot_level",
                          choices  = c("Functional module" = "functional_module",
                                       "Pathway" = "pathway"),
                          selected = "functional_module")
        updateCheckboxInput(session, "rel_net_include_modules", value = FALSE)
        shinyjs::disable("rel_net_include_modules")
        updateCheckboxInput(session, "rel_net_module_text", value = FALSE)
        shinyjs::disable("rel_net_module_text")
      }

    })

    # ── Toggle LLM checkboxes (separate observer so level changes don't
    #   re-trigger the data-update observe and reset selections) ────────
    observe({
      req(annotated_modules())
      obj <- annotated_modules()
      if (!methods::is(obj, "functional_module")) return()
      has_llm <- length(obj@llm_module_interpretation) > 0

      toggle_llm <- function(checkbox_id, extra_cond = TRUE) {
        if (!has_llm || !extra_cond) {
          updateCheckboxInput(session, checkbox_id, value = FALSE)
          shinyjs::disable(checkbox_id)
        } else {
          shinyjs::enable(checkbox_id)
        }
      }

      toggle_llm("barplot_llm_text",     input$barplot_level != "module")
      toggle_llm("sim_network_llm_text")
      toggle_llm("module_info_llm_text")
      toggle_llm("rel_net_llm_text")
      toggle_llm("heatmap_llm_text")
    })

    # ── Update module ID selectors ───────────────────────────────────
    observe({
      req(annotated_modules())
      obj <- annotated_modules()
      if (!methods::is(obj, "functional_module")) return()

      all_fm_ids <- stringr::str_sort(
        unique(obj@merged_module$result_with_module$module), numeric = TRUE
      )
      updateSelectInput(session, "module_info_module_id",
                        choices = all_fm_ids, selected = all_fm_ids[1])
      updateSelectInput(session, "rel_net_module_id", choices = all_fm_ids)
      updateSelectInput(session, "heatmap_module_id", choices = all_fm_ids)
    })

    # ── Expression data upload ───────────────────────────────────────
    expression_data <- reactiveVal(NULL)

    observeEvent(input$upload_expression_data, {
      req(input$upload_expression_data$datapath)
      path <- input$upload_expression_data$datapath
      ext  <- tools::file_ext(input$upload_expression_data$name)
      tryCatch({
        dt <- switch(tolower(ext),
          csv  = read.csv(path, check.names = FALSE),
          xlsx = readxl::read_excel(path),
          rda  = {
            tmp <- new.env()
            load(path, envir = tmp)
            get(ls(tmp)[1], envir = tmp)
          },
          stop("Unsupported file type: ", ext)
        )
        expression_data(as.data.frame(dt))
      }, error = function(e) {
        shinyalert::shinyalert(
          text = paste("Error loading expression data:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })
    })

    # ══════════════════════════════════════════════════════════════════
    # ── Tab 1: Barplot ───────────────────────────────────────────────
    # ══════════════════════════════════════════════════════════════════
    barplot_rv <- reactiveVal(NULL)

    observeEvent(input$btn_generate_barplot, {
      req(annotated_modules())
      obj <- annotated_modules()

      alert_id <- shinyalert::shinyalert(
        title = "Generating barplot",
        text  = tags$div(style = "text-align: center;", "This may take a moment…",
                         tags$div(tags$img(src = "www/spinner.gif", width = "50px"),
                                  style = "margin-top: 15px;")),
        type = "", showConfirmButton = FALSE, showCancelButton = FALSE,
        timer = 0, closeOnEsc = FALSE, closeOnClickOutside = FALSE, html = TRUE
      )

      tryCatch({
        qt <- query_type()
        if (qt == "gene") {
          db  <- input$gene_barplot_database
          col <- c(GO = input$barplot_go_color,
                   KEGG = input$barplot_kegg_color,
                   Reactome = input$barplot_reactome_color)
        } else {
          db  <- input$met_barplot_database
          col <- c(SMPDB = input$barplot_hmdb_color,
                   KEGG = input$barplot_metkegg_color)
        }

        p <- mapa::plot_pathway_bar(
          object          = obj,
          top_n           = input$barplot_top_n,
          x_axis_name     = input$barplot_x_axis_name,
          y_label_width   = input$barplot_y_label_width,
          p.adjust.cutoff = input$barplot_p_adjust_cutoff,
          count.cutoff    = input$barplot_count_cutoff,
          level           = input$barplot_level,
          llm_text        = input$barplot_llm_text,
          database        = db,
          line_type       = input$barplot_line_type,
          database_color  = col
        )
        barplot_rv(p)
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          text = paste("Error:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })

      shinyalert::closeAlert(id = alert_id)
    })

    output$barplot_plot <- renderPlot({ req(barplot_rv()); barplot_rv() }, res = 96)

    observe({
      if (is.null(barplot_rv())) shinyjs::disable("download_barplot")
      else shinyjs::enable("download_barplot")
    })

    output$download_barplot <- downloadHandler(
      filename = function() paste0("pathway_barplot.", input$barplot_type),
      content  = function(file) {
        ggplot2::ggsave(file, plot = barplot_rv(),
                        width = input$barplot_width, height = input$barplot_height)
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # ── Tab 2: Similarity Network ────────────────────────────────────
    # ══════════════════════════════════════════════════════════════════
    sim_network_rv        <- reactiveVal(NULL)
    sim_network_no_legend <- reactiveVal(NULL)
    show_sim_legend       <- reactiveVal(TRUE)

    observeEvent(input$btn_generate_sim_network, {
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
        n_above_cutoff <- sum(
          obj@merged_module$functional_module_result$module_content_number >
            input$sim_network_degree_cutoff
        )
        show_sim_legend(n_above_cutoff <= 34)

        p <- mapa::plot_similarity_network(
          object        = obj,
          level         = "functional_module",
          degree_cutoff = input$sim_network_degree_cutoff,
          text          = input$sim_network_text,
          llm_text      = input$sim_network_llm_text,
          text_all      = input$sim_network_text_all
        ) + ggplot2::theme(aspect.ratio = 1)

        sim_network_rv(p)
        if (!show_sim_legend()) {
          sim_network_no_legend(p + ggplot2::guides(fill = "none"))
        }
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          text = paste("Error:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })

      shinyalert::closeAlert(id = alert_id)

      if (!show_sim_legend()) {
        shinyalert::shinyalert(
          text = paste("With more than 34 modules, the legend is hidden in the display",
                       "to improve readability. The legend will be included when you download."),
          type = "info", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      }
    })

    output$sim_network_plot <- renderPlot({
      req(sim_network_rv())
      if (show_sim_legend()) sim_network_rv() else sim_network_no_legend()
    }, res = 96)

    observe({
      if (is.null(sim_network_rv())) shinyjs::disable("download_sim_network")
      else shinyjs::enable("download_sim_network")
    })

    output$download_sim_network <- downloadHandler(
      filename = function() {
        paste0("module_similarity_network_functional_module.", input$sim_network_type)
      },
      content = function(file) {
        Cairo::CairoPDF(file = file,
                        width  = input$sim_network_width,
                        height = input$sim_network_height)
        print(sim_network_rv())
        dev.off()
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # ── Tab 3: Module Information ─────────────────────────────────────
    # ══════════════════════════════════════════════════════════════════
    module_info1_rv <- reactiveVal(NULL)
    module_info2_rv <- reactiveVal(NULL)
    module_info3_rv <- reactiveVal(NULL)
    module_info_all <- reactiveVal(NULL)

    observeEvent(input$btn_generate_module_info, {
      req(annotated_modules(), input$module_info_module_id)
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
          object    = obj,
          level     = "functional_module",
          llm_text  = input$module_info_llm_text,
          module_id = input$module_info_module_id
        )

        if (methods::is(p, "ggplot")) {
          module_info_all(p + p + p + patchwork::plot_layout(ncol = 1))
          module_info1_rv(p); module_info2_rv(p); module_info3_rv(p)
        } else {
          module_info_all(p[[1]] + p[[2]] + p[[3]] + patchwork::plot_layout(ncol = 1))
          module_info1_rv(p[[1]]); module_info2_rv(p[[2]]); module_info3_rv(p[[3]])
        }
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          text = paste("Error:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })

      shinyalert::closeAlert(id = alert_id)
    })

    output$module_info_plot1 <- renderPlot({ req(module_info1_rv()); module_info1_rv() }, res = 96)
    output$module_info_plot2 <- renderPlot({ req(module_info2_rv()); module_info2_rv() }, res = 96)
    output$module_info_plot3 <- renderPlot({ req(module_info3_rv()); module_info3_rv() }, res = 96)

    observe({
      if (is.null(module_info_all())) shinyjs::disable("download_module_info")
      else shinyjs::enable("download_module_info")
    })

    output$download_module_info <- downloadHandler(
      filename = function() {
        paste0("module_info_", input$module_info_module_id, ".", input$module_info_type)
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = module_info_all(),
                        width = input$module_info_width, height = input$module_info_height)
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # ── Tab 4: Relationship Network ───────────────────────────────────
    # ══════════════════════════════════════════════════════════════════
    rel_net_rv <- reactiveVal(NULL)

    observeEvent(input$btn_generate_rel_net, {
      req(annotated_modules())
      obj <- annotated_modules()

      alert_id <- shinyalert::shinyalert(
        title = "Generating relationship network",
        text  = tags$div(style = "text-align: center;", "This may take a moment…",
                         tags$div(tags$img(src = "www/spinner.gif", width = "50px"),
                                  style = "margin-top: 15px;")),
        type = "", showConfirmButton = FALSE, showCancelButton = FALSE,
        timer = 0, closeOnEsc = FALSE, closeOnClickOutside = FALSE, html = TRUE
      )

      tryCatch({
        obj_filt <- obj
        sel_mods <- input$rel_net_module_id
        if (!is.null(sel_mods) && length(sel_mods) > 0) {
          obj_filt@merged_module$functional_module_result <-
            obj_filt@merged_module$functional_module_result |>
            dplyr::filter(module %in% sel_mods)
        }

        p <- mapa::plot_relationship_network(
          object                             = obj_filt,
          llm_text                           = input$rel_net_llm_text,
          include_functional_modules         = input$rel_net_include_fm,
          include_modules                    = input$rel_net_include_modules,
          include_pathways                   = input$rel_net_include_pathways,
          include_molecules                  = input$rel_net_include_molecules,
          include_variables                  = FALSE,
          functional_module_color            = input$rel_net_fm_color,
          module_color                       = input$rel_net_module_color,
          pathway_color                      = input$rel_net_pathway_color,
          molecule_color                     = input$rel_net_molecule_color,
          functional_module_text             = input$rel_net_fm_text,
          module_text                        = input$rel_net_module_text,
          pathway_text                       = input$rel_net_pathway_text,
          molecule_text                      = input$rel_net_molecule_text,
          functional_module_text_size        = input$rel_net_fm_text_size,
          module_text_size                   = input$rel_net_module_text_size,
          pathway_text_size                  = input$rel_net_pathway_text_size,
          molecule_text_size                 = input$rel_net_molecule_text_size,
          circular_plot                      = input$rel_net_circular,
          functional_module_arrange_position = TRUE,
          module_arrange_position            = TRUE,
          pathway_arrange_position           = TRUE,
          molecule_arrange_position          = TRUE,
          functional_module_position_limits  = input$rel_net_fm_position,
          module_position_limits             = input$rel_net_module_position,
          pathway_position_limits            = input$rel_net_pathway_position,
          molecule_position_limits           = input$rel_net_molecule_position
        )
        rel_net_rv(p)
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          text = paste("Error:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })

      shinyalert::closeAlert(id = alert_id)
    })

    output$rel_net_plot <- renderPlot({ req(rel_net_rv()); rel_net_rv() }, res = 96)

    observe({
      if (is.null(rel_net_rv())) shinyjs::disable("download_rel_net")
      else shinyjs::enable("download_rel_net")
    })

    output$download_rel_net <- downloadHandler(
      filename = function() paste0("relationship_network.", input$rel_net_type),
      content  = function(file) {
        ggplot2::ggsave(file, plot = rel_net_rv(),
                        width = input$rel_net_width, height = input$rel_net_height)
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # ── Tab 5: Expression Heatmap ─────────────────────────────────────
    # ══════════════════════════════════════════════════════════════════
    heatmap_rv <- reactiveVal(NULL)

    observeEvent(input$btn_generate_heatmap, {
      req(annotated_modules(), expression_data())
      obj  <- annotated_modules()
      edat <- expression_data()

      alert_id <- shinyalert::shinyalert(
        title = "Generating expression heatmap",
        text  = tags$div(style = "text-align: center;", "This may take a moment…",
                         tags$div(tags$img(src = "www/spinner.gif", width = "50px"),
                                  style = "margin-top: 15px;")),
        type = "", showConfirmButton = FALSE, showCancelButton = FALSE,
        timer = 0, closeOnEsc = FALSE, closeOnClickOutside = FALSE, html = TRUE
      )

      tryCatch({
        sel_mods <- if (length(input$heatmap_module_id) > 0) input$heatmap_module_id else NULL

        p <- mapa::plot_relationship_heatmap(
          object                            = obj,
          level                             = input$heatmap_level,
          expression_data                   = edat,
          module_ids                        = sel_mods,
          scale_expression_data             = input$heatmap_scale,
          cluster_rows                      = input$heatmap_cluster_rows,
          show_cluster_tree                 = input$heatmap_show_cluster_tree,
          wordcloud                         = input$heatmap_wordcloud,
          llm_text                          = input$heatmap_llm_text,
          functional_module_color           = input$heatmap_fm_color,
          pathway_color                     = input$heatmap_pathway_color,
          molecule_color                    = input$heatmap_molecule_color,
          functional_module_position_limits = input$heatmap_fm_position,
          pathway_position_limits           = input$heatmap_pathway_position,
          molecule_position_limits          = input$heatmap_molecule_position,
          heatmap_height_ratios             = c(input$heatmap_height_ratio_1,
                                                input$heatmap_height_ratio_2),
          network_height_ratios             = c(input$heatmap_network_ratio_1,
                                                input$heatmap_network_ratio_2)
        )
        heatmap_rv(p)
      }, error = function(e) {
        shinyalert::closeAlert(id = alert_id)
        shinyalert::shinyalert(
          text = paste("Error:", e$message),
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })

      shinyalert::closeAlert(id = alert_id)
    })

    output$heatmap_plot <- renderPlot({ req(heatmap_rv()); heatmap_rv() }, res = 96)

    observe({
      if (is.null(heatmap_rv())) shinyjs::disable("download_heatmap")
      else shinyjs::enable("download_heatmap")
    })

    output$download_heatmap <- downloadHandler(
      filename = function() paste0("expression_heatmap.", input$heatmap_type),
      content  = function(file) {
        ggplot2::ggsave(file, plot = heatmap_rv(),
                        width = input$heatmap_width, height = input$heatmap_height)
      }
    )

    # ── Navigation ───────────────────────────────────────────────────
    observeEvent(input$btn_next, go_next())
    observeEvent(input$btn_back, go_back())
  })
}
