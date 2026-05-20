# ── Step MO-3: Build Multi-Omics Network (Multi-Omics) ───────────────────────
# Pipeline: build_network_tables() → build_MNetwork() → get_multi_omics_sim()
# Edge databases (STRING, Reactome) are cached in the user's app cache dir.

#' @noRd
mod_mo_network_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Multi-Omics  •  Step 3",
    .title       = "Multi-Omics Network Construction and Encoding",
    .subtitle    = paste0(
      "Construct the semantic-biological network and compute functional similarity among multi-omics molecules and pathways",
      "using random walk with restart."
    ),

    # ── Upload card (full width, above parameters) ──
    mapa_card(
      "Upload Enrichment Results",
      status_alert(
        paste0(
          "Upload enrichment objects (.rda) saved from Step 2 to use them ",
          "here without re-running enrichment."
        ),
        "info"
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        gap = "1rem",
        fileInput(
          ns("upload_t_enrich"),
          tagList(div(class = "omics-badge omics-T d-inline-flex me-1", "T"),
                  "Transcriptomics (.rda)"),
          accept = ".rda", buttonLabel = "Browse…"
        ),
        fileInput(
          ns("upload_p_enrich"),
          tagList(div(class = "omics-badge omics-P d-inline-flex me-1", "P"),
                  "Proteomics (.rda)"),
          accept = ".rda", buttonLabel = "Browse…"
        ),
        fileInput(
          ns("upload_m_enrich"),
          tagList(div(class = "omics-badge omics-M d-inline-flex me-1", "M"),
                  "Metabolomics (.rda)"),
          accept = ".rda", buttonLabel = "Browse…"
        )
      )
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      # ── Left: parameter tabs + action buttons ──
      tagList(
      uiOutput(ns("db_status_ui")),
      bslib::navset_card_tab(

        bslib::nav_panel(
          "Network",
          div(
            class = "pt-3",

            # PPI cutoff
            tags$label(
              class = "form-label",
              "Protein-protein interaction cutoff",
              tags$span(
                title = paste0(
                  "Filters protein-protein interactions from the STRING database. ",
                  "Only interactions with a combined confidence score at or above ",
                  "this threshold are included. Higher values yield a sparser but ",
                  "more reliable PPI network."
                ),
                tags$i(class = "fas fa-info-circle text-muted ms-1",
                       style = "cursor: pointer;")
              )
            ),
            numericInput(ns("string_score"), label = NULL,
                         value = 0.9, min = 0, max = 1, step = 0.05),

            # TF-target confidence
            tags$label(
              class = "form-label mt-2",
              "TF-target regulation confidence",
              tags$span(
                title = paste0(
                  "Filters transcription factor (TF) to target gene edges. ",
                  "Level A (High) includes only the most reliably supported ",
                  "TF-target interactions; B (Medium) and C (Low) progressively ",
                  "include weaker evidence interactions."
                ),
                tags$i(class = "fas fa-info-circle text-muted ms-1",
                       style = "cursor: pointer;")
              )
            ),
            selectInput(
              ns("tf_confidence"), label = NULL,
              choices  = c("High (A)" = "A", "Medium (B)" = "B", "Low (C)" = "C"),
              selected = "A"
            ),

            # Minimum pathway similarity
            sliderInput(
              ns("min_path_sim"),
              tagList(
                "Minimum pathway similarity",
                tags$span(
                  title = paste0(
                    "Minimum semantic similarity required between two pathways ",
                    "for them to be connected in the pathway network. Higher ",
                    "values create a sparser pathway graph."
                  ),
                  tags$i(class = "fas fa-info-circle text-muted ms-1",
                         style = "cursor: pointer;")
                )
              ),
              min = 0.3, max = 0.9, value = 0.55, step = 0.05
            ),

            tags$hr(class = "my-2"),

            # Pathway text embedding source
            radioButtons(
              ns("embed_source"), "Pathway text embedding",
              choices = c(
                "Local database (pre-computed, no API key)" = "local",
                "Realtime API (on-the-fly, API key required)" = "realtime"
              ),
              selected = "local"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'realtime'", ns("embed_source")),
              div(
                class = "mt-1 ps-2 border-start border-2",
                selectInput(
                  ns("api_provider"), "API provider",
                  choices  = c("SiliconFlow" = "siliconflow",
                               "OpenAI"      = "openai",
                               "Google"      = "gemini"),
                  selected = "siliconflow"
                ),
                selectizeInput(
                  ns("embed_model"), "Embedding model",
                  choices  = .EMBED_MODELS[["SiliconFlow"]],
                  selected = "Qwen/Qwen3-Embedding-8B",
                  options  = list(create      = TRUE,
                                  placeholder = "Select or enter model name")
                ),
                passwordInput(ns("api_key"), "API key")
              )
            )
          )
        ),

        bslib::nav_panel(
          "Random Walk with Restart",
          div(
            class = "pt-3",
            tags$p(
              class = "text-muted small",
              paste0(
                "Default values work well for most datasets. Expand ",
                "Advanced settings only if you need fine-grained control."
              )
            ),
            tags$details(
              tags$summary(
                class = "text-muted small",
                style = "cursor: pointer;",
                "Advanced settings"
              ),
              div(
                class = "mt-3",
                sliderInput(
                  ns("r"),
                  tagList(
                    "Restart probability (r)",
                    tags$span(
                      title = paste0(
                        "Controls how locally the random walk stays around the ",
                        "starting molecule or pathway. Higher values emphasize ",
                        "closer network neighborhoods; lower values allow broader ",
                        "network propagation."
                      ),
                      tags$i(class = "fas fa-info-circle text-muted ms-1",
                             style = "cursor: pointer;")
                    )
                  ),
                  min = 0.1, max = 0.9, value = 0.5, step = 0.05
                ),
                sliderInput(
                  ns("lambda"),
                  tagList(
                    "Molecule-pathway transition weight (λ)",
                    tags$span(
                      title = paste0(
                        "Controls how strongly information flows between the ",
                        "molecular network and the pathway network. Higher values ",
                        "increase the influence of molecule-pathway annotation edges."
                      ),
                      tags$i(class = "fas fa-info-circle text-muted ms-1",
                             style = "cursor: pointer;")
                    )
                  ),
                  min = 0, max = 0.5, value = 0.2, step = 0.05
                ),
                sliderInput(
                  ns("delta1"),
                  tagList(
                    "Molecular layer switching probability (δ₁)",
                    tags$span(
                      title = paste0(
                        "Controls how easily the random walk switches between ",
                        "different molecular relationship layers, such as PPI, ",
                        "TF-target, enzyme-metabolite, and metabolite co-reaction."
                      ),
                      tags$i(class = "fas fa-info-circle text-muted ms-1",
                             style = "cursor: pointer;")
                    )
                  ),
                  min = 0.1, max = 0.9, value = 0.5, step = 0.05
                ),
                sliderInput(ns("eta"), "Inter-layer jump (η)",
                            min = 0.1, max = 0.9, value = 0.5, step = 0.05),
                sliderInput(ns("delta2"), "Pathway layer weight (δ₂)",
                            min = 0.1, max = 0.9, value = 0.5, step = 0.05)
              )
            )
          )
        )
      ),
      div(
        class = "mt-2",
        step_nav_buttons(ns,
                         next_label = "Build Network & Compute Similarity",
                         next_arrow = FALSE, show_code = TRUE),
        uiOutput(ns("proceed_ui"))
      )
      ),

      # ── Right: status + results ──
      mapa_card(
        "Network & Similarity",
        uiOutput(ns("network_status")),
        uiOutput(ns("network_stats")),
        uiOutput(ns("download_network_ui")),
        uiOutput(ns("sim_result_ui"))
      )
    )
  )
}

#' @noRd
mod_mo_network_server <- function(id, mo_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sim_result  <- reactiveVal(NULL)
    net_code    <- reactiveVal(NULL)
    db_checked  <- reactiveVal(0)  # incremented after a download to refresh UI

    observeEvent(input$btn_code, {
      if (is.null(net_code())) {
        shinyalert::shinyalert(
          title = "No code yet",
          text  = "Build the network first to view the reproducible code.",
          type  = "warning", confirmButtonCol = "#dd4b39"
        )
        return()
      }
      show_code_modal(net_code())
    })

    # ── Database status banner ────────────────────────────────────────
    output$db_status_ui <- renderUI({
      db_checked()
      st <- .db_status_check()
      if (st$string_ok && st$reactome_ok) {
        status_alert(
          tagList(tags$i(class = "fas fa-database me-1"),
                  "Edge databases cached locally."),
          "success"
        )
      } else {
        missing <- c(
          if (!st$string_ok)   "STRING (PPI, protein aliases)",
          if (!st$reactome_ok) "Reactome (enzyme-metabolite reactions)"
        )
        status_alert(
          tagList(
            tags$strong("Databases not yet downloaded:"),
            tags$ul(class = "mb-1 ps-3 mt-1", lapply(missing, tags$li)),
            tags$small(class = "text-muted",
                       paste0("They will be downloaded automatically the first ",
                              "time you click Build Network & Compute Similarity."))
          ),
          "warning"
        )
      }
    })

    # ── Network / computation status ──────────────────────────────────
    output$network_status <- renderUI({
      if (is.null(mo_data$mnet_obj)) {
        status_alert(
          "Configure parameters and click Build Network & Compute Similarity.",
          "info"
        )
      } else {
        status_alert(
          "Network built and similarity matrix computed successfully.", "success"
        )
      }
    })

    output$network_stats <- renderUI({
      req(mo_data$mnet_obj)
      obj <- mo_data$mnet_obj
      div(
        class = "d-flex gap-2 my-1",
        div(
          class = "rounded px-3 py-1 text-center text-white",
          style = "background: var(--bs-primary); min-width: 110px;",
          tags$div(class = "small", "Molecule nodes"),
          tags$div(class = "fw-semibold", nrow(obj@mol_nodes))
        ),
        div(
          class = "rounded px-3 py-1 text-center text-white",
          style = "background: var(--bs-info); min-width: 110px;",
          tags$div(class = "small", "Pathway nodes"),
          tags$div(class = "fw-semibold", nrow(obj@path_nodes))
        )
      )
    })

    # ── Similarity top-pairs table ────────────────────────────────────
    output$sim_result_ui <- renderUI({
      req(sim_result())
      m   <- sim_result()
      nnz <- tryCatch(Matrix::nnzero(m > 0), error = function(e) NA_integer_)
      tagList(
        tags$hr(class = "my-2"),
        tags$p(
          class = "small text-muted mb-1",
          sprintf(
            "Cosine-similarity matrix: %d × %d nodes, %s non-zero pairs. ",
            nrow(m), ncol(m),
            if (is.na(nnz)) "?" else format(nnz, big.mark = ",")
          ),
          "Showing top 300 pairs:"
        ),
        DT::DTOutput(ns("sim_top_pairs_dt"))
      )
    })

    output$sim_top_pairs_dt <- DT::renderDT({
      req(sim_result())
      m   <- sim_result()
      nms <- rownames(m)
      df  <- tryCatch({
        m_t <- methods::as(m, "TsparseMatrix")
        raw <- data.frame(i = m_t@i + 1L, j = m_t@j + 1L, x = m_t@x)
        raw <- raw[raw$i < raw$j & raw$x > 0, ]
        raw <- raw[order(-raw$x), ]
        raw <- utils::head(raw, 300)
        data.frame(
          "Node 1"     = nms[raw$i],
          "Node 2"     = nms[raw$j],
          "Similarity" = round(raw$x, 4),
          stringsAsFactors = FALSE,
          check.names  = FALSE
        )
      }, error = function(e) NULL)
      req(!is.null(df))
      DT::datatable(df,
                    options  = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # ── Download handlers ─────────────────────────────────────────────
    output$download_network_ui <- renderUI({
      req(mo_data$mnet_obj)
      tagList(
        tags$hr(class = "my-2"),
        div(class = "d-flex gap-2 mt-1",
          downloadButton(
            ns("download_mnet"),
            "Network object (.rda)",
            class = "btn-mapa-dl mb-1"
          ),
          downloadButton(
            ns("download_sim"),
            "Similarity matrix (.rda)",
            class = "btn-mapa-dl mb-1"
          )
        )
      )
    })

    output$download_mnet <- downloadHandler(
      filename = function() paste0("mnet_obj_", Sys.Date(), ".rda"),
      content  = function(file) {
        mnet_obj <- mo_data$mnet_obj
        save(mnet_obj, file = file)
      }
    )

    output$download_sim <- downloadHandler(
      filename = function() paste0("sim_matrix_", Sys.Date(), ".rda"),
      content  = function(file) {
        sim_matrix <- mo_data$sim_matrix
        save(sim_matrix, file = file)
      }
    )

    # ── Proceed button (appears after successful computation) ─────────
    output$proceed_ui <- renderUI({
      req(mo_data$mnet_obj, sim_result())
      actionButton(
        ns("btn_proceed"),
        tagList("Proceed to Module Identification",
                tags$i(class = "fas fa-arrow-right ms-1")),
        class = "btn-step-next w-100 mt-2"
      )
    })

    observeEvent(input$btn_proceed, go_next())

    # ── Auto-load enrichment files on upload ──────────────────────────
    .load_rda <- function(path) {
      env <- new.env(parent = emptyenv())
      load(path, envir = env)
      get(ls(env)[1], envir = env)
    }

    observeEvent(input$upload_t_enrich, {
      req(input$upload_t_enrich)
      mo_data$transcriptome_enrich <- .load_rda(input$upload_t_enrich$datapath)
    })
    observeEvent(input$upload_p_enrich, {
      req(input$upload_p_enrich)
      mo_data$proteome_enrich <- .load_rda(input$upload_p_enrich$datapath)
    })
    observeEvent(input$upload_m_enrich, {
      req(input$upload_m_enrich)
      mo_data$metabolome_enrich <- .load_rda(input$upload_m_enrich$datapath)
    })

    # ── Update embedding model choices on provider change ─────────────
    observeEvent(input$api_provider, {
      key <- switch(input$api_provider,
                    siliconflow = "SiliconFlow",
                    openai      = "OpenAI",
                    gemini      = "Google Gemini")
      updateSelectizeInput(
        session, "embed_model",
        choices  = .EMBED_MODELS[[key]],
        selected = .EMBED_DEFAULTS[input$api_provider]
      )
    })

    # ── Main build pipeline ───────────────────────────────────────────
    observeEvent(input$btn_next, {
      req(mo_data$transcriptome_enrich)

      shinyalert::shinyalert(
        title = "Building multi-omics network...",
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

      # 1. Download / verify edge databases
      db_paths <- tryCatch(
        .ensure_edge_databases(),
        error = function(e) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "Database download failed", text = e$message,
            type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          NULL
        }
      )
      if (is.null(db_paths)) return()
      db_checked(db_checked() + 1)

      # 2. Build network tables
      taxon_id <- .orgdb_to_taxon_id(mo_data$transcriptome_org)
      network_tables <- tryCatch(
        mapa::build_network_tables(
          transcriptome_enrich = mo_data$transcriptome_enrich,
          proteome_enrich      = mo_data$proteome_enrich,
          metabolome_enrich    = mo_data$metabolome_enrich,
          taxon_id             = taxon_id,
          string_score_cutoff  = input$string_score,
          tf_confidence_levels = input$tf_confidence
        ),
        error = function(e) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "Network table build failed", text = e$message,
            type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          NULL
        }
      )
      if (is.null(network_tables)) return()

      # 3. Build multiplex heterogeneous network
      embed_source <- input$embed_source %||% "local"
      mnet_params  <- list(
        network_tables   = network_tables,
        embedding_source = embed_source
      )
      if (embed_source == "realtime") {
        if (is.null(input$api_key) || nchar(trimws(input$api_key)) == 0) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "API key required",
            text  = "Enter an API key for realtime embedding.",
            type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          return()
        }
        mnet_params$api_provider         <- input$api_provider
        mnet_params$text_embedding_model <- input$embed_model
        mnet_params$api_key              <- input$api_key
      }
      mnet_obj <- tryCatch(
        do.call(mapa::build_MNetwork, mnet_params),
        error = function(e) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "Network build failed", text = e$message,
            type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          NULL
        }
      )
      if (is.null(mnet_obj)) return()

      # 4. Compute RWR similarity
      sim_matrix <- tryCatch(
        mapa::get_multi_omics_sim(
          mnet_obj     = mnet_obj,
          min_path_sim = input$min_path_sim,
          r            = input$r,
          eta          = input$eta,
          lambda       = input$lambda,
          delta1       = input$delta1,
          delta2       = input$delta2
        ),
        error = function(e) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "Similarity computation failed", text = e$message,
            type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          NULL
        }
      )
      if (is.null(sim_matrix)) return()

      shinyalert::closeAlert()

      mo_data$mnet_obj   <- mnet_obj
      mo_data$sim_matrix <- sim_matrix
      sim_result(sim_matrix)

      embed_part <- if (embed_source == "realtime") {
        sprintf(
          ",\n  api_provider = \"%s\",\n  text_embedding_model = \"%s\",\n  api_key = \"your_api_key\"",
          input$api_provider, input$embed_model
        )
      } else ""
      net_code(sprintf(
        paste0(
          "# Step 1: Build network tables\n",
          "network_tables <- mapa::build_network_tables(\n",
          "  transcriptome_enrich = transcriptome_enrich,\n",
          "  proteome_enrich = proteome_enrich,\n",
          "  metabolome_enrich = metabolome_enrich,\n",
          "  taxon_id = %s,\n",
          "  string_score_cutoff = %s,\n",
          "  tf_confidence_levels = \"%s\"\n",
          ")\n\n",
          "# Step 2: Build multiplex network\n",
          "mnet_obj <- mapa::build_MNetwork(\n",
          "  network_tables = network_tables,\n",
          "  embedding_source = \"%s\"%s\n",
          ")\n\n",
          "# Step 3: Compute RWR similarity\n",
          "sim_matrix <- mapa::get_multi_omics_sim(\n",
          "  mnet_obj = mnet_obj,\n",
          "  min_path_sim = %s,\n",
          "  r = %s, eta = %s,\n",
          "  lambda = %s, delta1 = %s, delta2 = %s\n",
          ")"
        ),
        taxon_id, input$string_score, input$tf_confidence,
        embed_source, embed_part,
        input$min_path_sim, input$r, input$eta,
        input$lambda, input$delta1, input$delta2
      ))
    })

    observeEvent(input$btn_back, go_back())
  })
}
