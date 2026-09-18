# ── Step SO-3: Pathway Similarity (Single-Omics) ─────────────────────────────

.EMBED_MODELS <- list(
  SiliconFlow = list(
    "Qwen/Qwen3-Embedding-0.6B" = "Qwen/Qwen3-Embedding-0.6B",
    "Qwen/Qwen3-Embedding-4B"   = "Qwen/Qwen3-Embedding-4B",
    "Qwen/Qwen3-Embedding-8B"   = "Qwen/Qwen3-Embedding-8B"
  ),
  OpenAI = list(
    "text-embedding-3-small" = "text-embedding-3-small",
    "text-embedding-3-large" = "text-embedding-3-large",
    "text-embedding-ada-002" = "text-embedding-ada-002"
  ),
  `Google Gemini` = list(
    "models/text-embedding-004"   = "models/text-embedding-004",
    "models/gemini-embedding-001" = "models/gemini-embedding-001"
  )
)

.EMBED_DEFAULTS <- c(
  siliconflow = "Qwen/Qwen3-Embedding-8B",
  openai      = "text-embedding-3-small",
  gemini      = "models/text-embedding-004"
)

.SIM_MEASURE_GENE <- c(
  "Sim_XGraSM_2013", "Sim_Wang_2007",       "Sim_Lin_1998",
  "Sim_Resnik_1999", "Sim_FaITH_2010",      "Sim_Relevance_2006",
  "Sim_SimIC_2010",  "Sim_EISI_2015",        "Sim_AIC_2014",
  "Sim_Zhang_2006",  "Sim_universal",         "Sim_GOGO_2018",
  "Sim_Rada_1989",   "Sim_Resnik_edge_2005", "Sim_Leocock_1998",
  "Sim_WP_1994",     "Sim_Slimani_2006",     "Sim_Shenoy_2012",
  "Sim_Pekar_2002",  "Sim_Stojanovic_2001",  "Sim_Wang_edge_2012",
  "Sim_Zhong_2002",  "Sim_AlMubaid_2006",    "Sim_Li_2003",
  "Sim_RSS_2013",    "Sim_HRSS_2013",         "Sim_Shen_2010",
  "Sim_SSDD_2013",   "Sim_Jiang_1997",        "Sim_Kappa",
  "Sim_Jaccard",     "Sim_Dice",              "Sim_Overlap",
  "Sim_Ancestor"
)

.SIM_MEASURE_MET <- c("jaccard", "dice", "overlap", "kappa")

# ── UI helpers ────────────────────────────────────────────────────────────────

.sim_db_card <- function(ns, label, db, measure_choices, measure_default) {
  tags$div(
    class = "mb-2 p-2 border rounded",
    tags$strong(class = "d-block mb-1 small", label),
    bslib::layout_columns(
      col_widths = c(6, 6),
      selectInput(ns(paste0("sim_measure_", db)), "Similarity measure",
                  choices = measure_choices, selected = measure_default),
      numericInput(ns(paste0("sim_cutoff_", db)), "Similarity cutoff",
                   0.5, 0, 1, 0.05)
    )
  )
}

# ── Module UI ─────────────────────────────────────────────────────────────────

#' @noRd
mod_so_similarity_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Single-Omics  •  Step 3",
    .title       = "Pathway Similarity Computation",
    .subtitle    = paste0("Compute functional similarity between enriched pathways using ",
                          "biotext embedding similarity or overlap/ontology-based similarity."),

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      # ── Left: parameters ──
      mapa_card(
        "Similarity Parameters",

        uiOutput(ns("carryover_ui")),
        optional_upload_caption("Step 2 · Pathway Enrichment"),
        fileInput(ns("upload_enriched"),
                  "Upload enrichment result (.rda)",
                  accept = ".rda"),
        tags$hr(class = "my-2"),

        # ── 1. Method ─────────────────────────────────────────────────
        radioButtons(
          ns("method"), "Method",
          choices = c(
            "Biotext embedding similarity (recommended)" = "embedcluster",
            "Overlap/Ontology-based similarity"          = "simcluster"
          ),
          selected = "embedcluster"
        ),

        tags$hr(class = "my-2"),

        # ── 2. EmbedCluster parameters ────────────────────────────────
        conditionalPanel(
          condition = sprintf("input['%s'] === 'embedcluster'", ns("method")),
          uiOutput(ns("embed_params_ui"))
        ),

        # ── 3. SimCluster parameters ──────────────────────────────────
        conditionalPanel(
          condition = sprintf("input['%s'] === 'simcluster'", ns("method")),
          uiOutput(ns("simcluster_params_ui"))
        ),

        step_nav_buttons(ns, next_label = "Compute Similarity",
                         next_arrow = FALSE, show_code = TRUE),
        uiOutput(ns("proceed_ui"))
      ),

      # ── Right: results ──
      mapa_card(
        "Similarity Results",
        uiOutput(ns("dl_rda_top")),
        uiOutput(ns("sim_status")),
        uiOutput(ns("result_panel_ui"))
      )
    )
  )
}

# ── Module Server ─────────────────────────────────────────────────────────────

#' @noRd
mod_so_similarity_server <- function(id, so_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sim_result      <- reactiveVal(NULL)
    sim_method_used <- reactiveVal(NULL)
    sim_code        <- reactiveVal(NULL)

    # ── Disable Run button until enrichment result is present ─────────
    observe({
      if (is.null(so_data$enriched_pathways)) shinyjs::disable("btn_next")
      else                                     shinyjs::enable("btn_next")
    })

    # ── Carry-over banner (is Step 2's result already in the session?) ─
    output$carryover_ui <- renderUI({
      carryover_status(
        !is.null(so_data$enriched_pathways),
        "Step 2 · Pathway Enrichment",
        ready_detail = "Enriched pathways were carried over from the previous step."
      )
    })

    # ── .rda download button above the status banner ──────────────────
    output$dl_rda_top <- renderUI({
      req(sim_result())
      downloadButton(
        ns("dl_object"),
        "Similarity result (.rda)",
        class = "btn-mapa-dl mb-2"
      )
    })

    # ── Show reproducible R code ──────────────────────────────────────
    observeEvent(input$btn_code, {
      if (is.null(sim_code())) {
        shinyalert::shinyalert(
          title = "No code yet",
          text  = "Run the similarity computation first to view the reproducible code.",
          type  = "warning", confirmButtonCol = "#dd4b39"
        )
        return()
      }
      show_code_modal(sim_code())
    })

    # ── Upload enrichment result ─────────────────────────────────────
    observeEvent(input$upload_enriched, {
      req(input$upload_enriched$datapath)
      tmp <- new.env()
      load(input$upload_enriched$datapath, envir = tmp)
      nms <- ls(tmp)
      if (length(nms) == 1) {
        so_data$enriched_pathways <- get(nms[1], envir = tmp)
      } else {
        shinyalert::shinyalert(
          title = "Invalid file content",
          text  = "The .rda file must contain exactly one object.",
          type  = "error", html = TRUE,
          confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── EmbedCluster parameters UI ───────────────────────────────────
    output$embed_params_ui <- renderUI({
      tagList(
        # 1. Embedding source ──────────────────────────────────────────
        radioButtons(
          ns("embed_source"), "Embedding source",
          choices = c(
            "Local database (pre-computed, no API key required)" = "local",
            "Realtime API (on-the-fly, API key required)"        = "realtime"
          ),
          selected = "local"
        ),

        # 2. Realtime-only parameters (shown only when source = realtime)
        conditionalPanel(
          condition = sprintf("input['%s'] === 'realtime'", ns("embed_source")),
          tags$div(
            class = "mt-1 ps-2 border-start border-2",
            selectInput(
              ns("api_provider"), "API provider",
              choices  = c("SiliconFlow" = "siliconflow",
                           "OpenAI"      = "openai",
                           "Google"      = "gemini"),
              selected = "siliconflow"
            ),
            selectizeInput(
              ns("embedding_model"), "Embedding model",
              choices  = .EMBED_MODELS[["SiliconFlow"]],
              selected = "Qwen/Qwen3-Embedding-8B",
              options  = list(create      = TRUE,
                              placeholder = "Select or enter model name")
            ),
            passwordInput(ns("api_key"), "API key")
          )
        )
      )
    })

    # ── SimCluster parameters UI ─────────────────────────────────────
    output$simcluster_params_ui <- renderUI({
      is_gene <- isTRUE(so_data$query_type == "gene")
      avail   <- .get_available_dbs(so_data$enriched_pathways)

      if (is_gene) {
        db_meta <- list(
          go       = list(label = "GO",       m = .SIM_MEASURE_GENE, d = "Sim_XGraSM_2013"),
          kegg     = list(label = "KEGG",     m = .SIM_MEASURE_MET,  d = "jaccard"),
          reactome = list(label = "Reactome", m = .SIM_MEASURE_MET,  d = "jaccard")
        )
        dbs <- intersect(avail, names(db_meta))
        tagList(lapply(dbs, function(db) {
          .sim_db_card(ns, db_meta[[db]]$label, db,
                       db_meta[[db]]$m, db_meta[[db]]$d)
        }))
      } else {
        db_meta <- list(
          hmdb    = list(label = "SMPDB",              m = .SIM_MEASURE_MET, d = "jaccard"),
          metkegg = list(label = "KEGG (metabolites)", m = .SIM_MEASURE_MET, d = "jaccard")
        )
        dbs <- intersect(avail, names(db_meta))
        tagList(lapply(dbs, function(db) {
          .sim_db_card(ns, db_meta[[db]]$label, db,
                       db_meta[[db]]$m, db_meta[[db]]$d)
        }))
      }
    })

    # ── Update embedding model choices on provider change ────────────
    observeEvent(input$api_provider, {
      key <- switch(input$api_provider,
                    siliconflow = "SiliconFlow",
                    openai      = "OpenAI",
                    gemini      = "Google Gemini")
      updateSelectizeInput(
        session, "embedding_model",
        choices  = .EMBED_MODELS[[key]],
        selected = .EMBED_DEFAULTS[input$api_provider]
      )
    })

    # ── Status banner ────────────────────────────────────────────────
    output$sim_status <- renderUI({
      if (is.null(sim_result())) {
        status_alert(
          "Configure parameters and click Compute Similarity.", "info"
        )
      } else if (sim_method_used() == "simcluster") {
        status_alert(
          "Overlap/Ontology-based similarity computed — ready to download.",
          "success"
        )
      } else {
        status_alert(
          "Biotext embedding similarity matrix computed — ready to download.",
          "success"
        )
      }
    })

    # ── Result panel ─────────────────────────────────────────────────
    output$result_panel_ui <- renderUI({
      res <- sim_result()
      if (is.null(res)) return(NULL)

      tags$div(
        class = "mt-2",
        uiOutput(ns("sim_matrix_ui")),
        tags$div(
          class = "mt-2",
          downloadButton(ns("dl_sim_table"), "Download (.xlsx)",
                         class = "btn-mapa-dl")
        )
      )
    })

    # ── Similarity matrix UI (wide, pathway IDs as rows/cols) ────────
    output$sim_matrix_ui <- renderUI({
      res <- sim_result()
      req(res)

      if (sim_method_used() == "embedcluster") {
        DT::DTOutput(ns("dt_embed_sim"))
      } else {
        db_labels <- c(go = "GO", kegg = "KEGG", reactome = "Reactome",
                       hmdb = "SMPDB", metkegg = "KEGG (Met)")
        panels <- lapply(names(db_labels), function(db) {
          mat <- .get_wide_sim_matrix(res, db)
          if (is.null(mat)) return(NULL)
          bslib::nav_panel(db_labels[[db]],
                           DT::DTOutput(ns(paste0("dt_sim_mat_", db))))
        })
        panels <- Filter(Negate(is.null), panels)
        if (length(panels) == 0)
          return(status_alert("No similarity data available.", "warning"))
        do.call(bslib::navset_tab,
                c(panels, list(id = ns("sim_mat_tabs"))))
      }
    })

    # EmbedCluster: display sim_result()$sim_matrix as wide DT
    output$dt_embed_sim <- DT::renderDT({
      res <- sim_result()
      req(res, sim_method_used() == "embedcluster")
      mat <- res$sim_matrix
      req(!is.null(mat))
      df <- cbind(pathway_id = rownames(mat), as.data.frame(mat))
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    # SimCluster: one wide-matrix DT per database
    for (.db in c("go", "kegg", "reactome", "hmdb", "metkegg")) {
      local({
        db <- .db
        output[[paste0("dt_sim_mat_", db)]] <- DT::renderDT({
          res <- sim_result()
          req(res, sim_method_used() == "simcluster")
          mat <- .get_wide_sim_matrix(res, db)
          req(!is.null(mat))
          DT::datatable(mat, options = list(pageLength = 10, scrollX = TRUE),
                        rownames = FALSE)
        })
      })
    }

    # ── Download handlers ────────────────────────────────────────────
    output$dl_sim_table <- downloadHandler(
      filename = function() "similarity_matrix.xlsx",
      content  = function(file) {
        res <- sim_result()
        req(res)
        if (sim_method_used() == "embedcluster") {
          mat <- res$sim_matrix
          df  <- cbind(pathway_id = rownames(mat), as.data.frame(mat))
          writexl::write_xlsx(df, file)
        } else {
          dbs    <- c("go", "kegg", "reactome", "hmdb", "metkegg")
          sheets <- Filter(Negate(is.null),
                           setNames(lapply(dbs, .get_wide_sim_matrix,
                                          res = res), dbs))
          writexl::write_xlsx(sheets, file)
        }
      }
    )

    output$dl_object <- downloadHandler(
      filename = "similarity_result.rda",
      content  = function(file) {
        similarity_result <- sim_result()
        save(similarity_result, file = file)
      }
    )

    # ── Proceed button ────────────────────────────────────────────────
    output$proceed_ui <- renderUI({
      req(sim_result())
      actionButton(
        ns("btn_proceed"),
        tagList("Proceed to Next Step",
                tags$i(class = "fas fa-arrow-right ms-1")),
        class = "btn-step-next w-100 mt-2"
      )
    })

    observeEvent(input$btn_proceed, go_next())

    # ── Run similarity ────────────────────────────────────────────────
    observeEvent(input$btn_next, {
      req(so_data$enriched_pathways)

      shinyalert::shinyalert(
        title = "Computing pathway similarity...",
        text  = tags$div(
          style = "text-align:center;",
          tags$p("This may take several minutes.",
                 class = "text-muted small"),
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

      is_gene <- isTRUE(so_data$query_type == "gene")
      method  <- input$method %||% "embedcluster"

      result <- tryCatch({

        sel_dbs <- .get_available_dbs(so_data$enriched_pathways)
        if (length(sel_dbs) == 0)
          stop("No enriched databases available.")

        if (method == "embedcluster") {
          embed_source <- input$embed_source %||% "local"

          params <- list(
            object           = so_data$enriched_pathways,
            embedding_source = embed_source,
            database         = sel_dbs,
            save_to_local    = FALSE
          )

          if (embed_source == "realtime") {
            if (is.null(input$api_key) || nchar(trimws(input$api_key)) == 0) {
              stop("An API key is required for realtime embedding.")
            }
            params$api_provider         <- input$api_provider
            params$text_embedding_model <- input$embedding_model
            params$api_key              <- input$api_key
          }

          do.call(mapa::get_bioembedsim, params)

        } else {
          org_obj <- if (is_gene && !is.null(so_data$organism)) {
            tryCatch(
              get(so_data$organism,
                  envir = asNamespace(so_data$organism)),
              error = function(e) NULL
            )
          } else NULL

          params <- list(
            object        = so_data$enriched_pathways,
            database      = sel_dbs,
            save_to_local = FALSE
          )
          if (!is.null(org_obj))
            params$go.orgdb <- org_obj

          for (db in sel_dbs) {
            measure <- input[[paste0("sim_measure_", db)]]
            cutoff  <- input[[paste0("sim_cutoff_", db)]]
            if (!is.null(measure))
              params[[paste0("measure.method.", db)]] <- measure
            if (!is.null(cutoff))
              params[[paste0("sim.cutoff.", db)]] <- cutoff
          }
          do.call(mapa::merge_pathways, params)
        }

      }, error = function(e) {
        shinyalert::closeAlert()
        shinyalert::shinyalert(
          title = "Similarity computation failed",
          text  = e$message,
          type  = "error",
          html  = TRUE,
          confirmButtonCol = "#dd4b39"
        )
        NULL
      })

      shinyalert::closeAlert()

      if (!is.null(result)) {
        sim_result(result)
        sim_method_used(method)
        so_data$similarity_result <- result

        avail   <- .get_available_dbs(so_data$enriched_pathways)
        dbs_str <- if (length(avail) == 1) sprintf('"%s"', avail)
                   else sprintf('c(%s)', paste0('"', avail, '"', collapse = ", "))

        if (method == "embedcluster") {
          embed_source <- input$embed_source %||% "local"
          if (embed_source == "local") {
            code <- sprintf(
              "similarity_result <- mapa::get_bioembedsim(\n  object = enriched_pathways,\n  embedding_source = \"local\",\n  database = %s,\n  save_to_local = FALSE\n)",
              dbs_str)
          } else {
            code <- sprintf(
              "similarity_result <- mapa::get_bioembedsim(\n  object = enriched_pathways,\n  embedding_source = \"realtime\",\n  database = %s,\n  api_provider = \"%s\",\n  text_embedding_model = \"%s\",\n  api_key = \"your_api_key\",\n  save_to_local = FALSE\n)",
              dbs_str, input$api_provider %||% "siliconflow",
              input$embedding_model %||% "Qwen/Qwen3-Embedding-8B")
          }
        } else {
          measure_lines <- paste(
            sapply(avail, function(db) {
              measure <- input[[paste0("sim_measure_", db)]] %||% "jaccard"
              cutoff  <- input[[paste0("sim_cutoff_",  db)]] %||% 0.5
              sprintf("  measure.method.%s = \"%s\",\n  sim.cutoff.%s = %s,",
                      db, measure, db, cutoff)
            }),
            collapse = "\n"
          )
          org_line <- if (isTRUE(so_data$query_type == "gene") &&
                          !is.null(so_data$organism) && nzchar(so_data$organism))
            sprintf("\n  go.orgdb = %s,", so_data$organism) else ""
          code <- sprintf(
            "similarity_result <- mapa::merge_pathways(\n  object = enriched_pathways,\n  database = %s,%s\n%s\n  save_to_local = FALSE\n)",
            dbs_str, org_line, measure_lines)
        }
        sim_code(code)
      }
    })

    observeEvent(input$btn_back, go_back())
  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────

.get_wide_sim_matrix <- function(res, db) {
  gd <- tryCatch(
    slot(res, paste0("merged_pathway_", db))$graph_data,
    error = function(e) NULL
  )
  if (is.null(gd) || length(gd) == 0) return(NULL)
  edges <- tryCatch(
    igraph::as_data_frame(gd, what = "edges"),
    error = function(e) NULL
  )
  if (is.null(edges) || nrow(edges) == 0) return(NULL)
  nodes <- unique(c(edges$from, edges$to))
  mat   <- matrix(0, nrow = length(nodes), ncol = length(nodes),
                  dimnames = list(nodes, nodes))
  diag(mat) <- 1
  for (i in seq_len(nrow(edges))) {
    mat[edges$from[i], edges$to[i]] <- edges$sim[i]
    mat[edges$to[i], edges$from[i]] <- edges$sim[i]
  }
  cbind(pathway_id = nodes, as.data.frame(mat))
}

.get_available_dbs <- function(enriched) {
  if (is.null(enriched)) return(character(0))
  present <- c(
    go       = tryCatch(nrow(enriched@enrichment_go_result@result) > 0,
                        error = function(e) FALSE),
    kegg     = tryCatch(nrow(enriched@enrichment_kegg_result@result) > 0,
                        error = function(e) FALSE),
    reactome = tryCatch(nrow(enriched@enrichment_reactome_result@result) > 0,
                        error = function(e) FALSE),
    hmdb     = tryCatch(nrow(enriched@enrichment_hmdb_result@result) > 0,
                        error = function(e) FALSE),
    metkegg  = tryCatch(nrow(enriched@enrichment_metkegg_result@result) > 0,
                        error = function(e) FALSE)
  )
  names(which(present))
}
