# ── Step MO-2: Pathway Enrichment (Multi-Omics) ──────────────────────────────
# Run pathway enrichment for each uploaded omics layer independently.

#' @noRd
mod_mo_enrich_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Multi-Omics  •  Step 2",
    .title       = "Pathway Enrichment",
    .subtitle    = "Enrich each omics layer against pathway databases.",

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      # ── Left: parameters ──
      mapa_card(
        "Enrichment Parameters",

        # ── 1. Gene / Protein databases ───────────────────────────────
        tags$label(
          class = "form-label fw-semibold",
          "Gene / Protein databases",
          tags$small(class = "text-muted fw-normal ms-1",
                     "(Transcriptomics & Proteomics)")
        ),
        checkboxGroupInput(
          ns("gene_databases"),
          label        = NULL,
          choiceValues = vapply(.GENE_DB_INFO, `[[`, character(1), "value"),
          choiceNames  = .db_choice_names(.GENE_DB_INFO),
          selected     = c("go", "kegg", "reactome")
        ),

        # GO parameters — shown when GO is selected
        conditionalPanel(
          condition = sprintf(
            "input['%s'] && input['%s'].includes('go')",
            ns("gene_databases"), ns("gene_databases")
          ),
          tags$div(
            class = "mt-2",
            selectInput(ns("go_ont"), "GO ontology",
                        choices  = c("All"                = "ALL",
                                     "Biological Process" = "BP",
                                     "Cellular Component" = "CC",
                                     "Molecular Function" = "MF"),
                        selected = "ALL"),
            selectInput(ns("go_keytype"), "GO ID type",
                        choices  = c("Entrez Gene ID"  = "ENTREZID",
                                     "Gene Symbol"     = "SYMBOL",
                                     "Ensembl Gene ID" = "ENSEMBL",
                                     "UniProt ID"      = "UNIPROT"),
                        selected = "ENTREZID")
          )
        ),

        # KEGG parameters — shown when KEGG is selected
        conditionalPanel(
          condition = sprintf(
            "input['%s'] && input['%s'].includes('kegg')",
            ns("gene_databases"), ns("gene_databases")
          ),
          tags$div(
            class = "mt-2",
            selectInput(ns("kegg_keytype"), "KEGG ID type",
                        choices  = c("KEGG / Entrez"   = "kegg",
                                     "NCBI Gene ID"    = "ncbi-geneid",
                                     "NCBI Protein ID" = "ncbi-proteinid",
                                     "UniProt"         = "uniprot"),
                        selected = "kegg")
          )
        ),

        tags$hr(class = "my-2"),

        # ── 3. Metabolite databases ───────────────────────────────────
        tags$label(
          class = "form-label fw-semibold",
          "Metabolite databases",
          tags$small(class = "text-muted fw-normal ms-1", "(Metabolomics)")
        ),
        checkboxGroupInput(
          ns("met_databases"),
          label        = NULL,
          choiceValues = vapply(.MET_DB_INFO, `[[`, character(1), "value"),
          choiceNames  = .db_choice_names(.MET_DB_INFO),
          selected     = "metkegg"
        ),

        tags$hr(class = "my-2"),

        # ── 4. Statistical thresholds ─────────────────────────────────
        numericInput(ns("p_cutoff"), "p-value cutoff",
                     value = 0.05, min = 0, max = 1, step = 0.01),
        selectInput(ns("p_adjust_method"), "p-value adjustment method",
                    choices  = .P_ADJUST_METHODS,
                    selected = "BH"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          numericInput(ns("min_gs_size"), "Min gene set size",
                       value = 10, min = 1, step = 1),
          numericInput(ns("max_gs_size"), "Max gene set size",
                       value = 500, min = 1, step = 1)
        ),

        step_nav_buttons(ns, next_label = "Run Enrichment",
                         next_arrow = FALSE, show_code = TRUE),
        uiOutput(ns("proceed_ui"))
      ),

      # ── Right: results ──
      mapa_card(
        "Enrichment Results",
        uiOutput(ns("download_all_ui")),
        uiOutput(ns("enrich_status")),
        tags$hr(class = "my-2"),
        bslib::navset_tab(
          id = ns("result_tabs"),
          bslib::nav_panel(
            title = tagList(
              div(class = "omics-badge omics-T d-inline-flex me-1", "T"),
              "Transcriptomics"),
            value = "T",
            uiOutput(ns("tab_status_T")),
            uiOutput(ns("result_tabs_T"))
          ),
          bslib::nav_panel(
            title = tagList(
              div(class = "omics-badge omics-P d-inline-flex me-1", "P"),
              "Proteomics"),
            value = "P",
            uiOutput(ns("tab_status_P")),
            uiOutput(ns("result_tabs_P"))
          ),
          bslib::nav_panel(
            title = tagList(
              div(class = "omics-badge omics-M d-inline-flex me-1", "M"),
              "Metabolomics"),
            value = "M",
            uiOutput(ns("tab_status_M")),
            uiOutput(ns("result_tabs_M"))
          )
        )
      )
    )
  )
}

#' @noRd
mod_mo_enrich_server <- function(id, mo_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    enrich_t       <- reactiveVal(NULL)
    enrich_p       <- reactiveVal(NULL)
    enrich_m       <- reactiveVal(NULL)
    mo_enrich_code <- reactiveVal(NULL)

    # ── Disable Run button until any two omics layers are present ────
    observe({
      input_layers <- list(
        mo_data$transcriptome_data,
        mo_data$proteome_data,
        mo_data$metabolome_data
      )
      if (.mo_has_minimum_layers(input_layers)) shinyjs::enable("btn_next")
      else                                      shinyjs::disable("btn_next")
    })

    # ── Show reproducible R code ──────────────────────────────────────
    observeEvent(input$btn_code, {
      if (is.null(mo_enrich_code())) {
        shinyalert::shinyalert(
          title = "No code yet",
          text  = "Run enrichment first to view the reproducible code.",
          type  = "warning", confirmButtonCol = "#dd4b39"
        )
        return()
      }
      show_code_modal(mo_enrich_code())
    })

    # ── Status banner ────────────────────────────────────────────────
    output$enrich_status <- renderUI({
      n_done <- sum(!sapply(list(enrich_t(), enrich_p(), enrich_m()), is.null))
      if (n_done == 0)
        status_alert(
          "Configure parameters then click Run Enrichment.", "info"
        )
      else if (n_done < 2)
        status_alert(
          paste0(
            n_done, " layer enriched successfully; at least two are needed ",
            "to proceed."
          ),
          "warning"
        )
      else
        status_alert(
          paste0(n_done, " layer(s) enriched successfully."), "success"
        )
    })

    # ── Per-layer status banners ─────────────────────────────────────
    layer_tab_status <- function(enrich_rv, layer_name) {
      renderUI({
        layer_data <- switch(layer_name,
          T = mo_data$transcriptome_data,
          P = mo_data$proteome_data,
          M = mo_data$metabolome_data
        )
        layer_label <- c(
          T = "Transcriptomics", P = "Proteomics", M = "Metabolomics"
        )[[layer_name]]

        if (is.null(enrich_rv())) {
          if (is.null(layer_data))
            status_alert(paste0(layer_label, " not uploaded — skipped."),
                         "info")
          else
            status_alert("Not yet enriched. Click Run Enrichment.", "info")
        } else {
          n <- .count_enriched_pathways(enrich_rv())
          status_alert(paste0(n, " pathways found."), "success")
        }
      })
    }

    output$tab_status_T <- layer_tab_status(enrich_t, "T")
    output$tab_status_P <- layer_tab_status(enrich_p, "P")
    output$tab_status_M <- layer_tab_status(enrich_m, "M")

    # ── Per-layer download buttons (one .rda file per omics layer) ───
    # Each file holds a single enrichment object, which is what Step 3
    # expects; layers that were not enriched are not offered at all.
    .dl_layers <- list(
      list(rv = enrich_t, output_id = "download_T", obj_name = "transcriptomics_enrich",
           stub = "transcriptomics_enrichment", badge = "T", label = "Transcriptomics"),
      list(rv = enrich_p, output_id = "download_P", obj_name = "proteomics_enrich",
           stub = "proteomics_enrichment",      badge = "P", label = "Proteomics"),
      list(rv = enrich_m, output_id = "download_M", obj_name = "metabolomics_enrich",
           stub = "metabolomics_enrichment",    badge = "M", label = "Metabolomics")
    )

    output$download_all_ui <- renderUI({
      available <- Filter(function(l) !is.null(l$rv()), .dl_layers)
      req(length(available) > 0)
      tagList(
        tags$p(
          class = "text-muted small mb-1",
          "Each layer downloads as its own .rda file, ready to upload in Step 3."
        ),
        div(
          class = "d-flex flex-wrap gap-2 mb-2",
          lapply(available, function(l) {
            downloadButton(
              ns(l$output_id),
              tagList(
                div(class = paste0("omics-badge omics-", l$badge,
                                   " d-inline-flex me-1"), l$badge),
                paste0(l$label, " (.rda)")
              ),
              class = "btn-mapa-dl mb-1"
            )
          })
        )
      )
    })

    for (.l in .dl_layers) {
      local({
        layer <- .l
        output[[layer$output_id]] <- downloadHandler(
          filename = function() paste0(layer$stub, "_", Sys.Date(), ".rda"),
          content  = function(file) {
            obj <- layer$rv()
            req(obj)
            env <- new.env(parent = emptyenv())
            assign(layer$obj_name, obj, envir = env)
            save(list = layer$obj_name, file = file, envir = env)
          }
        )
      })
    }

    # ── Per-layer result tabs (rendered after enrichment) ────────────
    make_layer_result_tabs <- function(enrich_rv, layer_id) {
      renderUI({
        res <- enrich_rv()
        if (is.null(res)) return(NULL)

        db_slots <- list(
          go       = tryCatch(res@enrichment_go_result,
                              error = function(e) NULL),
          kegg     = tryCatch(res@enrichment_kegg_result,
                              error = function(e) NULL),
          reactome = tryCatch(res@enrichment_reactome_result,
                              error = function(e) NULL),
          hmdb     = tryCatch(res@enrichment_hmdb_result,
                              error = function(e) NULL),
          metkegg  = tryCatch(res@enrichment_metkegg_result,
                              error = function(e) NULL)
        )
        db_labels <- c(go = "GO", kegg = "KEGG", reactome = "Reactome",
                       hmdb = "SMPDB", metkegg = "KEGG Metabolites")

        panels <- lapply(names(db_slots), function(db) {
          slot <- db_slots[[db]]
          if (is.null(slot)) return(NULL)
          df <- tryCatch(slot@result, error = function(e) NULL)
          if (is.null(df) || nrow(df) == 0) return(NULL)
          bslib::nav_panel(
            title = db_labels[[db]],
            DT::DTOutput(ns(paste0("dt_", layer_id, "_", db)))
          )
        })
        panels <- Filter(Negate(is.null), panels)
        if (length(panels) == 0) return(NULL)

        do.call(bslib::navset_tab,
                c(panels, list(id = ns(paste0("db_tabs_", layer_id)))))
      })
    }

    output$result_tabs_T <- make_layer_result_tabs(enrich_t, "T")
    output$result_tabs_P <- make_layer_result_tabs(enrich_p, "P")
    output$result_tabs_M <- make_layer_result_tabs(enrich_m, "M")

    # ── Individual DT renderers (layer × database) ───────────────────
    for (.layer in c("T", "P", "M")) {
      for (.db in c("go", "kegg", "reactome", "hmdb", "metkegg")) {
        local({
          layer     <- .layer
          db        <- .db
          enrich_rv <- switch(layer,
                              T = enrich_t, P = enrich_p, M = enrich_m)
          output[[paste0("dt_", layer, "_", db)]] <- DT::renderDT({
            res <- enrich_rv()
            req(res)
            df <- tryCatch({
              slot_obj <- switch(db,
                go       = res@enrichment_go_result,
                kegg     = res@enrichment_kegg_result,
                reactome = res@enrichment_reactome_result,
                hmdb     = res@enrichment_hmdb_result,
                metkegg  = res@enrichment_metkegg_result
              )
              slot_obj@result
            }, error = function(e) NULL)
            req(!is.null(df) && nrow(df) > 0)
            .enrich_datatable(df, input$p_cutoff)
          })
        })
      }
    }

    # "Proceed" button appears only after at least two layers enriched
    output$proceed_ui <- renderUI({
      req(.mo_has_minimum_layers(list(enrich_t(), enrich_p(), enrich_m())))
      actionButton(
        ns("btn_proceed"),
        tagList("Proceed to Next Step",
                tags$i(class = "fas fa-arrow-right ms-1")),
        class = "btn-step-next w-100 mt-2"
      )
    })

    observeEvent(input$btn_proceed, go_next())

    # ── Run enrichment ────────────────────────────────────────────────
    observeEvent(input$btn_next, {
      input_layers <- list(
        mo_data$transcriptome_data,
        mo_data$proteome_data,
        mo_data$metabolome_data
      )
      if (!.mo_has_minimum_layers(input_layers)) {
        shinyalert::shinyalert(
          title = "At least two omics layers required",
          text = "Return to Step 1 and provide any two omics datasets.",
          type = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return()
      }

      shinyalert::shinyalert(
        title = "Running enrichment...",
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

      # Shared gene-layer parameters (T and P)
      gene_common <- list(
        query_type    = "gene",
        pvalueCutoff  = input$p_cutoff,
        pAdjustMethod = input$p_adjust_method,
        minGSSize     = input$min_gs_size,
        maxGSSize     = input$max_gs_size,
        save_to_local = FALSE
      )
      if ("go"   %in% input$gene_databases) {
        gene_common$go.ont     <- input$go_ont
        gene_common$go.keytype <- input$go_keytype
      }
      if ("kegg" %in% input$gene_databases) {
        gene_common$kegg.keytype <- input$kegg_keytype
      }

      run_gene_layer <- function(variable_info, org_str) {
        org_obj <- get(org_str, envir = asNamespace(org_str))
        params  <- c(
          list(variable_info = variable_info,
               database      = input$gene_databases),
          gene_common
        )
        if ("go"   %in% input$gene_databases)
          params$go.orgdb <- org_obj
        if ("kegg" %in% input$gene_databases)
          params$kegg.organism <- .orgdb_to_kegg(org_str) %||% "hsa"

        do.call(mapa::enrich_pathway, params)
      }

      # ── Transcriptomics (optional) ──
      t_result <- if (!is.null(mo_data$transcriptome_data)) {
        tryCatch(
          run_gene_layer(mo_data$transcriptome_data, mo_data$transcriptome_org),
          error = function(e) {
            warning("Transcriptomics enrichment failed: ", e$message)
            NULL
          }
        )
      } else NULL

      # ── Proteomics (optional) ──
      p_result <- if (!is.null(mo_data$proteome_data)) {
        tryCatch(
          run_gene_layer(mo_data$proteome_data, mo_data$proteome_org),
          error = function(e) {
            warning("Proteomics enrichment failed: ", e$message)
            NULL
          }
        )
      } else NULL

      # ── Metabolomics (optional, always ORA) ──
      m_result <- if (!is.null(mo_data$metabolome_data)) {
        tryCatch(
          mapa::enrich_pathway(
            variable_info = mo_data$metabolome_data,
            query_type    = "metabolite",
            database      = input$met_databases,
            met_organism  = mo_data$metabolome_org %||% "hsa",
            pvalueCutoff  = input$p_cutoff,
            pAdjustMethod = input$p_adjust_method,
            minGSSize     = input$min_gs_size,
            maxGSSize     = input$max_gs_size,
            save_to_local = FALSE
          ),
          error = function(e) {
            warning("Metabolomics enrichment failed: ", e$message)
            NULL
          }
        )
      } else NULL

      shinyalert::closeAlert()

      enrich_t(t_result)
      enrich_p(p_result)
      enrich_m(m_result)

      mo_data$transcriptome_enrich <- t_result
      mo_data$proteome_enrich      <- p_result
      mo_data$metabolome_enrich    <- m_result

      if (!.mo_has_minimum_layers(list(t_result, p_result, m_result))) {
        shinyalert::shinyalert(
          title = "Fewer than two layers enriched",
          text = paste0(
            "At least two enrichment results are required for multi-omics ",
            "network construction. Review the warnings and input data, then retry."
          ),
          type = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      }

      if (!all(sapply(list(t_result, p_result, m_result), is.null))) {
        org_str  <- mo_data$transcriptome_org %||%
          mo_data$proteome_org %||% "org.Hs.eg.db"
        met_org  <- mo_data$metabolome_org %||% "hsa"
        gene_dbs <- input$gene_databases %||% c("go", "kegg", "reactome")
        met_dbs  <- input$met_databases  %||% "metkegg"
        fmt_dbs  <- function(v) if (length(v) == 1) sprintf('"%s"', v)
                                 else sprintf('c(%s)', paste0('"', v, '"', collapse = ", "))
        org_line <- if (!is.null(org_str) && nzchar(org_str))
          sprintf("\n  go.orgdb = %s,", org_str) else ""
        gene_code <- function(label, result_name, input_name) {
          sprintf(
            paste0(
              "# %s enrichment\n",
              "%s <- mapa::enrich_pathway(\n",
              "  variable_info = %s,\n",
              "  query_type = \"gene\",\n",
              "  database = %s,\n",
              "  pvalueCutoff = %s,\n",
              "  pAdjustMethod = \"%s\",\n",
              "  minGSSize = %s,\n",
              "  maxGSSize = %s,%s\n",
              "  go.keytype = \"%s\",\n",
              "  go.ont = \"%s\",\n",
              "  kegg.keytype = \"%s\",\n",
              "  save_to_local = FALSE\n",
              ")"
            ),
            label, result_name, input_name, fmt_dbs(gene_dbs),
            input$p_cutoff, input$p_adjust_method,
            input$min_gs_size, input$max_gs_size, org_line,
            input$go_keytype %||% "ENTREZID",
            input$go_ont %||% "ALL", input$kegg_keytype %||% "kegg"
          )
        }

        code <- character(0)
        if (!is.null(mo_data$transcriptome_data)) {
          code <- c(code, gene_code(
            "Transcriptomics", "transcriptomics_enrich",
            "transcriptomics_converted"
          ))
        }
        if (!is.null(mo_data$proteome_data)) {
          code <- c(code, gene_code(
            "Proteomics", "proteomics_enrich", "proteomics_converted"
          ))
        }
        if (!is.null(mo_data$metabolome_data)) {
          code <- c(code, sprintf(
            "# Metabolomics enrichment\nmetabolomics_enrich <- mapa::enrich_pathway(\n  variable_info = metabolomics_converted,\n  query_type = \"metabolite\",\n  database = %s,\n  met_organism = \"%s\",\n  pvalueCutoff = %s,\n  pAdjustMethod = \"%s\",\n  minGSSize = %s,\n  maxGSSize = %s,\n  save_to_local = FALSE\n)",
            fmt_dbs(met_dbs), met_org,
            input$p_cutoff, input$p_adjust_method,
            input$min_gs_size, input$max_gs_size
          ))
        }
        mo_enrich_code(paste(code, collapse = "\n\n"))
      }
    })

    observeEvent(input$btn_back, go_back())
  })
}
