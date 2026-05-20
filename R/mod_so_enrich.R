# ── Step SO-2: Pathway Enrichment (Single-Omics) ─────────────────────────────

.P_ADJUST_METHODS <- c(
  "BH (Benjamini-Hochberg)" = "BH",
  "FDR"                     = "fdr",
  "Bonferroni"              = "bonferroni",
  "Holm"                    = "holm",
  "Hochberg"                = "hochberg",
  "Hommel"                  = "hommel",
  "BY (Benjamini-Yekutieli)" = "BY",
  "None"                    = "none"
)

# Database info: list(value, label, description, url)
.GENE_DB_INFO <- list(
  list(value = "go", label = "Gene Ontology (GO)",
       desc  = "A comprehensive resource describing gene functions across species.",
       url   = "https://geneontology.org"),
  list(value = "kegg", label = "KEGG",
       desc  = "Kyoto Encyclopedia of Genes and Genomes — metabolic and signaling pathways.",
       url   = "https://www.genome.jp/kegg"),
  list(value = "reactome", label = "Reactome",
       desc  = "A curated, peer-reviewed pathway database for human biology.",
       url   = "https://reactome.org")
)

.MET_DB_INFO <- list(
  list(value = "metkegg", label = "KEGG",
       desc  = "KEGG metabolite pathway enrichment using compound IDs.",
       url   = "https://www.genome.jp/kegg/compound"),
  list(value = "hmdb", label = "SMPDB",
       desc  = paste0("Small Molecule Pathway Database — comprehensive metabolic and",
                      " disease pathways for small molecules."),
       url   = "https://www.smpdb.ca")
)

.org2react <- c(
  "org.Hs.eg.db"  = "human",     "org.Mm.eg.db"  = "mouse",
  "org.Rn.eg.db"  = "rat",       "org.Dm.eg.db"  = "fly",
  "org.Dr.eg.db"  = "zebrafish", "org.Sc.sgd.db" = "yeast",
  "org.Ce.eg.db"  = "celegans",  "org.Bt.eg.db"  = "bovine",
  "org.Cf.eg.db"  = "canine",    "org.Gg.eg.db"  = "chicken"
)

# Build choiceNames list with hover-info icons.
.db_choice_names <- function(db_info_list) {
  lapply(db_info_list, function(db) {
    tagList(
      db$label,
      bslib::tooltip(
        trigger = tags$span(
          shiny::icon("circle-info"),
          style = "color:#6c757d;font-size:0.8em;margin-left:4px;cursor:pointer;"
        ),
        paste0(db$desc, " — ", db$url)
      )
    )
  })
}

#' @noRd
mod_so_enrich_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Single-Omics  •  Step 2",
    .title       = "Pathway Enrichment",
    .subtitle    = "Enrich your markers against one or more pathway databases.",

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      # ── Left: parameters ──
      mapa_card(
        "Enrichment Parameters",

        # ── 1. Analysis type ──────────────────────────────────────────
        uiOutput(ns("analysis_type_ui")),

        tags$hr(class = "my-2"),

        # ── 2. Databases ──────────────────────────────────────────────
        tags$label(class = "form-label fw-semibold", "Pathway databases"),
        uiOutput(ns("databases_ui")),

        # GO parameters — shown when GO is selected
        conditionalPanel(
          condition = sprintf(
            "input['%s'] && input['%s'].includes('go')",
            ns("databases"), ns("databases")
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
            ns("databases"), ns("databases")
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

        # ── 3. Statistical thresholds ─────────────────────────────────
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
        uiOutput(ns("download_ui")),
        uiOutput(ns("enrich_status")),
        uiOutput(ns("result_tabs_ui"))
      )
    )
  )
}

#' @noRd
mod_so_enrich_server <- function(id, so_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    enrich_result <- reactiveVal(NULL)
    enrich_code   <- reactiveVal(NULL)

    # ── Disable Run button until upload data is present ──────────────
    observe({
      if (is.null(so_data$variable_info)) shinyjs::disable("btn_next")
      else                                shinyjs::enable("btn_next")
    })

    # ── Show reproducible R code (only available after a successful run)
    observeEvent(input$btn_code, {
      if (is.null(enrich_code())) {
        shinyalert::shinyalert(
          title = "No code available",
          text  = "Run enrichment first to generate the reproducible code.",
          type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      } else {
        show_code_modal(enrich_code())
      }
    })

    # ── Helper: build code string from run parameters ─────────────────
    .build_enrich_code <- function(qt, dbs, org_str, analysis,
                                   order_by, p_cutoff, p_adj,
                                   min_gs, max_gs,
                                   go_keytype, go_ont, kegg_keytype,
                                   met_org) {
      fmt_dbs <- function(v) {
        if (length(v) == 1) sprintf('"%s"', v)
        else sprintf('c(%s)', paste0('"', v, '"', collapse = ", "))
      }
      if (qt == "gene") {
        org_param <- if (!is.null(org_str) && nzchar(org_str))
          sprintf('\n  go.orgdb = %s,', org_str) else ""
        fn <- if (identical(analysis, "gsea")) "mapa::do_gsea" else "mapa::enrich_pathway"
        order_line <- if (identical(analysis, "gsea"))
          sprintf('\n  order_by = "%s",', order_by) else ""
        sprintf(paste0(
          'enriched_pathways <- %s(\n  variable_info = your_converted_data,\n',
          '  query_type = "gene",\n  database = %s,%s\n  pvalueCutoff = %s,\n',
          '  pAdjustMethod = "%s",\n  minGSSize = %s,\n  maxGSSize = %s,%s\n',
          '  go.keytype = "%s",\n  go.ont = "%s",\n  kegg.keytype = "%s",\n',
          '  save_to_local = FALSE\n)'),
          fn, fmt_dbs(dbs), order_line, p_cutoff, p_adj, min_gs, max_gs,
          org_param, go_keytype, go_ont, kegg_keytype)
      } else {
        sprintf(paste0(
          'enriched_pathways <- mapa::enrich_pathway(\n  variable_info = your_converted_data,\n',
          '  query_type = "metabolite",\n  database = %s,\n  met_organism = "%s",\n',
          '  pvalueCutoff = %s,\n  pAdjustMethod = "%s",\n  minGSSize = %s,\n',
          '  maxGSSize = %s,\n  save_to_local = FALSE\n)'),
          fmt_dbs(dbs), met_org %||% "hsa", p_cutoff, p_adj, min_gs, max_gs)
      }
    }

    # ── Analysis type UI (GSEA only for genes) ───────────────────────
    output$analysis_type_ui <- renderUI({
      is_gene <- !isTRUE(so_data$query_type == "metabolite")
      tagList(
        if (is_gene) {
          tagList(
            radioButtons(ns("analysis_type"), "Analysis type",
                         choices  = c("Over-representation (ORA)" = "ora",
                                      "Gene Set Enrichment (GSEA)" = "gsea"),
                         selected = isolate(input$analysis_type) %||% "ora"),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'gsea'", ns("analysis_type")),
              uiOutput(ns("gsea_order_by_ui"))
            )
          )
        } else {
          tagList(
            tags$p(class = "form-label fw-semibold mb-1", "Analysis type"),
            tags$p(class = "text-muted small",
                   "Over-representation analysis (ORA) for metabolite data.")
          )
        }
      )
    })

    # GSEA ranking column — populated from uploaded variable_info columns
    output$gsea_order_by_ui <- renderUI({
      vi      <- so_data$variable_info
      id_cols <- c("ensembl", "entrezid", "uniprot", "symbol", "keggid",
                   "hmdbid", "variable_id", "molecular_formula", "mz", "rt")
      if (is.null(vi)) {
        choices <- character(0)
      } else {
        num_cols <- names(vi)[sapply(vi, is.numeric)]
        choices  <- setdiff(num_cols, id_cols)
        if (length(choices) == 0) choices <- setdiff(names(vi), id_cols)
      }
      selectInput(ns("order_by"), "Rank genes by column",
                  choices = choices, selected = choices[1])
    })

    # ── Database checkboxes (gene vs metabolite) ─────────────────────
    output$databases_ui <- renderUI({
      is_gene <- !isTRUE(so_data$query_type == "metabolite")
      db_info <- if (is_gene) .GENE_DB_INFO else .MET_DB_INFO
      def_sel <- if (is_gene) c("go", "kegg", "reactome")
                 else c("hmdb", "metkegg")

      checkboxGroupInput(
        ns("databases"),
        label       = NULL,
        choiceValues = vapply(db_info, `[[`, character(1), "value"),
        choiceNames  = .db_choice_names(db_info),
        selected     = def_sel
      )
    })

    # ── Status banner ────────────────────────────────────────────────
    output$enrich_status <- renderUI({
      res <- enrich_result()
      if (is.null(res)) {
        status_alert(
          "Configure parameters and click Run Enrichment.", "info"
        )
      } else {
        n <- .count_enriched_pathways(res)
        status_alert(
          paste0(n, " pathways enriched across selected databases."),
          "success"
        )
      }
    })

    # ── Result tabs (one tab per database with results) ──────────────
    output$result_tabs_ui <- renderUI({
      res <- enrich_result()
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
          DT::DTOutput(ns(paste0("dt_", db)))
        )
      })
      panels <- Filter(Negate(is.null), panels)
      if (length(panels) == 0) return(NULL)

      do.call(bslib::navset_tab,
              c(panels, list(id = ns("result_db_tabs"))))
    })

    # One DT renderer per database slot
    for (.db in c("go", "kegg", "reactome", "hmdb", "metkegg")) {
      local({
        db <- .db
        output[[paste0("dt_", db)]] <- DT::renderDT({
          res <- enrich_result()
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

    # "Proceed" button appears only after enrichment succeeds
    output$proceed_ui <- renderUI({
      req(enrich_result())
      actionButton(
        ns("btn_proceed"),
        tagList("Proceed to Next Step",
                tags$i(class = "fas fa-arrow-right ms-1")),
        class = "btn-step-next w-100 mt-2"
      )
    })

    observeEvent(input$btn_proceed, go_next())

    # ── Download result as RDA ────────────────────────────────────────
    output$download_ui <- renderUI({
      req(enrich_result())
      downloadButton(
        ns("download_rds"),
        "Enrichment result (.rda)",
        class = "btn-mapa-dl mb-2"
      )
    })

    output$download_rds <- downloadHandler(
      filename = function() {
        paste0("enrichment_result_", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".rda")
      },
      content = function(file) {
        result <- enrich_result()
        save(result, file = file)
      }
    )

    # ── Run enrichment ────────────────────────────────────────────────
    observeEvent(input$btn_next, {
      req(so_data$variable_info, so_data$query_type)

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

      reactome_skipped <- FALSE
      result <- tryCatch({
        common_params <- list(
          variable_info = so_data$variable_info,
          query_type    = so_data$query_type,
          database      = input$databases,
          pvalueCutoff  = input$p_cutoff,
          pAdjustMethod = input$p_adjust_method,
          minGSSize     = input$min_gs_size,
          maxGSSize     = input$max_gs_size,
          save_to_local = FALSE
        )

        if (so_data$query_type == "gene") {
          org_str <- so_data$organism
          org_obj <- if (!is.null(org_str) && nchar(org_str) > 0)
            get(org_str, envir = asNamespace(org_str))
          else
            NULL

          if ("go" %in% input$databases) {
            common_params$go.orgdb   <- org_obj
            common_params$go.keytype <- input$go_keytype
            common_params$go.ont     <- input$go_ont
          }
          if ("kegg" %in% input$databases) {
            common_params$kegg.keytype <- input$kegg_keytype
            kegg_code <- .orgdb_to_kegg(org_str)
            if (!is.null(kegg_code))
              common_params$kegg.organism <- kegg_code
          }
          if ("reactome" %in% input$databases) {
            reactome_org <- if (!is.null(org_str) && nzchar(org_str))
              .org2react[org_str]
            else
              NA_character_
            if (!is.na(reactome_org)) {
              common_params$reactome.organism <- unname(reactome_org)
            } else {
              common_params$database <-
                setdiff(common_params$database, "reactome")
              reactome_skipped <- TRUE
            }
          }

          analysis <- input$analysis_type %||% "ora"
          if (analysis == "gsea") {
            req(input$order_by)
            common_params$order_by <- input$order_by
            do.call(mapa::do_gsea, common_params)
          } else {
            do.call(mapa::enrich_pathway, common_params)
          }
        } else {
          common_params$met_organism <- so_data$organism
          do.call(mapa::enrich_pathway, common_params)
        }
      }, error = function(e) {
        shinyalert::closeAlert()
        shinyalert::shinyalert(
          title = "Enrichment failed", text = e$message,
          type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        NULL
      })

      shinyalert::closeAlert()

      if (reactome_skipped) {
        shiny::showNotification(
          paste0(
            "Reactome was skipped: \"", so_data$organism, "\" has no ",
            "Reactome equivalent. Supported organisms: ",
            paste(unname(.org2react), collapse = ", "), "."
          ),
          type     = "warning",
          duration = 12
        )
      }

      if (!is.null(result)) {
        enrich_result(result)
        so_data$enriched_pathways <- result
        enrich_code(.build_enrich_code(
          qt          = so_data$query_type,
          dbs         = input$databases,
          org_str     = so_data$organism,
          analysis    = input$analysis_type %||% "ora",
          order_by    = input$order_by %||% "logFC",
          p_cutoff    = input$p_cutoff,
          p_adj       = input$p_adjust_method,
          min_gs      = input$min_gs_size,
          max_gs      = input$max_gs_size,
          go_keytype  = input$go_keytype,
          go_ont      = input$go_ont,
          kegg_keytype = input$kegg_keytype,
          met_org     = so_data$organism
        ))
      }
    })

    observeEvent(input$btn_back, go_back())
  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────

.enrich_datatable <- function(df, p_cutoff = 0.05) {
  if ("p.adjust" %in% names(df))
    df <- df[!is.na(df$p.adjust) & df$p.adjust < p_cutoff, , drop = FALSE]
  DT::datatable(
    df,
    options  = list(pageLength = 5, scrollX = TRUE),
    rownames = FALSE
  )
}

.count_enriched_pathways <- function(fm) {
  slots <- list(
    tryCatch(nrow(fm@enrichment_go_result@result),
             error = function(e) 0L),
    tryCatch(nrow(fm@enrichment_kegg_result@result),
             error = function(e) 0L),
    tryCatch(nrow(fm@enrichment_reactome_result@result),
             error = function(e) 0L),
    tryCatch(nrow(fm@enrichment_hmdb_result@result),
             error = function(e) 0L),
    tryCatch(nrow(fm@enrichment_metkegg_result@result),
             error = function(e) 0L)
  )
  sum(unlist(slots))
}

.orgdb_to_kegg <- function(orgdb_str) {
  if (is.null(orgdb_str) || nchar(orgdb_str) == 0) return(NULL)
  tryCatch({
    org_obj  <- get(orgdb_str, envir = asNamespace(orgdb_str))
    sci_name <- BiocGenerics::species(org_obj)
    kegg_res <- clusterProfiler::search_kegg_organism(
      str         = sci_name,
      by          = "scientific_name",
      ignore.case = TRUE
    )
    if (nrow(kegg_res) > 0 && !is.na(kegg_res$kegg_code[1]))
      kegg_res$kegg_code[1]
    else
      NULL
  }, error = function(e) NULL)
}
