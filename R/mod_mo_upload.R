# ── Step MO-1: Upload Data (Multi-Omics) ─────────────────────────────────────
# Users upload raw gene/metabolite marker lists for each omics layer (CSV/xlsx).

# Extensible: add more organisms here as additional species are supported
.MO_GENE_ORGANISM_CHOICES <- c(
  "Human (org.Hs.eg.db)" = "org.Hs.eg.db"
)

# Maps org DB package → KEGG organism code; extend alongside .MO_GENE_ORGANISM_CHOICES
.MO_ORG_TO_KEGG <- c(
  "org.Hs.eg.db" = "hsa"
)

#' @noRd
mod_mo_upload_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Multi-Omics  •  Step 1",
    .title       = "Upload Multi-Omics Marker Data",
    .subtitle    = paste("Upload marker lists for each omics layer,",
                         "then convert IDs before proceeding."),

    bslib::layout_columns(
      col_widths = c(5, 7),
      gap = "1.25rem",

      mapa_card(
        "Omics Layer Files",

        tags$p(class = "text-muted small mb-2", "Load an example dataset:"),
        actionButton(ns("eg_demo"), "Load multi-omics example",
                     class = "btn-step-back btn-sm w-100"),
        tags$hr(class = "my-3"),

        # ── Shared organism ───────────────────────────────────────────
        selectInput(ns("organism"), "Organism",
                    choices  = .MO_GENE_ORGANISM_CHOICES,
                    selected = "org.Hs.eg.db",
                    width = "100%"),
        tags$hr(class = "my-2"),

        # ── Transcriptomics ──────────────────────────────────────────
        div(class = "omics-upload-section mb-3",
          div(class = "d-flex align-items-center gap-2 mb-2",
            div(class = "omics-badge omics-T", "T"),
            tags$strong("Transcriptomics"),
            uiOutput(ns("status_chip_T"), inline = TRUE)
          ),
          fileInput(ns("file_transcriptome"), NULL,
                    accept = c(".csv", ".xlsx"), width = "100%",
                    placeholder = "No file selected"),
          selectInput(ns("id_type_transcriptome"), "Gene ID type",
                      choices = c("Symbol"   = "symbol",
                                  "Ensembl"  = "ensembl",
                                  "EntrezID" = "entrezid",
                                  "UniProt"  = "uniprot"),
                      width = "100%"),
          actionButton(ns("btn_convert_T"), "Convert Transcriptomics IDs",
                       class = "btn-step-next btn-sm w-100 mt-1")
        ),

        tags$hr(class = "my-2"),

        # ── Proteomics ──────────────────────────────────────────────
        div(class = "omics-upload-section mb-3",
          div(class = "d-flex align-items-center gap-2 mb-2",
            div(class = "omics-badge omics-P", "P"),
            tags$strong("Proteomics"),
            uiOutput(ns("status_chip_P"), inline = TRUE)
          ),
          fileInput(ns("file_proteome"), NULL,
                    accept = c(".csv", ".xlsx"), width = "100%",
                    placeholder = "No file selected"),
          selectInput(ns("id_type_proteome"), "Gene ID type",
                      choices = c("Symbol"   = "symbol",
                                  "Ensembl"  = "ensembl",
                                  "EntrezID" = "entrezid",
                                  "UniProt"  = "uniprot"),
                      width = "100%"),
          actionButton(ns("btn_convert_P"), "Convert Proteomics IDs",
                       class = "btn-step-next btn-sm w-100 mt-1")
        ),

        tags$hr(class = "my-2"),

        # ── Metabolomics ─────────────────────────────────────────────
        div(class = "omics-upload-section mb-3",
          div(class = "d-flex align-items-center gap-2 mb-2",
            div(class = "omics-badge omics-M", "M"),
            tags$strong("Metabolomics"),
            uiOutput(ns("status_chip_M"), inline = TRUE)
          ),
          fileInput(ns("file_metabolome"), NULL,
                    accept = c(".csv", ".xlsx"), width = "100%",
                    placeholder = "No file selected"),
          selectInput(ns("met_id_type"), "Metabolite ID type",
                      choices = c("KEGG ID" = "keggid",
                                  "HMDB ID" = "hmdbid"),
                      width = "100%"),
          actionButton(ns("btn_convert_M"), "Convert Metabolomics IDs",
                       class = "btn-step-next btn-sm w-100 mt-1")
        ),

        tags$p(class = "small text-muted mt-2",
               "All three layers are required. Convert IDs for each",
               "layer before proceeding."),

        step_nav_buttons(ns, back = FALSE, next_label = "Proceed to Enrichment",
                         show_code = TRUE)
      ),

      mapa_card(
        "Preview",
        uiOutput(ns("preview_tabs"))
      )
    )
  )
}

#' @noRd
mod_mo_upload_server <- function(id, mo_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_values <- reactiveValues(
      raw_T = NULL, converted_T = NULL,
      raw_P = NULL, converted_P = NULL,
      raw_M = NULL, converted_M = NULL
    )

    mo_upload_code <- reactiveVal(NULL)

    .build_mo_upload_code <- function() {
      org_str <- input$organism %||% "org.Hs.eg.db"
      met_org <- .MO_ORG_TO_KEGG[[org_str]] %||% "hsa"
      sprintf(
        "# Transcriptomics\ntranscriptomics_converted <- mapa::convert_id(\n  data = your_transcriptome_data,\n  query_type = \"gene\",\n  from_id_type = \"%s\",\n  organism = %s\n)\n\n# Proteomics\nproteomics_converted <- mapa::convert_id(\n  data = your_proteome_data,\n  query_type = \"gene\",\n  from_id_type = \"%s\",\n  organism = %s\n)\n\n# Metabolomics\nmetabolomics_converted <- mapa::convert_id(\n  data = your_metabolome_data,\n  query_type = \"metabolite\",\n  from_id_type = \"%s\",\n  organism = \"%s\"\n)",
        input$id_type_transcriptome %||% "symbol", org_str,
        input$id_type_proteome      %||% "symbol", org_str,
        input$met_id_type           %||% "keggid", met_org)
    }

    load_file <- function(fi) {
      if (is.null(fi)) return(NULL)
      ext <- tools::file_ext(fi$name)
      if (ext == "xlsx") readxl::read_xlsx(fi$datapath)
      else               utils::read.csv(fi$datapath)
    }

    # ── Disable each Convert button until its layer data is present ──
    observe({
      if (is.null(data_values$raw_T)) shinyjs::disable("btn_convert_T")
      else                            shinyjs::enable("btn_convert_T")

      if (is.null(data_values$raw_P)) shinyjs::disable("btn_convert_P")
      else                            shinyjs::enable("btn_convert_P")

      if (is.null(data_values$raw_M)) shinyjs::disable("btn_convert_M")
      else                            shinyjs::enable("btn_convert_M")
    })

    # ── File upload ───────────────────────────────────────────────────
    observeEvent(input$file_transcriptome, {
      data_values$raw_T       <- load_file(input$file_transcriptome)
      data_values$converted_T <- NULL
    })
    observeEvent(input$file_proteome, {
      data_values$raw_P       <- load_file(input$file_proteome)
      data_values$converted_P <- NULL
    })
    observeEvent(input$file_metabolome, {
      data_values$raw_M       <- load_file(input$file_metabolome)
      data_values$converted_M <- NULL
    })

    # ── Gene layer ID conversion helper ──────────────────────────────
    convert_gene_layer <- function(raw_data, org_str, id_type_val, layer_name) {
      if (is.null(raw_data)) {
        shinyalert::shinyalert(
          title = paste("No", layer_name, "data"),
          text  = "Upload a file first.",
          type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return(NULL)
      }
      if (!requireNamespace(org_str, quietly = TRUE)) {
        shinyalert::shinyalert(
          title = "Missing package",
          text  = paste0(
            "Package <code>", org_str, "</code> is not installed.<br>",
            "Install with: <code>BiocManager::install('",
            org_str, "')</code>"
          ),
          type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return(NULL)
      }
      org_db_obj <- get(org_str, envir = asNamespace(org_str))
      tryCatch(
        mapa::convert_id(
          data         = raw_data,
          query_type   = "gene",
          from_id_type = id_type_val,
          organism     = org_db_obj
        ),
        error = function(e) {
          shinyalert::shinyalert(
            title = paste(layer_name, "ID conversion failed"),
            text  = e$message,
            type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          NULL
        }
      )
    }

    # ── Convert buttons ───────────────────────────────────────────────
    observeEvent(input$btn_convert_T, {
      result <- convert_gene_layer(
        data_values$raw_T, input$organism,
        input$id_type_transcriptome, "Transcriptomics"
      )
      if (!is.null(result)) {
        data_values$converted_T <- result
        mo_upload_code(.build_mo_upload_code())
        shinyalert::shinyalert(
          title = "Transcriptomics IDs converted",
          text  = paste0(nrow(result), " rows ready."),
          type  = "success", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      }
    })

    observeEvent(input$btn_convert_P, {
      if (is.null(data_values$raw_P)) {
        shinyalert::shinyalert(
          title = "No Proteomics data",
          text  = "Upload a proteomics file first.",
          type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return()
      }
      result <- convert_gene_layer(
        data_values$raw_P, input$organism,
        input$id_type_proteome, "Proteomics"
      )
      if (!is.null(result)) {
        data_values$converted_P <- result
        mo_upload_code(.build_mo_upload_code())
        shinyalert::shinyalert(
          title = "Proteomics IDs converted",
          text  = paste0(nrow(result), " rows ready."),
          type  = "success", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      }
    })

    observeEvent(input$btn_convert_M, {
      if (is.null(data_values$raw_M)) {
        shinyalert::shinyalert(
          title = "No Metabolomics data",
          text  = "Upload a metabolomics file first.",
          type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return()
      }
      met_org <- .MO_ORG_TO_KEGG[[input$organism]]
      result <- tryCatch(
        mapa::convert_id(
          data         = data_values$raw_M,
          query_type   = "metabolite",
          from_id_type = input$met_id_type,
          organism     = met_org
        ),
        error = function(e) {
          shinyalert::shinyalert(
            title = "Metabolomics ID conversion failed",
            text  = e$message,
            type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          NULL
        }
      )
      if (!is.null(result)) {
        data_values$converted_M <- result
        mo_upload_code(.build_mo_upload_code())
        shinyalert::shinyalert(
          title = "Metabolomics IDs converted",
          text  = paste0(nrow(result), " rows ready."),
          type  = "success", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── Example dataset ───────────────────────────────────────────────
    observeEvent(input$eg_demo, {
      tryCatch({
        data("demo_mo_T_data", envir = environment())
        data("demo_mo_P_data", envir = environment())
        data("demo_mo_M_data", envir = environment())

        data_values$raw_T       <- demo_mo_T_data
        data_values$converted_T <- NULL
        data_values$raw_P       <- demo_mo_P_data
        data_values$converted_P <- NULL
        data_values$raw_M       <- demo_mo_M_data
        data_values$converted_M <- NULL

        updateSelectInput(session, "organism", selected = "org.Hs.eg.db")
        updateSelectInput(session, "id_type_transcriptome", selected = "symbol")
        updateSelectInput(session, "id_type_proteome", selected = "symbol")
        updateSelectInput(session, "met_id_type", selected = "keggid")

        shinyalert::shinyalert(
          title = "Multi-omics example data loaded",
          text  = paste0(
            "T: ", nrow(demo_mo_T_data), " rows, ",
            "P: ", nrow(demo_mo_P_data), " rows, ",
            "M: ", nrow(demo_mo_M_data), " rows.<br>",
            "Click each <strong>Convert</strong> button to proceed."
          ),
          type = "success", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Failed to load example data", text = e$message,
          type = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      })
    })

    # ── Status chips (inline, beside each layer header) ───────────────
    output$status_chip_T <- renderUI({
      if (is.null(data_values$raw_T)) return(NULL)
      if (!is.null(data_values$converted_T))
        tags$span(class = "badge bg-success ms-1",
                  paste0(nrow(data_values$converted_T), " rows ✓"))
      else
        tags$span(class = "badge bg-warning text-dark ms-1",
                  paste0(nrow(data_values$raw_T), " rows — not converted"))
    })

    output$status_chip_P <- renderUI({
      if (is.null(data_values$raw_P)) return(NULL)
      if (!is.null(data_values$converted_P))
        tags$span(class = "badge bg-success ms-1",
                  paste0(nrow(data_values$converted_P), " rows ✓"))
      else
        tags$span(class = "badge bg-warning text-dark ms-1",
                  paste0(nrow(data_values$raw_P), " rows — not converted"))
    })

    output$status_chip_M <- renderUI({
      if (is.null(data_values$raw_M)) return(NULL)
      if (!is.null(data_values$converted_M))
        tags$span(class = "badge bg-success ms-1",
                  paste0(nrow(data_values$converted_M), " rows ✓"))
      else
        tags$span(class = "badge bg-warning text-dark ms-1",
                  paste0(nrow(data_values$raw_M), " rows — not converted"))
    })


    # ── Preview tabs ──────────────────────────────────────────────────
    output$preview_tabs <- renderUI({
      has_t <- !is.null(data_values$raw_T)
      has_p <- !is.null(data_values$raw_P)
      has_m <- !is.null(data_values$raw_M)

      if (!has_t && !has_p && !has_m) {
        return(tags$p(class = "text-muted small", "Upload files to see a preview."))
      }

      tabs <- list()
      if (has_t) tabs <- c(tabs, list(bslib::nav_panel("T", DT::DTOutput(ns("dt_T")))))
      if (has_p) tabs <- c(tabs, list(bslib::nav_panel("P", DT::DTOutput(ns("dt_P")))))
      if (has_m) tabs <- c(tabs, list(bslib::nav_panel("M", DT::DTOutput(ns("dt_M")))))

      do.call(bslib::navset_tab, tabs)
    })

    dt_opts <- list(pageLength = 5, scrollX = TRUE)
    output$dt_T <- DT::renderDT({
      df <- data_values$converted_T %||% data_values$raw_T
      req(df)
      DT::datatable(head(df, 50), options = dt_opts, rownames = FALSE)
    })
    output$dt_P <- DT::renderDT({
      df <- data_values$converted_P %||% data_values$raw_P
      req(df)
      DT::datatable(head(df, 50), options = dt_opts, rownames = FALSE)
    })
    output$dt_M <- DT::renderDT({
      df <- data_values$converted_M %||% data_values$raw_M
      req(df)
      DT::datatable(head(df, 50), options = dt_opts, rownames = FALSE)
    })

    # ── Show reproducible R code ──────────────────────────────────────
    observeEvent(input$btn_code, {
      if (is.null(mo_upload_code())) {
        shinyalert::shinyalert(
          title = "No code yet",
          text  = "Convert at least one omics layer first to view the reproducible code.",
          type  = "warning", confirmButtonCol = "#dd4b39"
        )
        return()
      }
      show_code_modal(mo_upload_code())
    })

    # ── Navigate to next step ─────────────────────────────────────────
    observeEvent(input$btn_next, {
      layers <- list(
        list(raw = data_values$raw_T, conv = data_values$converted_T,
             name = "Transcriptomics"),
        list(raw = data_values$raw_P, conv = data_values$converted_P,
             name = "Proteomics"),
        list(raw = data_values$raw_M, conv = data_values$converted_M,
             name = "Metabolomics")
      )
      for (layer in layers) {
        if (is.null(layer$raw)) {
          shinyalert::shinyalert(
            title = paste(layer$name, "required"),
            text  = paste("Please upload a", tolower(layer$name), "file."),
            type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          return()
        }
        if (is.null(layer$conv)) {
          shinyalert::shinyalert(
            title = paste("Convert", layer$name, "IDs first"),
            text  = paste0("Click <strong>Convert ", layer$name,
                           " IDs</strong> before proceeding."),
            type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
          )
          return()
        }
      }

      mo_data$transcriptome_data    <- data_values$converted_T
      mo_data$transcriptome_org     <- input$organism
      mo_data$transcriptome_id_type <- input$id_type_transcriptome

      mo_data$proteome_data         <- data_values$converted_P
      mo_data$proteome_org          <- input$organism
      mo_data$proteome_id_type      <- input$id_type_proteome

      mo_data$metabolome_data       <- data_values$converted_M
      mo_data$metabolome_id_type    <- input$met_id_type
      mo_data$metabolome_org        <- .MO_ORG_TO_KEGG[[input$organism]]

      go_next()
    })
  })
}
