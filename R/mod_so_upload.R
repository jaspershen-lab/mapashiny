# ── Step SO-1: Upload Data (Single-Omics) ────────────────────────────────────

# Extensible list of model organism OrgDb packages for gene queries
.SO_GENE_ORGANISM_CHOICES <- c(
  " " = "",
  "Human (org.Hs.eg.db)"                         = "org.Hs.eg.db",
  "Mouse (org.Mm.eg.db)"                         = "org.Mm.eg.db",
  "Rat (org.Rn.eg.db)"                           = "org.Rn.eg.db",
  "Fly (org.Dm.eg.db)"                           = "org.Dm.eg.db",
  "Zebrafish (org.Dr.eg.db)"                     = "org.Dr.eg.db",
  "Arabidopsis (org.At.tair.db)"                 = "org.At.tair.db",
  "Yeast (org.Sc.sgd.db)"                        = "org.Sc.sgd.db",
  "Worm (org.Ce.eg.db)"                          = "org.Ce.eg.db",
  "Pig (org.Ss.eg.db)"                           = "org.Ss.eg.db",
  "Bovine (org.Bt.eg.db)"                        = "org.Bt.eg.db",
  "Rhesus (org.Mmu.eg.db)"                       = "org.Mmu.eg.db",
  "Canine (org.Cf.eg.db)"                        = "org.Cf.eg.db",
  "E. coli strain K12 (org.EcK12.eg.db)"        = "org.EcK12.eg.db",
  "E. coli strain Sakai (org.EcSakai.eg.db)"    = "org.EcSakai.eg.db",
  "Chicken (org.Gg.eg.db)"                       = "org.Gg.eg.db",
  "Xenopus (org.Xl.eg.db)"                       = "org.Xl.eg.db",
  "Chimp (org.Pt.eg.db)"                         = "org.Pt.eg.db",
  "Anopheles (org.Ag.eg.db)"                     = "org.Ag.eg.db",
  "Malaria (org.Pf.plasmo.db)"                   = "org.Pf.plasmo.db",
  "Myxococcus xanthus DK 1622 (org.Mxanthus.db)" = "org.Mxanthus.db"
)

#' @noRd
mod_so_upload_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Single-Omics  •  Step 1",
    .title       = "Upload & Convert Marker Data",
    .subtitle    = "Upload a gene or metabolite marker list and convert IDs before enrichment.",

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      # ── Left: parameters ──
      mapa_card(
        "Input Parameters",
        tags$p(class = "text-muted small mb-2", "Load an example dataset:"),
        shinyWidgets::actionGroupButtons(
          inputIds  = c(ns("eg_ora"), ns("eg_gsea"), ns("eg_metabolite")),
          labels    = list("Gene list (ORA)", "Gene list (GSEA)", "Metabolite list"),
          size      = "sm",
          fullwidth = TRUE
        ),
        tags$hr(class = "my-3"),

        div(
          class = "d-flex align-items-center gap-2 mb-1",
          tags$span(class = "form-label mb-0", "Or upload your own file"),
          uiOutput(ns("input_format_info"), inline = TRUE)
        ),
        fileInput(ns("file"), NULL,
                  accept = c(".csv", ".xlsx"),
                  placeholder = "No file selected"),

        radioButtons(ns("query_type"), "Query type",
                     choices  = c("Gene" = "gene", "Metabolite" = "metabolite"),
                     selected = "gene",
                     inline   = TRUE),

        # ── Gene parameters ──
        conditionalPanel(
          condition = sprintf("input['%s'] == 'gene'", ns("query_type")),

          radioButtons(ns("organism_type"), "Organism",
                       choices  = c("Model organism" = "model",
                                    "Non-model organism" = "non_model"),
                       selected = "model", inline = TRUE),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'model'", ns("organism_type")),
            selectInput(ns("organism"), NULL,
                        choices  = .SO_GENE_ORGANISM_CHOICES,
                        selected = ""),
            helpText(
              "Select an OrgDb package installed on your system.",
              "For the full list, visit: ",
              tags$a(href   = "https://bioconductor.org/packages/release/BiocViews.html#___OrgDb",
                     "Bioconductor OrgDb packages", target = "_blank")
            )
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'non_model'", ns("organism_type")),
            textInput(ns("ah_id"), "AnnotationHub ID", value = ""),
            helpText(
              "Enter an AnnotationHub ID (e.g. AH119559) to fetch the OrgDb for your organism.",
              "Browse IDs at: ",
              tags$a(href   = "https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html",
                     "AnnotationHub", target = "_blank")
            )
          ),

          selectInput(ns("id_type"), "Input ID type",
                      choices = c(
                        "ENSEMBL"  = "ensembl",
                        "UniProt"  = "uniprot",
                        "EntrezID" = "entrezid",
                        "Symbol"   = "symbol"
                      ),
                      selected = "ensembl")
        ),

        # ── Metabolite parameters ──
        conditionalPanel(
          condition = sprintf("input['%s'] == 'metabolite'", ns("query_type")),

          selectizeInput(ns("met_organism"), "Organism",
                         choices = NULL,
                         options = list(
                           placeholder = "Type to search organisms...",
                           maxOptions  = 100
                         )),
          helpText(
            "Select organism by KEGG organism code or name.",
            "For a complete list, visit: ",
            tags$a(href   = "https://www.genome.jp/kegg/catalog/org_list.html",
                   "KEGG Organism Codes", target = "_blank")
          ),

          selectInput(ns("met_id_type"), "Input ID type",
                      choices  = c("KEGG ID" = "keggid", "HMDB ID" = "hmdbid"),
                      selected = "hmdbid")
        ),

        div(class = "d-flex gap-2 mt-3",
          actionButton(ns("btn_convert"), "Convert IDs",
                       class = "btn-step-next flex-grow-1"),
          actionButton(ns("btn_code"), "Code",
                       class = "btn-step-back")
        ),

        step_nav_buttons(ns, back = FALSE, next_label = "Proceed to Enrichment")
      ),

      # ── Right: preview ──
      mapa_card(
        "Data Preview",
        uiOutput(ns("data_status")),
        DT::DTOutput(ns("preview_table")),
        uiOutput(ns("download_ui"))
      )
    )
  )
}

#' @noRd
mod_so_upload_server <- function(id, so_data, go_next, go_back, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_values <- reactiveValues(
      raw_data       = NULL,
      converted_data = NULL,
      conversion_code = NULL
    )

    # Load KEGG organism choices (server-side selectize for performance)
    kegg_choices <- local({
      rda_path <- system.file("app/www/met_org_kegg_choices.rda", package = "mapashiny")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        e$choices
      } else {
        c("Homo sapiens (human)" = "hsa")
      }
    })

    updateSelectizeInput(session, "met_organism",
                         choices = kegg_choices,
                         server  = TRUE)

    # Input requirements follow the selected query and identifier types.
    output$input_format_info <- renderUI({
      is_gene <- !isTRUE(input$query_type == "metabolite")
      .input_format_popover(
        query_type = if (is_gene) "gene" else "metabolite",
        id_type = if (is_gene) input$id_type else input$met_id_type,
        layer_label = if (is_gene) "Gene" else "Metabolite",
        multi_omics = FALSE
      )
    })

    # Only show HMDB option for human; KEGG-only for all other organisms
    observeEvent(input$met_organism, {
      if (!is.null(input$met_organism) && nchar(input$met_organism) > 0) {
        if (input$met_organism == "hsa") {
          updateSelectInput(session, "met_id_type",
                            choices  = c("KEGG ID" = "keggid", "HMDB ID" = "hmdbid"),
                            selected = "keggid")
        } else {
          updateSelectInput(session, "met_id_type",
                            choices  = c("KEGG ID" = "keggid"),
                            selected = "keggid")
        }
      }
    })

    # ── Disable Convert button until data is present ─────────────────────────
    observe({
      if (is.null(data_values$raw_data)) shinyjs::disable("btn_convert")
      else                               shinyjs::enable("btn_convert")
    })

    # ── File upload ──────────────────────────────────────────────────────────
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      tryCatch({
        data_values$raw_data       <- if (ext == "xlsx") readxl::read_xlsx(input$file$datapath)
                                      else               utils::read.csv(input$file$datapath)
        data_values$converted_data <- NULL
        shinyalert::shinyalert(title = "File uploaded successfully",
                               type = "success", html = TRUE, confirmButtonCol = "#dd4b39")
      }, error = function(e) {
        shinyalert::shinyalert(title = "Error reading file", text = e$message,
                               type = "error", html = TRUE, confirmButtonCol = "#dd4b39")
      })
    })

    # ── Example datasets ─────────────────────────────────────────────────────
    observeEvent(input$eg_ora, {
      tryCatch({
        data("example_ora_data", envir = environment())
        data_values$raw_data       <- example_ora_data
        data_values$converted_data <- NULL

        updateRadioButtons(session, "query_type", selected = "gene")
        updateRadioButtons(session, "organism_type", selected = "model")
        updateSelectInput(session, "organism",
                          choices  = .SO_GENE_ORGANISM_CHOICES,
                          selected = "org.Mm.eg.db")
        updateSelectInput(session, "id_type",
                          choices  = c("ENSEMBL" = "ensembl", "UniProt" = "uniprot",
                                       "EntrezID" = "entrezid", "Symbol" = "symbol"),
                          selected = "symbol")

        shinyalert::shinyalert(title = "Gene ORA example data loaded",
                               text  = "Click <strong>Convert IDs</strong> to proceed.",
                               type = "success", html = TRUE, confirmButtonCol = "#dd4b39")
      }, error = function(e) {
        shinyalert::shinyalert(title = "Failed to load gene ORA example", text = e$message,
                               type = "error", html = TRUE, confirmButtonCol = "#dd4b39")
      })
    })

    observeEvent(input$eg_gsea, {
      tryCatch({
        data("example_gsea_data", envir = environment())
        data_values$raw_data       <- example_gsea_data
        data_values$converted_data <- NULL

        updateRadioButtons(session, "query_type", selected = "gene")
        updateRadioButtons(session, "organism_type", selected = "model")
        updateSelectInput(session, "organism",
                          choices  = .SO_GENE_ORGANISM_CHOICES,
                          selected = "org.Mm.eg.db")
        updateSelectInput(session, "id_type",
                          choices  = c("ENSEMBL" = "ensembl", "UniProt" = "uniprot",
                                       "EntrezID" = "entrezid", "Symbol" = "symbol"),
                          selected = "symbol")

        shinyalert::shinyalert(title = "Gene GSEA example data loaded",
                               text  = "Click <strong>Convert IDs</strong> to proceed.",
                               type = "success", html = TRUE, confirmButtonCol = "#dd4b39")
      }, error = function(e) {
        shinyalert::shinyalert(title = "Failed to load gene GSEA example", text = e$message,
                               type = "error", html = TRUE, confirmButtonCol = "#dd4b39")
      })
    })

    observeEvent(input$eg_metabolite, {
      tryCatch({
        data("example_met_data", envir = environment())
        data_values$raw_data       <- example_met_data
        data_values$converted_data <- NULL

        updateRadioButtons(session, "query_type", selected = "metabolite")
        updateSelectizeInput(session, "met_organism",
                             choices  = kegg_choices,
                             selected = "hsa",
                             server   = TRUE)

        shinyalert::shinyalert(title = "Metabolite example data loaded",
                               text  = "Click <strong>Convert IDs</strong> to proceed.",
                               type = "success", html = TRUE, confirmButtonCol = "#dd4b39")
      }, error = function(e) {
        shinyalert::shinyalert(title = "Failed to load metabolite example", text = e$message,
                               type = "error", html = TRUE, confirmButtonCol = "#dd4b39")
      })
    })

    # ── ID conversion ────────────────────────────────────────────────────────
    observeEvent(input$btn_convert, {
      if (is.null(data_values$raw_data)) {
        shinyalert::shinyalert(title = "No data available",
                               text  = "Upload a file or load an example dataset.",
                               type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39")
        return()
      }

      # Show spinner while converting
      shinyalert::shinyalert(
        title = "Converting IDs...",
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

      if (input$query_type == "gene") {

        if (input$organism_type == "model") {
          org_val <- input$organism

          if (org_val == "") {
            shinyalert::closeAlert()
            shinyalert::shinyalert(title = "No organism selected",
                                   text  = "Please select a model organism.",
                                   type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39")
            return()
          }
          if (!grepl("^org\\.[A-Za-z0-9]+\\..+\\.db$", org_val)) {
            shinyalert::closeAlert()
            shinyalert::shinyalert(title = "Invalid OrgDb package name",
                                   text  = "Expected format: <code>org.XX.eg.db</code>.",
                                   type  = "error", html = TRUE, confirmButtonCol = "#dd4b39")
            return()
          }
          if (!requireNamespace(org_val, quietly = TRUE)) {
            shinyalert::closeAlert()
            shinyalert::shinyalert(
              title = "Missing package",
              text  = paste0("Package <code>", org_val, "</code> is not installed.<br>",
                             "Install it with:<br><code>BiocManager::install('", org_val, "')</code>"),
              type  = "error", html = TRUE, confirmButtonCol = "#dd4b39")
            return()
          }

          org_db_obj      <- get(org_val, envir = asNamespace(org_val))
          ah_id           <- NULL
          conversion_param <- sprintf('\n  organism = %s', org_val)

        } else {
          ah_id_val <- trimws(input$ah_id)
          if (nchar(ah_id_val) == 0) {
            shinyalert::closeAlert()
            shinyalert::shinyalert(title = "No AnnotationHub ID provided",
                                   text  = "Please enter an AnnotationHub ID.",
                                   type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39")
            return()
          }
          org_db_obj      <- NULL
          ah_id           <- ah_id_val
          conversion_param <- sprintf('\n  ah_id = "%s"', ah_id_val)
        }

        tryCatch({
          result <- mapa::convert_id(
            data         = data_values$raw_data,
            query_type   = "gene",
            from_id_type = input$id_type,
            organism     = org_db_obj,
            ah_id        = ah_id
          )

          data_values$converted_data  <- result
          data_values$conversion_code <- sprintf(
            'result <- mapa::convert_id(\n  data = your_data,\n  query_type = "gene",\n  from_id_type = "%s",%s\n)',
            input$id_type, conversion_param
          )

          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "Gene marker list successfully converted",
            text  = "Click <strong>Proceed to Enrichment</strong> to continue.",
            type  = "success", html = TRUE, confirmButtonCol = "#dd4b39"
          )
        }, error = function(e) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(title = "ID conversion failed", text = e$message,
                                 type = "error", html = TRUE, confirmButtonCol = "#dd4b39")
        })

      } else {
        # Metabolite conversion
        met_org <- input$met_organism
        if (is.null(met_org) || nchar(trimws(met_org)) == 0) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(title = "No organism selected",
                                 text  = "Please select an organism.",
                                 type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39")
          return()
        }

        tryCatch({
          result <- mapa::convert_id(
            data         = data_values$raw_data,
            query_type   = "metabolite",
            from_id_type = input$met_id_type,
            organism     = met_org
          )

          data_values$converted_data  <- result
          data_values$conversion_code <- sprintf(
            'result <- mapa::convert_id(\n  data = your_data,\n  query_type = "metabolite",\n  from_id_type = "%s",\n  organism = "%s"\n)',
            input$met_id_type, met_org
          )

          shinyalert::closeAlert()
          shinyalert::shinyalert(
            title = "Metabolite marker list successfully converted",
            text  = "Click <strong>Proceed to Enrichment</strong> to continue.",
            type  = "success", html = TRUE, confirmButtonCol = "#dd4b39"
          )
        }, error = function(e) {
          shinyalert::closeAlert()
          shinyalert::shinyalert(title = "ID conversion failed", text = e$message,
                                 type = "error", html = TRUE, confirmButtonCol = "#dd4b39")
        })
      }
    })

    # ── Show conversion code ─────────────────────────────────────────────────
    observeEvent(input$btn_code, {
      if (is.null(data_values$conversion_code)) {
        shinyalert::shinyalert(title = "No code available",
                               text  = "Convert IDs first to generate the reproducible code.",
                               type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39")
      } else {
        shinyalert::shinyalert(
          text = paste0(
            "<pre style='text-align:left;font-family:Consolas,Monaco,monospace;",
            "background:#f8f9fa;padding:15px;border-radius:5px;border:1px solid #e9ecef;",
            "overflow-x:auto;white-space:pre-wrap;font-size:13px;line-height:1.4;margin:0;",
            "max-height:400px;overflow-y:auto;'>",
            htmltools::htmlEscape(data_values$conversion_code),
            "</pre>"
          ),
          html             = TRUE,
          type             = "",
          confirmButtonText = "Close",
          confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── Preview ──────────────────────────────────────────────────────────────
    output$data_status <- renderUI({
      if (!is.null(data_values$converted_data)) {
        status_alert(paste0("Converted: ", nrow(data_values$converted_data), " rows. Ready to proceed."), "success")
      } else if (!is.null(data_values$raw_data)) {
        status_alert(paste0(nrow(data_values$raw_data), " rows uploaded. Click Convert IDs."), "info")
      } else {
        status_alert("Upload a file or load an example dataset.", "info")
      }
    })

    output$preview_table <- DT::renderDT({
      df <- if (!is.null(data_values$converted_data)) data_values$converted_data
            else                                       data_values$raw_data
      req(df)
      DT::datatable(head(df, 50),
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    output$download_ui <- renderUI({
      req(data_values$converted_data)
      downloadButton(ns("download"), "Download converted data",
                     class = "btn-step-back mt-2")
    })

    output$download <- downloadHandler(
      filename = function() "converted_data.csv",
      content  = function(file) write.csv(data_values$converted_data, file, row.names = FALSE)
    )

    # ── Navigate to next step ────────────────────────────────────────────────
    observeEvent(input$btn_next, {
      if (is.null(data_values$converted_data)) {
        shinyalert::shinyalert(title = "Conversion required",
                               text  = "Please convert IDs before proceeding.",
                               type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39")
        return()
      }

      so_data$variable_info <- data_values$converted_data
      so_data$query_type    <- input$query_type

      if (input$query_type == "gene") {
        so_data$organism <- if (input$organism_type == "model") input$organism else NULL
        so_data$ah_id    <- if (input$organism_type == "non_model") input$ah_id else NULL
        so_data$id_type  <- input$id_type
      } else {
        so_data$organism <- input$met_organism
        so_data$id_type  <- input$met_id_type
      }

      go_next()
    })
  })
}
