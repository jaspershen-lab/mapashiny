# ── Single-Omics LLM Annotation ──────────────────────────────────────────────
# Calls mapa::llm_interpret_module() on a functional_module S4 object.
# Supports module selection and a min/max size range filter applied before
# the call, so the internal cutoff is bypassed (set to 0).

#' @noRd
mod_so_llm_ui <- function(id) {
  ns <- NS(id)
  step_page(
    .badge_label = "Single-Omics  •  Step 5",
    .title       = "LLM Module Annotation",
    .subtitle    = paste0(
      "Use a large language model to interpret each functional ",
      "module with literature evidence."
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.25rem",

      # ── Left: controls ───────────────────────────────────────────────
      tagList(
        mapa_card(
          "Upload Clustering Result",
          tags$p(
            class = "text-muted small mb-2",
            "Upload the .rda object from the Module Identification step",
            "to bypass the previous steps."
          ),
          fileInput(
            ns("upload_modules"), NULL,
            accept      = ".rda",
            buttonLabel = "Browse…",
            placeholder = "No file selected"
          )
        ),

        bslib::navset_card_tab(
          # ── Tab 1: Model ──────────────────────────────────────────────
          bslib::nav_panel(
            "Model",
            div(
              class = "pt-3",
              selectInput(
                ns("api_provider"), "API provider",
                choices = c(
                  "OpenAI"      = "openai",
                  "Gemini"      = "gemini",
                  "SiliconFlow" = "siliconflow"
                )
              ),
              selectizeInput(
                ns("llm_model"), "LLM model",
                choices = list(
                  "OpenAI" = list(
                    "gpt-4o-mini-2024-07-18" = "gpt-4o-mini-2024-07-18",
                    "gpt-4o"                 = "gpt-4o"
                  ),
                  "Google Gemini" = list(
                    "models/gemini-1.5-flash" = "models/gemini-1.5-flash",
                    "models/gemini-2.5-flash" = "models/gemini-2.5-flash"
                  ),
                  "SiliconFlow" = list(
                    "Qwen/Qwen3-8B"                    = "Qwen/Qwen3-8B",
                    "Qwen/Qwen3-14B"                   = "Qwen/Qwen3-14B",
                    "Qwen/Qwen3-30B-A3B-Thinking-2507" =
                      "Qwen/Qwen3-30B-A3B-Thinking-2507",
                    "Qwen/Qwen3-32B"                   = "Qwen/Qwen3-32B"
                  )
                ),
                selected = "gpt-4o-mini-2024-07-18",
                options  = list(create = TRUE,
                                placeholder = "Select or type a model name")
              ),
              selectizeInput(
                ns("embed_model"), "Embedding model",
                choices = list(
                  "OpenAI" = list(
                    "text-embedding-3-small" = "text-embedding-3-small",
                    "text-embedding-3-large" = "text-embedding-3-large",
                    "text-embedding-ada-002" = "text-embedding-ada-002"
                  ),
                  "Google Gemini" = list(
                    "models/text-embedding-004"   = "models/text-embedding-004",
                    "models/gemini-embedding-001" = "models/gemini-embedding-001"
                  ),
                  "SiliconFlow" = list(
                    "Qwen/Qwen3-Embedding-0.6B" = "Qwen/Qwen3-Embedding-0.6B",
                    "Qwen/Qwen3-Embedding-4B"   = "Qwen/Qwen3-Embedding-4B",
                    "Qwen/Qwen3-Embedding-8B"   = "Qwen/Qwen3-Embedding-8B"
                  )
                ),
                selected = "text-embedding-3-small",
                options  = list(create = TRUE,
                                placeholder = "Select or type a model name")
              ),
              passwordInput(ns("api_key"), "API key"),
              radioButtons(
                ns("organism_type"),
                "Organism (for gene data)",
                choices  = c("Model organism"     = "model",
                             "Non-model organism" = "non_model"),
                selected = "model",
                inline   = TRUE
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'model'",
                                    ns("organism_type")),
                selectInput(
                  ns("orgdb"), NULL,
                  choices  = .SO_GENE_ORGANISM_CHOICES,
                  selected = ""
                )
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'non_model'",
                                    ns("organism_type")),
                textInput(
                  ns("ah_id"), NULL,
                  placeholder = "AnnotationHub ID, e.g. AH119559"
                )
              ),
              tags$div(
                class = "small text-muted mt-2",
                tags$span(
                  class = "text-success fw-semibold",
                  "✓ Listed models have been tested and work stably"
                ),
                tags$br(), tags$br(),
                tags$strong("Find more models:"), tags$br(),
                "SiliconFlow: ",
                tags$a("cloud.siliconflow.com/models",
                       href = "https://cloud.siliconflow.com/models",
                       target = "_blank"),
                tags$br(),
                "OpenAI: ",
                tags$a("platform.openai.com/docs/models",
                       href = "https://platform.openai.com/docs/models",
                       target = "_blank"),
                tags$br(),
                "Google Gemini: ",
                tags$a("ai.google.dev/gemini-api/docs/models",
                       href = "https://ai.google.dev/gemini-api/docs/models",
                       target = "_blank")
              )
            )
          ),

          # ── Tab 2: Literature Search ──────────────────────────────────
          bslib::nav_panel(
            "Literature Search",
            div(
              class = "pt-3",
              tags$p(
                class = "text-muted small",
                "Control how PubMed is queried to retrieve supporting evidence",
                "for each module."
              ),
              textInput(
                ns("phenotype"),
                "Disease / phenotype context (optional)",
                placeholder = "e.g. Alzheimer's disease, cancer"
              ),
              numericInput(ns("years"), "Search recent N years",
                           value = 5, min = 1, max = 20),
              numericInput(ns("retmax"), "Max records per query",
                           value = 10, min = 1, max = 100)
            )
          ),

          # ── Tab 3: Filtering ──────────────────────────────────────────
          bslib::nav_panel(
            "Filtering",
            div(
              class = "pt-3",
              tags$p(
                class = "text-muted small",
                "Select specific modules and set a size range to annotate.",
                "Leave the module selector empty to annotate all modules",
                "that fall within the size range."
              ),

              # Module multi-selection
              selectizeInput(
                ns("module_ids"),
                "Select modules to annotate",
                choices  = NULL,
                multiple = TRUE,
                options  = list(
                  placeholder = "Leave empty to annotate all in range",
                  plugins     = list("remove_button")
                )
              ),

              # Module size range (min / max)
              tags$label("Module size range (content count)",
                         class = "control-label mt-2"),
              div(
                class = "d-flex gap-2 align-items-start",
                div(
                  style = "flex:1;",
                  tags$small(class = "text-muted", "Min"),
                  numericInput(ns("size_min"), NULL,
                               value = 2, min = 1, max = 99999, width = "100%")
                ),
                div(
                  style = "flex:1;",
                  tags$small(class = "text-muted", "Max"),
                  numericInput(ns("size_max"), NULL,
                               value = 99999, min = 1, max = 99999, width = "100%")
                )
              ),

              numericInput(ns("sim_filter"),
                           "Keep top-N by embedding similarity",
                           value = 20, min = 5, max = 100),
              numericInput(ns("gpt_filter"),
                           "Keep top-N after LLM re-ranking",
                           value = 5, min = 1, max = 50),
              tags$hr(class = "my-2"),
              tags$label("Embedding cache folder", class = "control-label"),
              tags$p(
                class = "text-muted small mb-1",
                tags$strong("Note:"),
                " folder contents are cleared before each run."
              ),
              div(
                class = "d-flex gap-2 align-items-center mb-1",
                shinyFiles::shinyDirButton(
                  ns("embed_dir"),
                  label = "Browse…",
                  title = "Select embedding cache folder",
                  class = "btn btn-outline-secondary btn-sm"
                )
              ),
              uiOutput(ns("embed_dir_display"))
            )
          )
        ),

        # ── Run + Back buttons ────────────────────────────────────────────
        step_nav_buttons(ns, next_label = "Run LLM Annotation",
                         next_arrow = FALSE, show_code = TRUE),

        # ── Proceed button (appears after annotation completes) ───────────
        uiOutput(ns("proceed_ui"))
      ),

      # ── Right: results ───────────────────────────────────────────────────
      mapa_card(
        "Annotation Results",
        uiOutput(ns("dl_rda_top")),
        uiOutput(ns("llm_status")),
        tags$br(),
        uiOutput(ns("result_tabs_ui"))
      )
    )
  )
}

#' @noRd
mod_so_llm_server <- function(id, so_data, annotated_modules,
                               go_next, go_back) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    future::plan(future::sequential)

    llm_code <- reactiveVal(NULL)

    # ── Disable Run button until module result is present ────────────────────
    observe({
      has_modules <- !is.null(so_data$functional_modules)
      has_key     <- !is.null(input$api_key) && nzchar(trimws(input$api_key))
      if (has_modules && has_key) shinyjs::enable("btn_next")
      else                        shinyjs::disable("btn_next")
    })

    # ── .rda download button above the status banner ─────────────────────────
    output$dl_rda_top <- renderUI({
      req(annotated_modules())
      downloadButton(
        ns("dl_annotated_top"),
        "Annotated functional modules (.rda)",
        class = "btn-mapa-dl mb-2"
      )
    })

    output$dl_annotated_top <- downloadHandler(
      filename = "so_llm_annotated_modules.rda",
      content  = function(file) {
        llm_annotated_modules <- annotated_modules()
        save(llm_annotated_modules, file = file)
      }
    )

    # ── Show reproducible R code ──────────────────────────────────────────────
    observeEvent(input$btn_code, {
      if (is.null(llm_code())) {
        shinyalert::shinyalert(
          title = "No code yet",
          text  = "Run LLM annotation first to view the reproducible code.",
          type  = "warning", confirmButtonCol = "#dd4b39"
        )
        return()
      }
      show_code_modal(llm_code())
    })

    # ── Folder picker ────────────────────────────────────────────────────────
    dir_roots <- c(Home = path.expand("~"))

    shinyFiles::shinyDirChoose(input, "embed_dir",
                               roots = dir_roots, session = session)

    embedding_dir <- reactive({
      if (is.integer(input$embed_dir))
        file.path(tempdir(), "mapa_llm_embedding")
      else
        shinyFiles::parseDirPath(dir_roots, input$embed_dir)
    })

    output$embed_dir_display <- renderUI({
      d <- embedding_dir()
      tags$p(
        class = "text-muted small mb-0 text-break",
        if (is.integer(input$embed_dir)) tags$em(paste0("Default: ", d)) else d
      )
    })

    # ── Update model dropdowns when provider changes ─────────────────────────
    observeEvent(input$api_provider, {
      embed_choices <- switch(input$api_provider,
        openai = list(
          "text-embedding-3-small" = "text-embedding-3-small",
          "text-embedding-3-large" = "text-embedding-3-large",
          "text-embedding-ada-002" = "text-embedding-ada-002"
        ),
        gemini = list(
          "models/text-embedding-004"   = "models/text-embedding-004",
          "models/gemini-embedding-001" = "models/gemini-embedding-001"
        ),
        siliconflow = list(
          "Qwen/Qwen3-Embedding-0.6B" = "Qwen/Qwen3-Embedding-0.6B",
          "Qwen/Qwen3-Embedding-4B"   = "Qwen/Qwen3-Embedding-4B",
          "Qwen/Qwen3-Embedding-8B"   = "Qwen/Qwen3-Embedding-8B"
        )
      )
      llm_choices <- switch(input$api_provider,
        openai = list(
          "gpt-4o-mini-2024-07-18" = "gpt-4o-mini-2024-07-18",
          "gpt-4o"                 = "gpt-4o"
        ),
        gemini = list(
          "models/gemini-1.5-flash" = "models/gemini-1.5-flash",
          "models/gemini-2.5-flash" = "models/gemini-2.5-flash"
        ),
        siliconflow = list(
          "Qwen/Qwen3-8B"                    = "Qwen/Qwen3-8B",
          "Qwen/Qwen3-14B"                   = "Qwen/Qwen3-14B",
          "Qwen/Qwen3-30B-A3B-Thinking-2507" = "Qwen/Qwen3-30B-A3B-Thinking-2507",
          "Qwen/Qwen3-32B"                   = "Qwen/Qwen3-32B"
        )
      )
      default_embed <- switch(input$api_provider,
        openai      = "text-embedding-3-small",
        gemini      = "models/text-embedding-004",
        siliconflow = "Qwen/Qwen3-Embedding-8B"
      )
      default_llm <- switch(input$api_provider,
        openai      = "gpt-4o-mini-2024-07-18",
        gemini      = "models/gemini-1.5-flash",
        siliconflow = "Qwen/Qwen3-30B-A3B-Thinking-2507"
      )
      updateSelectizeInput(session, "embed_model",
                           choices = embed_choices, selected = default_embed)
      updateSelectizeInput(session, "llm_model",
                           choices = llm_choices, selected = default_llm)
    })

    # ── Helper: populate module selector from a functional_module object ─────
    .update_module_choices <- function(obj) {
      df <- obj@merged_module$functional_module_result
      choices <- setNames(
        df$module,
        paste0(df$module, " (n=", df$module_content_number, ")")
      )
      updateSelectizeInput(session, "module_ids",
                           choices = choices, selected = character(0))
      updateNumericInput(session, "size_max",
                         value = max(df$module_content_number, na.rm = TRUE))
    }

    # ── Upload clustering result ─────────────────────────────────────────────
    observeEvent(input$upload_modules, {
      req(input$upload_modules$datapath)
      tmp <- new.env()
      load(input$upload_modules$datapath, envir = tmp)
      nms <- ls(tmp)
      if (length(nms) == 1) {
        obj <- get(nms[1], envir = tmp)
        so_data$functional_modules <- obj
        .update_module_choices(obj)
      } else {
        shinyalert::shinyalert(
          title = "Invalid file content",
          text  = "The .rda file must contain exactly one object.",
          type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
        )
      }
    })

    # ── Populate module choices when upstream data arrives ───────────────────
    observeEvent(so_data$functional_modules, {
      req(so_data$functional_modules)
      .update_module_choices(so_data$functional_modules)
    }, ignoreNULL = TRUE)

    # ── Sync organism controls when coming from the normal upload flow ───────
    observeEvent(so_data$query_type, {
      req(so_data$query_type)
      if (identical(so_data$query_type, "gene")) {
        org_val <- so_data$organism
        ah_val  <- so_data$ah_id
        if (!is.null(org_val) && nchar(org_val) > 0) {
          updateRadioButtons(session, "organism_type", selected = "model")
          updateSelectInput(session, "orgdb",  selected = org_val)
        } else if (!is.null(ah_val) && nchar(trimws(ah_val)) > 0) {
          updateRadioButtons(session, "organism_type", selected = "non_model")
          updateTextInput(session,   "ah_id",  value = ah_val)
        }
      }
    }, ignoreNULL = TRUE)

    # ── Status banner ────────────────────────────────────────────────────────
    output$llm_status <- renderUI({
      if (is.null(annotated_modules()))
        status_alert(
          "Configure settings on the left and click Run LLM Annotation.",
          "info"
        )
      else
        status_alert(
          "LLM annotation complete. Review results below.", "success"
        )
    })

    # ── Run annotation ───────────────────────────────────────────────────────
    observeEvent(input$btn_next, {
      object <- so_data$functional_modules

      if (is.null(object)) {
        shinyalert::shinyalert(
          title = "No data available",
          text  = paste0("Complete the Module Identification step ",
                         "or upload an .rda file on the left."),
          type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return()
      }

      # Extract reactive values before future_promise
      embed_dir    <- embedding_dir()
      llm_model    <- input$llm_model
      embed_model  <- input$embed_model
      api_key      <- input$api_key
      api_provider <- input$api_provider
      size_min     <- input$size_min
      size_max     <- input$size_max
      module_ids   <- input$module_ids
      years        <- input$years
      retmax       <- input$retmax
      sim_filter   <- input$sim_filter
      gpt_filter   <- input$gpt_filter
      phenotype    <- trimws(input$phenotype)
      if (nchar(phenotype) == 0) phenotype <- NULL
      organism_type <- input$organism_type
      orgdb_input   <- input$orgdb
      ah_id_input   <- trimws(input$ah_id)

      # Pre-filter the object so we bypass the internal single-cutoff limit.
      # Keep full_df so we can merge LLM columns back after annotation.
      full_df <- object@merged_module$functional_module_result
      df <- full_df[full_df$module_content_number >= size_min &
                      full_df$module_content_number <= size_max, , drop = FALSE]
      if (length(module_ids) > 0) {
        df <- df[df$module %in% module_ids, , drop = FALSE]
      }

      if (nrow(df) == 0) {
        shinyalert::shinyalert(
          title = "No modules to annotate",
          text  = paste0("The current filters leave no modules. ",
                         "Adjust the module selection or size range."),
          type  = "warning", html = TRUE, confirmButtonCol = "#dd4b39"
        )
        return()
      }

      object@merged_module[["functional_module_result"]] <- df

      dir.create(embed_dir, showWarnings = FALSE, recursive = TRUE)

      alert_id <- shinyalert::shinyalert(
        title = "Running LLM annotation…",
        text  = tags$div(
          style = "text-align:center;",
          tags$p("This may take several minutes.", class = "text-muted small"),
          tags$img(src = "www/spinner.gif", width = "50px", height = "50px",
                   style = "margin-top:10px;")
        ),
        type = "", showConfirmButton = FALSE, showCancelButton = FALSE,
        timer = 0, closeOnEsc = FALSE, closeOnClickOutside = FALSE,
        html = TRUE
      )

      promises::future_promise({
        orgdb_val <- if (identical(organism_type, "non_model")) {
          ah <- AnnotationHub::AnnotationHub()
          ah[[ah_id_input]]
        } else {
          orgdb_input
        }
        mapa::llm_interpret_module(
          object                       = object,
          module_content_number_cutoff = 0,
          llm_model                    = llm_model,
          embedding_model              = embed_model,
          api_key                      = api_key,
          api_provider                 = api_provider,
          phenotype                    = phenotype,
          years                        = years,
          retmax                       = retmax,
          similarity_filter_num        = sim_filter,
          GPT_filter_num               = gpt_filter,
          embedding_output_dir         = embed_dir,
          orgdb                        = orgdb_val,
          thread                       = 1L
        )
      }) |>
        promises::then(
          function(result) {
            # Merge LLM-added columns back into the full (unfiltered) module
            # table so the visualization page sees all modules.
            llm_cols <- setdiff(
              colnames(result@merged_module$functional_module_result),
              colnames(full_df)
            )
            if (length(llm_cols) > 0) {
              merged_df <- merge(
                full_df,
                result@merged_module$functional_module_result[
                  , c("module", llm_cols), drop = FALSE],
                by = "module", all.x = TRUE
              )
              result@merged_module[["functional_module_result"]] <- merged_df
            }
            annotated_modules(result)

            org_part <- if (identical(organism_type, "non_model")) {
              sprintf("\n  orgdb = AnnotationHub::AnnotationHub()[[\"%s\"]],",
                      ah_id_input)
            } else if (!is.null(orgdb_input) && nzchar(orgdb_input)) {
              sprintf("\n  orgdb = \"%s\",", orgdb_input)
            } else ""
            pheno_str <- if (!is.null(phenotype) && nzchar(phenotype))
              sprintf("\"%s\"", phenotype) else "NULL"
            llm_code(sprintf(
              "llm_annotated_modules <- mapa::llm_interpret_module(\n  object = enriched_functional_module,\n  module_content_number_cutoff = 0,\n  llm_model = \"%s\",\n  embedding_model = \"%s\",\n  api_key = \"your_api_key\",\n  api_provider = \"%s\",\n  phenotype = %s,\n  years = %s,\n  retmax = %s,\n  similarity_filter_num = %s,\n  GPT_filter_num = %s,%s\n  thread = 1L\n)",
              llm_model, embed_model, api_provider, pheno_str,
              years, retmax, sim_filter, gpt_filter, org_part))

            shinyalert::closeAlert(id = alert_id)
            shinyalert::shinyalert(
              title = "Annotation complete",
              text  = paste0("Review results on the right, then click",
                             " Proceed to Visualization."),
              type  = "success", html = TRUE, confirmButtonCol = "#dd4b39"
            )
          },
          function(err) {
            shinyalert::closeAlert(id = alert_id)
            shinyalert::shinyalert(
              title = "LLM Annotation Failed",
              text  = conditionMessage(err),
              type  = "error", html = TRUE, confirmButtonCol = "#dd4b39"
            )
          }
        )

      NULL
    })

    # ── Proceed button ───────────────────────────────────────────────────────
    output$proceed_ui <- renderUI({
      req(annotated_modules())
      actionButton(
        ns("btn_proceed"),
        tagList("Proceed to Visualization",
                tags$i(class = "fas fa-arrow-right ms-1")),
        class = "btn-step-next w-100 mt-2"
      )
    })

    observeEvent(input$btn_proceed, go_next())
    observeEvent(input$btn_back,    go_back())

    # ── Result tabs ──────────────────────────────────────────────────────────
    output$result_tabs_ui <- renderUI({
      req(annotated_modules())
      bslib::navset_tab(
        id = ns("result_tabs"),
        bslib::nav_panel(
          "Module Table",
          DT::DTOutput(ns("annotation_table"))
        ),
        bslib::nav_panel(
          "Module Details",
          uiOutput(ns("module_detail_ui"))
        )
      )
    })

    output$annotation_table <- DT::renderDT({
      req(annotated_modules())
      df <- annotated_modules()@merged_module$functional_module_result
      cols <- intersect(
        c("module", "llm_module_name", "module_annotation",
          "module_content_number"),
        colnames(df)
      )
      DT::datatable(df[, cols, drop = FALSE],
                    options  = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })

    llm_interp <- reactive({
      req(annotated_modules())
      annotated_modules()@llm_module_interpretation
    })

    output$module_detail_ui <- renderUI({
      req(llm_interp())
      tagList(
        selectInput(ns("module_sel"), "Select module:",
                    choices  = names(llm_interp()),
                    selected = names(llm_interp())[1]),
        tags$hr(),
        tags$strong("Module name:"),
        textOutput(ns("det_name"), container = tags$p),
        tags$strong("Summary:"),
        textOutput(ns("det_summary"), container = tags$p),
        tags$strong("Association with phenotype:"),
        textOutput(ns("det_phenotype"), container = tags$p),
        tags$strong("Confidence score:"),
        textOutput(ns("det_confidence"), container = tags$p)
      )
    })

    output$det_name <- renderText({
      req(llm_interp(), input$module_sel)
      tryCatch(
        llm_interp()[[input$module_sel]]$generated_name$module_name,
        error = function(e) paste("Error:", e$message)
      )
    })

    output$det_summary <- renderText({
      req(llm_interp(), input$module_sel)
      tryCatch(
        llm_interp()[[input$module_sel]]$generated_name$summary,
        error = function(e) paste("Error:", e$message)
      )
    })

    output$det_phenotype <- renderText({
      req(llm_interp(), input$module_sel)
      tryCatch(
        llm_interp()[[input$module_sel]]$generated_name$phenotype_analysis,
        error = function(e) paste("Error:", e$message)
      )
    })

    output$det_confidence <- renderText({
      req(llm_interp(), input$module_sel)
      tryCatch(
        llm_interp()[[input$module_sel]]$generated_name$confidence_score,
        error = function(e) paste("Error:", e$message)
      )
    })

  })
}
