#' Application server
#'
#' @param input,output,session Internal shiny parameters — do not remove.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # ── Step definitions ──────────────────────────────────────────────────────

  so_steps <- list(
    list(id = "so_upload",  n = 1, title = "Input Data Upload",           desc = "Gene / metabolite markers"),
    list(id = "so_enrich",  n = 2, title = "Pathway Enrichment",    desc = "GO, KEGG, Reactome"),
    list(id = "so_sim",     n = 3, title = "Pathway Similarity Computation",    desc = "Pathway network construction"),
    list(id = "so_modules", n = 4, title = "Module Identification", desc = "Clustering"),
    list(id = "so_llm",     n = 5, title = "LLM Annotation",        desc = "AI interpretation"),
    list(id = "so_viz",     n = 6, title = "Visualization",         desc = "Barplots, Networks, ..."),
    list(id = "so_report",  n = 7, title = "Results & Report",      desc = "Report & result tables export")
  )

  mo_steps <- list(
    list(id = "mo_upload",  n = 1, title = "Input Data Upload",           desc = "Gene / metabolite markers"),
    list(id = "mo_enrich",  n = 2, title = "Pathway Enrichment",    desc = "GO, KEGG, Reactome"),
    list(id = "mo_network", n = 3, title = "Network Construction and Encoding",         desc = "Random walk with restart & Diffusion profile"),
    list(id = "mo_modules", n = 4, title = "Module Identification", desc = "Clustering"),
    list(id = "mo_llm",     n = 5, title = "LLM Annotation",        desc = "AI interpretation"),
    list(id = "mo_viz",     n = 6, title = "Visualization",         desc = "Networks"),
    list(id = "mo_report",  n = 7, title = "Results & Report",      desc = "Report & result tables export")
  )

  # ── Reactive state ────────────────────────────────────────────────────────

  current_step   <- reactiveVal("landing")
  completed_steps <- reactiveVal(character(0))
  mode           <- reactive(input$analysis_mode %||% "so")

  active_steps <- reactive({
    if (mode() == "so") so_steps else mo_steps
  })

  # ── Body class tracks mode (drives CSS colour theming) ───────────────────
  observe({
    shinyjs::removeClass(selector = "#main_app_body", class = "mode-so mode-mo")
    shinyjs::addClass(   selector = "#main_app_body", class = paste0("mode-", mode()))
  })

  # ── Reset to first step when mode changes ────────────────────────────────
  observeEvent(input$analysis_mode, {
    completed_steps(character(0))
    current_step(active_steps()[[1]]$id)
  }, ignoreInit = TRUE)

  # ── Logo click → back to landing ─────────────────────────────────────────
  observeEvent(input$nav_to_landing, {
    current_step("landing")
  })

  # ── Switch visible panel ──────────────────────────────────────────────────
  observeEvent(current_step(), {
    bslib::nav_select("step_panels", selected = current_step())
  }, ignoreInit = FALSE)

  # ── Sidebar step navigation ───────────────────────────────────────────────
  output$sidebar_step_nav <- renderUI({
    build_step_nav(active_steps(), current_step(), completed_steps())
  })

  # ── Click-on-step navigation (JS sets nav_to_step) ───────────────────────
  observeEvent(input$nav_to_step, {
    ids <- sapply(active_steps(), `[[`, "id")
    if (input$nav_to_step %in% ids) current_step(input$nav_to_step)
  })

  # ── Shared navigation helpers (passed to modules) ────────────────────────
  go_next <- function() {
    ids <- sapply(active_steps(), `[[`, "id")
    idx <- which(ids == current_step())
    if (length(idx) && idx < length(ids)) {
      completed_steps(unique(c(completed_steps(), current_step())))
      current_step(ids[[idx + 1L]])
    }
  }

  go_back <- function() {
    ids <- sapply(active_steps(), `[[`, "id")
    idx <- which(ids == current_step())
    if (length(idx) && idx > 1L) current_step(ids[[idx - 1L]])
  }

  # ── Single-omics reactive data chain ─────────────────────────────────────
  so_data <- reactiveValues(
    variable_info      = NULL,   # data.frame from upload
    query_type         = NULL,   # "gene" | "metabolite"
    organism           = NULL,   # e.g. "org.Hs.eg.db"
    enriched_pathways  = NULL,   # S4 functional_module object after enrichment
    similarity_result  = NULL,   # similarity matrix / object
    functional_modules = NULL    # S4 functional_module after clustering
  )

  # ── Multi-omics reactive data chain ──────────────────────────────────────
  mo_data <- reactiveValues(
    # Step 1 – raw marker data
    transcriptome_data    = NULL,  # data.frame from upload
    transcriptome_org     = NULL,  # e.g. "org.Hs.eg.db"
    transcriptome_id_type = NULL,  # e.g. "SYMBOL"
    proteome_data         = NULL,
    proteome_org          = NULL,
    proteome_id_type      = NULL,
    metabolome_data       = NULL,
    metabolome_id_type    = NULL,  # e.g. "HMDB"
    metabolome_org        = NULL,  # e.g. "hsa"
    # Step 2 – enrichment results (functional_module S4 objects)
    transcriptome_enrich  = NULL,
    proteome_enrich       = NULL,
    metabolome_enrich     = NULL,
    # Step 3+ – network & modules
    network_tables        = NULL,  # output of build_network_tables()
    mnet_obj              = NULL,  # multi_omics_functional_module S4
    sim_matrix            = NULL,  # sparse cosine-similarity matrix
    mo_modules            = NULL   # list from merge_multi_omics_nodes()
  )

  # Shared: whichever mode produced the final annotated modules
  annotated_modules <- reactiveVal(NULL)

  # ── Initialise all module servers ─────────────────────────────────────────

  # Landing
  mod_landing_server("landing",
    current_step = current_step, mode = mode, parent_session = session)

  # Single-omics
  mod_so_upload_server("so_upload",
    so_data = so_data, go_next = go_next, go_back = go_back, mode = mode)

  mod_so_enrich_server("so_enrich",
    so_data = so_data, go_next = go_next, go_back = go_back, mode = mode)

  mod_so_similarity_server("so_sim",
    so_data = so_data, go_next = go_next, go_back = go_back, mode = mode)

  mod_so_modules_server("so_modules",
    so_data = so_data, go_next = go_next, go_back = go_back, mode = mode)

  # Multi-omics
  mod_mo_upload_server("mo_upload",
    mo_data = mo_data, go_next = go_next, go_back = go_back, mode = mode)

  mod_mo_enrich_server("mo_enrich",
    mo_data = mo_data, go_next = go_next, go_back = go_back, mode = mode)

  mod_mo_network_server("mo_network",
    mo_data = mo_data, go_next = go_next, go_back = go_back, mode = mode)

  mod_mo_modules_server("mo_modules",
    mo_data = mo_data, go_next = go_next, go_back = go_back, mode = mode)

  # LLM annotation (mode-specific)
  mod_so_llm_server("so_llm",
    so_data = so_data, annotated_modules = annotated_modules,
    go_next = go_next, go_back = go_back)

  mod_mo_llm_server("mo_llm",
    mo_data = mo_data, annotated_modules = annotated_modules,
    go_next = go_next, go_back = go_back)

  mod_so_viz_server("so_viz",
    annotated_modules = annotated_modules, mode = mode,
    go_next = go_next, go_back = go_back)

  mod_mo_viz_server("mo_viz",
    annotated_modules = annotated_modules, mode = mode,
    go_next = go_next, go_back = go_back, mo_data = mo_data)

  mod_so_report_server("so_report",
    annotated_modules = annotated_modules,
    so_data = so_data, mo_data = mo_data, mode = mode,
    go_back = go_back)

  mod_mo_report_server("mo_report",
    annotated_modules = annotated_modules,
    so_data = so_data, mo_data = mo_data, mode = mode,
    go_back = go_back)
}

# Null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x
