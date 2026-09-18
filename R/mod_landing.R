# ── Landing page ─────────────────────────────────────────────────────────────

#' @noRd
mod_landing_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "landing-page",

    # ── Hero ──────────────────────────────────────────────────────────────────
    div(
      class = "text-center mb-4",
      tags$h1(
        class = "landing-title d-flex align-items-center justify-content-center gap-3",
        tags$img(src = "www/mapa_logo.png", height = "60px",
                 alt = "MAPA logo", class = "landing-logo"),
        "MAPA"
      ),
      tags$p(
        class = "landing-subtitle",
        HTML("<strong>M</strong>odular <strong>A</strong>nalysis and <strong>P</strong>henotype-informed <strong>A</strong>nnotation using large language models"),
        tags$br(),
        tags$span(
          class = "text-muted fs-6",
          "A semantic-biological network framework for functional module discovery",
          tags$br(),
          "and interpretation in single-omics and multi-omics data"
        )
      )
    ),

    # ── About ─────────────────────────────────────────────────────────────────
    div(
      class = "landing-section mb-5",
      tags$h2(class = "landing-section-title",
              tags$i(class = "fas fa-circle-info me-2"), "About"),
      tags$p(
        "MAPA is designed to help users move from long lists of omics molecules or enriched
         pathways to coherent, biologically interpretable functional modules. MAPA integrates molecular
         interactions and pathway-level functional context into a shared functional space,
         enabling genes, proteins, metabolites, and pathways to be compared and interpreted
         together."
      ),
      tags$p(
        "MAPA is developed by the ",
        tags$a(href = "https://www.shen-lab.org/", target = "_blank",
               "Shen Lab at Nanyang Technological University, Singapore"),
        ". ", tags$strong("MAPAShiny"), " provides an interactive web interface for running
         MAPA analyses and exploring the results."
      )
    ),

    # ── What MAPA does ────────────────────────────────────────────────────────
    div(
      class = "landing-section mb-5",
      tags$h2(class = "landing-section-title",
              tags$i(class = "fas fa-rocket me-2"), "What MAPA Does"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        gap = "1rem",

        div(
          class = "info-step-card",
          tags$span(class = "landing-step-badge", "Step 1"),
          tags$h5("Input preparation"),
          tags$p(class = "text-muted small",
            "Accepts phenotype-associated molecule list from transcriptomics,
             proteomics, and metabolomics. Supports over-representation analysis (ORA)
             and gene set enrichment analysis (GSEA) to identify enriched pathways or
             Gene Ontology terms.")
        ),

        div(
          class = "info-step-card",
          tags$span(class = "landing-step-badge", "Step 2"),
          tags$h5("Semantic-biological network construction"),
          tags$p(class = "text-muted small",
            "In multi-omics analysis, constructs a unified network with molecular
             and pathway nodes. Molecular nodes are linked through curated biological
             relationships. Pathway nodes are linked through biotext embedding similarity
             Molecular nodes and Pathway nodes are connected by pathway annotations.
             In single-omics analysis, constructs a pathway network with enriched pathways
             connected by biotext embedding similarity.")
        ),

        div(
          class = "info-step-card",
          tags$span(class = "landing-step-badge", "Step 3"),
          tags$h5("Functional similarity computation"),
          tags$p(class = "text-muted small",
            "In multi-omics analysis, computes the functional similarities among the molecules and 
             pathways based on the semantic-biological network.")
        ),
        
        div(
          class = "info-step-card",
          tags$span(class = "landing-step-badge", "Step 4"),
          tags$h5("Functional module discovery"),
          tags$p(class = "text-muted small",
                 "Identifies coherent functional modules from the similarity maytrix. 
                  Multi-omics modules may contain genes, proteins, metabolites, and pathways from different layers.
                  Single-omics modules only contain enriched pathways.")
        ),

        div(
          class = "info-step-card",
          tags$span(class = "landing-step-badge", "Step 5"),
          tags$h5("Literature-informed LLM annotation"),
          tags$p(class = "text-muted small",
            "Uses a retrieval-augmented LLM workflow to interpret each functional module.
             Evidence can be retrieved from PubMed, pathway database references, and
             optional user-uploaded materials to generate structured module names,
             biological summaries, and confidence scores.")
        ),

        div(
          class = "info-step-card",
          tags$span(class = "landing-step-badge", "Step 6"),
          tags$h5("Visualization & reporting"),
          tags$p(class = "text-muted small",
            "Provides interactive outputs for exploring enrichment results, functional
             modules, module networks, pathway-molecule relationships, expression
             heatmaps, and structured analysis reports.")
        )
      )
    ),

    tags$hr(class = "my-5"),

    # ── Mode selection cards ───────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(6, 6),
      gap = "1.5rem",

      # Single-Omics card
      div(
        class = "mode-card mode-so-card",
        id    = ns("so_card"),
        onclick = "Shiny.setInputValue('landing_mode_select', 'so', {priority:'event'})",
        div(class = "mode-icon", tags$i(class = "fas fa-dna")),
        tags$h3(class = "mode-title", "Single-Omics"),
        tags$p(
          class = "mode-desc",
          "Analyse gene or metabolite markers. Enrich pathways, compute a
           semantic similarity network, and identify functional modules
           with LLM-powered annotation."
        ),
        tags$ul(
          class = "mode-steps-list",
          tags$li(tags$i(class = "fas fa-upload me-2"), "Gene / Metabolite list upload"),
          tags$li(tags$i(class = "fas fa-magnifying-glass me-2"), "Pathway enrichment (GO, KEGG, Reactome)"),
          tags$li(tags$i(class = "fas fa-share-nodes me-2"), "Pathway similarity computation"),
          tags$li(tags$i(class = "fas fa-object-group me-2"), "Functional module identification"),
          tags$li(tags$i(class = "fas fa-brain me-2"), "LLM annotation"),
          tags$li(tags$i(class = "fas fa-chart-bar me-2"), "Data visualization"),
          tags$li(tags$i(class = "fas fa-file-export me-2"), "Result report")
        ),
        actionButton(ns("btn_start_so"), "Start Single-Omics Analysis",
                     class = "btn btn-so mt-3 w-100")
      ),

      # Multi-Omics card
      div(
        class = "mode-card mode-mo-card",
        id    = ns("mo_card"),
        onclick = "Shiny.setInputValue('landing_mode_select', 'mo', {priority:'event'})",
        div(class = "mode-icon", tags$i(class = "fas fa-circle-nodes")),
        tags$h3(class = "mode-title", "Multi-Omics"),
        tags$p(
          class = "mode-desc",
          "Integrate transcriptomics, proteomics and metabolomics. Enrich each
           omics layer, build a semantic-biological network, quantify
           nodes similarity using Random Walk with Restart, and cluster 
           functionally related multi-omics molecules and pathways together."
        ),
        tags$ul(
          class = "mode-steps-list",
          tags$li(tags$i(class = "fas fa-upload me-2"), "Gene / Metabolite markers per omics layer upload"),
          tags$li(tags$i(class = "fas fa-magnifying-glass me-2"), "Pathway enrichment for each omics layer"),
          tags$li(tags$i(class = "fas fa-network-wired me-2"), "Build semantic-biological network"),
          tags$li(tags$i(class = "fas fa-wave-square me-2"), "RWR diffusion-based similarity computation"),
          tags$li(tags$i(class = "fas fa-object-group me-2"), "Multi-omics module identification"),
          tags$li(tags$i(class = "fas fa-brain me-2"), "LLM annotation"),
          tags$li(tags$i(class = "fas fa-chart-bar me-2"), "Data visualization"),
          tags$li(tags$i(class = "fas fa-file-export me-2"), "Result report")
        ),
        actionButton(ns("btn_start_mo"), "Start Multi-Omics Analysis",
                     class = "btn btn-mo mt-3 w-100")
      )
    ),

    # ── Note ──────────────────────────────────────────────────────────────────
    div(
      class = "landing-section mb-5",
      bslib::card(
        class = "border-warning",
        bslib::card_body(
          class = "d-flex gap-3 align-items-start",
          tags$i(class = "fas fa-triangle-exclamation text-warning fs-4 mt-1 flex-shrink-0"),
          div(
            tags$h6(class = "fw-bold mb-1", "LLM API Key Required for Annotation"),
            tags$p(
              class = "mb-0 small text-muted",
              "LLM-based functional module annotation requires an API key from OpenAI,
               Gemini, or SiliconFlow. If you do not have an API key, please ",
              tags$a(href = "mailto:xiaotao.shen@ntu.edu.sg", "contact us"),
              " and we can provide a temporary key for testing or help run the
               annotation step for you.",
              tags$br(),
              "The MAPA confidence score should be interpreted as an annotation-level
               coherence score rather than a calibrated statistical probability."
            )
          )
        )
      )
    ),

    # ── Documentation & Links ─────────────────────────────────────────────────
    div(
      class = "landing-section mb-5",
      tags$h2(class = "landing-section-title",
              tags$i(class = "fas fa-book me-2"), "Documentation & Links"),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        gap = "1rem",

        div(
          class = "links-card",
          tags$h6(class = "fw-bold mb-2",
                  tags$i(class = "fas fa-file-lines me-1"), "Docs & Tutorial"),
          tags$ul(
            class = "links-list",
            tags$li(tags$a(href = "https://www.shen-lab.org/mapa-tutorial/",
                           target = "_blank", "MAPA Tutorial")),
            tags$li(tags$a(href = "https://www.shen-lab.org/mapa-website/",
                           target = "_blank", "MAPA Project Website")),
            tags$li(tags$a(href = "https://doi.org/10.1101/2025.08.23.671949",
                           target = "_blank", "Citation / Manuscript"))
          )
        ),

        div(
          class = "links-card",
          tags$h6(class = "fw-bold mb-2",
                  tags$i(class = "fab fa-github me-1"), "Source Code"),
          tags$ul(
            class = "links-list",
            tags$li(tags$a(href = "https://github.com/jaspershen-lab/mapa",
                           target = "_blank", "MAPA R Package")),
            tags$li(tags$a(href = "https://github.com/jaspershen-lab/mapashiny",
                           target = "_blank", "MAPAShiny")),
            tags$li(tags$a(href = "https://github.com/jaspershen-lab/mapa_manuscript",
                           target = "_blank", "Manuscript Analysis Scripts"))
          )
        ),

        div(
          class = "links-card",
          tags$h6(class = "fw-bold mb-2",
                  tags$i(class = "fas fa-server me-1"), "Deploy & Run"),
          tags$ul(
            class = "links-list",
            tags$li(tags$a(href = "https://hub.docker.com/r/jaspershenlab/mapa",
                           target = "_blank", "MAPA Docker Image")),
            tags$li(tags$a(href = "https://hub.docker.com/r/jaspershenlab/mapashiny",
                           target = "_blank", "MAPAShiny Docker Image")),
            tags$li(tags$a(href = "https://mapashiny.jaspershenlab.com",
                           target = "_blank", "Hosted MAPAShiny App"))
          )
        )
      )
    ),

    # ── Contact ───────────────────────────────────────────────────────────────
    div(
      class = "landing-section mb-5",
      tags$h2(class = "landing-section-title",
              tags$i(class = "fas fa-envelope me-2"), "Contact Us"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        gap = "1rem",
        div(
          tags$p(tags$i(class = "fas fa-envelope me-2"),
                 tags$strong("Xiaotao Shen: "),
                 tags$a(href = "mailto:xiaotao.shen@ntu.edu.sg", "xiaotao.shen@ntu.edu.sg")),
          tags$p(tags$i(class = "fas fa-envelope me-2"),
                 tags$strong("Chuchu Wang: "),
                 tags$a(href = "mailto:chuchu.wang@ntu.edu.sg", "chuchu.wang@ntu.edu.sg")),
          tags$p(tags$i(class = "fas fa-house me-2"),
                 tags$strong("Shen Lab: "),
                 tags$a(href = "https://www.shen-lab.org/", target = "_blank",
                        "shen-lab.org"))
        ),
        div(
          tags$p(tags$i(class = "fab fa-weixin me-2"),
                 tags$strong("WeChat: "),
                 tags$a(href = "https://jaspershen.github.io/image/wechat_QR.jpg",
                        target = "_blank", "jaspershen1990")),
          tags$p(tags$i(class = "fab fa-twitter me-2"),
                 tags$strong("Twitter/X: "),
                 tags$a(href = "https://twitter.com/xiaotaoshen1990",
                        target = "_blank", "@xiaotaoshen1990"))
        )
      )
    ),

    # ── Footer ────────────────────────────────────────────────────────────────
    div(
      class = "text-center mt-4 mb-3",
      tags$small(
        class = "text-muted",
        "Powered by ",
        tags$a(href = "https://github.com/jaspershen-lab/mapa", target = "_blank", "mapa"),
        " · Shen Lab, Nanyang Technological University"
      ),
      tags$br(),
      tags$a(
        href   = "https://www.shen-lab.org",
        target = "_blank",
        tags$img(
          src    = "www/shen_lab_logo.png",
          height = "36",
          alt    = "Shen Lab – Omics For Health",
          style  = "width:auto; margin-top:8px;"
        )
      )
    )
  )
}

#' @noRd
mod_landing_server <- function(id, current_step, mode, parent_session) {
  moduleServer(id, function(input, output, session) {

    # Card click → switch mode toggle + go to first step
    observeEvent(input$btn_start_so, {
      shinyWidgets::updateRadioGroupButtons(session = parent_session,
        inputId = "analysis_mode", selected = "so")
      current_step("so_upload")
    })

    observeEvent(input$btn_start_mo, {
      shinyWidgets::updateRadioGroupButtons(session = parent_session,
        inputId = "analysis_mode", selected = "mo")
      current_step("mo_upload")
    })

    # Also respond to click anywhere on the mode card (via JS input)
    observeEvent(session$userData$landing_mode_select, {
      sel <- session$userData$landing_mode_select
      if (!is.null(sel)) current_step(paste0(sel, "_upload"))
    })
  })
}
