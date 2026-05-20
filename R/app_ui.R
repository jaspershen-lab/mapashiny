#' Application UI
#'
#' @param request Internal shiny parameter — do not remove.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    bslib::page_sidebar(
      title  = navbar_brand(),
      theme  = mapa_theme(),

      # ── Sidebar ──────────────────────────────────────────────────────────
      sidebar = bslib::sidebar(
        id     = "main_sidebar",
        width  = 275,
        bg     = "#1A3A5C",
        fg     = "#FFFFFF",
        open   = list(desktop = "open", mobile = "closed"),
        border = FALSE,
        padding = "0px",

        # Mode toggle
        div(
          class = "mode-toggle-container",
          shinyWidgets::radioGroupButtons(
            inputId  = "analysis_mode",
            label    = NULL,
            choices  = c("Single-Omics" = "so", "Multi-Omics" = "mo"),
            justified = TRUE,
            size     = "sm",
            selected = "so"
          )
        ),

        # Step navigation (server-rendered)
        uiOutput("sidebar_step_nav"),

      ),

      # ── Main content ─────────────────────────────────────────────────────
      div(
        id    = "main_app_body",
        class = "mode-so",

        bslib::navset_hidden(
          id = "step_panels",

          # Landing
          bslib::nav_panel("landing",    mod_landing_ui("landing")),

          # ── Single-omics steps ──
          bslib::nav_panel("so_upload",  mod_so_upload_ui("so_upload")),
          bslib::nav_panel("so_enrich",  mod_so_enrich_ui("so_enrich")),
          bslib::nav_panel("so_sim",     mod_so_similarity_ui("so_sim")),
          bslib::nav_panel("so_modules", mod_so_modules_ui("so_modules")),

          # ── Multi-omics steps ──
          bslib::nav_panel("mo_upload",  mod_mo_upload_ui("mo_upload")),
          bslib::nav_panel("mo_enrich",  mod_mo_enrich_ui("mo_enrich")),
          bslib::nav_panel("mo_network", mod_mo_network_ui("mo_network")),
          bslib::nav_panel("mo_modules", mod_mo_modules_ui("mo_modules")),

          # ── LLM annotation (mode-specific) ──
          bslib::nav_panel("so_llm", mod_so_llm_ui("so_llm")),
          bslib::nav_panel("mo_llm", mod_mo_llm_ui("mo_llm")),
          bslib::nav_panel("so_viz",     mod_so_viz_ui("so_viz")),
          bslib::nav_panel("mo_viz",     mod_mo_viz_ui("mo_viz")),
          bslib::nav_panel("so_report",  mod_so_report_ui("so_report")),
          bslib::nav_panel("mo_report",  mod_mo_report_ui("mo_report"))
        )
      )
    )
  )
}

#' Register external resources (CSS, JS, favicon)
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))

  tags$head(
    golem::favicon(ext = "png"),
    golem::bundle_resources(path = app_sys("app/www"), app_title = "MAPA"),
    tags$link(
      rel  = "stylesheet",
      type = "text/css",
      href = "www/custom.css"
    ),
    # Font Awesome for icons
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  )
}
