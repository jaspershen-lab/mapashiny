# ============================================================
#  Shared UI helpers
# ============================================================

#' Build the bslib theme for MAPA
#' @noRd
mapa_theme <- function() {
  bslib::bs_theme(
    version   = 5,
    primary   = "#2C6FAC",
    secondary = "#6C757D",
    success   = "#28A745",
    info      = "#17A2B8",
    warning   = "#FFC107",
    danger    = "#DC3545",
    bg        = "#F7F9FB",
    fg        = "#2D3748",
    "body-bg"            = "#F7F9FB",
    "body-color"         = "#2D3748",
    "link-color"         = "#2C6FAC",
    "card-bg"            = "#FFFFFF",
    "card-border-color"  = "#E8ECF0",
    "card-border-radius" = "0.6rem",
    "border-radius"      = "0.4rem",
    base_font    = bslib::font_google("Inter",         local = FALSE),
    heading_font = bslib::font_google("Inter",         local = FALSE, wght = "600;700"),
    code_font    = bslib::font_google("JetBrains Mono",local = FALSE)
  ) |>
  bslib::bs_add_rules("
    /* Multi-omics purple overrides – injected after Bootstrap compile */
    .mode-mo .form-check-input:checked,
    .mode-mo .form-check-input[type='checkbox']:checked,
    .mode-mo .form-check-input[type='radio']:checked {
      background-color: #7C3AED !important;
      border-color:     #7C3AED !important;
    }
    .mode-mo .form-check-input:focus {
      border-color: #7C3AED !important;
      box-shadow: 0 0 0 0.2rem rgba(124,58,237,.25) !important;
    }
    .mode-mo .irs--shiny .irs-bar,
    .mode-mo .irs--shiny .irs-bar--single {
      background: #7C3AED !important;
      border-top-color: #7C3AED !important;
      border-bottom-color: #7C3AED !important;
    }
    .mode-mo .irs--shiny .irs-handle { background: #7C3AED !important; border-color: #7C3AED !important; }
    .mode-mo .irs--shiny .irs-from,
    .mode-mo .irs--shiny .irs-to,
    .mode-mo .irs--shiny .irs-single { background: #7C3AED !important; }
    .mode-mo .irs--shiny .irs-from::before,
    .mode-mo .irs--shiny .irs-to::before,
    .mode-mo .irs--shiny .irs-single::before { border-top-color: #7C3AED !important; }
  ")
}

#' Build the navbar brand HTML
#' @noRd
navbar_brand <- function() {
  tags$span(
    class   = "d-flex align-items-center gap-2 navbar-home-link",
    style   = "cursor: pointer;",
    onclick = "Shiny.setInputValue('nav_to_landing', Math.random(), {priority:'event'})",
    tags$img(
      src    = "www/mapa_logo.png",
      height = "34",
      alt    = "MAPA",
      style  = "width:auto; border-radius:5px; flex-shrink:0;"
    ),
    tags$span(class = "fw-bold fs-5 text-white", "MAPA"),
    tags$span(
      class = "badge rounded-pill",
      style = "background:#FFFFFF22; font-size:0.65rem; font-weight:500;",
      paste0("v", utils::packageVersion("mapa"))
    )
  )
}

#' Render the step navigation list in the sidebar
#'
#' @param steps  list of step definitions (id, n, title, desc)
#' @param current character, the active step id
#' @param completed character vector of completed step ids
#' @noRd
build_step_nav <- function(steps, current, completed) {
  items <- lapply(seq_along(steps), function(i) {
    s <- steps[[i]]
    state <- dplyr::case_when(
      s$id == current          ~ "active",
      s$id %in% completed      ~ "completed",
      TRUE                     ~ "pending"
    )

    num_content <- if (state == "completed") {
      tags$i(class = "fas fa-check", style = "font-size:12px;")
    } else {
      as.character(s$n)
    }

    connector <- if (i < length(steps)) {
      div(class = paste("step-connector", if (s$id %in% completed) "completed" else ""))
    } else NULL

    tagList(
      div(
        class   = paste("step-item", state),
        `data-step` = s$id,
        onclick = sprintf(
          "Shiny.setInputValue('nav_to_step', '%s', {priority:'event'})", s$id
        ),
        div(class = "step-number", num_content),
        div(
          class = "step-text",
          div(class = "step-title", s$title),
          div(class = "step-desc",  s$desc)
        )
      ),
      connector
    )
  })

  div(class = "step-nav", items)
}

#' Wrapper that gives every step a consistent page layout
#'
#' @param ...  content to place after the header
#' @param .badge_label  short badge text shown above the title
#' @param .title        step title (h2)
#' @param .subtitle     one-line description below the title
#' @noRd
step_page <- function(..., .badge_label = NULL, .title = NULL, .subtitle = NULL) {
  div(
    class = "step-page",
    if (!is.null(.badge_label))
      div(class = "step-badge", .badge_label),
    if (!is.null(.title))
      tags$h2(class = "step-heading", .title),
    if (!is.null(.subtitle))
      tags$p(class = "step-subtitle text-muted", .subtitle),
    ...
  )
}

#' Standard Back / Next navigation buttons at the bottom of a step
#'
#' @param ns         namespace function
#' @param back       logical — show the Back button?
#' @param next_label label for the Next button
#' @param show_code  logical — show a Code button to the right of Next?
#' @noRd
step_nav_buttons <- function(ns, back = TRUE, next_label = "Next",
                             next_arrow = TRUE, show_code = FALSE) {
  next_label_ui <- if (next_arrow)
    tagList(next_label, tags$i(class = "fas fa-arrow-right ms-1"))
  else
    next_label

  next_btn <- actionButton(ns("btn_next"), label = next_label_ui,
                           class = "btn-step-next")

  right_side <- if (show_code) {
    div(class = "d-flex gap-2",
      next_btn,
      actionButton(ns("btn_code"), "Code", class = "btn-step-back")
    )
  } else {
    next_btn
  }

  div(
    class = "step-nav-buttons mt-4",
    if (back) {
      actionButton(ns("btn_back"),
                   label = tagList(tags$i(class = "fas fa-arrow-left me-1"),
                                   "Back"),
                   class = "btn-step-back")
    } else {
      div()
    },
    right_side
  )
}

#' A section card with a coloured header
#' @noRd
mapa_card <- function(title, ..., class = "") {
  bslib::card(
    class = paste("step-card", class),
    bslib::card_header(title),
    bslib::card_body(...)
  )
}

#' Show a mapa R code snippet in a shinyalert modal
#' @noRd
show_code_modal <- function(code_str) {
  shinyalert::shinyalert(
    text = paste0(
      "<pre style='text-align:left;font-family:Consolas,Monaco,monospace;",
      "background:#f8f9fa;padding:15px;border-radius:5px;border:1px solid #e9ecef;",
      "overflow-x:auto;white-space:pre-wrap;font-size:13px;line-height:1.4;margin:0;",
      "max-height:400px;overflow-y:auto;'>",
      htmltools::htmlEscape(code_str),
      "</pre>"
    ),
    html              = TRUE,
    type              = "",
    confirmButtonText = "Close",
    confirmButtonCol  = "#dd4b39"
  )
}

#' Banner telling the user whether upstream results are already in the session
#'
#' Steps that accept an .rda saved from an earlier step use this to make clear
#' that the upload is only a fallback: as long as the session has not been
#' restarted, the previous step's result is carried over automatically and the
#' user can go straight to the settings.
#'
#' @param has_data     logical — is the upstream result already available?
#' @param from_step    character, e.g. "Step 2 · Pathway Enrichment"
#' @param ready_detail optional extra line shown under the "ready" message
#' @param upload_hint  optional replacement for the default "please upload" line
#' @noRd
carryover_status <- function(has_data, from_step, ready_detail = NULL,
                             upload_hint = NULL) {
  if (isTRUE(has_data)) {
    status_alert(
      tags$span(
        tags$strong(from_step, " results are already loaded in this session."),
        " Continue with the settings below — no upload needed.",
        if (!is.null(ready_detail))
          tagList(tags$br(), tags$small(ready_detail))
      ),
      "success"
    )
  } else {
    status_alert(
      tags$span(
        tags$strong("No ", from_step, " result in this session."),
        " ",
        upload_hint %||% paste0(
          "Run that step first, or upload the .rda you saved from it to ",
          "resume from here."
        )
      ),
      "info"
    )
  }
}

#' Caption marking a "resume from file" upload control as optional
#'
#' @param from_step character, e.g. "Step 2 · Pathway Enrichment"
#' @noRd
optional_upload_caption <- function(from_step) {
  tags$p(
    class = "text-muted small mb-2",
    tags$span(class = "badge rounded-pill bg-secondary me-1", "Optional"),
    paste0("Only needed if you did not run ", from_step,
           " in this session (e.g. after reopening the app).")
  )
}

#' Inline status alert
#' @noRd
status_alert <- function(text, type = c("info", "success", "warning", "danger")) {
  type <- match.arg(type)
  icon_map <- c(info = "circle-info", success = "circle-check",
                warning = "triangle-exclamation", danger = "circle-xmark")
  div(
    class = paste0("alert alert-", type, " d-flex align-items-center gap-2 py-2"),
    tags$i(class = paste("fas fa-", icon_map[[type]], sep = "")),
    text
  )
}
