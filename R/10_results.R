#' Results UI Module
#'
#' Internal UI for generating and displaying the results report.
#'
#' @param id Module id.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @noRd

results_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "results",
    fluidPage(titlePanel("Results and Report"),
              fluidPage(
                column(4,
                       br(),
                       fluidRow(
                         fileInput(inputId = ns("upload_enriched_functional_module"),
                                   label = "Upload functional module (.rda)",
                                   accept = ".rda")
                       ),
                       fluidRow(
                         actionButton(
                           inputId = ns("generate_report"),
                           label = "Generate report",
                           class = "btn-primary",
                           style = "background-color: #d83428; color: white;"
                         ),
                         shinyjs::useShinyjs(),
                         downloadButton(ns("download_report"),
                                        "Download",
                                        class = "btn-primary",
                                        style = "background-color: #d83428; color: white;"),
                         actionButton(
                           inputId = ns("show_report_code"),
                           label = "Code",
                           class = "btn-primary",
                           style = "background-color: #d83428; color: white;"
                         )
                       ),
                       style = "border-right: 1px solid #ddd; padding-right: 20px;"
                ),
                column(8,
                       tabsetPanel(
                         tabPanel(
                           title = "Report",
                           uiOutput(ns("mapa_report"))
                         ))
                )
              )
    )
  )
}

#' Results Server Module
#'
#' Internal server logic for generating the report, displaying its content,
#' and handling report download.
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param id Module id.
#' @param enriched_functional_module Reactive value containing enriched functional module data.
#' @param tab_switch Function to switch tabs.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom mapa report_functional_module
#' 
#' @noRd

results_server <- function(id, enriched_functional_module, tab_switch) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      report_code <- reactiveVal()
      report_path <- reactiveVal()

      observeEvent(input$upload_enriched_functional_module, {
        if (!is.null(input$upload_enriched_functional_module$datapath)) {
          message("Loading data")
          tempEnv <- new.env()
          load(input$upload_enriched_functional_module$datapath,
               envir = tempEnv)
          
          names <- ls(tempEnv)
          
          if (length(names) == 1) {
            object <- get(names[1], envir = tempEnv)
            if (!("merge_modules" %in% names(object@process_info))) {
              shinyalert::shinyalert(
                text = "Do <strong>Module Identification</strong> before generating result report.",
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            } else {
              enriched_functional_module(get(names[1], envir = tempEnv)) 
            }
          } else {
            message("The .rda file does not contain exactly one object.")
            
            shinyalert::shinyalert(
              text = "The uploaded file should contain exactly one object.",
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          }
        }
      })
      
      observeEvent(input$generate_report, {
        # Check if enriched_functional_module and llm_interpretation_result are available
        if (is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0) {
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched functional modules data available.",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched functional modules data",
            html = TRUE,
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          # shinyjs::show("loading")

          generate_report_alert_id <- shinyalert::shinyalert(
            title = "Generating result report",
            text = tags$div(
              style = "text-align: center;",
              "This may take several minutes. Please be patient...",
              tags$div(
                tags$img(src = "www/spinner.gif", width = "50px", height = "50px"),
                style = "margin-top: 20px;"
              )
            ),
            type = "",
            showConfirmButton = FALSE,
            showCancelButton = FALSE,
            timer = 0,
            closeOnEsc = FALSE,
            closeOnClickOutside = FALSE,
            html = TRUE
          )
          
          tryCatch({
            report_path <-
              file.path("files",
                        paste(sample(
                          c(letters, LETTERS, 0:9),
                          30, replace = TRUE
                        ), collapse = ""))
            
            mapa::report_functional_module(
              object = enriched_functional_module(),
              path = report_path,
              type = "html"
            )
          },
          error = function(e) {
            shinyalert::closeAlert(id = generate_report_alert_id)
            # shiny::showModal(
            #   modalDialog(
            #     title = "Error",
            #     paste("Details:", e$message),
            #     easyClose = TRUE,
            #     footer = modalButton("Close")
            #   )
            # )
            shinyalert::shinyalert(
              title = "Report generation failed",
              text = e$message,
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          })
          
          shinyalert::closeAlert(id = generate_report_alert_id)

          report_path(report_path)

          # shinyjs::hide("loading")

          ##save code
          report_code <-
            sprintf(
              '
report_functional_module(
  object = enriched_functional_module,
  path = %s,
  type = "html")
              ',
              paste0('"', report_path(), '"')
            )
          report_code(report_code)
        }
      })

      # Update UI to display HTML
      output$mapa_report <-
        renderUI({
          tryCatch(
            includeHTML(file.path(
              report_path(), "Report/mapa_report.html"
            )),
            error = function(e) {
              NULL
            }
          )
        })

      ###donload the zip report file
      output$download_report <-
        shiny::downloadHandler(
          filename = function() {
            "Report.zip"
          },
          content = function(file) {
            req(report_path())
            zip_path <-
              paste0(report_path(), "/Report.zip")
            zip(zipfile = zip_path,
                files = paste0(report_path(),
                               "/Report"))
            file.copy(zip_path, file)
          }
        )

      observe({
        if (is.null(report_path()) ||
            length(report_path()) == 0) {
          shinyjs::disable("download_report")
        } else {
          shinyjs::enable("download_report")
        }
      })

      ###To delete the zip file and folder when the user closes the app
      session$onSessionEnded(function() {
        all_files_folders <-
          list.files("files", full.names = TRUE)

        folders <-
          Filter(function(x) {
            file.info(x)$isdir
          }, all_files_folders)

        regex_pattern <- "^[A-Za-z0-9]{30}$"

        report_dirs <-
          Filter(function(folder) {
            folder_name <- basename(folder)
            grepl(regex_pattern, folder_name)
          }, folders)

        unlink(report_dirs, recursive = TRUE)
      })

      ####show code
      observeEvent(input$show_report_code, {
        if (is.null(report_code()) ||
            length(report_code()) == 0) {
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No available code",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No available code",
            html = TRUE,
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else{
          code_content <-
            report_code()
          code_content <-
            paste(code_content, collapse = "\n")
          # shiny::showModal(modalDialog(
          #   title = "Code",
          #   tags$pre(code_content),
          #   easyClose = TRUE,
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            text = paste0("<pre style='text-align: left; font-family: Consolas, Monaco, monospace; background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #e9ecef; overflow-x: auto; white-space: pre-wrap; font-size: 13px; line-height: 1.4; margin: 0; max-height: 400px; overflow-y: auto;'>",
                          htmltools::htmlEscape(code_content),
                          "</pre>"),
            html = TRUE,
            type = "",
            confirmButtonText = "Close",
            confirmButtonCol = "#dd4b39"
          )
        }
      })
    }
  )
}
