#' Upload Data Module UI
#'
#' Internal UI for data upload.
#'
#' @param id Module id.
#' @import shiny
#' @importFrom shinyjs hidden
#' @noRd

upload_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "upload_data",
    fluidPage(
      titlePanel("Data Upload"),
      fluidRow(
        column(4,
               fileInput(
                 ns("variable_info"),
                 "Choose marker information",
                 accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv",
                   ".xlsx",
                   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                   "application/vnd.ms-excel"
                 )
               ),

               radioButtons(
                 ns("query_type"),
                 "Query type",
                 choices = c(
                   "Gene" = "gene",
                   "Metabolite" = "metabolite"
                 ),
                 inline = TRUE
               ),

               shinyjs::hidden(
                 div(id = ns("gene_panel"),
                     checkboxGroupInput(
                       ns("example_choice"),
                       "Example dataset",
                       choices = c(
                         "Pathway Enrichment Example" = "example_enrich_pathway",
                         "GSEA Example" = "example_gsea"
                       ),
                       selected = character(0)
                     ),
                     tags$h5("Organism"),
                     selectInput(
                       ns("organism"),
                       "Model organism",
                       choices = c(
                         " " = "",
                         "Human (org.Hs.eg.db)" = "org.Hs.eg.db",
                         "Mouse (org.Mm.eg.db)" = "org.Mm.eg.db",
                         "Rat (org.Rn.eg.db)" = "org.Rn.eg.db",
                         "Fly (org.Dm.eg.db)" = "org.Dm.eg.db",
                         "Zebrafish (org.Dr.eg.db)" = "org.Dr.eg.db",
                         "Arabidopsis (org.At.tair.db)" = "org.At.tair.db",
                         "Yeast (org.Sc.sgd.db)" = "org.Sc.sgd.db",
                         "Worm (org.Ce.eg.db)" = "org.Ce.eg.db",
                         "Pig (org.Ss.eg.db)" = "org.Ss.eg.db",
                         "Bovine (org.Bt.eg.db)" = "org.Bt.eg.db",
                         "Rhesus (org.Mmu.eg.db)" = "org.Mmu.eg.db",
                         "Canine (org.Cf.eg.db)" = "org.Cf.eg.db",
                         "E. coli strain K12(org.EcK12.eg.db)" = "org.EcK12.eg.db",
                         "E coli strain Sakai" = "org.EcSakai.eg.db",
                         "Chicken (org.Gg.eg.db)" = "org.Gg.eg.db",
                         "Xenopus (org.Xl.eg.db)" = "org.Xl.eg.db",
                         "Chimp (org.Pt.eg.db)" = "org.Pt.eg.db",
                         "Anopheles (org.Ag.eg.db)" = "org.Ag.eg.db",
                         "Malaria (org.Pf.plasmo.db)" = "org.Pf.plasmo.db",
                         "Myxococcus xanthus DK 1622" = "org.Mxanthus.db"
                       ),
                       selected = ""
                     ),
                     helpText("Select the name of an OrgDb package that is installed on your system.",
                              "Common examples: org.Hs.eg.db (Human), org.Mm.eg.db (Mouse), org.Rn.eg.db (Rat)",
                              "For the current list of OrgDb packages, visit: ",
                              tags$a(
                                href = "https://bioconductor.org/packages/release/BiocViews.html#___OrgDb",
                                "Bioconductor OrgDb packages",
                                target = "_blank"
                              )),
                     strong("Non-model organism"),
                     fluidRow(
                       column(6,
                              textInput(ns("ah_id"),
                                        "AnnotationHub ID",
                                        value = "")),
                       column(6,
                              tags$div(
                                style = "display: flex; flex-direction: column;",
                                tags$label("Return OrgDb", `for` = ns("return_orgdb")),
                                checkboxInput(ns("return_orgdb"), "", FALSE)
                              ))
                     ),

                     selectInput(
                       ns("id_type"),
                       "Input ID type",
                       choices = list(
                         "ENSEMBL" = "ensembl",
                         "UniProt" = "uniprot",
                         "EntrezID" = "entrezid",
                         "Symbol" = "symbol"
                       ),
                       selected = "ensembl"
                     )
                 )),

               shinyjs::hidden(
                 div(id = ns("metabolite_panel"),
                     checkboxGroupInput(
                       ns("met_example_choice"),
                       "Example dataset",
                       choices = c(
                         "Pathway Enrichment Example" = "example_enrich_pathway"
                       ),
                       selected = character(0)
                     ),

                     # selectInput(
                     #   ns("met_organism"),
                     #   "Organism",
                     #   choices = c(
                     #     "Human (hsa)"       = "hsa",
                     #     "crab-eating macaque (mcf)" = "mcf",
                     #     "Mouse (mmu)"       = "mmu",
                     #     "Rat (rno)"         = "rno",
                     #     "Fly (dme)"         = "dme",
                     #     "Zebrafish (dre)"   = "dre",
                     #     "Yeast (sce)"       = "sce",
                     #     "Worm (cel)"        = "cel",
                     #     "Pig (ssc)"         = "ssc",
                     #     "Bovine (bta)"      = "bta",
                     #     "Canine (cfa)"      = "cfa"
                     #   ),
                     #   selected = "hsa"
                     # ),
                     selectizeInput(
                       ns("met_organism"), 
                       "Organism", 
                       choices = NULL,  # Set choices to NULL for server-side processing
                       options = list(
                         placeholder = "Type to search organisms...",
                         maxOptions = 100
                        )),
                     helpText(
                       "Select organism by KEGG organism code or name.",
                       "For a complete list of organism codes and names, visit: ",
                       tags$a(
                         href = "https://www.genome.jp/kegg/catalog/org_list.html",
                         "KEGG Organism Codes",
                         target = "_blank"
                       )
                     ),
                     selectInput(
                       ns("met_id_type"),
                       "Input ID type",
                       choices = list(
                         "KEGG ID" = "keggid",
                         "HMDB ID" = "hmdbid"
                       ),
                       selected = "hmdbid"
                     )
                 )),

               actionButton(
                 ns("map_id"),
                 "Submit",
                 class = "btn-primary",
                 style = "background-color: #d83428; color: white;"
                ),
               actionButton(
                 ns("go2enrich_pathways"),
                 "Next",
                 class = "btn-primary",
                 style = "background-color: #d83428; color: white;"
               ),
               actionButton(
                 ns("show_conversion_code"),
                 "Code",
                 class = "btn-primary",
                 style = "background-color: #d83428; color: white;"
               ),
               style = "border-right: 1px solid #ddd; padding-right: 20px;"
        ),
        column(8,
               tabsetPanel(
                 tabPanel(
                   title = "Marker information",
                   shiny::dataTableOutput(ns("show_variable_info")),
                   br(),
                   shinyjs::useShinyjs(),
                   downloadButton(
                     ns("download_variable_info"),
                     "Download",
                     class = "btn-primary",
                     style = "background-color: #d83428; color: white;"
                  )
                 )
               )
        )
      )
    ))
}

#' Upload Data Module Server
#'
#' Internal server function for handling data upload, conversion, and processing in the Shiny application.
#' This module manages file uploads, example data selection, ID conversion for both genes and metabolites,
#' and provides download capabilities for processed data.
#'
#' @param id Character string. The module ID for namespace isolation in Shiny.
#' @param processed_info Reactive value object. Stores the processed data information to be shared across modules.
#' @param tab_switch Function. Callback function to switch between different tabs in the application.
#'
#' @details
#' The module handles the following functionalities:
#' * File upload support for CSV and Excel files
#' * Example dataset selection
#' * ID conversion for genes (ENSEMBL, UniProt, EntrezID) and metabolites
#' * Dynamic UI updates based on query type
#' * Data validation and error handling
#' * Download capabilities for processed data
#' * Conversion code display
#'
#' @return None (creates a Shiny module server)
#'
#' @import shiny
#' @importFrom shinyjs toggleElement toggleState disable enable hidden useShinyjs
#' @importFrom readxl read_excel
#'
#' @noRd

upload_data_server <- function(id, processed_info, tab_switch) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Initialize reactive values for storing data throughout the module
      data_values <- reactiveValues(
        raw_data = NULL,        # Original uploaded or example data
        converted_data = NULL,  # Data after ID conversion
        conversion_code = NULL
      )

      ## Toggle UI panels based on query type
      observe({
        shinyjs::toggleElement(
          id = "gene_panel",
          condition = input$query_type == "gene"
        )
      })

      observe({
        shinyjs::toggleElement(
          id = "metabolite_panel",
          condition = input$query_type == "metabolite"
        )
      })

      # # Update metabolite ID type when organism changes
      # updateSelectizeInput(session,
      #                      "met_organism",
      #                      choices = choices,
      #                      server = TRUE)
      
      observeEvent(input$met_organism, {
        if (!is.null(input$met_organism)) {
          if (input$met_organism == "hsa") {
            # For human (hsa), show both KEGG and HMDB options
            updateSelectInput(
              session,
              "met_id_type",
              choices = list(
                "KEGG ID" = "keggid",
                "HMDB ID" = "hmdbid"
              ),
              selected = "hmdbid"  # Default to HMDB for human
            )
          } else {
            # For non-human organisms, only show KEGG
            updateSelectInput(
              session,
              "met_id_type",
              choices = list(
                "KEGG ID" = "keggid"
              ),
              selected = "keggid"
            )
          }
        }
      })
      
      # observeEvent(input$met_organism, {
      #   if (input$met_organism != "hsa") {
      #     updateSelectInput(
      #       session,
      #       "met_id_type",
      #       choices = list(
      #         "KEGG ID" = "keggid"
      #       ),
      #       selected = "keggid"
      #     )
      #   }
      # })

      # Load data from file upload or example selection
      # Observer for gene example data selection
      observeEvent(input$example_choice, {
        if (length(input$example_choice) == 0) {
          updateSelectInput(
            session,
            "organism",
            choices = c(
              " " = "",
              "Human (org.Hs.eg.db)" = "org.Hs.eg.db",
              "Mouse (org.Mm.eg.db)" = "org.Mm.eg.db",
              "Rat (org.Rn.eg.db)" = "org.Rn.eg.db",
              "Fly (org.Dm.eg.db)" = "org.Dm.eg.db",
              "Zebrafish (org.Dr.eg.db)" = "org.Dr.eg.db",
              "Arabidopsis (org.At.tair.db)" = "org.At.tair.db",
              "Yeast (org.Sc.sgd.db)" = "org.Sc.sgd.db",
              "Worm (org.Ce.eg.db)" = "org.Ce.eg.db",
              "Pig (org.Ss.eg.db)" = "org.Ss.eg.db",
              "Bovine (org.Bt.eg.db)" = "org.Bt.eg.db",
              "Rhesus (org.Mmu.eg.db)" = "org.Mmu.eg.db",
              "Canine (org.Cf.eg.db)" = "org.Cf.eg.db",
              "E. coli strain K12(org.EcK12.eg.db)" = "org.EcK12.eg.db",
              "E coli strain Sakai" = "org.EcSakai.eg.db",
              "Chicken (org.Gg.eg.db)" = "org.Gg.eg.db",
              "Xenopus (org.Xl.eg.db)" = "org.Xl.eg.db",
              "Chimp (org.Pt.eg.db)" = "org.Pt.eg.db",
              "Anopheles (org.Ag.eg.db)" = "org.Ag.eg.db",
              "Malaria (org.Pf.plasmo.db)" = "org.Pf.plasmo.db",
              "Myxococcus xanthus DK 1622" = "org.Mxanthus.db"
            ),
            selected = ""
          )
          
          updateSelectInput(
            session,
            "id_type",
            choices = list(
              "ENSEMBL" = "ensembl",
              "UniProt" = "uniprot",
              "EntrezID" = "entrezid",
              "Symbol" = "symbol"
            ),
            selected = "ensembl"
          )
        }
        if (input$query_type == "gene" && length(input$example_choice) > 0) {
          # Select the organism directly
          updateSelectInput(
            session,
            "organism",
            choices = list(
              "Mouse (org.Mm.eg.db)" = "org.Mm.eg.db"
            ),
            selected = "org.Mm.eg.db"
          )
          
          updateSelectInput(
            session,
            "id_type",
            choices = list(
              "Symbol" = "symbol"
            ),
            selected = "symbol"
          )
          
          selected_example <- input$example_choice[1]
          
          # updateSelectInput(
          #   session,
          #   "example_choice",
          #   selected = selected_example
          # )
          
          # Example data selected - load from package data
          example_data <- switch(selected_example,
                                 "example_enrich_pathway" = {
                                   # Load the package data object
                                   data("example_ora_data", envir = environment())
                                   example_ora_data
                                 },
                                 "example_gsea" = {
                                   # Load the package data object
                                   data("example_gsea_data", envir = environment())
                                   example_gsea_data
                                 },
                                 NULL)
          
          if (!is.null(example_data)) {
            tryCatch({
              data_values$raw_data <- example_data
              # showNotification("Example data loaded successfully", type = "message")
              shinyalert::shinyalert(
                title = "Example data loaded successfully",
                html = TRUE,
                type = "success",
                confirmButtonCol = "#dd4b39"
              )
              
            }, error = function(e) {
              # showNotification(paste("Failed to load example data:", e$message), type = "error")
              shinyalert::shinyalert(
                title = "Failed to load example data",
                text = e$message,
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            })
          } else {
            # showNotification("Selected example data not available", type = "warning")
            shinyalert::shinyalert(
              title = "Selected example data not available",
              html = TRUE,
              type = "warning",
              confirmButtonCol = "#dd4b39"
            )
          }
        }
      }, ignoreNULL = FALSE)
      
      # Observer for metabolite example data selection
      observeEvent(input$met_example_choice, {
        if (length(input$met_example_choice) == 0) {
          
          updateSelectizeInput(session,
                               "met_organism",
                               choices = choices,
                               server = TRUE)
          
          if (!is.null(input$met_organism)) {
            if (input$met_organism == "hsa") {
              # For human (hsa), show both KEGG and HMDB options
              updateSelectInput(
                session,
                "met_id_type",
                choices = list(
                  "KEGG ID" = "keggid",
                  "HMDB ID" = "hmdbid"
                ),
                selected = "hmdbid"  # Default to HMDB for human
              )
            } else {
              # For non-human organisms, only show KEGG
              updateSelectInput(
                session,
                "met_id_type",
                choices = list(
                  "KEGG ID" = "keggid"
                ),
                selected = "keggid"
              )
            }
          }
        }
        if (input$query_type == "metabolite" && length(input$met_example_choice) > 0) {
          # Select the organism directly
          updateSelectInput(
            session,
            "met_organism",
            choices = list(
              "Homo sapiens (human) (hsa)" = "hsa"
            ),
            selected = "hsa"
          )
          
          updateSelectInput(
            session,
            "met_id_type",
            choices = list(
              "KEGG ID" = "keggid"
            ),
            selected = "keggid"
          )
          
          # Example data selected - load from package data
          example_data <- switch(input$met_example_choice,
                                 "example_enrich_pathway" = {
                                   # Load the package data object
                                   data("example_met_data", envir = environment())
                                   example_met_data
                                 },
                                 NULL)
          
          if (!is.null(example_data)) {
            tryCatch({
              data_values$raw_data <- example_data
              # showNotification("Example data loaded successfully!", type = "message")
              shinyalert::shinyalert(
                title = "Example data loaded successfully",
                html = TRUE,
                type = "success",
                confirmButtonCol = "#dd4b39"
              )
            }, error = function(e) {
              # showNotification(paste("Failed to load example data:", e$message), type = "error")
              shinyalert::shinyalert(
                title = "Failed to load example data",
                text = e$message,
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            })
          } else {
            # showNotification("Selected example data not available", type = "warning")
            shinyalert::shinyalert(
              title = "Selected example data not available",
              html = TRUE,
              type = "warning",
              confirmButtonCol = "#dd4b39"
            )
          }
        }
      }, ignoreNULL = FALSE)
      
      # Observer for file uploads (works for both gene and metabolite)
      observeEvent(input$variable_info, {
        if (!is.null(input$variable_info)) {
          # File uploaded
          in_file <- input$variable_info
          tryCatch({
            if (grepl("\\.csv$", in_file$name)) {
              data_values$raw_data <- read.csv(in_file$datapath)
            } else if (grepl("\\.(xlsx|xls)$", in_file$name)) {
              data_values$raw_data <- readxl::read_excel(in_file$datapath)
            } else {
              # showNotification("Unsupported file format. Please upload CSV or Excel file.", type = "error")
              shinyalert::shinyalert(
                title = "Unsupported file format",
                text = "Only support <strong>CSV</strong> or <strong>Excel</strong> file",
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            }
            # showNotification("File uploaded successfully!", type = "message")
            shinyalert::shinyalert(
              title = "File uploaded successfully",
              html = TRUE,
              type = "success",
              confirmButtonCol = "#dd4b39"
            )
          }, error = function(e) {
            # showNotification(paste("Error reading uploaded file:", e$message), type = "error")
            shinyalert::shinyalert(
              title = "Error reading uploaded file",
              text = e$message,
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          })
        }
      }, ignoreNULL = TRUE)

      # Process data when Submit button is clicked
      observeEvent(input$map_id, {
        if (is.null(data_values$raw_data)) {
          # shiny::showModal(modalDialog(
          #   title = "Warning",
          #   "No data is available. Please upload a file or select an example dataset.",
          #   easyClose = TRUE,
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "No data available",
            text = "Upload a file or select an example dataset",
            html = TRUE,
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
          return()
        }

        shinyalert::shinyalert(
          title = "Converting IDs ....",
          text = tags$div(
            style = "text-align: center;",
            # "This may take several minutes. Please be patient...",
            tags$div(
              tags$img(src = "www/spinner.gif", width = "50px", height = "50px"),
              style = "margin-top: 20px;"
            )
          ),
          type = "",
          # imageUrl = "www/spinner.gif",
          # imageWidth = 50,
          # imageHeight = 50,
          showConfirmButton = FALSE,
          showCancelButton = FALSE,
          timer = 0,
          closeOnEsc = FALSE,
          closeOnClickOutside = FALSE,
          html = TRUE
        )
        # Process based on query type
        if (input$query_type == "gene") {
          # Set up conversion parameters
          # conversion_params <- switch(input$id_type,
          #                             "ensembl" = list(
          #                               from_id_type = "ENSEMBL",
          #                               to_id_type = c("UNIPROT", "ENTREZID", "SYMBOL")
          #                             ),
          #                             "uniprot" = list(
          #                               from_id_type = "UNIPROT",
          #                               to_id_type = c("ENSEMBL", "ENTREZID", "SYMBOL")
          #                             ),
          #                             "entrezid" = list(
          #                               from_id_type = "ENTREZID",
          #                               to_id_type = c("ENSEMBL", "UNIPROT", "SYMBOL")
          #                             )
          # )

          # Validate OrgDb format
          if (input$organism != "") {
            if (!grepl("^org\\.[A-Za-z0-9]+\\..+\\.db$", input$organism)) {
              # showNotification("Invalid OrgDb package name. Expected format: org.XX.eg.db", type = "error")
              shinyalert::closeAlert()
              
              shinyalert::shinyalert(
                title = "Invalid OrgDb package name",
                text = "Expected format: <code>org.XX.eg.db</code>.",
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
              return()
            }
            
            # Check if package is installed
            if (!requireNamespace(input$organism, quietly = TRUE)) {
              shinyalert::closeAlert()
              # shiny::showModal(modalDialog(
              #   title = "Missing Package",
              #   paste0("Package ", input$organism, " is not installed. Please install it using:\n",
              #          "BiocManager::install('", input$organism, "')"),
              #   easyClose = TRUE,
              #   footer = modalButton("Close")
              # ))
              shinyalert::shinyalert(
                title = "Missing Package",
                text = paste0("Package ", input$organism, " is not installed. Please install it using:\n",
                              "BiocManager::install('", input$organism, "')"),
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
              return()
            }
            
            # Load the package and get OrgDb object
            requireNamespace(input$organism)
            org_db_obj <- get(input$organism, envir = asNamespace(input$organism))
            ah_id <- NULL
            conversion_param <- sprintf(
              '
  organism = %s',
              input$organism
            )
          } else if (input$ah_id != "") {
            org_db_obj <- NULL
            ah_id <- input$ah_id
            conversion_param <- sprintf(
              '
  ah_id = "%s",
  return_orgdb = %s',
              input$ah_id,
              input$return_orgdb
            )
          }

          # Perform conversion
          tryCatch({
            result <- mapa::convert_id(
              data = data_values$raw_data,
              query_type = input$query_type,
              from_id_type = input$id_type,
              organism = org_db_obj,
              ah_id = ah_id,
              return_orgdb = input$return_orgdb
            )
            
            conversion_code <- sprintf(
            '
result <- mapa::convert_id(
  data = your_input_data,
  query_type = "gene",
  from_id_type = "%s",%s
)
            ',
            input$id_type,
            conversion_param
            )
            data_values$conversion_code <- conversion_code
            
            if (input$return_orgdb) {
              data_values$converted_data <- result$data
              processed_info$organism <- result$orgdb
              processed_info$return_orgdb = TRUE
            } else {
              data_values$converted_data <- result
              processed_info$organism <- input$organism
            }
            
            processed_info$variable_info <- data_values$converted_data
            processed_info$query_type <- input$query_type
            
            shinyalert::closeAlert()
            
            # showNotification("Data successfully processed", type = "message")
            shinyalert::shinyalert(
              title = "Gene marker list successfully processed",
              text = "Switch to <strong>Pathway Enrichment</strong> by clicking <strong style='color: #dd4b39;'>Next</strong>",
              html = TRUE,
              type = "success",
              confirmButtonCol = "#dd4b39"
            )
          }, error = function(e) {
            # shiny::showModal(modalDialog(
            #   title = "Error",
            #   paste("Conversion failed:", e$message),
            #   easyClose = TRUE,
            #   footer = modalButton("Close")
            # ))
            shinyalert::closeAlert()
            
            shinyalert::shinyalert(
              title = "ID conversion failed",
              text = e$message,
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          })
        }
        else if (input$query_type == "metabolite") {
          # Perform metabolite conversion
          tryCatch({
            result <- mapa::convert_id(
              data = data_values$raw_data,
              query_type = input$query_type,
              from_id_type = input$met_id_type,
              organism = input$met_organism
            )
            
            conversion_code <- sprintf(
              '
result <- mapa::convert_id(
  data = your_input_data,
  query_type = "metabolite",
  from_id_type = "%s",
  organism = "%s"
)
              ',
              input$met_id_type,
              input$met_organism
            )

            data_values$converted_data <- result
            data_values$conversion_code <- conversion_code

            processed_info$variable_info <- data_values$converted_data
            processed_info$query_type <- input$query_type
            processed_info$organism <- input$met_organism

            shinyalert::closeAlert()
            # showNotification("Data successfully processed", type = "message")
            shinyalert::shinyalert(
              title = "Metabolite marker list successfully processed",
              text = "Switch to <strong>Pathway Enrichment</strong> by clicking <strong style='color: #dd4b39;'>Next</strong>.",
              html = TRUE,
              type = "success",
              confirmButtonCol = "#dd4b39"
            )
          }, error = function(e) {
            shinyalert::closeAlert()
            
            # shiny::showModal(modalDialog(
            #   title = "Error",
            #   paste("Conversion failed:", e$message),
            #   easyClose = TRUE,
            #   footer = modalButton("Close")
            # ))
            shinyalert::shinyalert(
              title = "ID conversion failed",
              text = e$message,
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          })
        }
      })

      # Render data table with converted data
      output$show_variable_info <- shiny::renderDataTable({
        if (!is.null(data_values$converted_data)) {
          data_values$converted_data
        } else if (!is.null(data_values$raw_data)) {
          data_values$raw_data
        }
      }, options = list(pageLength = 10, scrollX = TRUE))

      # Handle download button state
      observe({
        if (is.null(data_values$converted_data)) {
          shinyjs::disable("download_variable_info")
        } else {
          shinyjs::enable("download_variable_info")
        }
      })

      # Download handler for processed data
      output$download_variable_info <- shiny::downloadHandler(
        filename = function() {
          "processed_data.csv"
        },
        content = function(file) {
          write.csv(data_values$converted_data, file, row.names = FALSE)
        }
      )

      # Show conversion code when requested
      observeEvent(input$show_conversion_code, {
        if (is.null(data_values$conversion_code)) {
          # shiny::showModal(modalDialog(
          #   title = "Warning",
          #   "No conversion code available. Please process data first.",
          #   easyClose = TRUE,
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "No conversion code available",
            text = "Process data before checking code",
            html = TRUE,
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          # shiny::showModal(modalDialog(
          #   title = "Conversion Code",
          #   tags$pre(data_values$conversion_code),
          #   easyClose = TRUE,
          #   size = "l",
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            text = paste0("<pre style='text-align: left; font-family: Consolas, Monaco, monospace; background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #e9ecef; overflow-x: auto; white-space: pre-wrap; font-size: 13px; line-height: 1.4; margin: 0; max-height: 400px; overflow-y: auto;'>",
                          htmltools::htmlEscape(data_values$conversion_code),
                          "</pre>"),
            html = TRUE,
            type = "",
            confirmButtonText = "Close",
            confirmButtonCol = "#dd4b39"
          )
        }
      })

      # Handle navigation to next tab
      observeEvent(input$go2enrich_pathways, {
        if (is.null(data_values$converted_data)) {
          # shiny::showModal(modalDialog(
          #   title = "Warning",
          #   "Please process data before proceeding to the next step.",
          #   easyClose = TRUE,
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "Process data before proceeding to the next step",
            html = TRUE,
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          tab_switch("enrich_pathways")
        }
      })
    }
  )
}
