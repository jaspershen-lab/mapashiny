#' Module Annotation Module UI
#'
#' Creates the user interface for annotation of functional modules,
#' including file upload controls, parameter inputs, and result display tabs.
#'
#' @param id Character string. The module's namespace ID.
#'
#' @return A Shiny UI element containing the module annotation interface
#'   with input controls on the left (file uploads, model parameters, API key,
#'   directory selection) and tabbed output panels on the right (annotation
#'   results, full prompt, R object).
#'
#' @import shiny
#' @importFrom shinyBS bsButton bsPopover
#' @importFrom shinyjs useShinyjs
#' @noRd

llm_interpretation_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "llm_interpretation",
    fluidPage(titlePanel("Module Annotation"),
              fluidPage(
                fluidRow(
                ## Input layout ====
                column(
                  4,
                  h4("Step1: Perform Module Annotation"),
                  fluidRow(
                    column(12,
                           fileInput(inputId = ns("upload_enriched_functional_module"),
                                     label = "Upload functional module (.rda)",
                                     accept = ".rda"),
                           # bslib::tooltip(
                           #   bsicons::bs_icon("info-circle", class = "ms-1 text-info"),
                           #   "You can upload the functional module result here.",
                           #   placement = "right",
                           #   id = ns("upload_functional_module_tooltip")
                           # )
                           # bsPopover(
                           #   id = ns("upload_functional_module_info"),
                           #   title = "",
                           #   content = "You can upload the functional module result here.",
                           #   placement = "right",
                           #   trigger = "hover",
                           #   options = list(container = "body")
                           # )
                    )
                  ),
                  # br(),
                  fluidRow(
                    column(4,
                           selectInput(
                             ns("llm_api_provider"),
                             "API provider",
                             choices = c("OpenAI" = "openai", 
                                         "Google" = "gemini",
                                         "SiliconFlow" = "siliconflow"),
                             selected = "siliconflow")),
                    column(8,
                           textInput(ns("api_key"),
                                     "API key",
                                     value = ""))
                  ),
                  # fluidRow(
                  #   column(
                  #     6,
                  #     textInput(ns("llm_model"),
                  #               "LLM model",
                  #               value = "gpt-4o-mini-2024-07-18")
                  #   ),
                  #   column(
                  #     6,
                  #     textInput(ns("embedding_model"),
                  #               "Embedding model",
                  #               value = "text-embedding-3-small")
                  #   )),
                  fluidRow(
                    column(
                      12,
                      selectizeInput(ns("embedding_model"),
                                     "Embedding model",
                                     choices = list(
                                       "SiliconFlow" = list(
                                         "Qwen/Qwen3-Embedding-0.6B" = "Qwen/Qwen3-Embedding-0.6B",
                                         "Qwen/Qwen3-Embedding-4B" = "Qwen/Qwen3-Embedding-4B",
                                         "Qwen/Qwen3-Embedding-8B" = "Qwen/Qwen3-Embedding-8B"
                                       ),
                                       "OpenAI" = list(
                                         "text-embedding-3-small" = "text-embedding-3-small",
                                         "text-embedding-3-large" = "text-embedding-3-large",
                                         "text-embedding-ada-002" = "text-embedding-ada-002"
                                       ),
                                       "Google Gemini" = list(
                                         "models/text-embedding-004" = "models/text-embedding-004",
                                         "models/gemini-embedding-001" = "models/gemini-embedding-001"
                                       )
                                     ),
                                     selected = "Qwen/Qwen3-Embedding-0.6B",
                                     options = list(
                                       create = TRUE,
                                       placeholder = "Select or type a model name"
                                     ))
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      selectizeInput(ns("llm_model"),
                                     "LLM model",
                                     choices = list(
                                       "SiliconFlow" = list(
                                         "Qwen/Qwen3-8B" = "Qwen/Qwen3-8B",
                                         "Qwen/Qwen3-14B" = "Qwen/Qwen3-14B",
                                         "Qwen/Qwen3-30B-A3B-Thinking-2507" = "Qwen/Qwen3-30B-A3B-Thinking-2507",
                                         "Qwen/Qwen3-32B" = "Qwen/Qwen3-32B"
                                       ),
                                       "OpenAI GPT" = list(
                                         "gpt-4o-mini-2024-07-18" = "gpt-4o-mini-2024-07-18"
                                       ),
                                       "Google Gemini" = list(
                                         "models/gemini-1.5-flash" = "models/gemini-1.5-flash",
                                         "models/gemini-2.5-flash" = "models/gemini-2.5-flash"
                                       )
                                     ),
                                     selected = "Qwen/Qwen3-8B",
                                     options = list(
                                       create = TRUE,
                                       placeholder = "Select or type a model name"
                                     ))
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      helpText(
                        tags$div(
                          style = "color: #28a745; font-weight: 500; margin-bottom: 8px;",
                          "✓ Models shown in the selection have been tested and work stably"
                        ),
                        tags$strong("Find more models at:"),
                        tags$br(),
                        "• SiliconFlow (Intl): ", tags$a("cloud.siliconflow.com/models", 
                                                         href = "https://cloud.siliconflow.com/models", 
                                                         target = "_blank"),
                        tags$br(),
                        "• SiliconFlow (CN): ", tags$a("cloud.siliconflow.cn/me/models", 
                                                       href = "https://cloud.siliconflow.cn/me/models", 
                                                       target = "_blank"),
                        tags$br(),
                        "• OpenAI: ", tags$a("platform.openai.com/docs/models", 
                                             href = "https://platform.openai.com/docs/models", 
                                             target = "_blank"),
                        tags$br(),
                        "• Google Gemini: ", tags$a("ai.google.dev/gemini-api/docs/models", 
                                                    href = "https://ai.google.dev/gemini-api/docs/models", 
                                                    target = "_blank")
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      numericInput(ns("module_content_number_cutoff"),
                                   "Module size cutoff",
                                   value = 1,
                                   min = 0,
                                   max = 1000)     
                    ),
                    column(
                      6,
                      numericInput(ns("years"),
                                   "Years to search",
                                   value = 5,
                                   min = 1,
                                   max = 1000)
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      textInput(
                        ns("phenotype"),
                        "Disease or phenotype",
                        value = "NULL",
                        width = "100%"
                      )
                    )
                  ),
                  # gene panel
                  shinyjs::hidden(
                    div(id = ns("gene_panel"),
                        tags$h5("Organism gene annotation database"),
                        fluidRow(
                          column(6, style = "padding-right: 0px;",
                                 selectInput(
                                   ns("model_orgdb"),
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
                                 )),
                          column(4,
                                 actionButton(
                                   ns("load_orgdb"),
                                   "Load orgDb",
                                   class = "btn-primary",
                                   style = "background-color: #d83428; color: white; margin-top: 25px;"
                                 ))
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
                          column(6, style = "padding-right: 0px;",
                                 textInput(ns("non_model_ah_id"),
                                           "AnnotationHub ID",
                                           value = "")),
                          column(4,
                                 actionButton(
                                   ns("retrieve_orgdb"),
                                   "Retrieve orgDb",
                                   class = "btn-primary",
                                   style = "background-color: #d83428; color: white; margin-top: 25px;"
                                 ))
                        )
                  )),
                  
                  fluidRow(
                    column(12,
                           wellPanel(
                             strong("Your current working directory: "),
                             verbatimTextOutput(ns("current_wd"), placeholder = TRUE),
                             style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 15px;"
                           )
                    )
                  ),
                  helpText(
                    "Tip: Current working directory is displayed above. You can use relative paths (e.g., 'output/embeddings') or absolute paths (e.g., '/home/user/project/embeddings')",
                    HTML("<br><span style='color: red;'><strong>NOTE: This will clean the folder content at first! Please check the folder before selecting it.</strong></span>")
                  ),
                  textInput(ns("embedding_output_dir_path"), 
                            "(Required) Embeddings output directory",
                            width = "100%", 
                            placeholder = paste0("e.g., ", file.path(getwd(), "embeddings")),
                            value = ""),
                  fluidRow(
                    column(12,
                           textInput(ns("local_corpus_dir_path"), 
                                     "(Optional) Local corpus directory",
                                     width = "100%", 
                                     placeholder = "Enter path to directory containing PDF files (optional)",
                                     value = "")
                    )
                  ),
                  
                  actionButton(
                    ns("submit_llm_interpretation"),
                    "Submit",
                    class = "btn-primary",
                    style = "background-color: #d83428; color: white;"
                  ),

                  actionButton(
                    ns("show_llm_interpretation_code"),
                    "Code",
                    class = "btn-primary",
                    style = "background-color: #d83428; color: white;"
                  ),
                  br(),br(),
                  h4("Step2: Check Module Annotation Result"),
                  fluidRow(
                    column(12,
                           fileInput(
                             inputId = ns("upload_interpretation_result"),
                             label   = "Upload module annotation result (.rda)",
                             accept = ".rda"
                           )
                           # shinyBS::bsPopover(
                           #   id        = ns("upload_interpretation_result_info"),
                           #   title     = "",
                           #   content   = "Drop a previously saved result with LLM interpretation (.rda) here to inspect it without re-running the LLM.",
                           #   placement = "right", trigger = "hover",
                           #   options   = list(container = "body")
                           # )
                    )
                  ),
                  actionButton(
                    ns("go2data_visualization"),
                    "Next",
                    class = "btn-primary",
                    style = "background-color: #d83428; color: white;"
                  ),
                  
                  style = "border-right: 1px solid #ddd; padding-right: 20px;"
                ),
                ## Output layout ====
                column(8,
                       tabsetPanel(
                         tabPanel(
                             title = "Module annotation results",
                           uiOutput(ns("module_details")),
                           br(),
                           shinyjs::useShinyjs(),
                           downloadButton(ns("download_llm_interpretation_result"),
                                          "Download",
                                          class = "btn-primary",
                                          style = "background-color: #d83428; color: white;")
                         ),
                         tabPanel(
                           title = "Full prompt",
                           # h3("LLM Prompt Used"),
                           uiOutput(ns("prompt")),
                           br(),
                           shinyjs::useShinyjs(),
                           downloadButton(ns("download_llm_interpretation_prompt"),
                                          "Download",
                                          class = "btn-primary",
                                          style = "background-color: #d83428; color: white;")
                         ),
                         tabPanel(
                           title = "R object",
                           verbatimTextOutput(ns("llm_interpreted_functional_module_object")),
                           br(),
                           shinyjs::useShinyjs(),
                           downloadButton(ns("download_llm_interpreted_functional_module_object"),
                                          "Download",
                                          class = "btn-primary",
                                          style = "background-color: #d83428; color: white;")
                         )
                       )
                  )
                )))
   )
}

#' LLM Interpretation Module Server
#'
#' Server-side logic for the LLM interpretation module. Handles file uploads,
#' directory selection, asynchronous LLM processing, and result display.
#' Performs functional module interpretation using large language models
#' with literature search and embedding generation capabilities.
#'
#' @param id Character string. The module's namespace ID.
#' @param enriched_functional_module Reactive value containing the enriched
#'   functional module data object to be interpreted.
#' @param tab_switch Function to switch between application tabs.
#'
#' @return Server function that manages:
#'   - File upload and validation for .rda files
#'   - Directory selection for corpus and embedding storage
#'   - Asynchronous LLM interpretation using future/promises
#'   - Dynamic UI updates for module selection and results display
#'   - Download handlers for results, prompts, and R objects
#'   - Navigation to next analysis step
#'
#' @import shiny
#' @importFrom shinyjs disable enable useShinyjs
#' @importFrom future plan multisession
#' @importFrom promises %...>% %...!% future_promise
#' @importFrom markdown markdownToHTML
#' @importFrom mapa llm_interpret_module
#' @noRd

llm_interpretation_server <- function(id, enriched_functional_module, tab_switch) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$current_wd <- renderText({
        getwd()
      })
      
      observeEvent(input$llm_api_provider, {
        
        # Define embedding model choices for each API provider
        embedding_choices <- switch(input$llm_api_provider,
                                    "siliconflow" = list(
                                      "Qwen/Qwen3-Embedding-0.6B" = "Qwen/Qwen3-Embedding-0.6B",
                                      "Qwen/Qwen3-Embedding-4B" = "Qwen/Qwen3-Embedding-4B",
                                      "Qwen/Qwen3-Embedding-8B" = "Qwen/Qwen3-Embedding-8B"
                                    ),
                                    "openai" = list(
                                      "text-embedding-3-small" = "text-embedding-3-small",
                                      "text-embedding-3-large" = "text-embedding-3-large",
                                      "text-embedding-ada-002" = "text-embedding-ada-002"
                                    ),
                                    "gemini" = list(
                                      "models/text-embedding-004" = "models/text-embedding-004",
                                      "models/gemini-embedding-001" = "models/gemini-embedding-001"
                                    )
        )
        
        # Define LLM model choices for each API provider
        llm_choices <- switch(input$llm_api_provider,
                              "siliconflow" = list(
                                "Qwen/Qwen3-8B" = "Qwen/Qwen3-8B",
                                "Qwen/Qwen3-14B" = "Qwen/Qwen3-14B",
                                "Qwen/Qwen3-30B-A3B-Thinking-2507" = "Qwen/Qwen3-30B-A3B-Thinking-2507",
                                "Qwen/Qwen3-32B" = "Qwen/Qwen3-32B"
                              ),
                              "openai" = list(
                                "gpt-4o-mini-2024-07-18" = "gpt-4o-mini-2024-07-18"
                              ),
                              "gemini" = list(
                                "models/gemini-1.5-flash" = "models/gemini-1.5-flash",
                                "models/gemini-2.5-flash" = "models/gemini-2.5-flash"
                              )
        )
        
        # Set default selections based on API provider
        default_embedding <- switch(input$llm_api_provider,
                                    "siliconflow" = "Qwen/Qwen3-Embedding-0.6B",
                                    "openai" = "text-embedding-3-small",
                                    "gemini" = "models/text-embedding-004"
        )
        
        default_llm <- switch(input$llm_api_provider,
                              "siliconflow" = "Qwen/Qwen3-8B",
                              "openai" = "gpt-4o-mini-2024-07-18",
                              "gemini" = "models/gemini-1.5-flash"
        )
        
        # Update the embedding model selectizeInput
        updateSelectizeInput(
          session,
          "embedding_model",
          choices = embedding_choices,
          selected = default_embedding
        )
        
        # Update the LLM model selectizeInput
        updateSelectizeInput(
          session,
          "llm_model",
          choices = llm_choices,
          selected = default_llm
        )
      })
      
      ## Load enriched_functional_module.rda and navigate to specified directory
      observeEvent(
        input$upload_enriched_functional_module, {
          if (!is.null(input$upload_enriched_functional_module$datapath)) {
            message("Loading data")
            tempEnv <- new.env()
            load(input$upload_enriched_functional_module$datapath,
                 envir = tempEnv)

            names <- ls(tempEnv)

            if (length(names) == 1) {
              enriched_functional_module(get(names[1], envir = tempEnv))
              # uploaded_enriched_functional_module(get(names[1], envir = tempEnv))
            } else {
              message("The .rda file does not contain exactly one object.")
              # shiny::showModal(
              #   modalDialog(
              #     title = "Error",
              #     "The uploaded file should contain exactly one object.",
              #     easyClose = TRUE,
              #     footer = modalButton("Close")
              #   )
              # )
              shinyalert::shinyalert(
                title = "Invalid file content",
                text = "The uploaded file should contain exactly one object.",
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            }
          }
      })

      ## Display interpreted result
      observeEvent(input$upload_interpretation_result, {
        tmp <- new.env()
        load(input$upload_interpretation_result$datapath, envir = tmp)

        # The .rda created by mapa::llm_interpret_module() should contain exactly
        # ONE object (a named list of modules).  Guard against user mistakes:
        if (length(ls(tmp)) == 1) {
          interpreted_functional_module <- get(ls(tmp), envir = tmp)
          annotation_result(interpreted_functional_module@llm_module_interpretation)
          enriched_functional_module(interpreted_functional_module)

          # showNotification("Module annotation result loaded successfully!",
          #                  type = "message")
          shinyalert::shinyalert(
            title = "Module annotation result loaded",
            html = TRUE,
            type = "success",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          # shiny::showModal(modalDialog(
          #   title   = "Error",
          #   "The uploaded .rda must contain exactly one object
          # (what `mapa::llm_interpret_module()` saves).",
          #   easyClose = TRUE,
          #   footer    = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "Invalid file content",
            text = "The uploaded .rda must contain exactly one object.",
            html = TRUE,
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        }
      })

      observe(
        {
          req(enriched_functional_module())
          
          if (!("merge_modules" %in% names(enriched_functional_module()@process_info))) {
            return()
          }
          
          max_module_content_number <- max(enriched_functional_module()@merged_module$functional_module_result$module_content_number)
          updateNumericInput(
            session,
            inputId = "module_content_number_cutoff",  # No need for ns() here
            label = "Module size cutoff",
            value = 1,
            min = 0,
            max = max_module_content_number - 1
          ) 
        }
      )
      
      ## Get organism annotation database
      query_type <- reactive({
        req(enriched_functional_module())
        tryCatch(
          {
            if (!("merge_modules" %in% names(enriched_functional_module()@process_info))) {
              return()
            }
            enriched_functional_module()@process_info$merge_pathways@parameter$query_type
          },
          error = function(e) {
            shinyalert::shinyalert(
              text = paste("Details:", e$message),
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          }
        )
      })
      
      observe({
        shinyjs::toggleElement(
          id = "gene_panel",
          condition = query_type() == "gene"
        )
      })
      
      ## Get OrgDb Database
      orgdb <- reactiveVal()
      orgdb_text <- reactiveVal()
      
      observe({
        req(query_type())
        req(input$retrieve_orgdb)
        
        tryCatch(
          expr = {
            # Show loading message
            db_load_alert_id <- shinyalert::shinyalert(
              title = paste0("Retrieving organism database from AnnotationHub"),
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
            
            if (query_type() == "gene" & grepl("^AH", input$non_model_ah_id)) {
              
              ah_id <- input$non_model_ah_id
              
              # Check and install AnnotationHub if needed
              if (!requireNamespace("AnnotationHub", quietly = TRUE)) {
                
                install_annotationhub_alert_id <- shinyalert::shinyalert(
                  title = "Installing AnnotationHub package",
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
                  BiocManager::install("AnnotationHub")
                  shinyalert::closeAlert(id = install_annotationhub_alert_id)
                }, error = function(e) {
                  shinyalert::closeAlert(id = install_annotationhub_alert_id)
                  # showNotification(
                  #   paste0("Failed to install AnnotationHub: ", e$message),
                  #   type = "error",
                  #   duration = 10
                  # )
                  shinyalert::shinyalert(
                    title = "Failed to install AnnotationHub",
                    text = e$message,
                    html = TRUE,
                    type = "error",
                    confirmButtonCol = "#dd4b39"
                  )
                  return()
                })
              }
              
              # Retrieve from AnnotationHub
              
              ah <- AnnotationHub::AnnotationHub()
              orgdb(ah[[ah_id]])
              orgdb_text(paste0(ah_id, "_orgdb_object"))
              # Success message
              shinyalert::closeAlert(id = db_load_alert_id)
              shinyalert::shinyalert(
                title = "Organism database retrieval done",
                text = paste0("AnnotationHub ID: ", ah_id),
                html = TRUE,
                type = "success",
                confirmButtonCol = "#dd4b39"
              )
            }
          },
          error = function(e) {
            
            shinyalert::closeAlert(id = db_load_alert_id)
            shinyalert::shinyalert(
              title = "Database Loading Error",
              text = div(
                p("An error occurred while loading the organism database:"),
                tags$code(e$message),
                p("Please check your input and try again.")
              ),
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
            
            return()
          })
      })
      
      observeEvent(input$load_orgdb,
        {
          req(query_type())
          
          tryCatch(
            expr = {
              db_load_alert_id <- shinyalert::shinyalert(
                title = "Loading organism database",
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
              if (query_type() == "gene" & grepl("^org\\.[A-Za-z]+\\..+\\.db$", input$model_orgdb)) {
                
                # Check if package is installed
                if (!requireNamespace(input$model_orgdb, quietly = TRUE)) {
                  shinyalert::closeAlert(id = db_load_alert_id)
                  
                  shinyalert::shinyalert(
                    title = "Missing Package",
                    text = paste0("Package ", input$model_orgdb, " is not installed. Please install it using:\n",
                                  "<code>BiocManager::install('", input$model_orgdb, "')</code>"),
                    html = TRUE,
                    type = "error",
                    confirmButtonCol = "#dd4b39"
                  )
                  return()
                }
                
                # Load the package and get OrgDb object
                requireNamespace(input$model_orgdb)
                db <- get(input$model_orgdb, envir = asNamespace(input$model_orgdb))
                
                orgdb(db)
                orgdb_text(input$model_orgdb)
                shinyalert::closeAlert(id = db_load_alert_id)
                
                shinyalert::shinyalert(
                  text = paste0("Successfully loaded organism database: ", input$model_orgdb),
                  type = "success",
                  confirmButtonCol = "#dd4b39",
                  timer = 2000
                )
              }
            },
            error = function(e) {
              shinyalert::closeAlert(id = db_load_alert_id)
              shinyalert::shinyalert(
                title = "Database Loading Error",
                text = div(
                  p("An error occurred while loading the organism database:"),
                  tags$code(e$message),
                  p("Please check your input and try again.")
                ),
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
              
              return()
            }
          )
        })
      
      observe({
        req(query_type())
        if (query_type() == "metabolite") {
          orgdb("NULL")
          orgdb_text("NULL")
        }
      })
      
      ## Define annotation result as reactive values
      annotation_result <- reactiveVal()
      llm_interpretation_code <- reactiveVal()
      module_prompt <- reactiveVal()

      # Set up the future plan - this determines how parallel tasks will run
      # future::plan(future::multisession)
      future::plan(future::sequential)

      observeEvent(input$submit_llm_interpretation, {
        req(enriched_functional_module())
        
        module_annotation_alert_id <- shinyalert::shinyalert(
          title = "Annotating modules",
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
        
        # validate dir before processing
        embed_path <- trimws(input$embedding_output_dir_path)
        corpus_path <- if(!is.null(input$local_corpus_dir_path) && input$local_corpus_dir_path != "") {
          trimws(input$local_corpus_dir_path)
        } else {
          NULL
        }
        if (is.null(embed_path) || embed_path == "") {
          shinyalert::closeAlert(id = module_annotation_alert_id)
          # shiny::showModal(modalDialog(
          #   title = "Error",
          #   "Please specify an embeddings output directory.",
          #   easyClose = TRUE,
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "Directory not provided",
            text = "An embeddings output directory has not been specified",
            html = TRUE,
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
          return()
        }
        if (!dir.exists(embed_path)) {
          shinyalert::closeAlert(id = module_annotation_alert_id)
          # shiny::showModal(modalDialog(
          #   title = "Error", 
          #   "The specified embeddings output directory does not exist. Please create it first or specify an existing directory.",
          #   easyClose = TRUE,
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "embeddings output directory does not exist",
            text = "The specified embeddings output directory does not exist. Please create it first or specify an existing directory.",
            html = TRUE,
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
          return()
        }
        if (!is.null(corpus_path) && !dir.exists(corpus_path)) {
          shinyalert::closeAlert(id = module_annotation_alert_id)
          # shiny::showModal(modalDialog(
          #   title = "Error",
          #   "The specified local corpus directory does not exist. Please specify an existing directory or leave it empty.",
          #   easyClose = TRUE, 
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "local corpus directory does not exist",
            text = "The specified local corpus directory does not exist. Please specify an existing directory or leave it empty.",
            html = TRUE,
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
          return()
        }
        
        message("Annotating functional modules in progress. This comprehensive analysis requires some time...")

        requireNamespace("future")
        requireNamespace("promises")
        requireNamespace("mapa")
        
        object <- enriched_functional_module()
        annotation_db <- orgdb()
        module_content_number_cutoff <- input$module_content_number_cutoff
        api_provider <- input$llm_api_provider
        llm_model <- input$llm_model
        embedding_model <- input$embedding_model
        api_key <- input$api_key
        
        embedding_output_dir <- normalizePath(embed_path)
        local_corpus_dir <- if(!is.null(corpus_path)) normalizePath(corpus_path) else NULL
        
        phenotype <- if(input$phenotype == "NULL") NULL else input$phenotype
        years <- input$years

        # Show a modal with a spinner to indicate work is happening
        # shiny::showModal(modalDialog(
        #   title = "Analysis in Progress",
        #   "The LLM interpretation is running in the background. Results will appear when ready.",
        #   footer = modalButton("Close"),
        #   easyClose = FALSE,
        #   size = "m"
        # ))
        
        if (is.null(enriched_functional_module())) {
          shinyalert::closeAlert(id = module_annotation_alert_id)
          # removeModal()
          # shiny::showModal(modalDialog(
          #   title = "Warning",
          #   "No enriched functional module data available. Please complete the previous steps or upload the data",
          #   easyClose = TRUE,
          #   footer = modalButton("Close")
          # ))
          shinyalert::shinyalert(
            title = "No enriched functional module data",
            text = "Complete previous steps or upload data.",
            html = TRUE,
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
          return()
        }
        
        promises::future_promise({
          requireNamespace("mapa", quietly = TRUE)
          # This code runs in a separate R process
          result <- mapa::llm_interpret_module(
            object = object,
            module_content_number_cutoff = module_content_number_cutoff,
            api_provider = api_provider,
            llm_model = llm_model,
            embedding_model = embedding_model,
            api_key = api_key,
            embedding_output_dir = embedding_output_dir,
            local_corpus_dir = local_corpus_dir,
            phenotype = phenotype,
            years = years,
            orgdb = annotation_db
          )
        }) |>
          promises::then(
            # Success handler
            function(result) {
              enriched_functional_module(result)
              annotation_result(result@llm_module_interpretation)
              shinyalert::closeAlert(id = module_annotation_alert_id)
              # showNotification("Module annotation completed successfully!", type = "message")
              shinyalert::shinyalert(
                title = "Module annotation completed",
                html = TRUE,
                type = "success",
                confirmButtonCol = "#dd4b39"
              )
            },
            # Error handler
            function(error) {
              shinyalert::closeAlert(id = module_annotation_alert_id)
              # shiny::showModal(modalDialog(
              #   title = "Error",
              #   HTML(paste("An error occurred during module annotation:<br><pre>",
              #              error$message, "</pre>")),
              #   easyClose = TRUE,
              #   footer = modalButton("Close")
              # ))
              shinyalert::shinyalert(
                title = "Annotation failed",
                text = error$message,
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            }
          )
      })

      output$module_details <- renderUI({
        req(annotation_result())

        tagList(
          selectInput(
            inputId = ns("module_selector"),
            label = "Select Functional Module:",
            choices = names(annotation_result()),
            selected = names(annotation_result())[1]
          ),
          hr(),
          h3("Module Information"),
          strong("Module Name:"),
          textOutput(ns("module_name"), container = span),
          br(),
          strong("Module Summary:"),
          textOutput(ns("module_summary"), container = span),
          br(),
          strong("Association With Phenotype:"),
          textOutput(ns("association_summary"), container = span),
          br(),
          strong("Confidence Score:"),
          textOutput(ns("confidence_score"), container = span)
        )
      })

      observeEvent(                          # fire when either changes
        list(annotation_result(), input$module_selector),
        {
          req(annotation_result(), input$module_selector)

          if (input$module_selector %in% names(annotation_result())) {
            module_prompt(
              annotation_result()[[input$module_selector]]$generated_name$prompt
            )
          }
        },
        ignoreInit = TRUE                    # skip the very first (empty) run
      )

      observeEvent(enriched_functional_module(), {
        req(annotation_result())
        req(orgdb_text())
        
        tryCatch({
          interpretation_code <-
            sprintf(
              '
functional_module_annotation <-
  llm_interpret_module(
    object = enriched_functional_module,
    module_content_number_cutoff = %s,
    api_provider = "%s",
    llm_model = "%s",
    embedding_model = "%s",
    api_key = "%s",
    embedding_output_dir = "%s",
    local_corpus_dir = %s,
    phenotype = "%s",
    years = %s,
    orgdb = %s
  )
              ',
              ## wrap character inputs in quotes:
              input$module_content_number_cutoff,
              input$llm_api_provider,
              input$llm_model,
              input$embedding_model,
              input$api_key,
              input$embedding_output_dir_path,
              if (input$local_corpus_dir_path == "") "NULL" else (paste0('"', input$local_corpus_dir_path, '"')),
              input$phenotype,
              input$years,
              orgdb_text()
            )
          llm_interpretation_code(interpretation_code)
          
          # print(input$module_content_number_cutoff)
          # print(input$llm_api_provider)
          # print(input$llm_model)
          # print(input$embedding_model)
          # print(input$api_key)
          # print(input$embedding_output_dir_path)
          # print(if (input$local_corpus_dir_path == "") "NULL" else (paste0('"', input$local_corpus_dir_path, '"')))
          # print(input$phenotype)
          # print(input$years)
          # print(orgdb_text())
          
        }, error = function(e) {
          # Show error alert to user
          shinyalert(
            text = paste("An error occurred while saving the functional module interpretation code:",
                         e$message),
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        })
      })
      
      output$module_name <- renderText({
        req(annotation_result(), input$module_selector)
        tryCatch(
          annotation_result()[[input$module_selector]]$generated_name$module_name,
          error = function(e)
            paste("Error details:", e$message)
        )
      })

      output$module_summary <- renderText({
        req(annotation_result(), input$module_selector)
        tryCatch(
          annotation_result()[[input$module_selector]]$generated_name$summary,
          error = function(e)
            paste("Error details:", e$message)
        )
      })

      output$association_summary <- renderText({
        req(annotation_result(), input$module_selector)
        tryCatch(
          annotation_result()[[input$module_selector]]$generated_name$phenotype_analysis,
          error = function(e)
            paste("Error details:", e$message)
        )
      })

      output$confidence_score <- renderText({
        req(annotation_result(), input$module_selector)
        tryCatch(
          annotation_result()[[input$module_selector]]$generated_name$confidence_score,
          error = function(e)
            paste("Error details:", e$message)
        )
      })

      ### Show object
      output$llm_interpreted_functional_module_object <-
        renderText({
          req(enriched_functional_module())
          llm_interpreted_functional_module_obj <- enriched_functional_module()
          captured_output1 <-
            capture.output(llm_interpreted_functional_module_obj,
                           type = "message")
          captured_output2 <-
            capture.output(llm_interpreted_functional_module_obj,
                           type = "output")
          captured_output <-
            c(captured_output1,
              captured_output2)
          paste(captured_output, collapse = "\n")
        })

      ### Show code
      observeEvent(input$show_llm_interpretation_code, {
        if (is.null(llm_interpretation_code()) ||
            length(llm_interpretation_code()) == 0) {
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
            llm_interpretation_code()
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

      ### Format the prompt for display
      output$prompt <- renderUI({
        req(module_prompt())

        moduleID_display <- tags$div(
          tags$strong("Module ID:"),
          textOutput(ns("module_id"), inline = TRUE)
        )

        all_content <- paste(
          sapply(module_prompt(), function(item) {
            item$content
          }),
          collapse = "\n\n---\n\n"
        )

        content_html <- HTML(markdown::markdownToHTML(
          text = all_content,
          fragment.only = TRUE
        ))

        tagList(
          h3("LLM Prompt Used"),
          moduleID_display,
          br(),
          content_html
        )
      })

      output$module_id <- renderText({
        req(annotation_result(), input$module_selector)
        tryCatch(
          input$module_selector,
          error = function(e)
            paste("Error details:", e$message)
        )
      })

      observe({
        if (is.null(annotation_result()) ||
            length(annotation_result()) == 0) {
          shinyjs::disable("download_llm_interpretation_result")
          shinyjs::disable("download_llm_interpreted_functional_module_object")
        } else {
          shinyjs::enable("download_llm_interpretation_result")
          shinyjs::enable("download_llm_interpreted_functional_module_object")
        }
        if (is.null(module_prompt()) ||
            length(module_prompt()) == 0) {
          shinyjs::disable("download_llm_interpretation_prompt")
        } else {
          shinyjs::enable("download_llm_interpretation_prompt")
        }
      })

      # Download handler for llm interpreted functional modules object
      output$download_llm_interpreted_functional_module_object <-
        shiny::downloadHandler(
          filename = function() {
            "llm_interpreted_functional_module.rda"
          },
          content = function(file) {
            llm_interpreted_functional_module <-
              enriched_functional_module()
            save(llm_interpreted_functional_module, file = file)
          }
        )

      # Download handler for llm interpretation for a specific functional module ====
      output$download_llm_interpretation_result <- downloadHandler(
        filename = function() {
          # e.g. “FM3_interpretation.txt”
          paste0(input$module_selector, "_interpretation.txt")
        },
        content = function(file) {
          req(annotation_result(),
              input$module_selector)

          mod_id   <- input$module_selector
          mod_info <- annotation_result()[[mod_id]]$generated_name

          # Compose the text lines
          txt <- c(
            paste0("Module ID: ",        mod_id),
            paste0("Module Name: ",      mod_info$module_name),
            paste0("Module Summary: ",   mod_info$summary),
            paste0("Association Summary: ", mod_info$phenotype_analysis),
            paste0("Confidence Score: ", mod_info$confidence_score)
          )

          writeLines(txt, file, useBytes = TRUE)
        }
      )


      # Download handler for the prompt
      output$download_llm_interpretation_prompt <- downloadHandler(
        filename = function() {
          "module_annotation_prompt.txt"
        },
        content = function(file) {
          req(module_prompt())

          if (is.list(module_prompt())) {
            text_content <- paste(
              vapply(module_prompt(), function(item) item$content, character(1)),
              collapse = "\n\n---\n\n"
            )
          }

          writeLines(text_content, file, useBytes = TRUE)
        }
      )

      ###Go to go2data_visualization tab
      ####if there is not enriched_functional_module, show a warning message
      observeEvent(input$go2data_visualization, {
        # Check if enriched_functional_module is available
        if ((is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0)) {
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
          # # User never pressed “Submit” in this tab
          # if (is.null(llm_interpreted_functional_module()) ||
          #     length(llm_interpreted_functional_module()) == 0) {
          #
          #   llm_interpreted_functional_module(
          #     enriched_functional_module()
          #   )
          # }

          tab_switch("data_visualization")
        }
      })
    }
  )
}
