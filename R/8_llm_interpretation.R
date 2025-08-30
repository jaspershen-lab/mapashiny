#' LLM Interpretation Module UI
#'
#' Creates the user interface for LLM interpretation of functional modules,
#' including file upload controls, parameter inputs, and result display tabs.
#'
#' @param id Character string. The module's namespace ID.
#'
#' @return A Shiny UI element containing the LLM interpretation interface
#'   with input controls on the left (file uploads, model parameters, API key,
#'   directory selection) and tabbed output panels on the right (interpretation
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
    fluidPage(titlePanel("LLM Interpretation"),
              fluidPage(
                fluidRow(
                ## Input layout ====
                column(
                  4,
                  h4("Step1: Perform LLM Interpretation"),
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
                             selected = "openai")),
                    column(8,
                           textInput(ns("api_key"),
                                     "API key",
                                     value = ""))
                  ),
                  fluidRow(
                    column(
                      6,
                      textInput(ns("llm_model"),
                                "LLM model",
                                value = "gpt-4o-mini-2024-07-18")
                    ),
                    column(
                      6,
                      textInput(ns("embedding_model"),
                                "Embedding model",
                                value = "text-embedding-3-small")
                    )),
                  fluidRow(
                    column(
                      3,
                      numericInput(ns("module_content_number_cutoff"),
                                   "Module size cutoff",
                                   value = 1,
                                   min = 0,
                                   max = 1000)     
                    ),
                    column(
                      3,
                      numericInput(ns("years"),
                                   "Years to search",
                                   value = 5,
                                   min = 1,
                                   max = 1000)
                    ),
                    column(
                      6,
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
                        textInput(ns("non_model_ah_id"),
                                  "AnnotationHub ID",
                                  value = "")
                  )),
                  
                  # fluidRow(
                  #   column(12,
                  #          wellPanel(
                  #            strong("Your current working directory: "),
                  #            verbatimTextOutput(ns("current_wd"), placeholder = TRUE),
                  #            style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 15px;"
                  #          )
                  #   )
                  # ),
                  # helpText(
                  #   "Tip: Current working directory is displayed above. You can use relative paths (e.g., 'output/embeddings') or absolute paths (e.g., '/home/user/project/embeddings')",
                  #   HTML("<br><span style='color: red;'><strong>NOTE: This will clean the folder content at first! Please check the folder before selecting it.</strong></span>")
                  # ),
                  # textInput(ns("embedding_output_dir_path"), 
                  #           "(Required) Embeddings output directory",
                  #           width = "100%", 
                  #           placeholder = paste0("e.g., ", file.path(getwd(), "embeddings")),
                  #           value = ""),
                  fluidRow(
                    column(12,
                           # textInput(ns("local_corpus_dir_path"), 
                           #           "(Optional) Local corpus directory",
                           #           width = "100%", 
                           #           placeholder = "Enter path to directory containing PDF files (optional)",
                           #           value = ""),
                           fileInput(
                             ns("local_corpus_file"),
                             "Choose local corpus PDF Files",
                             accept = c(".pdf", "application/pdf")
                           )
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
                  h4("Step2: Check Interpretation Result"),
                  fluidRow(
                    column(12,
                           fileInput(
                             inputId = ns("upload_interpretation_result"),
                             label   = "Upload interpretation result (.rda)",
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
                           title = "Interpretation results",
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
#' @param temp_dir Reactive value containing the temporary directory path. 
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

llm_interpretation_server <- function(id, enriched_functional_module, temp_dir, tab_switch) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$current_wd <- renderText({
        getwd()
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
              shiny::showModal(
                modalDialog(
                  title = "Error",
                  "The uploaded file should contain exactly one object.",
                  easyClose = TRUE,
                  footer = modalButton("Close")
                )
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

          showNotification("Interpretation result loaded successfully!",
                           type = "message")
        } else {
          shiny::showModal(modalDialog(
            title   = "Error",
            "The uploaded .rda must contain exactly one object
       (what `mapa::llm_interpret_module()` saves).",
            easyClose = TRUE,
            footer    = modalButton("Close")
          ))
        }
      })

      observe(
        {
          req(enriched_functional_module())
          max_module_content_number <- max(enriched_functional_module()@merged_module$functional_module_result$module_content_number)
          updateNumericInput(
            session,
            inputId = "module_content_number_cutoff",  # No need for ns() here
            label = "Cutoff for module content number",
            value = 1,
            min = 0,
            max = max_module_content_number - 1
          ) 
        }
      )
      
      ## Get organism annotation database
      query_type <- reactive({
        req(enriched_functional_module())
        enriched_functional_module()@process_info$merge_pathways@parameter$query_type
      })
      
      observe({
        shinyjs::toggleElement(
          id = "gene_panel",
          condition = query_type() == "gene"
        )
      })
      
      orgdb <- reactiveVal()
      observe({
        req(query_type())
        if (query_type() == "gene") {
          tryCatch({
            if (grepl("^org\\.[A-Za-z]+\\..+\\.db$", input$model_orgdb)) {
              # Show loading message
              showNotification("Loading organism database...", type = "message", duration = 3)
              
              # Check if package is installed
              if (!requireNamespace(input$model_orgdb, quietly = TRUE)) {
                shiny::showModal(modalDialog(
                  title = "Missing Package",
                  paste0("Package ", input$model_orgdb, " is not installed. Please install it using:\n",
                         "BiocManager::install('", input$model_orgdb, "')"),
                  easyClose = TRUE,
                  footer = modalButton("Close")
                ))
                return()
              }
              
              # Load the package and get OrgDb object
              requireNamespace(input$model_orgdb)
              db <- get(input$model_orgdb)
              orgdb(db)
              
              # Success message
              showNotification(
                paste0("Successfully loaded organism database: ", input$model_orgdb),
                type = "message",
                duration = 5
              )
              
            } else if (grepl("^AH", input$non_model_ah_id)) {
              ah_id <- input$non_model_ah_id
              
              # Show loading message
              showNotification("Connecting to AnnotationHub...", type = "message", duration = 3)
              
              # Check and install AnnotationHub if needed
              if (!requireNamespace("AnnotationHub", quietly = TRUE)) {
                showNotification("Installing AnnotationHub package...", type = "message", duration = 5)
                tryCatch({
                  BiocManager::install("AnnotationHub")
                }, error = function(e) {
                  showNotification(
                    paste0("Failed to install AnnotationHub: ", e$message),
                    type = "error",
                    duration = 10
                  )
                  return()
                })
              }
              
              # Retrieve from AnnotationHub
              showNotification(
                paste0("Retrieving organism database from AnnotationHub (ID: ", ah_id, ")..."),
                type = "message",
                duration = 5
              )
              
              ah <- AnnotationHub::AnnotationHub()
              orgdb(ah[[ah_id]])
              
              # Success message
              showNotification(
                paste0("Successfully loaded organism database from AnnotationHub (ID: ", ah_id, ")"),
                type = "message",
                duration = 5
              )
              
            } 
          }, error = function(e) {
            # General error handling
            showNotification(
              paste0("Error loading organism database: ", e$message),
              type = "error",
              duration = 10
            )
            
            # Optional: Show detailed error in modal for debugging
            shiny::showModal(modalDialog(
              title = "Database Loading Error",
              div(
                p("An error occurred while loading the organism database:"),
                tags$code(e$message),
                p("Please check your input and try again.")
              ),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            
            return()
            
          }, warning = function(w) {
            # Handle warnings
            showNotification(
              paste0("Warning: ", w$message),
              type = "warning",
              duration = 8
            )
          })
        } else {
          orgdb("NULL")
        }
      })
      
      ## Upload local corpus ====
      user_temp_corpus_path <- reactiveVal(NULL)
      
      observeEvent(input$local_corpus_file, {
        req(input$local_corpus_file)
        
        local_corpus_valid_files <- input$local_corpus_file[grepl("\\.pdf$", input$local_corpus_file$name, ignore.case = TRUE), ]
        
        if (nrow(local_corpus_valid_files) == 0) {
          showNotification("Please upload only PDF files.", type = "error")
          return()
        }
        
        local_corpus_file_info <- data.frame(
          Name = local_corpus_valid_files$name,
          Size = paste(round(local_corpus_valid_files$size / 1024 / 1024, 2), "MB"),
          Path = local_corpus_valid_files$datapath,
          stringsAsFactors = FALSE
        )
        
        showNotification(
          paste("Successfully uploaded", nrow(local_corpus_valid_files), "PDF file(s)"),
          type = "message"
        )
        
        ## Create a folder for users' uploaded files under user's temporary directory
        uploaded_corpus_path <- file.path(temp_dir(), "uploaded_local_corpus")
        user_temp_corpus_path(uploaded_corpus_path)
        dir.create(user_temp_corpus_path(), recursive = TRUE)
        
        saved_files <- c()
        
        for (i in 1:nrow(local_corpus_file_info)) {
          temp_path <- local_corpus_file_info$Path[i]
          original_name <- local_corpus_file_info$Name[i]
          
          permanent_path <- file.path(user_temp_corpus_path(), original_name)
          
          if (file.copy(temp_path, permanent_path, overwrite = TRUE)) {
            saved_files <- c(saved_files, permanent_path)
          }
        }
        
      })
      
      ## Create embedding output directory ====
      user_embedding_output_dir <- reactiveVal(NULL)
      
      ## Define annotation result as reactive values
      annotation_result <- reactiveVal()
      llm_interpretation_code <- reactiveVal()
      module_prompt <- reactiveVal()

      # Set up the future plan - this determines how parallel tasks will run
      # future::plan(future::multisession)
      future::plan(future::sequential)

      observeEvent(input$submit_llm_interpretation, {
        req(enriched_functional_module())
        
        embedding_output_path <- file.path(temp_dir(), "embedding_output")
        user_embedding_output_dir(embedding_output_path)
        dir.create(user_embedding_output_dir(), recursive = TRUE)
        
        embed_path <- user_embedding_output_dir()
        corpus_path <- if(!is.null(user_temp_corpus_path()) && user_temp_corpus_path() != "") {
          user_temp_corpus_path()
        } else {
          NULL
        }
        
        if (is.null(embed_path) || embed_path == "") {
          shiny::showModal(modalDialog(
            title = "Error",
            "Please specify an embeddings output directory.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        if (!dir.exists(embed_path)) {
          shiny::showModal(modalDialog(
            title = "Error", 
            "The specified embeddings output directory does not exist. Please create it first or specify an existing directory.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        
        if (!is.null(corpus_path) && !dir.exists(corpus_path)) {
          shiny::showModal(modalDialog(
            title = "Error",
            "The specified local corpus directory does not exist. Please specify an existing directory or leave it empty.",
            easyClose = TRUE, 
            footer = modalButton("Close")
          ))
          return()
        }
        
        message("Interpreting functional modules in progress. This comprehensive analysis requires some time...")

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
        #   duration = 5,
        #   size = "m"
        # ))
        
        showNotification("The LLM interpretation is running in the background. Results will appear when ready.", 
                         type = "message", 
                         duration = 5)

        if (is.null(enriched_functional_module())) {
          removeModal()
          shiny::showModal(modalDialog(
            title = "Warning",
            "No enriched functional module data available. Please complete the previous steps or upload the data",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
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
              showNotification("LLM interpretation completed successfully!", type = "message")
            },
            # Error handler
            function(error) {
              removeModal()
              shiny::showModal(modalDialog(
                title = "Error",
                HTML(paste("An error occurred during LLM interpretation:<br><pre>",
                           error$message, "</pre>")),
                easyClose = TRUE,
                footer = modalButton("Close")
              ))
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
        req(orgdb())
        ### Save code
        interpretation_code <-
          functional_module_annotation_code <-
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
            "user_temp_embedding_ouput_dir",
            if (user_temp_corpus_path() == "") NULL else ("user_temp_local_corpus_dir"),
            input$phenotype,
            input$years,
            as.character(substitute(orgdb))
          )
        llm_interpretation_code(interpretation_code)
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
          shiny::showModal(
            modalDialog(
              title = "Warning",
              "No available code",
              easyClose = TRUE,
              footer = modalButton("Close")
            )
          )
        } else{
          code_content <-
            llm_interpretation_code()
          code_content <-
            paste(code_content, collapse = "\n")
          shiny::showModal(modalDialog(
            title = "Code",
            tags$pre(code_content),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
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
          "llm_interpretation_prompt.txt"
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

      # # Define enriched_functional_module as a reactive value
      # llm_interpretation_result <- reactiveVal("")
      #
      # llm_interpretation_code <- reactiveVal()
      #
      # openai_key <- reactiveVal()
      #
      # observeEvent(input$submit_llm_interpretation, {
      #   openai_key1 <-
      #     Sys.getenv("chatgpt_api_key")
      #
      #   openai_key2 <-
      #     input$openai_key
      #
      #   # Check if enriched_modules is available
      #   if (is.null(enriched_functional_module()) ||
      #       length(enriched_functional_module()) == 0) {
      #     shiny::showModal(
      #       modalDialog(
      #         title = "Warning",
      #         "No enriched functional modules data available.",
      #         easyClose = TRUE,
      #         footer = modalButton("Close")
      #       )
      #     )
      #   } else {
      #     if (openai_key1 != "") {
      #       openai_key(openai_key1)
      #     } else{
      #       if (openai_key2 != "") {
      #         openai_key(openai_key2)
      #       } else{
      #         openai_key("")
      #       }
      #     }
      #
      #     if (openai_key() == "") {
      #       shiny::showModal(
      #         modalDialog(
      #           title = "Warning",
      #           "No OpenAI Key provided. No interpretation will be generated.",
      #           easyClose = TRUE,
      #           footer = modalButton("Close")
      #         )
      #       )
      #     } else{
      #       set_chatgpt_api_key(api_key = openai_key())
      #     }
      #
      #     # shinyjs::show("loading")
      #
      #     withProgress(message = 'Analysis in progress...', {
      #       tryCatch({
      #         llm_interpretation_result <-
      #           interpret_pathways(
      #             object = enriched_functional_module(),
      #             p.adjust.cutoff = input$llm_interpretation_p_adjust_cutoff,
      #             disease = input$llm_interpretation_disease,
      #             count.cutoff = input$llm_interpretation_count_cutoff,
      #             top_n = input$llm_interpretation_top_n
      #           )
      #         llm_interpretation_result(llm_interpretation_result)
      #       },
      #       error = function(e) {
      #         shiny::showModal(modalDialog(
      #           title = "Error",
      #           paste("Details:", e$message),
      #           easyClose = TRUE,
      #           footer = modalButton("Close")
      #         ))
      #         llm_interpretation_result("No result")
      #       })
      #     })
      #     # shinyjs::hide("loading")
      #
      #     ##save code
      #     llm_interpretation_code <-
      #       sprintf(
      #         '
      #       llm_interpretation_result <-
      #       interpret_pathways(
      #       object = enriched_functional_module,
      #       p.adjust.cutoff = %s,
      #       disease = %s,
      #       count.cutoff = %s,
      #       top_n = %s)
      #       ',
      #         input$llm_interpretation_p_adjust_cutoff,
      #         paste0('"', input$llm_interpretation_disease, '"'),
      #         input$llm_interpretation_count_cutoff,
      #         input$llm_interpretation_top_n
      #       )
      #
      #     llm_interpretation_code(llm_interpretation_code)
      #   }
      # })
      #
      # output$llm_interpretation_result <-
      #   renderUI({
      #     shiny::HTML(markdown::markdownToHTML(llm_interpretation_result(),
      #                                          fragment.only = TRUE))
      #   })
      #
      #
      # output$llm_enriched_functional_modules1 <-
      #   shiny::renderDataTable({
      #     req(tryCatch(
      #       enriched_functional_module()@merged_module$functional_module_result,
      #       error = function(e)
      #         NULL
      #     ))
      #   },
      #   options = list(pageLength = 10,
      #                  scrollX = TRUE))
      #
      # output$llm_enriched_functional_modules2 <-
      #   shiny::renderDataTable({
      #     req(tryCatch(
      #       enriched_functional_module()@merged_module$result_with_module,
      #       error = function(e)
      #         NULL
      #     ))
      #   },
      #   options = list(pageLength = 10,
      #                  scrollX = TRUE))
      #
      # observe({
      #   if (is.null(llm_interpretation_result()) ||
      #       length(llm_interpretation_result()) == 0 ||
      #       llm_interpretation_result() == "") {
      #     shinyjs::disable("download_llm_interpretation_result")
      #   } else {
      #     shinyjs::enable("download_llm_interpretation_result")
      #
      #   }
      # })
      #
      #

      ###Go to go2data_visualization tab
      ####if there is not enriched_functional_module, show a warning message
      observeEvent(input$go2data_visualization, {
        # Check if enriched_functional_module is available
        if ((is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0)) {
          shiny::showModal(
            modalDialog(
              title = "Warning",
              "No enriched functional modules data available.",
              easyClose = TRUE,
              footer = modalButton("Close")
            )
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
