#' Pathway Clustering UI Module
#'
#' @param id Module id.
#' @import shiny
#' @importFrom shinyjs hidden
#' @noRd
pathway_clustering_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "pathway_clustering",
    fluidPage(
      titlePanel("Pathway Clustering"),
      fluidRow(
        column(4,
               fileInput(ns("upload_similarity_result"), 
                         "Upload Similarity Result (.rda)",
                         accept = ".rda"),
               h4("Step 1: Find Optimal Parameters"),
               numericInput(ns("cutoff_increment"), "Cutoff Increment", value = 0.1, min = 0.01, max = 0.5, step = 0.01),
               actionButton(ns("find_optimal"), "Submit", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("show_code_find_optimal"), "Code", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               br(),br(),
               
               h4("Step 2: Perform Clustering"),
               numericInput(ns("sim_cutoff"), "Similarity Cutoff", value = 0.5, min = 0, max = 1, step = 0.05),
               selectInput(ns("cluster_method"), "Clustering Method", 
                           choices = c("Binary cut" = "binary cut", "Girvan Newman" = "girvan newman", "Hierarchical" = "hierarchical"),
                           selected = "binary cut"),
               
               # This is only for hierarchical clustering
               shinyjs::hidden(
                 div(id = ns("hclust_method_panel"),
                     selectInput(ns("hclust.method"), "Hierarchical Linkage", 
                                 choices = c("ward.D", "ward.D2", "single", "complete", "average"), 
                                 selected = "complete"))
               ),
               
               actionButton(ns("submit_clustering"), "Submit", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("go2llm_interpretation"), "Next", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("show_code_clustering"), "Code", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               
               style = "border-right: 1px solid #ddd; padding-right: 20px;"
        ),
        column(8,
               tabsetPanel(id = ns("clustering_tabs"),
                           # Tab 1: For the optimal parameter analysis results
                           tabPanel("Optimal Parameter Analysis",
                                    h4("Evaluation Plot"),
                                    p("This plot shows the performance (Modularity and Silhouette scores) of different clustering methods across various similarity cutoffs. Higher values are generally better."),
                                    plotOutput(ns("optimal_plot")),
                                    hr(),
                                    h4("Best Recommended Combination(s)"),
                                    p("The table below lists the parameter combination(s) that achieved the highest score for each metric."),
                                    dataTableOutput(ns("optimal_table"))
                           ),
                           # Tab 2: For the final clustering results
                           tabPanel("Final Clustering Result",
                                    tabsetPanel(
                                      tabPanel(
                                        title = "Table",
                                        shiny::dataTableOutput(ns("enriched_functional_modules")),
                                        br(),
                                        shinyjs::useShinyjs(),
                                        downloadButton(ns("download_enriched_functional_modules"),
                                                       "Download",
                                                       class = "btn-primary",
                                                       style = "background-color: #d83428; color: white;")
                                      ),
                                      tabPanel(
                                        title = "Data visualization",
                                        shiny::plotOutput(ns("enirched_functional_module_plot")),
                                        br(),
                                        fluidRow(
                                          column(3,
                                                 actionButton(ns("generate_enirched_functional_module"),
                                                              "Generate plot",
                                                              class = "btn-primary",
                                                              style = "background-color: #d83428; color: white;")
                                          ),
                                          column(3,
                                                 checkboxInput(ns("enirched_functional_module_plot_text"), "Text", FALSE)
                                          ),
                                          column(3,
                                                 checkboxInput(ns("enirched_functional_module_plot_text_all"), "Text all", FALSE)
                                          ),
                                          column(3,
                                                 numericInput(
                                                   ns("enirched_functional_module_plot_degree_cutoff"),
                                                   "Degree cutoff",
                                                   value = 0,
                                                   min = 0,
                                                   max = 1000)
                                          )
                                        )
                                      ),
                                      tabPanel(
                                        title = "R object",
                                        verbatimTextOutput(ns("enriched_functional_module_object")),
                                        br(),
                                        shinyjs::useShinyjs(),
                                        downloadButton(ns("download_enriched_functional_module_object"),
                                                       "Download",
                                                       class = "btn-primary",
                                                       style = "background-color: #d83428; color: white;")
                                      )
                                    )
                           )
               )
        )
      )
    )
  )
}

#' Pathway Clustering Server Module
#'
#' @param id Module id.
#' @param similarity_result Reactive input from the similarity step.
#' @param enriched_functional_module Reactive output to be passed to the next step.
#' @param tab_switch Function to switch tabs.
#' @import shiny
#' @importFrom shinyjs toggleElement
#' @noRd
pathway_clustering_server <- function(id, similarity_result, enriched_functional_module, tab_switch) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$upload_similarity_result, {
      if (!is.null(input$upload_similarity_result$datapath)) {
        message("Loading data")
        tempEnv <- new.env()
        load(input$upload_similarity_result$datapath,
             envir = tempEnv)
        
        names <- ls(tempEnv)
        
        if (length(names) == 1) {
          similarity_result(get(names[1], envir = tempEnv))
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
    
    optimal_results <- reactiveVal(NULL)
    
    observeEvent(input$cluster_method, {
      shinyjs::toggleElement("hclust_method_panel", condition = input$cluster_method == "hierarchical")
    })
    
    # --- Server Logic for "Find Optimal Parameters" ----
    eval_results_code <- reactiveVal()
    
    observeEvent(input$find_optimal, {
      req(similarity_result())
      
      withProgress(message = 'Determining optimal parameters...', {
        tryCatch({
          # Use the diagnostic function [cite: 11_determine_optimal_clutsers.R]
          eval_results <- mapa::determine_optimal_clusters(object = similarity_result(),
                                                           cutoff_increment = input$cutoff_increment)
          
          # Store results in the reactive value
          optimal_results(eval_results)
          
          # Switch the user's view to the results tab
          updateTabsetPanel(session, "clustering_tabs", selected = "Optimal Parameter Analysis")
          
          showNotification("Optimal parameter analysis complete.", type = "message")
          
        }, error = function(e) {
          showModal(modalDialog(title = "Error", paste("Failed to determine parameters:", e$message)))
        })
      })
      
      eval_results_code_str <- sprintf('
                                      eval_results <- mapa::determine_optimal_clusters(object = similarity_result,
                                                                                       cutoff_increment = %s)
                                       ',
                                       input$cutoff_increment)
      eval_results_code(eval_results_code_str)
    })
    
    # Render the plot for optimal parameter analysis
    output$optimal_plot <- renderPlot({
      req(optimal_results())
      optimal_results()$evaluation_plot
    })
    
    # Render the table for optimal parameter analysis
    output$optimal_table <- renderDataTable({
      req(optimal_results())
      optimal_results()$best_combination
    }, options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE))
    
    ## Show code
    observeEvent(input$show_code_find_optimal, {
      if (is.null(eval_results_code()) ||
          length(eval_results_code()) == 0) {
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
          eval_results_code()
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
    
    # --- Server Logic for "Submit Clustering" ----
    clustering_code <- reactiveVal()
    
    observeEvent(input$submit_clustering, {
      req(similarity_result())
      
      withProgress(message = 'Clustering pathways...', {
        tryCatch({
          # Use the generic function for final clustering [cite: 11_get_functional_modules.R]
          result <- mapa::get_functional_modules(
            object = similarity_result(),
            sim.cutoff = input$sim_cutoff,
            cluster_method = input$cluster_method,
            hclust.method = if (input$cluster_method == "hierarchical") input$hclust.method else NULL,
            save_to_local = FALSE
          )
          enriched_functional_module(result)
          
          # Switch the user's view to the results tab
          updateTabsetPanel(session, "clustering_tabs", selected = "Final Clustering Result")
          
          showNotification("Clustering complete.", type = "message")
          
        }, error = function(e) {
          showModal(modalDialog(title = "Error", paste("Clustering failed:", e$message)))
        })
      })
      
      hclust_param <- ""
      if (input$cluster_method == "hierarchical") {
        hclust_param <- sprintf(',
                   hclust.method = "%s"', input$hclust.method)
      }
      
      clustering_code_str <- sprintf(
      'enriched_functional_module <- 
         mapa::get_functional_modules(
           object = similarity_result,
           sim.cutoff = %s,
           cluster_method = "%s",%s
         )
      ',
        input$sim_cutoff, 
        input$cluster_method, 
        hclust_param)
      
      clustering_code(clustering_code_str)
    })
    
    # Show code
    observeEvent(input$show_code_clustering, {
      if (is.null(clustering_code()) ||
          length(clustering_code()) == 0) {
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
          clustering_code()
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
    
    # Render outputs (table, object) and downloading for the final clustered result ====
    output$enriched_functional_module_object <-
      renderText({
        req(enriched_functional_module())
        enriched_functional_module <- enriched_functional_module()
        captured_output1 <-
          capture.output(enriched_functional_module,
                         type = "message")
        captured_output2 <-
          capture.output(enriched_functional_module,
                         type = "output")
        captured_output <-
          c(captured_output1,
            captured_output2)
        paste(captured_output, collapse = "\n")
      })
    
    output$enriched_functional_modules <-
      shiny::renderDataTable({
        req(tryCatch(
          enriched_functional_module()@merged_module$functional_module_result,
          error = function(e)
            NULL
        ))
      },
      options = list(pageLength = 10,
                     scrollX = TRUE))
    
    output$download_enriched_functional_modules <-
      shiny::downloadHandler(
        filename = function() {
          "functional_module_result.csv"
        },
        content = function(file) {
          write.csv(
            enriched_functional_module()@merged_module$functional_module_result,
            file,
            row.names = FALSE
          )
        }
      )
    
    output$download_enriched_functional_module_object <-
      shiny::downloadHandler(
        filename = function() {
          "enriched_functional_module.rda"
        },
        content = function(file) {
          enriched_functional_module_res <- enriched_functional_module()
          save(enriched_functional_module_res, file = file)
        }
      )
    
    observe({
      tryCatch(
        expr = {
          if (is.null(enriched_functional_module()) ||
              length(enriched_functional_module()) == 0) {
            shinyjs::disable("download_enriched_functional_modules")
            shinyjs::disable("download_enriched_functional_module_object")
          } else {
            if (length(enriched_functional_module()@merged_module) == 0) {
              shinyjs::disable("download_enriched_functional_modules")
              shinyjs::disable("download_enriched_functional_module_object")
            } else {
              shinyjs::enable("download_enriched_functional_modules")
              shinyjs::enable("download_enriched_functional_module_object")
            }
          }
        },
        error = function(e) {
          shinyjs::disable("download_enriched_functional_modules")
          shinyjs::disable("download_enriched_functional_module_object")
        }
      )
    })
    
    # Data visualization ====
    ###define enirched_functional_module_plot
    enirched_functional_module_plot <- reactiveVal()
    observeEvent(input$generate_enirched_functional_module, {
      # Check if enriched_functional_module is available
      if (is.null(enriched_functional_module()) ||
          length(enriched_functional_module()) == 0) {
        shiny::showModal(
          modalDialog(
            title = "Warning",
            "No enriched functional modules data available. Please 'Merge modules' first.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      } else {
        withProgress(message = 'Analysis in progress...', {
          tryCatch(
            {
              plot <-
                mapa::plot_similarity_network(
                  object = enriched_functional_module(),
                  level = "functional_module",
                  degree_cutoff = input$enirched_functional_module_plot_degree_cutoff,
                  text = input$enirched_functional_module_plot_text,
                  text_all = input$enirched_functional_module_plot_text_all
                )
              
              enirched_functional_module_plot(plot)
            },
            error = function(e) {
              shiny::showModal(
                modalDialog(
                  title = "Error",
                  paste("Details:", e$message),
                  easyClose = TRUE,
                  footer = modalButton("Close")
                )
              )
            }
          )
        })
      }
    })
    
    output$enirched_functional_module_plot <-
      shiny::renderPlot({
        req(tryCatch(
          enirched_functional_module_plot(),
          error = function(e)
            NULL
        ))
      })
    
    
    # --- Navigation to the next step ---
    observeEvent(input$go2llm_interpretation, {
      if (is.null(enriched_functional_module())) {
        showModal(modalDialog(title = "Warning", "Please generate functional modules first."))
      } else {
        tab_switch("llm_interpretation") # [cite: 6_merge_modules.R]
      }
    })
    
  })
}