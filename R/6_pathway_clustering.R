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
      titlePanel("Module Identification"),
      fluidRow(
        column(4,
               fileInput(ns("upload_similarity_result"), 
                         "Upload Similarity Result (.rda)",
                         accept = ".rda"),
               # h4("Step 1: Find Optimal Parameters"),
               # fluidRow(
               #   column(3,
               #          numericInput(ns("cutoff_min"), "Min cutoff", value = 0.2, min = 0, max = 1, step = 0.01)),
               #   column(3,
               #          numericInput(ns("cutoff_max"), "Max cutoff", value = 0.9, min = 0, max = 1, step = 0.01)),
               #   column(6,
               #          numericInput(ns("cutoff_increment"), "Increment", value = 0.05, min = 0.01, max = 0.5, step = 0.01))
               # ),
               # selectInput(
               #   ns("methods_evaluate"),
               #   "Select clustering methods to evaluate:",
               #   choices = c("Hierarchical_ward.D" = "h_ward.D",
               #               "Hierarchical_ward.D2" = "h_ward.D2", 
               #               "Hierarchical_single" = "h_single",
               #               "Hierarchical_complete" = "h_complete",
               #               "Hierarchical_average" = "h_average",
               #               "Hierarchical_mcquitty" = "h_mcquitty",
               #               "Hierarchical_median" = "h_median",
               #               "Hierarchical_centroid" = "h_centroid",
               #               "Binary cut" = "binary_cut",
               #               "Louvain" = "louvain", 
               #               "Walktrap" = "walktrap",
               #               "Infomap" = "infomap",
               #               "Edge betweenness" = "edge_betweenness",
               #               "Fast greedy" = "fast_greedy",
               #               "Label propagation" = "label_prop",
               #               "Leading eigenvector" = "leading_eigen",
               #               "Optimal" = "optimal"
               #              ),
               #   selected = c("h_ward.D2", "binary_cut", "louvain"),
               #   multiple = TRUE
               # ),
               # actionButton(ns("find_optimal"), "Submit", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               # actionButton(ns("show_code_find_optimal"), "Code", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               # br(),br(),
               
               h4("Step 1: Perform Module Identification"),
               numericInput(ns("sim_cutoff"), "Similarity cutoff", value = 0.5, min = 0, max = 1, step = 0.05),
               selectInput(ns("cluster_method"), "Clustering method", 
                           choices = c("Hierarchical_ward.D" = "h_ward.D",
                                       "Hierarchical_ward.D2" = "h_ward.D2", 
                                       "Hierarchical_single" = "h_single",
                                       "Hierarchical_complete" = "h_complete",
                                       "Hierarchical_average" = "h_average",
                                       "Hierarchical_mcquitty" = "h_mcquitty",
                                       "Hierarchical_median" = "h_median",
                                       "Hierarchical_centroid" = "h_centroid",
                                       "Binary cut" = "binary_cut",
                                       "Louvain" = "louvain", 
                                       "Walktrap" = "walktrap",
                                       "Infomap" = "infomap",
                                       "Edge betweenness" = "edge_betweenness",
                                       "Fast greedy" = "fast_greedy",
                                       "Label propagation" = "label_prop",
                                       "Leading eigenvector" = "leading_eigen",
                                       "Optimal" = "optimal"
                                       ),
                           selected = "louvain"),
               
               actionButton(ns("submit_clustering"), "Submit", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               # actionButton(ns("go2llm_interpretation"), "Next", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("show_code_clustering"), "Code", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               br(),br(),
               # Step3: assess clustering quality ui =====
               h4("Step 2: Assess Module Identification Quality"),
               fileInput(ns("upload_clustering_result"), 
                         "Upload module identification result (.rda)",
                         accept = ".rda"),
               actionButton(ns("assess_clustering"), "Submit", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("show_assess_clustering_code"), "Code", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("go2llm_interpretation"), "Next", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               
               style = "border-right: 1px solid #ddd; padding-right: 20px;"
        ),
        column(8,
               tabsetPanel(id = ns("clustering_tabs"),
                           # # Tab 1: For the optimal parameter analysis results
                           # tabPanel("Optimal Parameter Analysis",
                           #          h4("Evaluation Plot"),
                           #          p("This plot shows the performance (Modularity and Silhouette scores) of different clustering methods across various similarity cutoffs. Higher values are generally better."),
                           #          plotOutput(ns("optimal_plot")),
                           #          hr(),
                           #          h4("Best Recommended Combination(s)"),
                           #          p("The table below lists the parameter combination(s) that achieved the highest score for each metric."),
                           #          dataTableOutput(ns("optimal_table"))
                           # ),
                           # Tab 2: For the final clustering results
                           tabPanel("Module identification result",
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
                                        div(class = "scrollable-container",
                                            shiny::plotOutput(ns("enirched_functional_module_plot"), 
                                                              width = "100%", height = "700px")
                                        ),
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
                                                   value = 1,
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
                           ),
                           # Tab 3: For the clustering assessment results ====
                           tabPanel("Quality assessment",
                                    tabsetPanel(
                                      tabPanel(
                                        title = "Module Size",
                                        div(
                                          class = "scrollable-container",
                                          shiny::plotOutput(ns("assess_cluster_size_plot"),
                                                            height = "800px")
                                        ),
                                        br(),
                                        fluidRow(
                                          column(4,
                                                 numericInput(ns("size_plot_width"),
                                                              "Width",
                                                              value = 8, min = 4, max = 30)
                                          ),
                                          column(4,
                                                 numericInput(ns("size_plot_height"),
                                                              "Height",
                                                              value = 6, min = 4, max = 30)
                                          )
                                        ),
                                        downloadButton(ns("download_size_plot"),
                                                       "Download",
                                                       class = "btn-primary",
                                                       style = "background-color: #d83428; color: white;")
                                      ),
                                      tabPanel(
                                        title = "Silhouette Scores",
                                        div(
                                          class = "scrollable-container",
                                          shiny::plotOutput(ns("assess_cluster_evaluation_plot"),
                                                            height = "600px")
                                        ),
                                        br(),
                                        fluidRow(
                                          column(4,
                                                 numericInput(ns("evaluation_plot_width"),
                                                              "Width",
                                                              value = 8, min = 4, max = 30)
                                                 ),
                                          column(4,
                                                 numericInput(ns("evaluation_plot_height"),
                                                              "Height",
                                                              value = 6, min = 4, max = 30)
                                                 )
                                        ),
                                        downloadButton(ns("download_evaluation_plot"),
                                                       "Download",
                                                       class = "btn-primary",
                                                       style = "background-color: #d83428; color: white;")
                                      ),
                                      tabPanel(
                                        title = "Quality Metrics Table",
                                        shiny::dataTableOutput(ns("quality_metrics")),
                                        br(),
                                        shinyjs::useShinyjs(),
                                        downloadButton(ns("download_quality_metrics"),
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
    
    optimal_results <- reactiveVal(NULL)
    
    # --- Server Logic for "Find Optimal Parameters" ----
    eval_results_code <- reactiveVal()
    cutoff_range <- reactive({
      c(input$cutoff_min, input$cutoff_max)
    })
    
    observeEvent(input$find_optimal, {
      req(similarity_result())
      req(cutoff_range())
      
      withProgress(message = 'Determining optimal parameters...', {
        tryCatch({
          # Use the diagnostic function [cite: 11_determine_optimal_clutsers.R]
          eval_results <- mapa::determine_optimal_clusters(object = similarity_result(),
                                                           cutoff_range = cutoff_range(),
                                                           cutoff_increment = input$cutoff_increment,
                                                           methods = input$methods_evaluate)
          
          # Store results in the reactive value
          optimal_results(eval_results)
          
          # Switch the user's view to the results tab
          updateTabsetPanel(session, "clustering_tabs", selected = "Optimal Parameter Analysis")
          
          # showNotification("Optimal parameter analysis complete.", type = "message")
          shinyalert::shinyalert(
            title = "Optimal parameter analysis complete",
            html = TRUE,
            type = "success",
            confirmButtonCol = "#dd4b39"
          )
          
        }, error = function(e) {
          # showModal(modalDialog(title = "Error", paste("Failed to determine parameters:", e$message)))
          shinyalert::shinyalert(
            title = "Failed to determine parameters",
            text = e$message,
            html = TRUE,
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        })
      })
      
      eval_results_code_str <- sprintf(
        '
eval_results <- mapa::determine_optimal_clusters(
  object = similarity_result,
  cutoff_range = c(%s, %s),
  cutoff_increment = %s,
  methods = "%s"
)
        ',
      input$cutoff_min, 
      input$cutoff_max,
      input$cutoff_increment,
      toString(input$methods_evaluate))
      
      eval_results_code(eval_results_code_str)
    })
    
    # Render the plot for optimal parameter analysis
    output$optimal_plot <- renderPlot({
      req(optimal_results())
      optimal_results()$evaluation_plot
    },
    res = 96)
    
    # Render the table for optimal parameter analysis
    output$optimal_table <- renderDataTable({
      req(optimal_results())
      optimal_results()$best_combination
    }, options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE))
    
    ## Show code
    observeEvent(input$show_code_find_optimal, {
      if (is.null(eval_results_code()) ||
          length(eval_results_code()) == 0) {
        # shiny::showModal(
        #   modalDialog(
        #     title = "Warning",
        #     "No available code",
        #     easyClose = TRUE,
        #     footer = modalButton("Close")
        #   )
        # )
        shinyalert::shinyalert(
          title = "No code available",
          html = TRUE,
          type = "warning",
          confirmButtonCol = "#dd4b39"
        )
      } else{
        code_content <-
          eval_results_code()
        code_content <-
          paste(code_content, collapse = "\n")
        # shiny::showModal(modalDialog(
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
    
    # --- Server Logic for "Submit Clustering" ----
    clustering_code <- reactiveVal()
    
    observeEvent(input$submit_clustering, {
      req(similarity_result())
      
      identify_module_alert_id <- shinyalert::shinyalert(
        title = "Identifying modules",
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
        # Use the generic function for final clustering [cite: 11_get_functional_modules.R]
        result <- mapa::get_functional_modules(
          object = similarity_result(),
          sim.cutoff = input$sim_cutoff,
          cluster_method = input$cluster_method,
          save_to_local = FALSE
        )
        enriched_functional_module(result)
        
        # Switch the user's view to the results tab
        updateTabsetPanel(session, "clustering_tabs", selected = "Final Clustering Result")
        
        # showNotification("Clustering complete.", type = "message")
        shinyalert::shinyalert(
          title = "Module identification complete",
          text = "Go to Step2 to assess the module identification quality",
          html = TRUE,
          type = "success",
          confirmButtonCol = "#dd4b39"
        )
        
      }, error = function(e) {
        shinyalert::closeAlert(id = identify_module_alert_id)
        # showModal(modalDialog(title = "Error", paste("Clustering failed:", e$message)))
        shinyalert::shinyalert(
          title = "Module identification failed",
          text = e$message,
          html = TRUE,
          type = "error",
          confirmButtonCol = "#dd4b39"
        )
      })
      
      shinyalert::closeAlert(id = identify_module_alert_id)
      
      clustering_code_str <- sprintf(
        '
enriched_functional_module <- 
  mapa::get_functional_modules(
    object = similarity_result,
    sim.cutoff = %s,
    cluster_method = "%s"
  )
        ',
        input$sim_cutoff, 
        input$cluster_method)
      
      clustering_code(clustering_code_str)
    })
    
    # Show code
    observeEvent(input$show_code_clustering, {
      if (is.null(clustering_code()) ||
          length(clustering_code()) == 0) {
        # shiny::showModal(
        #   modalDialog(
        #     title = "Warning",
        #     "No available code",
        #     easyClose = TRUE,
        #     footer = modalButton("Close")
        #   )
        # )
        shinyalert::shinyalert(
          title = "No code available",
          html = TRUE,
          type = "warning",
          confirmButtonCol = "#dd4b39"
        )
      } else{
        code_content <-
          clustering_code()
        code_content <-
          paste(code_content, collapse = "\n")
        
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
          req(enriched_functional_module())
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
          req(enriched_functional_module())
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
    enirched_functional_module_plot_without_module_legend <- reactiveVal()
    show_fm_module_color_legend <- reactiveVal(TRUE)
    
    observe({
      req(input$generate_enirched_functional_module)
      req(input$enirched_functional_module_plot_degree_cutoff)
      
      # Check if enriched_functional_module is available
      if (is.null(enriched_functional_module()) ||
          length(enriched_functional_module()) == 0) {
        # shiny::showModal(
        #   modalDialog(
        #     title = "Warning",
        #     "No enriched functional modules data available. Please 'Merge modules' first.",
        #     easyClose = TRUE,
        #     footer = modalButton("Close")
        #   )
        # )
        shinyalert::shinyalert(
          title = "No modules data available",
          text = "Do <strong>Module Identification</strong> before <strong>Data Visualization</strong>.",
          html = TRUE,
          type = "warning",
          confirmButtonCol = "#dd4b39"
        )
      } else {
        
        plot_fm_sim_network_id <- shinyalert::shinyalert(
          title = "Generating plot",
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
        
        tryCatch(
          {
            if (sum(enriched_functional_module()@merged_module$functional_module_result$module_content_number > input$enirched_functional_module_plot_degree_cutoff) > 34) {
              show_fm_module_color_legend(FALSE)
            } else {
              show_fm_module_color_legend(TRUE)
            }
            
            plot <-
              mapa::plot_similarity_network(
                object = enriched_functional_module(),
                level = "functional_module",
                degree_cutoff = input$enirched_functional_module_plot_degree_cutoff,
                text = input$enirched_functional_module_plot_text,
                text_all = input$enirched_functional_module_plot_text_all
              ) + ggplot2::theme(aspect.ratio = 1)
            
            if (!show_fm_module_color_legend()) {
              plot_without_module_legend <- 
                plot +
                ggplot2::guides(fill = "none")
              
              enirched_functional_module_plot_without_module_legend(plot_without_module_legend)
            }
            
            enirched_functional_module_plot(plot)
          },
          error = function(e) {
            shinyalert::closeAlert(id = plot_fm_sim_network_id)
            # shiny::showModal(
            #   modalDialog(
            #     title = "Error",
            #     paste("Details:", e$message),
            #     easyClose = TRUE,
            #     footer = modalButton("Close")
            #   )
            # )
            shinyalert::shinyalert(
              text = paste("Details:", e$message),
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          }
        )
        
        shinyalert::closeAlert(id = plot_fm_sim_network_id)
        
        if (!show_fm_module_color_legend()) {
          shinyalert::shinyalert(
            title = "Module color legend hidden in display",
            text = "It will be included when you download the figure.",
            html = TRUE,
            type = "info",
            confirmButtonCol = "#dd4b39"
          )
        }
      }
    })
    
    output$enirched_functional_module_plot <-
      shiny::renderPlot({
        req(tryCatch(
          {
            if (show_fm_module_color_legend()) {
              enirched_functional_module_plot()
            } else {
              enirched_functional_module_plot_without_module_legend()
            }
          },
          error = function(e)
            NULL
        ))
      },
      res = 96)
    
    # --- Server Logic for "Assess Clustering" ----
    assess_clustering_code <- reactiveVal()
    assess_clustering_result <- reactiveVal()
    
    observeEvent(input$upload_clustering_result, {
      if (!is.null(input$upload_clustering_result$datapath)) {
        message("Loading data")
        tempEnv <- new.env()
        load(input$upload_clustering_result$datapath,
             envir = tempEnv)
        
        names <- ls(tempEnv)
        
        if (length(names) == 1) {
          enriched_functional_module(get(names[1], envir = tempEnv))
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
    
    observeEvent(input$assess_clustering, {
      req(enriched_functional_module())
      
      assess_res_alert_id <- shinyalert::shinyalert(
        title = "Assessing module identification results",
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
        # Use the generic function for final clustering [cite: 11_get_functional_modules.R]
        assess_result <- mapa::assess_clustering_quality(
          object = enriched_functional_module()
        )
        
        assess_clustering_result(assess_result)
        
        # Switch the user's view to the results tab
        updateTabsetPanel(session, "clustering_tabs", selected = "Quality assessment")
        
        # showNotification("Clustering quality assessment complete.", type = "message")
        shinyalert::shinyalert(
          title = "Module identification quality assessment complete",
          text = "Switch to <strong>Module Annotation</strong> by clicking <strong style='color: #dd4b39;'>Next</strong>",
          html = TRUE,
          type = "success",
          confirmButtonCol = "#dd4b39"
        )
        
      }, error = function(e) {
        shinyalert::closeAlert(id = assess_res_alert_id)
        # showModal(modalDialog(title = "Error", paste("Clustering quality assessment failed:", e$message)))
        shinyalert::shinyalert(
          title = "Module identification quality assessment failed",
          text = e$message,
          html = TRUE,
          type = "error",
          confirmButtonCol = "#dd4b39"
        )
      })
      
      shinyalert::closeAlert(id = assess_res_alert_id)
      
      assess_clustering_code_str <- sprintf(
        '
assess_clustering_result <- 
  mapa::assess_clustering_quality(
    object = enriched_functional_module
  )
        ')
      
      assess_clustering_code(assess_clustering_code_str)
    })
    
    # Show assessment result
    output$assess_cluster_size_plot <-
      renderPlot({
        req(assess_clustering_result())
        assess_clustering_result()$size_plot
      },
      res = 96)
    
    output$assess_cluster_evaluation_plot <-
      renderPlot({
        req(assess_clustering_result())
        assess_clustering_result()$evaluation_plot
      },
      res = 96)
    
    output$quality_metrics <-
      shiny::renderDataTable({
        req(tryCatch(
          assess_clustering_result()$quality_metrics,
          error = function(e)
            NULL
        ))
      },
      options = list(pageLength = 10,
                     scrollX = TRUE))
    
    # Show code
    observeEvent(input$show_assess_clustering_code, {
      if (is.null(assess_clustering_code()) ||
          length(assess_clustering_code()) == 0) {
        # shiny::showModal(
        #   modalDialog(
        #     title = "Warning",
        #     "No available code",
        #     easyClose = TRUE,
        #     footer = modalButton("Close")
        #   )
        # )
        shinyalert::shinyalert(
          title = "No code available",
          html = TRUE,
          type = "warning",
          confirmButtonCol = "#dd4b39"
        )
      } else{
        code_content <-
          assess_clustering_code()
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
    
    # Download quality assessment
    output$download_quality_metrics <-
      shiny::downloadHandler(
        filename = function() {
          "assess_clustering_quality_metrics.csv"
        },
        content = function(file) {
          req(assess_clustering_result())
          write.csv(
            assess_clustering_result()$quality_metrics,
            file,
            row.names = FALSE
          )
        }
      )
    
    output$download_size_plot <-
      downloadHandler(
        filename = "assess_size_plot.pdf",
        content = function(file) {
          req(assess_clustering_result())
          ggplot2::ggsave(
            file,
            plot = assess_clustering_result()$size_plot,
            width = input$size_plot_width,
            height = input$size_plot_height
          )
        }
      )
    
    output$download_evaluation_plot <-
      downloadHandler(
        filename = "assess_evaluation_plot.pdf",
        content = function(file) {
          req(assess_clustering_result())
          ggplot2::ggsave(
            file,
            plot = assess_clustering_result()$evaluation_plot,
            width = input$evaluation_plot_width,
            height = input$evaluation_plot_height
          )
        }
      )
    
    observe({
      tryCatch(
        expr = {
          if (is.null(assess_clustering_result()) ||
              length(assess_clustering_result()) == 0) {
            shinyjs::disable("download_quality_metrics")
            shinyjs::disable("download_size_plot")
            shinyjs::disable("download_evaluation_plot")
          } else {
            if (length(assess_clustering_result()$quality_metrics) == 0) {
              shinyjs::disable("download_quality_metrics")
              shinyjs::disable("download_size_plot")
              shinyjs::disable("download_evaluation_plot")
            } else {
              shinyjs::enable("download_quality_metrics")
              shinyjs::enable("download_size_plot")
              shinyjs::enable("download_evaluation_plot")
            }
          }
        },
        error = function(e) {
          shinyjs::disable("download_quality_metrics")
          shinyjs::disable("download_size_plot")
          shinyjs::disable("download_evaluation_plot")
        }
      )
    })
    
    # --- Navigation to the next step ---
    observeEvent(input$go2llm_interpretation, {
      if (is.null(enriched_functional_module())) {
        # showModal(modalDialog(title = "Warning", "Please generate functional modules first."))
        shinyalert::shinyalert(
          title = "Do Module Identification before next step.",
          html = TRUE,
          type = "warning",
          confirmButtonCol = "#dd4b39"
        )
      } else {
        tab_switch("llm_interpretation") # [cite: 6_merge_modules.R]
      }
    })
    
  })
}