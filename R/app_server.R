#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  tab_switch <- reactiveVal()
  # Observer to handle tab switching
  observe({
    req(tab_switch())
    updateTabItems(session, "tabs", selected = tab_switch())
  })
  
  # Create temp file for intermediate data for each user
  user_temp_dir <- reactiveVal(NULL)
  observe({
    if (length(grep("user", dir("users"))) > 0) {
      idx <-
        max(
          as.numeric(stringr::str_extract(
            grep(pattern = "user", dir("users"), value = TRUE),
            "[0-9]{1,10}"
          )), na.rm = TRUE
        )
      
      if(is.na(idx)) idx <- 0
      
      if (!is.finite(idx)) idx <- 0
      
      user_temp_path <- file.path("users", paste('user', idx + 1, sep = "_"))
    } else{
      user_temp_path <- file.path("users", "user_1")
    }
    
    user_temp_dir(user_temp_path)
    
    dir.create(user_temp_dir(), recursive = TRUE)
  })
  
  # Register cleanup function to delete user's temp directory when session ends
  session$onSessionEnded(function() {
    temp_path <- isolate(user_temp_dir())
    if (!is.null(temp_path) && dir.exists(temp_path)) {
      tryCatch({
        unlink(temp_path, recursive = TRUE, force = TRUE)
        cat("Cleaned up user's temp directory:", temp_path, "\n")
      }, error = function(e) {
        warning("Failed to clean up user's temp directory: ", temp_path, " - ", e$message)
      })
    }
  })
  
  ### Step 1: Upload data ----
  #upload_data_result <- reactive(upload_data_server("upload_data_tab"))
  processed_info <- reactiveValues(
    variable_info = NULL,
    query_type = NULL,
    organism = NULL,
    return_orgdb = FALSE
  )
  upload_data_server("upload_data_tab",
                     processed_info = processed_info,
                     tab_switch)
  
  ### Step 2 enrich pathways ----
  enriched_pathways <- reactiveValues(
    enriched_pathways_res = NULL,
    query_type = NULL,
    organism = NULL,
    available_db = NULL
  )
  enrich_pathway_server("enrich_pathway_tab",
                        processed_info = processed_info,
                        enriched_pathways = enriched_pathways,
                        tab_switch)
  
  ### Step 3: Pathway Similarity ----
  # This reactive value will hold the output of the similarity step.
  # It can be an S4 object (from SimCluster) or a list (from EmbedCluster).
  similarity_result <- reactiveVal(NULL)
  pathway_similarity_server("pathway_similarity_tab",
                            enriched_pathways = enriched_pathways,
                            similarity_result = similarity_result,
                            tab_switch = tab_switch)
  
  ### Step 4: Pathway Clustering ----
  # This reactive value holds the final functional module object.
  enriched_functional_module <- reactiveVal(NULL)
  pathway_clustering_server("pathway_clustering_tab",
                            similarity_result = similarity_result,
                            enriched_functional_module = enriched_functional_module,
                            tab_switch = tab_switch)
  
  ### Step 5 Translation ----
  
  ### Step 6 LLM interpretation ----
  llm_interpretation_server("llm_interpretation_tab",
                            enriched_functional_module = enriched_functional_module,
                            temp_dir = user_temp_dir,
                            tab_switch)
  
  ### Step 7 Data visualization ----
  data_visualization_server("data_visualization_tab",
                            enriched_functional_module = enriched_functional_module,
                            tab_switch)
  
  ### Step 8 Result and report ----
  results_server("results_tab",
                 enriched_functional_module = enriched_functional_module,
                 temp_dir = user_temp_dir,
                 tab_switch)
}
