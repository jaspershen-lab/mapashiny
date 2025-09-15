#' Pathway Similarity UI Module
#'
#' @param id Module id.
#' @import shiny
#' @importFrom shinyjs hidden
#' @noRd
pathway_similarity_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "pathway_similarity",
    fluidPage(
      titlePanel("Pathway Similarity Calculation"),
      fluidRow(
        column(4,
               fileInput(ns("upload_enriched_pathways"), 
                         "Upload Enriched Pathways Result (.rda)",
                         accept = ".rda"),
               # hr(),
               radioButtons(
                 ns("similarity_method"),
                 "Choose similarity method",
                 choices = c(
                   "Biotext embedding" = "embedcluster",
                   "Traditional methods" = "simcluster"
                 ),
                 selected = "embedcluster"
               ),
               # hr(),
               
               # Parameters for SimCluster (from old 5_merge_pathways.R) ====
               shinyjs::hidden(
                 div(
                   id = ns("simcluster_params"),
                   h4("Traditional Pathway Similarity Calculation Parameters"),
                   # Add all the parameter inputs from the old merge_pathways_ui here.
                   
                   ### Query metabolite parameter panel ----
                   shinyjs::hidden(
                     div(
                       id = ns("sim_cluster_parameter_panel_metabolite"),
                       h4("SMPDB Network"),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.hmdb"),
                                  "P-adjust cutoff",
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.hmdb"),
                                  "Metabolite count cutoff",
                                  value = 5,
                                  min = 0,
                                  max = 1000)
                         )
                       ),
                       fluidRow(
                         column(6,
                                selectInput(
                                  ns("measure.method.hmdb"),
                                  "Similarity method",
                                  choices = c("jaccard", "dice", "overlap", "kappa"),
                                  selected = "jaccard")
                         ),
                         column(6,
                                numericInput(
                                  ns("sim.cutoff.hmdb"),
                                  "Similarity cutoff",
                                  value = 0.5,
                                  min = 0,
                                  max = 1)
                         )
                       ),
                       
                       h4("KEGG Network"),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.metkegg"),
                                  "P-adjust cutoff",
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.metkegg"),
                                  "Metabolite count cutoff",
                                  value = 5,
                                  min = 0,
                                  max = 1000)
                         )
                       ),
                       fluidRow(
                         column(6,
                                selectInput(
                                  ns("measure.method.metkegg"),
                                  "Similarity method",
                                  choices = c("jaccard", "dice", "overlap", "kappa"),
                                  selected = "jaccard")
                         ),
                         column(6,
                                numericInput(
                                  ns("sim.cutoff.metkegg"),
                                  "Similarity cutoff",
                                  value = 0.5,
                                  min = 0,
                                  max = 1)
                         )
                       )
                     )
                   ),
                   
                   ### Query gene parameter panel ----
                   shinyjs::hidden(
                     div(
                       id = ns("sim_cluster_parameter_panel_gene"),
                       
                       checkboxGroupInput(
                         ns("sim_cluster_cluster_module_database"),
                         "Available database",
                         choices = c(
                           "GO" = "go",
                           "KEGG" = "kegg",
                           "Reactome" = "reactome"
                         ),
                         selected = NULL
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("sim_cluster_gene_go_network_panel"),
                       
                       h4("GO Network"),
                       
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.go"),
                                  "P-adjust cutoff",
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.go"),
                                  "Gene count cutoff",
                                  value = 5,
                                  min = 0,
                                  max = 1000
                                )
                         )
                       ),
                       fluidRow(
                         column(6,
                                selectInput(
                                  ns("measure.method.go"),
                                  "Similarity method",
                                  choices = c("Sim_XGraSM_2013", "Sim_Wang_2007", "Sim_Lin_1998",
                                              "Sim_Resnik_1999", "Sim_FaITH_2010", "Sim_Relevance_2006",
                                              "Sim_SimIC_2010", "Sim_EISI_2015", "Sim_AIC_2014",
                                              "Sim_Zhang_2006", "Sim_universal", "Sim_GOGO_2018",
                                              "Sim_Rada_1989", "Sim_Resnik_edge_2005", "Sim_Leocock_1998",
                                              "Sim_WP_1994", "Sim_Slimani_2006", "Sim_Shenoy_2012",
                                              "Sim_Pekar_2002", "Sim_Stojanovic_2001", "Sim_Wang_edge_2012",
                                              "Sim_Zhong_2002", "Sim_AlMubaid_2006", "Sim_Li_2003",
                                              "Sim_RSS_2013", "Sim_HRSS_2013", "Sim_Shen_2010",
                                              "Sim_SSDD_2013", "Sim_Jiang_1997", "Sim_Kappa", "Sim_Jaccard",
                                              "Sim_Dice",  "Sim_Overlap", "Sim_Ancestor"),
                                  selected = "Sim_XGraSM_2013"
                                )
                         ),
                         column(6,
                                numericInput(
                                  ns("sim.cutoff.go"),
                                  "Similarity cutoff",
                                  value = 0.5,
                                  min = 0,
                                  max = 1)
                         )
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("sim_cluster_gene_kegg_network_panel"),
                       
                       h4("KEGG Network"),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.kegg"),
                                  "P-adjust cutoff",
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.kegg"),
                                  "Gene count cutoff",
                                  value = 5,
                                  min = 0,
                                  max = 1000)
                         )
                       ),
                       fluidRow(
                         column(6,
                                selectInput(
                                  ns("measure.method.kegg"),
                                  "Similarity method",
                                  choices = c("jaccard", "dice", "overlap", "kappa"),
                                  selected = "jaccard")
                         ),
                         column(6,
                                numericInput(
                                  ns("sim.cutoff.kegg"),
                                  "Similarity cutoff",
                                  value = 0.5,
                                  min = 0,
                                  max = 1)
                         )
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("sim_cluster_gene_reactome_network_panel"),
                       
                       h4("Reactome Network"),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.reactome"),
                                  "P-adjust cutoff",
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.reactome"),
                                  "Gene count cutoff",
                                  value = 5,
                                  min = 0,
                                  max = 1000)
                         )
                       ),
                       fluidRow(
                         column(6,
                                selectInput(
                                  ns("measure.method.reactome"),
                                  "Similarity method",
                                  choices = c("jaccard", "dice", "overlap", "kappa"),
                                  selected = "jaccard")
                         ),
                         column(6,
                                numericInput(
                                  ns("sim.cutoff.reactome"),
                                  "Similarity cutoff",
                                  value = 0.5,
                                  min = 0,
                                  max = 1)
                         )
                       )
                     )
                   )
                 )
                 #######
               ),
               
               # Parameters for EmbedCluster (from old 5_embed_cluster_pathways.R) =====
               shinyjs::hidden(
                 div(
                   id = ns("embedcluster_params"),
                   h4("Biotext Embedding Parameters"),
                   fluidRow(
                     column(4,
                            selectInput(
                              ns("api_provider"),
                              tags$span(
                                class = "normal-label",
                                "API provider"),
                              choices = c("OpenAI" = "openai", 
                                          "Google" = "gemini",
                                          "SiliconFlow" = "siliconflow"),
                              selected = "siliconflow")
                     ),
                     column(8,
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
                                           selected = "Qwen/Qwen3-Embedding-8B",
                                           options = list(
                                             create = TRUE,
                                             placeholder = "Select or type a model name"
                                           ))
                     )),
                   fluidRow(
                     column(
                       12,
                       textInput(ns("api_key"),
                                 tags$span(
                                   class = "normal-label",
                                   "API key"),
                                 value = "")
                     )
                   ),
                   
                   ### Query metabolite parameter panel ====
                   shinyjs::hidden(
                     div(
                       id = ns("embed_parameter_panel_metabolite"),
                       
                       checkboxGroupInput(
                         ns("embed_cluster_module_database_metabolite"),
                         "Available Database",
                         choices = c(
                           "SMPDB" = "hmdb",
                           "KEGG" = "metkegg"
                         ),
                         selected = NULL
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("embed_metabolite_hmdb_panel"),
                       
                       span(tags$b("SMPDB")),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.hmdb"),
                                  tags$span(
                                    class = "normal-label",
                                    "P-adjust cutoff"),
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.hmdb"),
                                  tags$span(
                                    class = "normal-label",
                                    "Metabolite count cutoff"),
                                  value = 5,
                                  min = 0,
                                  max = 1000)
                         )
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("embed_metabolite_metkegg_panel"),
                       
                       span(tags$b("KEGG")),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.metkegg"),
                                  tags$span(
                                    class = "normal-label",
                                    "P-adjust cutoff"),
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.metkegg"),
                                  tags$span(
                                    class = "normal-label",
                                    "Metabolite count cutoff"),
                                  value = 5,
                                  min = 0,
                                  max = 1000)
                         )
                       )
                     )
                   ),
                   
                   ### Query gene parameter panel ----
                   shinyjs::hidden(
                     div(
                       id = ns("embed_parameter_panel_gene"),
                       
                       checkboxGroupInput(
                         ns("embed_cluster_module_database_gene"),
                         "Available Database",
                         choices = c(
                           "GO" = "go",
                           "KEGG" = "kegg",
                           "Reactome" = "reactome"
                         ),
                         selected = NULL
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("embed_gene_go_panel"),
                       
                       span(tags$b("GO")),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.go"),
                                  tags$span(
                                    class = "normal-label",
                                    "P-adjust cutoff"),
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.go"),
                                  tags$span(
                                    class = "normal-label",
                                    "Gene count cutoff"),
                                  value = 5,
                                  min = 0,
                                  max = 1000
                                )
                         )
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("embed_gene_kegg_panel"),
                       
                       span(tags$b("KEGG")),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.kegg"),
                                  tags$span(
                                    class = "normal-label",
                                    "P-adjust cutoff"),
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.kegg"),
                                  tags$span(
                                    class = "normal-label",
                                    "Gene count cutoff"),
                                  value = 5,
                                  min = 0,
                                  max = 1000
                                )
                         )
                       )
                     )
                   ),
                   
                   shinyjs::hidden(
                     div(
                       id = ns("embed_gene_reactome_panel"),
                       
                       span(tags$b("Reactome")),
                       fluidRow(
                         column(6,
                                numericInput(
                                  ns("p.adjust.cutoff.reactome"),
                                  tags$span(
                                    class = "normal-label",
                                    "P-adjust cutoff"),
                                  value = 0.05,
                                  min = 0,
                                  max = 0.5)
                         ),
                         column(6,
                                numericInput(
                                  ns("count.cutoff.reactome"),
                                  tags$span(
                                    class = "normal-label",
                                    "Gene count cutoff"),
                                  value = 5,
                                  min = 0,
                                  max = 1000
                                )
                         )
                       )
                     )
                   )
                 )
                 #####
               ),
               
               actionButton(ns("submit_similarity"), "Submit", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("go2pathway_clustering"), "Next", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               actionButton(ns("show_code"), "Code", class = "btn-primary", style = "background-color: #d83428; color: white;"),
               
               style = "border-right: 1px solid #ddd; padding-right: 20px;"
        ),
        column(8,
               shinyjs::useShinyjs(),
               uiOutput(ns("dynamic_output_panel"))
        )
      )
    )
  )
}

#' Pathway Similarity Server Module
#'
#' @param id Module id.
#' @param enriched_pathways Reactive input from enrichment step.
#' @param similarity_result Reactive output to be passed to the next step.
#' @param tab_switch Function to switch tabs.
#' @import shiny
#' @importFrom shinyjs toggleElement enable disable
#' @noRd
pathway_similarity_server <- function(id, enriched_pathways, similarity_result, tab_switch) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Server logic to update model choices based on API provider selection
    observeEvent(input$api_provider, {
      
      # Define embedding model choices for each API provider
      embedding_choices <- switch(input$api_provider,
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
      
      # Set default selections based on API provider
      default_embedding <- switch(input$api_provider,
                                  "siliconflow" = "Qwen/Qwen3-Embedding-8B",
                                  "openai" = "text-embedding-3-small",
                                  "gemini" = "models/text-embedding-004"
      )
      
      # Update the embedding model selectizeInput
      updateSelectizeInput(
        session,
        "embedding_model",
        choices = embedding_choices,
        selected = default_embedding
      )
    })
    
    # Toggle parameter panels based on method selection
    observeEvent(input$similarity_method, {
      shinyjs::toggleElement("simcluster_params", condition = input$similarity_method == "simcluster")
      shinyjs::toggleElement("embedcluster_params", condition = input$similarity_method == "embedcluster")
    }, ignoreNULL = FALSE)
    
    go_orgdb <- reactiveVal(NULL)
    observe(
      { req(enriched_pathways$available_db, input$similarity_method)
        
        # print(enriched_pathways$available_db)
        if (enriched_pathways$query_type == "gene" && input$similarity_method == "simcluster") {
          db_choices <- c("GO" = "go", "KEGG" = "kegg", "Reactome" = "reactome")
          updateCheckboxGroupInput(
            session, "embed_cluster_module_database_gene",
            choices  = db_choices,
            selected = enriched_pathways$available_db
          )
          updateCheckboxGroupInput(
            session, "sim_cluster_cluster_module_database",
            choices  = db_choices,
            selected = enriched_pathways$available_db
          )
        } else if (enriched_pathways$query_type == "metabolite" && input$similarity_method == "embedcluster") {
          db_choices <- c("SMPDB" = "hmdb", "KEGG" = "metkegg")
          updateCheckboxGroupInput(
            session, "embed_cluster_module_database_metabolite",
            choices  = db_choices,
            selected = enriched_pathways$available_db
          )
        }
      }
    )
    
    observe({
      req(input$similarity_method, enriched_pathways$query_type)
      
      if (input$similarity_method == "embedcluster") {
        
        # Determine which input to read from
        active_input <- if (enriched_pathways$query_type == "gene") {
          input$embed_cluster_module_database_gene
        } else {
          input$embed_cluster_module_database_metabolite
        }
        
        db_panels <- list(
          "go" = "embed_gene_go_panel",
          "kegg" = "embed_gene_kegg_panel",
          "reactome" = "embed_gene_reactome_panel",
          "hmdb" = "embed_metabolite_hmdb_panel",
          "metkegg" = "embed_metabolite_metkegg_panel"
        )
        
        for (db in names(db_panels)) {
          shinyjs::toggleElement(
            id = db_panels[[db]],
            condition = db %in% active_input # Use the correct input value
          )
        }
      } else {
        db_panels <- list(
          "go" = "sim_cluster_gene_go_network_panel",
          "kegg" = "sim_cluster_gene_kegg_network_panel",
          "reactome" = "sim_cluster_gene_reactome_network_panel"
        )
        
        # Loop through each database-panel pair and toggle accordingly
        for (db in names(db_panels)) {
          shinyjs::toggleElement(
            id = db_panels[[db]],
            condition = db %in% input$sim_cluster_cluster_module_database
          )
        }
        
        if ("go" %in% input$sim_cluster_cluster_module_database) {
          go_orgdb(enriched_pathways$organism)
        }
      }
    })
    
    # Logic to handle uploaded file (ensure this populates 'enriched_pathways') ====
    observeEvent(input$upload_enriched_pathways, {
      if (!is.null(input$upload_enriched_pathways$datapath)) {
        message("Loading data")
        tempEnv <- new.env()
        load(input$upload_enriched_pathways$datapath,
             envir = tempEnv)
        
        names <- ls(tempEnv)
        
        if (length(names) == 1) {
          enriched_pathways$enriched_pathways_res <- get(names[1], envir = tempEnv)
          if ("enrich_pathway" %in% names(enriched_pathways$enriched_pathways_res@process_info)) {
            enriched_pathways$query_type <- enriched_pathways$enriched_pathways_res@process_info$enrich_pathway@parameter$query_type
            enriched_pathways$available_db <- enriched_pathways$enriched_pathways_res@process_info$enrich_pathway@parameter$database
            if (enriched_pathways$query_type == "gene") {
              enriched_pathways$organism <- enriched_pathways$enriched_pathways_res@process_info$enrich_pathway@parameter$go.orgdb
            } else if (enriched_pathways$query_type == "metabolite") {
              enriched_pathways$organism <- enriched_pathways$enriched_pathways_res@process_info$enrich_pathway@parameter$met_organism
            }
          } else if ("do_gsea" %in% names(enriched_pathways$enriched_pathways_res@process_info)) {
            enriched_pathways$query_type <- enriched_pathways$enriched_pathways_res@process_info$do_gsea@parameter$query_type
            enriched_pathways$available_db <- enriched_pathways$enriched_pathways_res@process_info$do_gsea@parameter$database
            if (enriched_pathways$query_type == "gene") {
              enriched_pathways$organism <- enriched_pathways$enriched_pathways_res@process_info$do_gsea@parameter$go.orgdb
            } else if (enriched_pathways$query_type == "metabolite") {
              enriched_pathways$organism <- enriched_pathways$enriched_pathways_res@process_info$do_gsea@parameter$met_organism
            }
          }
          
          if (is.null(enriched_pathways$organism)) {
            # shiny::showModal(
            #   modalDialog(
            #     title = "Error",
            #     "Organism information is not available. For gene-cnetric analysis, please provide organism in this way: `enriched_pathways@process_info$enrich_pathway@parameter$go.orgdb <- \"org.Hs.eg.db\"`. For metabolite-centric analysis, please provide organism in this way: `enriched_pathways@process_info$do_gsea@parameter$met_organism <- \"hsa\"`",
            #     easyClose = TRUE,
            #     footer = modalButton("Close")
            #   )
            # )
            shinyalert::shinyalert(
              title = "Organism information missing",
              text = "For genes: set go.orgdb (e.g., 'org.Hs.eg.db'). For metabolites: set met_organism (e.g., 'hsa').",
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          }
          
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
            text = "The uploaded file should contain exactly one object",
            html = TRUE,
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        }
      }
    })
    
    # Main submit logic that populates `similarity_result()` ====
    similarity_code <- reactiveVal()
    
    observeEvent(
      input$submit_similarity,
      {
        if (is.null(enriched_pathways$enriched_pathways_res) || length(enriched_pathways$enriched_pathways_res) == 0) {
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched pathways data available. Please 'Enrich pathways' first.",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched pathways data",
            text = "Please 'Enrich pathways' first.",
            html = TRUE,
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          # shinyjs::show("loading")
          
          shinyalert::shinyalert(
            title = "Calculating pathway similarities",
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
          
          if (input$similarity_method == "simcluster") {
            ## Traditional methods - Calculate pathway similarity ====
            
            tryCatch({
              
              if (!is.null(go_orgdb())) {
                # Validate input format
                if (!grepl("^org\\.[A-Za-z]+\\..+\\.db$", go_orgdb())) {
                  stop("Invalid OrgDb package name. Expected format: org.XX.eg.db")
                }
                # Check if the package is installed
                if (!requireNamespace(go_orgdb(), quietly = TRUE)) {
                  stop(paste("Package", go_orgdb(), "is not installed. Please install it using BiocManager::install('", go_orgdb(), "')"))
                }
                # Load the package
                requireNamespace(go_orgdb())
                # Get the OrgDb object
                org_db_obj <- get(go_orgdb())
              } else {
                org_db_obj <- NULL
              }
              
              result <-
                mapa::merge_pathways(
                  object = enriched_pathways$enriched_pathways_res,
                  database = input$sim_cluster_cluster_module_database,
                  go.orgdb = org_db_obj,
                  p.adjust.cutoff.go = input$p.adjust.cutoff.go,
                  p.adjust.cutoff.kegg = input$p.adjust.cutoff.kegg,
                  p.adjust.cutoff.reactome = input$p.adjust.cutoff.reactome,
                  p.adjust.cutoff.hmdb = input$p.adjust.cutoff.hmdb,
                  p.adjust.cutoff.metkegg = input$p.adjust.cutoff.metkegg,
                  count.cutoff.go = input$count.cutoff.go,
                  count.cutoff.kegg = input$count.cutoff.kegg,
                  count.cutoff.reactome = input$count.cutoff.reactome,
                  count.cutoff.hmdb = input$count.cutoff.hmdb,
                  count.cutoff.metkegg = input$count.cutoff.metkegg,
                  sim.cutoff.go = input$sim.cutoff.go,
                  sim.cutoff.kegg = input$sim.cutoff.kegg,
                  sim.cutoff.reactome = input$sim.cutoff.reactome,
                  sim.cutoff.hmdb = input$sim.cutoff.hmdb,
                  sim.cutoff.metkegg = input$sim.cutoff.metkegg,
                  measure.method.go = input$measure.method.go,
                  measure.method.kegg = input$measure.method.kegg,
                  measure.method.reactome = input$measure.method.reactome,
                  measure.method.hmdb = input$measure.method.hmdb,
                  measure.method.metkegg = input$measure.method.metkegg,
                  path = "result",
                  save_to_local = FALSE
                )
              
              similarity_result(result)
            },
            error = function(e) {
              shinyalert::closeAlert()
              # shiny::showModal(modalDialog(
              #   title = "Error",
              #   paste("Details:", e$message),
              #   easyClose = TRUE,
              #   footer = modalButton("Close")
              # ))
              shinyalert::shinyalert(
                title = "Similarity calculation failed",
                text = e$message,
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            })
            
            ## Save code =====
            if (enriched_pathways$query_type == "gene") {
              
              go_params <- ""
              if ("go" %in% input$sim_cluster_cluster_module_database) {
                go_params <- sprintf(
                  '
    p.adjust.cutoff.go = %s,
    count.cutoff.go = %s,
    sim.cutoff.go = %s,
    measure.method.go = %s,
    go.orgdb = %s,',
                  input$p.adjust.cutoff.go,
                  input$count.cutoff.go,
                  input$sim.cutoff.go,
                  paste0('"', input$measure.method.go, '"'),
                  go_orgdb()
                )}
              
              kegg_params <- ""
              if ("kegg" %in% input$sim_cluster_cluster_module_database) {
                kegg_params <- sprintf(
                  '
    p.adjust.cutoff.kegg = %s,
    count.cutoff.kegg = %s,
    sim.cutoff.kegg = %s,
    measure.method.kegg = %s,',
                  input$p.adjust.cutoff.kegg,
                  input$count.cutoff.kegg,
                  input$sim.cutoff.kegg,
                  paste0('"', input$measure.method.kegg, '"')
                )}
              
              reactome_params <- ""
              if ("reactome" %in% input$sim_cluster_cluster_module_database) {
                reactome_params <- sprintf(
                  '
    p.adjust.cutoff.reactome = %s,
    count.cutoff.reactome = %s,
    sim.cutoff.reactome = %s,
    measure.method.reactome = %s,',
                  input$p.adjust.cutoff.reactome,
                  input$count.cutoff.reactome,
                  input$sim.cutoff.reactome,
                  paste0('"', input$measure.method.reactome, '"')
                )}
              
              db_vector <- paste0('c("', paste(input$sim_cluster_cluster_module_database, collapse = '", "'), '")')
              
              similarity_code_str <- sprintf(
                '
similarity_result <-
  merge_pathways(
    object = enriched_pathways,
    database = %s,%s%s%s
    save_to_local = FALSE
  )
                ',
                db_vector,
                go_params,
                kegg_params,
                reactome_params
              )
              
            } else if (enriched_pathways$query_type == "metabolite") {
              
              similarity_code_str <- sprintf(
                '
similarity_result <-
  merge_pathways(
    object = enriched_pathways,
    database = c("hmdb", "kegg"),
    p.adjust.cutoff.hmdb = %s,
    p.adjust.cutoff.metkegg = %s,
    count.cutoff.hmdb = %s,
    count.cutoff.metkegg = %s,
    sim.cutoff.hmdb = %s,
    sim.cutoff.metkegg = %s,
    measure.method.hmdb = %s,
    measure.method.metkegg = %s
  )
                ',
                input$p.adjust.cutoff.hmdb,
                input$p.adjust.cutoff.metkegg,
                input$count.cutoff.hmdb,
                input$count.cutoff.metkegg,
                input$sim.cutoff.hmdb,
                input$sim.cutoff.metkegg,
                paste0('"', input$measure.method.hmdb, '"'),
                paste0('"', input$measure.method.metkegg, '"')
              )
            }
          } else if (input$similarity_method == "embedcluster") {
            
            selected_db_embed <- if (enriched_pathways$query_type == "gene") {
              input$embed_cluster_module_database_gene
            } else if (enriched_pathways$query_type == "metabolite") {
              input$embed_cluster_module_database_metabolite
            } else {
              NULL # Default to NULL if query type is not set
            }
            
            ## Biotext embedding - Calculate pathway similarity ====
            
            tryCatch({
              bioembed_sim_matrix <-
                mapa::get_bioembedsim(
                  object = enriched_pathways$enriched_pathways_res,
                  api_provider = input$api_provider,
                  text_embedding_model = input$embedding_model,
                  api_key = input$api_key,
                  database = selected_db_embed,
                  p.adjust.cutoff.go = input$p.adjust.cutoff.go,
                  p.adjust.cutoff.kegg = input$p.adjust.cutoff.kegg,
                  p.adjust.cutoff.reactome = input$p.adjust.cutoff.reactome,
                  p.adjust.cutoff.hmdb = input$p.adjust.cutoff.hmdb,
                  p.adjust.cutoff.metkegg = input$p.adjust.cutoff.metkegg,
                  count.cutoff.go = input$count.cutoff.go,
                  count.cutoff.kegg = input$count.cutoff.kegg,
                  count.cutoff.reactome = input$count.cutoff.reactome,
                  count.cutoff.hmdb = input$count.cutoff.hmdb,
                  count.cutoff.metkegg = input$count.cutoff.metkegg,
                  save_to_local = FALSE
                )
              
              similarity_result(bioembed_sim_matrix)
            },
            error = function(e) {
              shinyalert::closeAlert()
              # shiny::showModal(modalDialog(
              #   title = "Error",
              #   paste("Details:", e$message),
              #   easyClose = TRUE,
              #   footer = modalButton("Close")
              # ))
              shinyalert::shinyalert(
                title = "Embedding similarity failed",
                text = e$message,
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            })
            
            ### Save code ====
            if (enriched_pathways$query_type == "gene") {
              
              go_params <- ""
              if ("go" %in% input$embed_cluster_module_database_gene) {
                go_params <- sprintf(
                  '
    p.adjust.cutoff.go = %s,
    count.cutoff.go = %s,',
                  input$p.adjust.cutoff.go,
                  input$count.cutoff.go
                )}
              
              kegg_params <- ""
              if ("kegg" %in% input$embed_cluster_module_database_gene) {
                kegg_params <- sprintf(
                  '
    p.adjust.cutoff.kegg = %s,
    count.cutoff.kegg = %s,',
                  input$p.adjust.cutoff.kegg,
                  input$count.cutoff.kegg
                )}
              
              reactome_params <- ""
              if ("reactome" %in% input$embed_cluster_module_database_gene) {
                reactome_params <- sprintf(
                  '
    p.adjust.cutoff.reactome = %s,
    count.cutoff.reactome = %s',
                  input$p.adjust.cutoff.reactome,
                  input$count.cutoff.reactome
                )}
              
              db_vector <- paste0('c("', paste(input$embed_cluster_module_database_gene, collapse = '", "'), '")')
              
              similarity_code_str <- sprintf(
                '
similarity_result <-
  get_bioembedsim(
    object = enriched_pathways,
    api_provider = "%s",
    text_embedding_model = "%s",
    api_key = "%s",
    database = %s,%s%s%s
    save_to_local = FALSE
  )
                ',
                input$api_provider,
                input$embedding_model,
                input$api_key,
                db_vector,
                go_params,
                kegg_params,
                reactome_params
              )
              
            } else if (enriched_pathways$query_type == "metabolite") {
              hmdb_params <- ""
              if ("hmdb" %in% input$embed_cluster_module_database_metabolite) {
                hmdb_params <- sprintf(
                  '
    p.adjust.cutoff.hmdb = %s,
    count.cutoff.hmdb = %s,',
                  input$p.adjust.cutoff.hmdb,
                  input$count.cutoff.hmdb
                )}
              
              metkegg_params <- ""
              if ("metkegg" %in% input$embed_cluster_module_database_metabolite) {
                metkegg_params <- sprintf(
                  '
    p.adjust.cutoff.metkegg = %s,
    count.cutoff.metkegg = %s,',
                  input$p.adjust.cutoff.metkegg,
                  input$count.cutoff.metkegg
                )}
              
              db_vector <- paste0('c("', paste(input$embed_cluster_module_database_metabolite, collapse = '", "'), '")')
              
              similarity_code_str <- sprintf(
                '
similarity_result <-
  get_bioembedsim(
    object = enriched_pathways,
    api_provider = "%s",
    text_embedding_model = "%s",
    api_key = "%s",
    database = %s,%s%s
    save_to_local = FALSE
  )
                ',
                input$api_provider,
                input$embedding_model,
                input$api_key,
                db_vector,
                hmdb_params,
                metkegg_params
              )
              
            }
          }
          
          similarity_code(similarity_code_str)
          
          shinyalert::closeAlert()
        }
      }
    )
    
    # observeEvent(input$similarity_method, {
    #   shinyjs::toggleElement(
    #     id = "simcluster_tabs",
    #     condition = input$similarity_method == "simcluster"
    #   )
    # })
    
    observe({
      req(enriched_pathways$query_type)
      req(input$similarity_method)
      cat("Query type:", enriched_pathways$query_type, "\n")
      cat("Similarity method:", input$similarity_method, "\n")
      ## For gene + simcluster
      shinyjs::toggleElement(
        id = "sim_cluster_parameter_panel_gene",
        condition = (enriched_pathways$query_type == "gene" && input$similarity_method == "simcluster")
      )
      
      ## For gene + embed
      shinyjs::toggleElement(
        id = "embed_parameter_panel_gene",
        condition = (enriched_pathways$query_type == "gene" && input$similarity_method == "embedcluster")
      )
      
      ## For metabolite + simcluster
      shinyjs::toggleElement(
        id = "sim_cluster_parameter_panel_metabolite",
        condition = (enriched_pathways$query_type == "metabolite" && input$similarity_method == "simcluster")
      )
      
      ## For metabolite + embed
      shinyjs::toggleElement(
        id = "embed_parameter_panel_metabolite",
        condition = (enriched_pathways$query_type == "metabolite" && input$similarity_method == "embedcluster")
      )
    })
    
    # --- DYNAMICALLY RENDER THE OUTPUT UI ----
    output$dynamic_output_panel <- renderUI({
      req(input$similarity_method)
      req(enriched_pathways$query_type)

      cat("Show result for query type:", enriched_pathways$query_type, "\n")

      # UI FOR SIMCLUSTER METHOD =====
      if (input$similarity_method == 'simcluster' && enriched_pathways$query_type == "gene") {
        tabsetPanel(
          ## Table ====
          tabPanel("Table",
                   tabsetPanel(
                     tabPanel(
                       title = "GO",
                       shiny::dataTableOutput(ns("merged_pathway_go")),
                       br(),
                       shinyjs::useShinyjs(),
                       downloadButton(ns("download_merged_pathway_go"),
                                      "Download",
                                      class = "btn-primary",
                                      style = "background-color: #d83428; color: white;")
                         # shinyjs::disabled()
                     ),
                     tabPanel(
                       title = "KEGG",
                       shiny::dataTableOutput(ns("merged_pathway_kegg")),
                       br(),
                       shinyjs::useShinyjs(),
                       downloadButton(ns("download_merged_pathway_kegg"),
                                      "Download",
                                      class = "btn-primary",
                                      style = "background-color: #d83428; color: white;")
                         # shinyjs::disabled()
                     ),
                     tabPanel(
                       title = "Reactome",
                       shiny::dataTableOutput(ns("merged_pathway_reactome")),
                       br(),
                       shinyjs::useShinyjs(),
                       downloadButton(ns("download_merged_pathway_reactome"),
                                      "Download",
                                      class = "btn-primary",
                                      style = "background-color: #d83428; color: white;")
                         # shinyjs::disabled()
                     )
                   )
          ),
          #####
          ## Data visualization ====
          tabPanel(
            title = "Data visualization",
            tabsetPanel(
              tabPanel(
                title = "GO",
                div(class = "scrollable-container",
                    shiny::plotOutput(ns("enirched_module_go_plot"),
                                      width = "100%", height = "700px")
                ),
                br(),
                fluidRow(
                  column(3,
                         actionButton(ns("generate_enirched_module_plot_go"),
                                      "Generate plot",
                                      class = "btn-primary",
                                      style = "background-color: #d83428; color: white;")
                  ),
                  column(3,
                         checkboxInput(ns("enirched_module_plot_text_go"), "Text", FALSE)
                  ),
                  column(3,
                         checkboxInput(ns("enirched_module_plot_text_all_go"), "Text all", FALSE)
                  ),
                  column(3,
                         numericInput(
                           ns("enirched_module_plot_degree_cutoff_go"),
                           "Degree cutoff",
                           value = 1,
                           min = 0,
                           max = 1000)
                  )
                )
              ),
              tabPanel(
                title = "KEGG",
                div(class = "scrollable-container",
                    shiny::plotOutput(ns("enirched_module_kegg_plot"),
                                      width = "100%", height = "700px")
                ),
                br(),
                fluidRow(
                  column(3,
                         actionButton(ns("generate_enirched_module_plot_kegg"),
                                      "Generate plot",
                                      class = "btn-primary",
                                      style = "background-color: #d83428; color: white;")
                  ),
                  column(3,
                         checkboxInput(ns("enirched_module_plot_text_kegg"), "Text", FALSE)
                  ),
                  column(3,
                         checkboxInput(ns("enirched_module_plot_text_all_kegg"), "Text all", FALSE)
                  ),
                  column(3,
                         numericInput(
                           ns("enirched_module_plot_degree_cutoff_kegg"),
                           "Degree cutoff",
                           value = 1,
                           min = 0,
                           max = 1000
                         )
                  )
                )
              ),
              tabPanel(
                title = "Reactome",
                div(class = "scrollable-container",
                    shiny::plotOutput(ns("enirched_module_reactome_plot"),
                                      width = "100%", height = "700px")
                ),
                br(),
                fluidRow(
                  column(3,
                         actionButton(ns("generate_enirched_module_plot_reactome"),
                                      "Generate plot",
                                      class = "btn-primary",
                                      style = "background-color: #d83428; color: white;")
                  ),
                  column(3,
                         checkboxInput(ns("enirched_module_plot_text_reactome"), "Text", FALSE)
                  ),
                  column(3,
                         checkboxInput(ns("enirched_module_plot_text_all_reactome"), "Text all", FALSE)
                  ),
                  column(3,
                         numericInput(
                           ns("enirched_module_plot_degree_cutoff_reactome"),
                           "Degree cutoff",
                           value = 1,
                           min = 0,
                           max = 1000)
                  )
                )
              )
            )
          ),
          #####
          ## R object =====
          tabPanel(
            title = "R object",
            verbatimTextOutput(ns("simcluster_object_output")),
            br(),
            shinyjs::useShinyjs(),
            downloadButton(ns("download_simcluster_object"),
                           label = "Download",
                           class = "btn-primary",
                           style = "background-color: #d83428; color: white;"),
              # shinyjs::disabled(),
            shinyBS::bsPopover(
              id = ns("download_simcluster_object_info"),
              title = "",
              content = "You can download the functional module file for data visualization.",
              placement = "right",
              trigger = "hover",
              options = list(container = "body")
            )
          )
        )
    } 
      else if (input$similarity_method == 'simcluster' && enriched_pathways$query_type == "metabolite") {
      tabsetPanel(
        ## Table ====
        tabPanel("Table",
                 tabsetPanel(
                   tabPanel(
                     title = "SMPDB",
                     shiny::dataTableOutput(ns("merged_pathway_hmdb")),
                     br(),
                     shinyjs::useShinyjs(),
                     downloadButton(ns("download_merged_pathway_hmdb"),
                                    "Download",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;")
                       # shinyjs::disabled()
                   ),
                   tabPanel(
                     title = "KEGG",
                     shiny::dataTableOutput(ns("merged_pathway_metkegg")),
                     br(),
                     shinyjs::useShinyjs(),
                     downloadButton(ns("download_merged_pathway_metkegg"),
                                    "Download",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;")
                       # shinyjs::disabled()
                   )
                 )
        ),
        #####
        ## Data visualization ====
        tabPanel(
          title = "Data visualization",
          tabsetPanel(
            tabPanel(
              title = "SMPDB",
              div(class = "scrollable-container",
                  shiny::plotOutput(ns("enirched_module_hmdb_plot"),
                                    width = "100%", height = "700px")
              ),
              br(),
              fluidRow(
                column(3,
                       actionButton(ns("generate_enirched_module_plot_hmdb"),
                                    "Generate plot",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;")
                ),
                column(3,
                       checkboxInput(ns("enirched_module_plot_text_hmdb"), "Text", FALSE)
                ),
                column(3,
                       checkboxInput(ns("enirched_module_plot_text_all_hmdb"), "Text all", FALSE)
                ),
                column(3,
                       numericInput(
                         ns("enirched_module_plot_degree_cutoff_hmdb"),
                         "Degree cutoff",
                         value = 1,
                         min = 0,
                         max = 1000)
                )
              )
            ),
            tabPanel(
              title = "KEGG",
              div(class = "scrollable-container",
                  shiny::plotOutput(ns("enirched_module_metkegg_plot"),
                                    width = "100%", height = "700px")
              ),
              br(),
              fluidRow(
                column(3,
                       actionButton(ns("generate_enirched_module_plot_metkegg"),
                                    "Generate plot",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;")
                ),
                column(3,
                       checkboxInput(ns("enirched_module_plot_text_metkegg"), "Text", FALSE)
                ),
                column(3,
                       checkboxInput(ns("enirched_module_plot_text_all_metkegg"), "Text all", FALSE)
                ),
                column(3,
                       numericInput(
                         ns("enirched_module_plot_degree_cutoff_metkegg"),
                         "Degree cutoff",
                         value = 1,
                         min = 0,
                         max = 1000
                       )
                )
              )
            )
          )
        ),
        #####
        ## R object =====
        tabPanel(
          title = "R object",
          verbatimTextOutput(ns("simcluster_object_output")),
          br(),
          shinyjs::useShinyjs(),
          downloadButton(ns("download_simcluster_object"),
                         label = "Download",
                         class = "btn-primary",
                         style = "background-color: #d83428; color: white;"),
            # shinyjs::disabled(),
          shinyBS::bsPopover(
            id = ns("download_simcluster_object_info"),
            title = "",
            content = "You can download the functional module file for data visualization.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          )
        )
      )
    } 
      else if (input$similarity_method == 'embedcluster' && length(similarity_result()) != 0 && is.list(similarity_result())) {
      # UI FOR EMBEDCLUSTER METHOD ====
        div(
          class = "well",
          h4("Result"),
          p("The similarity matrix from embedding is generated and ready for download. Due to its potentially large size, a preview is not displayed here."),
          br(),
          downloadButton(ns("download_embedcluster_result"), "Download Result (.rda)", class = "btn-primary", style = "background-color: #d83428; color: white;")
        )
      }
    })
  
  # --- SERVER-SIDE RENDERING FOR SIMCLUSTER UI ----
  # This logic is taken from the old 5_merge_pathways_server.R server function.
  
  # Render tables for SimCluster
  output$merged_pathway_go <-
    shiny::renderDataTable({
      req(tryCatch(
        similarity_result()@merged_pathway_go$module_result,
        error = function(e)
          NULL
      ))
    },
    options = list(pageLength = 10,
                   scrollX = TRUE))
  
  output$merged_pathway_kegg <-
    shiny::renderDataTable({
      req(tryCatch(
        similarity_result()@merged_pathway_kegg$module_result,
        error = function(e)
          NULL
      ))
    },
    options = list(pageLength = 10,
                   scrollX = TRUE))
  
  output$merged_pathway_reactome <-
    shiny::renderDataTable({
      req(tryCatch(
        similarity_result()@merged_pathway_reactome$module_result,
        error = function(e)
          NULL
      ))
    },
    options = list(pageLength = 10,
                   scrollX = TRUE))
  
  ### For metabolite
  output$merged_pathway_hmdb <-
    shiny::renderDataTable({
      req(tryCatch(
        similarity_result()@merged_pathway_hmdb$module_result,
        error = function(e)
          NULL
      ))
    },
    options = list(pageLength = 10,
                   scrollX = TRUE))
  output$merged_pathway_metkegg <-
    shiny::renderDataTable({
      req(tryCatch(
        similarity_result()@merged_pathway_metkegg$module_result,
        error = function(e)
          NULL
      ))
    },
    options = list(pageLength = 10,
                   scrollX = TRUE))
  
  # Render S4 object printout for SimCluster
  output$simcluster_object_output <-
    renderText({
      enriched_modules <- similarity_result()
      captured_output1 <- capture.output(enriched_modules,
                                         type = "message")
      captured_output2 <- capture.output(enriched_modules,
                                         type = "output")
      captured_output <-
        c(captured_output1,
          captured_output2)
      paste(captured_output, collapse = "\n")
    })
  
  # Download handlers for SimCluster tables and the final object [cite: 5_merge_pathways.R]
  output$download_merged_pathway_go <- downloadHandler(
    filename = "merged_pathway_go.csv", 
    content = function(file) { 
      req(similarity_result())
      write.csv(similarity_result()@merged_pathway_go$module_result, file, row.names = FALSE) 
    })
  output$download_merged_pathway_kegg <- downloadHandler(
    filename = "merged_pathway_kegg.csv", 
    content = function(file) { 
      req(similarity_result())
      write.csv(similarity_result()@merged_pathway_kegg$module_result, file, row.names = FALSE) 
    })
  output$download_merged_pathway_reactome <- downloadHandler(
    filename = "merged_pathway_reactome.csv", 
    content = function(file) {
      req(similarity_result())
      write.csv(similarity_result()@merged_pathway_reactome$module_result, file, row.names = FALSE) 
    })
  output$download_merged_pathway_hmdb <- downloadHandler(
    filename = "merged_pathway_hmdb.csv", 
    content = function(file) { 
      req(similarity_result())
      write.csv(similarity_result()@merged_pathway_hmdb$module_result, file, row.names = FALSE) 
    })
  output$download_merged_pathway_metkegg <- downloadHandler(
    filename = "merged_pathway_metkegg.csv", 
    content = function(file) { 
      req(similarity_result())
      write.csv(similarity_result()@merged_pathway_metkegg$module_result, file, row.names = FALSE) 
    })
  
  output$download_simcluster_object <- downloadHandler(
    filename = "sim_cluster_result.rda",
    content = function(file) {
      req(similarity_result())
      sim_cluster_result <- similarity_result()
      save(sim_cluster_result, file = file)
    }
  )
  
  ## Data visualization ====
  ### GO Plot generation logic ====
  enirched_module_go_plot <- reactiveVal()
  enriched_module_go_plot_without_module_legend <- reactiveVal()
  show_go_module_color_legend <- reactiveVal(TRUE)
  
  observe({
    req(input$generate_enirched_module_plot_go)
    req(input$enirched_module_plot_degree_cutoff_go)
    
    # Check if enriched_modules is available
    if (is.null(similarity_result()) ||
        length(similarity_result()) == 0) {
      # shiny::showModal(
      #   modalDialog(
      #     title = "Warning",
      #     "No enriched modules data available. Please 'Merge pathways' first.",
      #     easyClose = TRUE,
      #     footer = modalButton("Close")
      #   )
      # )
      shinyalert::shinyalert(
        title = "No enriched modules data available",
        text = "Calculate pathway similarity before generating plot",
        type = "warning",
        confirmButtonCol = "#dd4b39"
      )
    } else {
      go_plot_alert_id <- shinyalert::shinyalert(
        title = "Generating plot",
        text = tags$div(
          style = "text-align: center;",
          # "This may take several minutes. Please be patient...",
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
          if (sum(similarity_result()@merged_pathway_go$module_result$module_content_number > input$enirched_module_plot_degree_cutoff_go) > 34) {
            show_go_module_color_legend(FALSE)
          } else {
            show_go_module_color_legend(TRUE)
          }
          
          plot <-
            mapa::plot_similarity_network(
              object = similarity_result(),
              level = "module",
              database = "go",
              degree_cutoff = input$enirched_module_plot_degree_cutoff_go,
              text = input$enirched_module_plot_text_go,
              text_all = input$enirched_module_plot_text_all_go
            ) + 
            ggplot2::theme(aspect.ratio = 1)
          
          if (!show_go_module_color_legend()) {
            plot_without_legend <- plot +
              ggplot2::guides(fill = "none")
            enriched_module_go_plot_without_module_legend(plot_without_legend)
          }
          
          enirched_module_go_plot(plot)
        },
        error = function(e) {
          shinyalert::closeAlert(id = go_plot_alert_id)
          # shiny::showModal(
          #   modalDialog(
          #     title = "Error",
          #     paste("Details:", e$message),
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "Plot generation failed",
            text = e$message,
            html = TRUE,
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        }
      )
      
      shinyalert::closeAlert(id = go_plot_alert_id)
      
      if (!show_go_module_color_legend()) {
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
  
  output$enirched_module_go_plot <-
    shiny::renderPlot({
      req(tryCatch(
        {
          if (show_go_module_color_legend()) {
            enirched_module_go_plot()
          } else {
            enriched_module_go_plot_without_module_legend()
          }
        },
        error = function(e) {
          shinyalert::shinyalert(
            text = paste("Details:", e$message),
            type = "error",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
        }
      ))
    })
  
  ### KEGG Plot generation logic ====
  enirched_module_kegg_plot <- reactiveVal()
  enriched_module_kegg_plot_without_module_legend <- reactiveVal()
  show_kegg_module_color_legend <- reactiveVal(TRUE)
  
  observe({
    req(input$generate_enirched_module_plot_kegg)
    req(input$enirched_module_plot_degree_cutoff_kegg)
    
    # Check if similarity_result is available
    if (is.null(similarity_result()) ||
        length(similarity_result()) == 0) {
      # shiny::showModal(
      #   modalDialog(
      #     title = "Warning",
      #     "No enriched modules data available. Please 'Merge pathways' first.",
      #     easyClose = TRUE,
      #     footer = modalButton("Close")
      #   )
      # )
      shinyalert::shinyalert(
        title = "No enriched modules data available",
        text = "Calculate pathway similarity before generating plot",
        type = "warning",
        confirmButtonCol = "#dd4b39"
      )
    } else {
      
      kegg_plot_alert_id <- shinyalert::shinyalert(
        title = "Generating plot",
        text = tags$div(
          style = "text-align: center;",
          # "This may take several minutes. Please be patient...",
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
          if (sum(similarity_result()@merged_pathway_kegg$module_result$module_content_number > input$enirched_module_plot_degree_cutoff_kegg) > 34) {
            show_kegg_module_color_legend(FALSE)
          } else {
            show_kegg_module_color_legend(TRUE)
          }
          
          plot <-
            mapa::plot_similarity_network(
              object = similarity_result(),
              level = "module",
              database = "kegg",
              degree_cutoff = input$enirched_module_plot_degree_cutoff_kegg,
              text = input$enirched_module_plot_text_kegg,
              text_all = input$enirched_module_plot_text_all_kegg
            ) + 
            ggplot2::theme(aspect.ratio = 1)
          
          if (!show_kegg_module_color_legend()) {
            plot_without_legend <- plot +
              ggplot2::guides(fill = "none")
            enriched_module_kegg_plot_without_module_legend(plot_without_legend)
          }
          
          enirched_module_kegg_plot(plot)
        },
        error = function(e) {
          shinyalert::closeAlert(id = kegg_plot_alert_id)
          
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
            type = "error",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
        }
      )
      
      shinyalert::closeAlert(id = kegg_plot_alert_id)
      
      if (!show_kegg_module_color_legend()) {
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
  
  output$enirched_module_kegg_plot <-
    shiny::renderPlot({
      req(tryCatch(
        {
          if (show_go_module_color_legend()) {
            enirched_module_kegg_plot()
          } else {
            enriched_module_kegg_plot_without_module_legend()
          }
        },
        error = function(e) {
          shinyalert::shinyalert(
            text = paste("Details:", e$message),
            type = "error",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
        }
      ))
    })
  
  ### Reactome Plot generation logic ====
  enirched_module_reactome_plot <- reactiveVal()
  enriched_module_reactome_plot_without_module_legend <- reactiveVal()
  show_reactome_module_color_legend <- reactiveVal(TRUE)
  
  observe({
    req(input$generate_enirched_module_plot_reactome)
    req(input$enirched_module_plot_degree_cutoff_reactome)
    
    # Check if similarity_result is available
    if (is.null(similarity_result()) ||
        length(similarity_result()) == 0) {
      # shiny::showModal(
      #   modalDialog(
      #     title = "Warning",
      #     "No enriched modules data available. Please 'Merge pathways' first.",
      #     easyClose = TRUE,
      #     footer = modalButton("Close")
      #   )
      # )
      shinyalert::shinyalert(
        title = "No enriched modules data available",
        text = "Calculate pathway similarity before generating plot",
        type = "warning",
        confirmButtonCol = "#dd4b39"
      )
    } else {
      
      reactome_plot_alert_id <- shinyalert::shinyalert(
        title = "Generating plot",
        text = tags$div(
          style = "text-align: center;",
          # "This may take several minutes. Please be patient...",
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
          if (sum(similarity_result()@merged_pathway_reactome$module_result$module_content_number > input$enirched_module_plot_degree_cutoff_reactome) > 34) {
            show_reactome_module_color_legend(FALSE)
          } else {
            show_reactome_module_color_legend(TRUE)
          }
          
          plot <-
            mapa::plot_similarity_network(
              object = similarity_result(),
              level = "module",
              database = "reactome",
              degree_cutoff = input$enirched_module_plot_degree_cutoff_reactome,
              text = input$enirched_module_plot_text_reactome,
              text_all = input$enirched_module_plot_text_all_reactome
            ) + 
            ggplot2::theme(aspect.ratio = 1)
          
          if (!show_reactome_module_color_legend()) {
            plot_without_legend <- plot +
              ggplot2::guides(fill = "none")
            enriched_module_reactome_plot_without_module_legend(plot_without_legend)
          }
          
          enirched_module_reactome_plot(plot)
        },
        error = function(e) {
          shinyalert::closeAlert(id = reactome_plot_alert_id)
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
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        }
      )
      
      shinyalert::closeAlert(id = reactome_plot_alert_id)
      
      if (!show_reactome_module_color_legend()) {
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
  
  output$enirched_module_reactome_plot <-
    shiny::renderPlot({
      req(tryCatch(
        {
          if (show_reactome_module_color_legend()) {
            enirched_module_reactome_plot()
          } else {
            enriched_module_reactome_plot_without_module_legend()
          }
        },
        error = function(e) {
          shinyalert::shinyalert(
            text = paste("Details:", e$message),
            type = "error",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
        }
      ))
    })
  
  ### SMPDB Plot generation logic ====
  enirched_module_hmdb_plot <- reactiveVal()
  enriched_module_hmdb_plot_without_module_legend <- reactiveVal()
  show_hmdb_module_color_legend <- reactiveVal(TRUE)
  
  observe({
    req(input$generate_enirched_module_plot_hmdb)
    req(input$enirched_module_plot_degree_cutoff_hmdb)
    
    # Check if similarity_result is available
    if (is.null(similarity_result()) ||
        length(similarity_result()) == 0) {
      # shiny::showModal(
      #   modalDialog(
      #     title = "Warning",
      #     "No enriched modules data available. Please 'Merge pathways' first.",
      #     easyClose = TRUE,
      #     footer = modalButton("Close")
      #   )
      # )
      shinyalert::shinyalert(
        title = "No enriched modules data available",
        text = "Calculate pathway similarity before generating plot",
        type = "warning",
        confirmButtonCol = "#dd4b39"
      )
    } else {
      
      smpdb_plot_alert_id <- shinyalert::shinyalert(
        title = "Generating plot",
        text = tags$div(
          style = "text-align: center;",
          # "This may take several minutes. Please be patient...",
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
          if (sum(similarity_result()@merged_pathway_hmdb$module_result$module_content_number > input$enirched_module_plot_degree_cutoff_hmdb) > 34) {
            show_hmdb_module_color_legend(FALSE)
          } else {
            show_hmdb_module_color_legend(TRUE)
          }
          
          plot <- 
            mapa::plot_similarity_network(
              object = similarity_result(),
              level = "module",
              database = "hmdb",
              degree_cutoff = input$enirched_module_plot_degree_cutoff_hmdb,
              text = input$enirched_module_plot_text_hmdb,
              text_all = input$enirched_module_plot_text_all_hmdb
            ) + ggplot2::theme(aspect.ratio = 1)
          
          if (!show_hmdb_module_color_legend()) {
            plot_without_legend <- plot +
              ggplot2::guides(fill = "none")
            enriched_module_hmdb_plot_without_module_legend(plot_without_legend)
          }
          
          enirched_module_hmdb_plot(plot)
        },
        error = function(e) {
          shinyalert::closeAlert(id = smpdb_plot_alert_id)
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
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        }
      )
      
      shinyalert::closeAlert(id = smpdb_plot_alert_id)
      
      if (!show_hmdb_module_color_legend()) {
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
  
  output$enirched_module_hmdb_plot <-
    shiny::renderPlot({
      req(tryCatch(
        {
          if (show_hmdb_module_color_legend()) {
            enirched_module_hmdb_plot()
          } else {
            enriched_module_hmdb_plot_without_module_legend()
          }
        },
        error = function(e) {
          shinyalert::shinyalert(
            text = paste("Details:", e$message),
            type = "error",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
        }
      ))
    })
  
  ### metabolite KEGG Plot generation logic ====
  enirched_module_metkegg_plot <- reactiveVal()
  enriched_module_metkegg_plot_without_module_legend <- reactiveVal()
  show_metkegg_module_color_legend <- reactiveVal(TRUE)
  
  observe({
    req(input$generate_enirched_module_plot_metkegg)
    req(input$enirched_module_plot_degree_cutoff_metkegg)
    
    # Check if similarity_result is available
    if (is.null(similarity_result()) ||
        length(similarity_result()) == 0) {
      # shiny::showModal(
      #   modalDialog(
      #     title = "Warning",
      #     "No enriched modules data available. Please 'Merge pathways' first.",
      #     easyClose = TRUE,
      #     footer = modalButton("Close")
      #   )
      # )
      shinyalert::shinyalert(
        title = "No enriched modules data available",
        text = "Calculate pathway similarity before generating plot",
        type = "warning",
        confirmButtonCol = "#dd4b39"
      )
    } else {
      
      metkegg_plot_alert_id <- shinyalert::shinyalert(
        title = "Generating plot",
        text = tags$div(
          style = "text-align: center;",
          # "This may take several minutes. Please be patient...",
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
          if (sum(similarity_result()@merged_pathway_metkegg$module_result$module_content_number > input$enirched_module_plot_degree_cutoff_metkegg) > 34) {
            show_metkegg_module_color_legend(FALSE)
          } else {
            show_metkegg_module_color_legend(TRUE)
          }
          
          plot <- 
            mapa::plot_similarity_network(
              object = similarity_result(),
              level = "module",
              database = "metkegg",
              degree_cutoff = input$enirched_module_plot_degree_cutoff_metkegg,
              text = input$enirched_module_plot_text_metkegg,
              text_all = input$enirched_module_plot_text_all_metkegg
            ) + ggplot2::theme(aspect.ratio = 1)
          
          if (!show_metkegg_module_color_legend()) {
            plot_without_legend <- plot +
              ggplot2::guides(fill = "none")
            enriched_module_metkegg_plot_without_module_legend(plot_without_legend)
          }
          
          enirched_module_metkegg_plot(plot)
        },
        error = function(e) {
          shinyalert::closeAlert(id = metkegg_plot_alert_id)
          shinyalert::shinyalert(
            text = paste("Details:", e$message),
            type = "error",
            confirmButtonCol = "#dd4b39"
          )
        }
      )
      
      shinyalert::closeAlert(id = metkegg_plot_alert_id)
      
      if (!show_metkegg_module_color_legend()) {
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
  
  output$enirched_module_metkegg_plot <-
    shiny::renderPlot({
      req(tryCatch(
        {
          if (show_metkegg_module_color_legend()) {
            enirched_module_metkegg_plot()
          } else {
            enriched_module_metkegg_plot_without_module_legend()
          }
        },
        error = function(e) {
          shinyalert::shinyalert(
            text = paste("Details:", e$message),
            type = "error",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
        }
      ))
    })
  
  # --- SERVER-SIDE RENDERING FOR EMBEDCLUSTER UI ----
  output$download_embedcluster_result <- downloadHandler(
    filename = "embedcluster_similarity_matrix.rda",
    content = function(file) {
      req(is.list(similarity_result()))
      embedcluster_similarity_matrix <- similarity_result()
      save(embedcluster_similarity_matrix, file = file)
    }
  )
  
  # Manage button states =====
  # Better approach for managing button states
  observe({
    # Disable all download buttons initially
    download_buttons <- c("download_merged_pathway_go", "download_merged_pathway_kegg", 
                          "download_merged_pathway_reactome", "download_merged_pathway_hmdb", 
                          "download_merged_pathway_metkegg", "download_simcluster_object",
                          "download_embedcluster_result")
    
    if (is.null(similarity_result()) || length(similarity_result()) == 0) {
      lapply(download_buttons, shinyjs::disable)
    } else {
      tryCatch({
        # Check each database result and enable/disable accordingly
        if (!is.null(similarity_result()@merged_pathway_go) && 
            length(similarity_result()@merged_pathway_go) > 0 &&
            nrow(similarity_result()@merged_pathway_go$module_result) > 0) {
          shinyjs::enable("download_merged_pathway_go")
        } else {
          shinyjs::disable("download_merged_pathway_go")
        }
        
        if (!is.null(similarity_result()@merged_pathway_kegg) && 
            length(similarity_result()@merged_pathway_kegg) > 0 &&
            nrow(similarity_result()@merged_pathway_kegg$module_result) > 0) {
          shinyjs::enable("download_merged_pathway_kegg")
        } else {
          shinyjs::disable("download_merged_pathway_kegg")
        }
        
        if (!is.null(similarity_result()@merged_pathway_reactome) && 
            length(similarity_result()@merged_pathway_reactome) > 0 &&
            nrow(similarity_result()@merged_pathway_reactome$module_result) > 0) {
          shinyjs::enable("download_merged_pathway_reactome")
        } else {
          shinyjs::disable("download_merged_pathway_reactome")
        }
        
        if (!is.null(similarity_result()@merged_pathway_hmdb) && 
            length(similarity_result()@merged_pathway_hmdb) > 0 &&
            nrow(similarity_result()@merged_pathway_hmdb$module_result) > 0) {
          shinyjs::enable("download_merged_pathway_hmdb")
        } else {
          shinyjs::disable("download_merged_pathway_hmdb")
        }
        
        if (!is.null(similarity_result()@merged_pathway_metkegg) && 
            length(similarity_result()@merged_pathway_metkegg) > 0 &&
            nrow(similarity_result()@merged_pathway_metkegg$module_result) > 0) {
          shinyjs::enable("download_merged_pathway_metkegg")
        } else {
          shinyjs::disable("download_merged_pathway_metkegg")
        }
        
        if (!is.null(similarity_result()) && 
            length(similarity_result()) > 0) {
          # cat("similarity_result is generated!")
          shinyjs::enable("download_simcluster_object")
        } else {
          shinyjs::disable("download_simcluster_object")
        }
        
        # For embedcluster result, we assume it's a list and check its length
        if (is.list(similarity_result()) && 
            length(similarity_result()) > 0) {
          shinyjs::enable("download_embedcluster_result")
        } else {
          shinyjs::disable("download_embedcluster_result")
        }
        
      }, error = function(e) {
        lapply(download_buttons, shinyjs::disable)
      })
    }
  })
  
  # Show code =====
  observeEvent(input$show_code, {
    if (is.null(similarity_code()) ||
        length(similarity_code()) == 0) {
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
        similarity_code()
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
  
  # Navigation to the next step =====
  observeEvent(input$go2pathway_clustering, {
    if (is.null(similarity_result())) {
      # showModal(modalDialog(title = "Warning", "Please calculate similarity first."))
      shinyalert::shinyalert(
        title = "Do Pathway Similarity Calculation before Module Identification.",
        html = TRUE,
        type = "warning",
        confirmButtonCol = "#dd4b39"
      )
    } else {
      tab_switch("pathway_clustering")
    }
  })
  
  })
}
