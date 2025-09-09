#' Data Visualization UI Module
#'
#' Internal UI for data visualization including barplot, module similarity network,
#' module information, and relationship network.
#'
#' @param id Module id.
#' @import shiny
#' @importFrom shinyjs hidden toggleElement useShinyjs
#' @importFrom shinyBS bsButton bsPopover
#' @importFrom shinyWidgets colorPickr
#' @noRd

data_visualization_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "data_visualization",
          fluidPage(
            titlePanel("Data Visualization"),
            tabsetPanel(
              ## tab1: Barplot panel ----
              tabPanel(
                title = "Barplot",
                fluidRow(
                  column(4,
                         br(),
                         fluidRow(
                           column(8,
                                  fileInput(inputId = ns("upload_enriched_functional_module"),
                                            label = "Upload functional module (.rda)",
                                            accept = ".rda")
                                  # bsPopover(
                                  #   id = ns("upload_functional_module_info"),
                                  #   title = "",
                                  #   content = "You can upload the functional module file here for data visualization only.",
                                  #   placement = "right",
                                  #   trigger = "hover",
                                  #   options = list(container = "body")
                                  # )
                           )
                         ),
                         fluidRow(
                           column(6,
                                  selectInput(
                                    ns("barplot_level"),
                                    "Level",
                                    choices = c(
                                      "FM" = "functional_module",
                                      "Module" = "module",
                                      "Pathway" = "pathway"),
                                    selected = "pathway")
                           )
                         ),
                         fluidRow(
                           # ### level selection panel for gene ----
                           # shinyjs::hidden(
                           #   div(
                           #     id = ns("level_panel_for_gene"),
                           #     column(4,
                           #            selectInput(
                           #              inputId = ns("barplot_level_gene"),
                           #              label = "Level",
                           #              choices = c(
                           #                "FM" = "functional_module",
                           #                "Module" = "module",
                           #                "Pathway" = "pathway"),
                           #              selected = "functional_module")
                           #     )
                           #   )
                           # ),
                           # ### level selection panel for metabolite ----
                           # shinyjs::hidden(
                           #   div(
                           #     id = ns("level_panel_for_metabolite"),
                           #     column(4,
                           #            selectInput(
                           #              inputId = ns("barplot_level_metabolite"),
                           #              label = "Level",
                           #              choices = c("Pathway" = "pathway")
                           #            )
                           #            )
                           #   )
                           # ),

                           column(4,
                                  numericInput(
                                    inputId = ns("barplot_top_n"),
                                    label = "Top N",
                                    value = 5,
                                    min = 1,
                                    max = 1000)
                           ),
                           column(4,
                                  selectInput(
                                    ns("line_type"),
                                    "Line type",
                                    choices = c(
                                      "Straight" = "straight",
                                      "Meteor" = "meteor"))
                           ),
                           # column(4,
                           #        checkboxInput(ns("barplot_llm_text"), "LLM text", FALSE)
                           # ),
                           column(
                             4,
                             tags$div(
                               style = "display: flex; flex-direction: column;",
                               tags$label("LLM text", `for` = ns("barplot_llm_text")),
                               checkboxInput(ns("barplot_llm_text"), "", FALSE)
                             )
                           )
                         ),
                         fluidRow(
                           column(6,
                                  numericInput(
                                    ns("barplot_y_lable_width"),
                                    "Y label width",
                                    value = 50,
                                    min = 20,
                                    max = 100)
                           ),
                           column(6,
                                  numericInput(ns("barplot_count_cutoff"),
                                               "Count cutoff",
                                               value = 5,
                                               min = 1,
                                               max = 1000)
                           )
                         ),
                         fluidRow(
                           column(6,
                                  numericInput(ns("barplot_p_adjust_cutoff"),
                                               "P-adjust cutoff",
                                               value = 0.05,
                                               min = 0,
                                               max = 0.5)),
                           column(6,
                                  selectInput(
                                    ns("x_axis_name"),
                                    "X axis name",
                                    choices = NULL
                                  ))
                         ),

                         ### db_color_panel_for_gene ----
                         shinyjs::hidden(
                           div(
                             id = ns("db_color_panel_gene"),
                             fluidRow(
                               column(8,
                                      selectInput(ns("gene_barplot_database"),
                                                         "Database",
                                                         choices = c(
                                                           "GO" = "go",
                                                           "KEGG" = "kegg",
                                                           "Reactome" = "reactome"
                                                         ),
                                                  selected = NULL,
                                                  multiple = TRUE)
                               ) #,
                               # column(4,
                               #        checkboxInput(ns("barplot_translation"), "Translation", FALSE)
                               # )
                             ),
                             h4("Database color"),
                             fluidRow(
                               column(4,
                                      shinyWidgets::colorPickr(
                                        inputId = ns("barplot_go_color"),
                                        label = "GO",
                                        selected = "#eeca40",
                                        theme = "monolith",
                                        width = "100%")
                               ),
                               column(4,
                                      shinyWidgets::colorPickr(
                                        inputId = ns("barplot_kegg_color"),
                                        label = "KEGG",
                                        selected = "#fd7541",
                                        theme = "monolith",
                                        width = "100%")
                               ),
                               column(4,
                                      shinyWidgets::colorPickr(
                                        inputId = ns("barplot_reactome_color"),
                                        label = "Reactome",
                                        selected = "#23b9c7",
                                        theme = "monolith",
                                        width = "100%")
                               )
                             )
                           )
                         ),

                         ### db_color_panel_for_metabolite ----
                         shinyjs::hidden(
                           div(
                             id = ns("db_color_panel_metabolite"),
                             fluidRow(
                               column(8,
                                      selectInput(ns("met_barplot_database"),
                                                  "Database",
                                                  choices = c("HMDB" = "hmdb", "KEGG" = "metkegg"),
                                                  selected = NULL,
                                                  multiple = TRUE)
                               ) #,
                               # column(4,
                               #        checkboxInput(ns("barplot_translation"), "Translation", FALSE)
                               # )
                             ),
                             h4("Database color"),
                             fluidRow(
                               column(4,
                                      shinyWidgets::colorPickr(
                                        inputId = ns("barplot_hmdb_color"),
                                        label = "HMDB",
                                        selected = "#7998ad",
                                        theme = "monolith",
                                        width = "100%")
                               ),
                               column(4,
                                      shinyWidgets::colorPickr(
                                        inputId = ns("barplot_metkegg_color"),
                                        label = "KEGG",
                                        selected = "#fd7541",
                                        theme = "monolith",
                                        width = "100%")
                               )
                             )
                           )
                         ),

                         fluidRow(
                           column(4,
                                  selectInput(ns("barplot_type"), "Type",
                                              choices = c("pdf", "png", "jpeg"))
                           ),
                           column(4,
                                  numericInput(ns("barplot_width"), "Width",
                                               value = 7, min = 4, max = 20)

                           ),
                           column(4,
                                  numericInput(ns("barplot_height"), "Height",
                                               value = 7, min = 4, max = 20)

                           )
                         ),
                         fluidRow(
                           column(12,
                                  actionButton(ns("generate_barplot"),
                                               "Generate plot",
                                               class = "btn-primary",
                                               style = "background-color: #d83428; color: white;"),
                                  downloadButton(ns("download_barplot"),
                                                 "Download",
                                                 class = "btn-primary",
                                                 style = "background-color: #d83428; color: white;")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(12,
                                  actionButton(
                                    ns("go2results_1"),
                                    "Next",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;"),
                                  actionButton(
                                    ns("show_barplot_code"),
                                    "Code",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;")
                           )
                         ),
                         style = "border-right: 1px solid #ddd; padding-right: 20px;"
                  ),
                  column(8,
                         br(),
                         shiny::plotOutput(ns("barplot"))
                  )
                )
              ),

              ## tab2: Module Similarity Network panel ----
              tabPanel(
                title = "Module similarity network",
                fluidRow(
                  column(4,
                         br(),
                         fluidRow(
                           column(6,
                                  selectInput(
                                    ns("module_similarity_network_database"),
                                    "Database",
                                    choices = c("GO" = "go",
                                                "KEGG" = "kegg",
                                                "Reactome" = "reactome"),
                                    selected = "go")
                           ),
                           column(6,
                                  numericInput(
                                    ns("module_similarity_network_degree_cutoff"),
                                    "Degree cutoff",
                                    value = 1,
                                    min = 0,
                                    max = 1000)
                           )
                         ),
                         fluidRow(
                           column(6,
                                  selectInput(
                                    ns("module_similarity_network_level"),
                                    "Level",
                                    choices = c(
                                      "FM" = "functional_module",
                                      "Module" = "module"),
                                    selected = "functional_module")
                           )
                         ),
                         fluidRow(
                           # column(4,
                           #        checkboxInput("module_similarity_network_translation", "Translation", FALSE)
                           # ),
                           column(4,
                                  checkboxInput(ns("module_similarity_network_text"), "Text", FALSE)
                           ),
                           column(4,
                                  checkboxInput(ns("module_similarity_network_llm_text"), "LLM text", FALSE)
                           ),
                           column(4,
                                  checkboxInput(ns("module_similarity_network_text_all"), "Text all", FALSE)
                           )
                         ),
                         fluidRow(
                           column(4,
                                  selectInput(ns("module_similarity_network_type"), "Type",
                                              choices = c("pdf", "png", "jpeg"))
                           ),
                           column(4,
                                  numericInput(ns("module_similarity_network_width"), "Width",
                                               value = 7, min = 4, max = 20)),
                           column(4,
                                  numericInput(ns("module_similarity_network_height"), "Height",
                                               value = 7, min = 4, max = 20))
                         ),
                         fluidRow(
                           column(12,
                                  shinyjs::useShinyjs(),
                                  actionButton(ns("generate_module_similarity_network"),
                                               "Generate plot",
                                               class = "btn-primary",
                                               style = "background-color: #d83428; color: white;"),
                                  shinyjs::useShinyjs(),
                                  downloadButton(ns("download_module_similarity_network"),
                                                 "Download",
                                                 class = "btn-primary",
                                                 style = "background-color: #d83428; color: white;")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(12,
                                  actionButton(
                                    ns("go2results_2"),
                                    "Next",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;"
                                  ),
                                  actionButton(
                                    ns("show_module_similarity_network_code"),
                                    "Code",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;")
                           )
                         ),
                         style = "border-right: 1px solid #ddd; padding-right: 20px;"
                  ),
                  column(8,
                         br(),
                         # shiny::plotOutput(ns("module_similarity_network"))
                         div(class = "scrollable-container",
                             shiny::plotOutput(ns("module_similarity_network"), 
                                        width = "100%", height = "700px")
                         )
                  )
                )
              ),

              ## tab3: Module information panel ----
              tabPanel(
                title = "Module information",
                fluidRow(
                  column(4,
                         br(),
                         fluidRow(
                           column(6,
                                  selectInput(
                                    ns("module_information_level"),
                                    "Level",
                                    choices = c(
                                      "FM" = "functional_module",
                                      "Module" = "module"),
                                    selected = "functional_module"
                                    )
                           ),
                           column(6,
                                  selectInput(
                                    ns("module_information_database"),
                                    "Database",
                                    choices = c("GO" = "go",
                                                "KEGG" = "kegg",
                                                "Reactome" = "reactome"),
                                    selected = "go")
                           )
                         ),
                         fluidRow(
                           column(7,
                                  selectInput(
                                    ns("module_information_module_id"),
                                    "Module ID",
                                    choices = NULL)
                           ),
                           column(
                             4,
                             tags$div(
                               style = "display: flex; flex-direction: column;",
                               tags$label("LLM text", `for` = ns("module_information_llm_text")),
                               checkboxInput(ns("module_information_llm_text"), "", FALSE)
                             )
                           )
                           # column(4,
                           #        checkboxInput(ns("module_information_llm_text"), "LLM text", FALSE)
                           # )
                           # column(5,
                           #        checkboxInput("module_information_translation",
                           #                      "Translation", FALSE)
                           # )
                         ),
                         fluidRow(
                           column(4,
                                  selectInput(ns("module_information_type"), "Type",
                                              choices = c("pdf", "png", "jpeg"))
                           ),
                           column(4,
                                  numericInput(ns("module_information_width"), "Width",
                                               value = 7, min = 4, max = 30)),
                           column(4,
                                  numericInput(ns("module_information_height"), "Height",
                                               value = 21, min = 4, max = 20))
                         ),
                         fluidRow(
                           column(12,
                                  shinyjs::useShinyjs(),
                                  actionButton(ns("generate_module_information"),
                                               "Generate plot",
                                               class = "btn-primary",
                                               style = "background-color: #d83428; color: white;"),
                                  downloadButton(ns("download_module_information"),
                                                 "Download",
                                                 class = "btn-primary",
                                                 style = "background-color: #d83428; color: white;")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(12,
                                  shinyjs::useShinyjs(),
                                  actionButton(
                                    ns("go2results_3"),
                                    "Next",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;"),
                                  actionButton(
                                    ns("show_module_information_code"),
                                    "Code",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;")
                           )
                         ),
                         style = "border-right: 1px solid #ddd; padding-right: 20px;"
                  ),
                  column(8,
                         br(),
                         div(
                           style = "width: 100%; height: 400px; overflow: auto; border: 1px solid #ccc;",
                           shiny::plotOutput(ns("module_information1"),
                                             width = "100%", height = "600px")
                         ),
                         shiny::plotOutput(ns("module_information2")),
                         shiny::plotOutput(ns("module_information3"))
                  )
                )
              ),

              ## tab4: Relationship network panel ----
              tabPanel(
                title = "Relationship network",
                fluidRow(
                  column(4,
                         br(),
                         fluidRow(
                           column(6,
                                  checkboxInput(ns("relationship_network_circular_plot"),
                                                "Circular layout", FALSE)
                           ),
                           # column(4,
                           #        checkboxInput(ns("relationship_network_filter"),
                           #                      "Filter", FALSE)
                           # ),
                           column(6,
                                  checkboxInput(ns("relationship_network_llm_text"), "LLM text", value = TRUE)
                           )
                           # column(4,
                           #        checkboxInput(ns("relationship_network_translation"),
                           #                      "Translation", FALSE)
                           # )
                         ),
                         fluidRow(
                           column(6,
                                  selectInput(
                                    ns("relationship_network_level"),
                                    "Filter Level",
                                    choices = c(
                                      "FM" = "functional_module",
                                      "Module" = "module"),
                                    selected = "functional_module")
                           ),
                           column(6,
                                  selectInput(
                                    ns("relationship_network_module_id"),
                                    "Module ID",
                                    choices = NULL,
                                    multiple = TRUE,
                                    selected = NULL)
                           )
                         ),
                         h4("Levels Included"),
                         fluidRow(
                           column(3,
                                  checkboxInput(ns("relationship_network_include_functional_modules"),
                                                label = tags$span("FM",
                                                                  shinyBS::bsButton(ns("functional_module_info"),
                                                                                    label = "",
                                                                                    icon = icon("info"),
                                                                                    style = "info",
                                                                                    size = "extra-small")),
                                                value = TRUE)),
                           bsPopover(
                             id = ns("functional_module_info"),
                             title = "",
                             content = "FM is functional module",
                             placement = "right",
                             trigger = "hover",
                             options = list(container = "body")
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_include_modules"),
                                                "Modules",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_include_pathways"),
                                                "Pathways",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_include_molecules"),
                                                "Molecules",
                                                value = TRUE)
                           )
                         ),
                         h4("Colors"),
                         fluidRow(
                           column(3,
                                  shinyWidgets::colorPickr(
                                    inputId = ns("relationship_network_functional_module_color"),
                                    label = "FM",
                                    selected = "#DD4124FF",
                                    theme = "monolith",
                                    width = "100%"
                                  )
                           ),
                           column(3,
                                  shinyWidgets::colorPickr(
                                    inputId = ns("relationship_network_module_color"),
                                    label = "Module",
                                    selected = "#0F7BA2FF",
                                    theme = "monolith",
                                    width = "100%"
                                  )
                           ),
                           column(3,
                                  shinyWidgets::colorPickr(
                                    inputId = ns("relationship_network_pathway_color"),
                                    label = "Pathway",
                                    selected = "#43B284FF",
                                    theme = "monolith",
                                    width = "100%"
                                  )
                           ),
                           column(3,
                                  shinyWidgets::colorPickr(
                                    inputId = ns("relationship_network_molecule_color"),
                                    label = "Molecule",
                                    selected = "#FAB255FF",
                                    theme = "monolith",
                                    width = "100%"
                                  )
                           )
                         ),
                         h4("Text"),
                         fluidRow(
                           column(3,
                                  checkboxInput(ns("relationship_network_functional_module_text"),
                                                "FM",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_module_text"),
                                                "Module",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_pathway_text"),
                                                "Pathway",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_molecule_text"),
                                                "Molecules",
                                                value = TRUE)
                           )
                         ),
                         h4("Text size"),
                         fluidRow(
                           column(3,
                                  numericInput(ns("relationship_network_functional_module_text_size"),
                                               "FM",
                                               value = 3, min = 0.3, max = 10)
                           ),
                           column(3,
                                  numericInput(ns("relationship_network_module_text_size"),
                                               "Module",
                                               value = 3, min = 0.3, max = 10)
                           ),
                           column(3,
                                  numericInput(ns("relationship_network_pathway_text_size"),
                                               "Pathway",
                                               value = 3, min = 0.3, max = 10)
                           ),
                           column(3,
                                  numericInput(ns("relationship_network_molecule_text_size"),
                                               "Molecule",
                                               value = 3, min = 0.3, max = 10)
                           )
                         ),
                         h4("Arrange position"),
                         fluidRow(
                           column(3,
                                  checkboxInput(ns("relationship_network_functional_module_arrange_position"),
                                                "FM",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_module_arrange_position"),
                                                "Module",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_pathway_arrange_position"),
                                                "Pathway",
                                                value = TRUE)
                           ),
                           column(3,
                                  checkboxInput(ns("relationship_network_molecule_arrange_position"),
                                                "Molecules",
                                                value = TRUE)
                           )
                         ),
                         h4("Position limits"),
                         fluidRow(
                           column(6,
                                  sliderInput(
                                    ns("relationship_network_functional_module_position_limits"),
                                    "Functional module",
                                    min = 0, max = 1,
                                    value = c(0, 1))
                           ),
                           column(6,
                                  sliderInput(
                                    ns("relationship_network_module_position_limits"),
                                    "Module",
                                    min = 0, max = 1,
                                    value = c(0, 1))
                           )
                         ),
                         fluidRow(
                           column(6,
                                  sliderInput(
                                    ns("relationship_network_pathway_position_limits"),
                                    "Pathway",
                                    min = 0, max = 1,
                                    value = c(0, 1))
                           ),
                           column(6,
                                  sliderInput(
                                    ns("relationship_network_molecule_position_limits"),
                                    "Molecule",
                                    min = 0, max = 1,
                                    value = c(0, 1))
                           )
                         ),
                         fluidRow(
                           column(4,
                                  selectInput(ns("relationship_network_type"),
                                              "Type",
                                              choices = c("pdf", "png", "jpeg")
                                  )
                           ),
                           column(4,
                                  numericInput(ns("relationship_network_width"),
                                               "Width",
                                               value = 21, min = 4, max = 30)
                           ),
                           column(4,
                                  numericInput(ns("relationship_network_height"),
                                               "Height",
                                               value = 7, min = 4, max = 20)
                           )
                         ),
                         fluidRow(
                           column(12,
                                  actionButton(ns("generate_relationship_network"),
                                               "Generate plot",
                                               class = "btn-primary",
                                               style = "background-color: #d83428; color: white;"),
                                  shinyjs::useShinyjs(),
                                  downloadButton(ns("download_relationship_network"),
                                                 "Download",
                                                 class = "btn-primary",
                                                 style = "background-color: #d83428; color: white;")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(12,
                                  actionButton(
                                    ns("go2results_4"),
                                    "Next",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;"),
                                  actionButton(
                                    ns("show_relationship_network_code"),
                                    "Code",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;"
                                  )
                           )
                         ),
                         style = "border-right: 1px solid #ddd; padding-right: 20px;"
                  ),
                  column(8,
                         br(),
                         div(
                           class = "scrollable-container",
                           shiny::plotOutput(ns("relationship_network"),
                                             height = "800px", width = "800px")
                         )
                  )
                )
              ),
              
              ## tab5: Module expression heatmap panel ----
              tabPanel(
                title = "Module-Expression Heatmap",
                fluidRow(
                  column(4,
                         br(),
                         fluidRow(
                           column(12,
                                  fileInput(inputId = ns("upload_expression_data"),
                                            label = "Upload expression data (.csv, .xlsx, .rda)",
                                            accept = c(".csv", ".xlsx", ".rda"))
                           )
                         ),
                         fluidRow(
                           column(4,
                                  selectInput(
                                    ns("relationship_heatmap_level"),
                                    "Level",
                                    choices = c(
                                      "Pathway" = "pathway",
                                      "Molecule" = "molecule"),
                                    selected = "pathway")
                           ),
                           column(8,
                                  selectInput(
                                    ns("relationship_heatmap_module_id"),
                                    "Module ID",
                                    choices = NULL,
                                    multiple = TRUE,
                                    selected = NULL)
                           )
                         ),
                         fluidRow(
                           column(6,
                                  checkboxInput(ns("relationship_heatmap_scale_expression_data"),
                                                "Scale expression data", value = TRUE)
                           ),
                           column(6,
                                  checkboxInput(ns("relationship_heatmap_wordcloud"), "Word cloud", value = TRUE)
                           )
                         ),
                         fluidRow(
                           column(6,
                                  checkboxInput(ns("relationship_heatmap_cluster_rows"),
                                                "Cluster rows", value = FALSE)
                           ),
                           column(6,
                                  checkboxInput(ns("relationship_heatmap_show_cluster_tree"),
                                                "Show cluster tree", value = TRUE)
                           )
                         ),
                         fluidRow(
                           column(6,
                                  checkboxInput(ns("relationship_heatmap_llm_text"), "LLM text", value = FALSE)
                           )
                         ),
                         h4("Colors"),
                         fluidRow(
                           column(4,
                                  shinyWidgets::colorPickr(
                                    inputId = ns("relationship_heatmap_functional_module_color"),
                                    label = "FM",
                                    selected = "#DD4124FF",
                                    theme = "monolith",
                                    width = "100%"
                                  )
                           ),
                           column(4,
                                  shinyWidgets::colorPickr(
                                    inputId = ns("relationship_heatmap_pathway_color"),
                                    label = "Pathway",
                                    selected = "#43B284FF",
                                    theme = "monolith",
                                    width = "100%"
                                  )
                           ),
                           column(4,
                                  shinyWidgets::colorPickr(
                                    inputId = ns("relationship_heatmap_molecule_color"),
                                    label = "Molecule",
                                    selected = "#FAB255FF",
                                    theme = "monolith",
                                    width = "100%"
                                  )
                           )
                         ),
                         h4("Position limits"),
                         fluidRow(
                           column(6,
                                  sliderInput(
                                    ns("relationship_heatmap_functional_module_position_limits"),
                                    "Functional module",
                                    min = 0, max = 1,
                                    value = c(0.2, 0.8))
                           ),
                           column(6,
                                  sliderInput(
                                    ns("relationship_heatmap_pathway_position_limits"),
                                    "Pathway",
                                    min = 0, max = 1,
                                    value = c(0.1, 0.9))
                           )
                         ),
                         fluidRow(
                           column(6,
                                  sliderInput(
                                    ns("relationship_heatmap_molecule_position_limits"),
                                    "Molecule",
                                    min = 0, max = 1,
                                    value = c(0, 1))
                           )
                         ),
                         h4("Layout ratios"),
                         fluidRow(
                           column(6,
                                  sliderInput(
                                    ns("relationship_heatmap_heatmap_height_ratios_1"),
                                    "Heatmap spacing",
                                    min = 0.1, max = 10,
                                    value = 1, step = 0.1)
                           ),
                           column(6,
                                  sliderInput(
                                    ns("relationship_heatmap_heatmap_height_ratios_2"),
                                    "Heatmap main",
                                    min = 10, max = 200,
                                    value = 100, step = 10)
                           )
                         ),
                         fluidRow(
                           column(6,
                                  sliderInput(
                                    ns("relationship_heatmap_network_height_ratios_1"),
                                    "Network main",
                                    min = 1, max = 50,
                                    value = 10, step = 1)
                           ),
                           column(6,
                                  sliderInput(
                                    ns("relationship_heatmap_network_height_ratios_2"),
                                    "Network spacing",
                                    min = 1, max = 20,
                                    value = 5, step = 1)
                           )
                         ),
                         fluidRow(
                           column(4,
                                  selectInput(ns("relationship_heatmap_type"), "Type",
                                              choices = c("pdf", "png", "jpeg"))
                           ),
                           column(4,
                                  numericInput(ns("relationship_heatmap_width"), "Width",
                                               value = 12, min = 4, max = 30)
                           ),
                           column(4,
                                  numericInput(ns("relationship_heatmap_height"), "Height",
                                               value = 8, min = 4, max = 20)
                           )
                         ),
                         fluidRow(
                           column(12,
                                  actionButton(ns("generate_relationship_heatmap"),
                                               "Generate plot",
                                               class = "btn-primary",
                                               style = "background-color: #d83428; color: white;"),
                                  shinyjs::useShinyjs(),
                                  downloadButton(ns("download_relationship_heatmap"),
                                                 "Download",
                                                 class = "btn-primary",
                                                 style = "background-color: #d83428; color: white;")
                           )
                         ),
                         br(),
                         fluidRow(
                           column(12,
                                  actionButton(
                                    ns("go2results_5"),
                                    "Next",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;"),
                                  actionButton(
                                    ns("show_relationship_heatmap_code"),
                                    "Code",
                                    class = "btn-primary",
                                    style = "background-color: #d83428; color: white;"
                                  )
                           )
                         ),
                         style = "border-right: 1px solid #ddd; padding-right: 20px;"
                  ),
                  column(8,
                         br(),
                         div(
                           class = "scrollable-container",
                           shiny::plotOutput(ns("relationship_heatmap"),
                                             height = "800px", width = "800px")
                         )
                  )
                )
              )
            )
          ))
}

#' Data Visualization Server Module
#'
#' Internal server logic for data visualization including barplot, module similarity network,
#' module information, and relationship network.
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param id Module id.
#' @param enriched_functional_module Reactive value containing enriched functional module data.
#' @param tab_switch Function to switch tabs.
#' @import shiny
#' @importFrom shinyjs toggleElement useShinyjs
#' @importFrom stringr str_sort
#' @noRd

data_visualization_server <- function(id, enriched_functional_module, tab_switch) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      query_type <- reactive({
        req(enriched_functional_module())
        if ("enrich_pathway" %in% names(enriched_functional_module()@process_info)) {
          enriched_functional_module()@process_info$enrich_pathway@parameter$query_type
        } else {
          enriched_functional_module()@process_info$do_gsea@parameter$query_type
        }
        })

      observe({
        req(query_type())
        shinyjs::toggleElement(
          id = "level_panel_for_gene",
          condition = query_type() == "gene"
        )
        shinyjs::toggleElement(
          id = "db_color_panel_gene",
          condition = query_type() == "gene"
        )
      })

      observe({
        req(query_type())
        shinyjs::toggleElement(
          id = "level_panel_for_metabolite",
          condition = query_type() == "metabolite"
        )
        shinyjs::toggleElement(
          id = "db_color_panel_metabolite",
          condition = query_type() == "metabolite"
        )
      })

      observe({
        req(enriched_functional_module())
        db_choices <- c("GO" = "go", "KEGG" = "kegg", "Reactome" = "reactome", "HMDB" = "hmdb", "KEGG" = "metkegg")
        if ((query_type() == "gene") & ("enrich_pathway" %in% names(enriched_functional_module()@process_info))) {
          all_choices <- c("qscore", "RichFactor", "FoldEnrichment")
          available_db <- enriched_functional_module()@process_info$enrich_pathway@parameter$database
          updateSelectInput(
            session,
            "gene_barplot_database",
            choices = db_choices[db_choices %in% available_db],
            selected = available_db
          )
        } else if ((query_type() == "gene") & ("do_gsea" %in% names(enriched_functional_module()@process_info))) {
          all_choices <- c("NES")
          available_db <- enriched_functional_module()@process_info$do_gsea@parameter$database
          updateSelectInput(
            session,
            "gene_barplot_database",
            choices = db_choices[db_choices %in% available_db],
            selected = available_db
          )
        } else if (query_type() == "metabolite") {
          all_choices <- c("qscore")
          available_db <- enriched_functional_module()@process_info$enrich_pathway@parameter$database
          updateSelectInput(
            session,
            "met_barplot_database",
            choices = db_choices[db_choices %in% available_db],
            selected = available_db
          )
        }

        updateSelectInput(
          session,
          "x_axis_name",
          choices = all_choices
        )
        # Module similarity network
        updateSelectInput(
          session,
          "module_similarity_network_database",
          choices = db_choices[db_choices %in% tolower(levels(factor(enriched_functional_module()@merged_module$result_with_module$database)))]
        )
        # Module information
        updateSelectInput(
          session,
          "module_information_database",
          choices = db_choices[db_choices %in% tolower(levels(factor(enriched_functional_module()@merged_module$result_with_module$database)))]
        )
      })

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
              # shiny::showModal(
              #   modalDialog(
              #     title = "Error",
              #     "Please perform module identification before visualization.",
              #     easyClose = TRUE,
              #     footer = modalButton("Close")
              #   )
              # )
              shinyalert::shinyalert(
                text = "Do <strong>Module Identification</strong> before Data Visualization.",
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            } else {
              enriched_functional_module(get(names[1], envir = tempEnv)) 
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
              text = "The uploaded file should contain exactly one object.",
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          }
        }
        
        if ((query_type() == "gene") & ("enrich_pathway" %in% names(enriched_functional_module()@process_info))) {
          all_choices <- c("qscore", "RichFactor", "FoldEnrichment")
        } else if ((query_type() == "gene") & ("do_gsea" %in% names(enriched_functional_module()@process_info))) {
          all_choices <- c("NES")
        } else if (query_type() == "metabolite") {
          all_choices <- c("qscore")
        }
        # barplot x_axis
        updateSelectInput(
          session,
          "x_axis_name",
          choices = all_choices
        )
        # Module similarity network
        updateSelectInput(
          session,
          "module_similarity_network_database",
          choices = levels(factor(enriched_functional_module()@merged_module$result_with_module$database))
        )
        # Module information
        updateSelectInput(
          session,
          "module_information_database",
          choices = levels(factor(enriched_functional_module()@merged_module$result_with_module$database))
        )
      })

      observe({
        req(enriched_functional_module())
        
        if (query_type() == "gene") {
          if (length(c(enriched_functional_module()@merged_pathway_go,
                       enriched_functional_module()@merged_pathway_kegg,
                       enriched_functional_module()@merged_pathway_reactome)) == 0) {
            
            current_selection <- input$barplot_level
            valid_choices <- c("functional_module", "pathway")
            selected_value <- if (!is.null(current_selection) && current_selection %in% valid_choices) {
              current_selection
            } else {
              "functional_module"
            }
            
            updateSelectInput(session, "barplot_level",
                              choices = c("FM" = "functional_module",
                                          "Pathway" = "pathway"),
                              selected = selected_value)
            
            updateSelectInput(session, "module_similarity_network_level",
                              choices = c("FM" = "functional_module"),
                              selected = "functional_module")
            updateSelectInput(session, "module_information_level",
                              choices = c("FM" = "functional_module"),
                              selected = "functional_module")
            updateSelectInput(session, "relationship_network_level", choices = c("FM" = "functional_module"), selected = "functional_module")
            updateCheckboxInput(session, "relationship_network_include_modules", value = FALSE)
            disable("relationship_network_include_modules")
            updateCheckboxInput(session, "relationship_network_module_text", value = FALSE)
            disable("relationship_network_module_text")
            updateCheckboxInput(session, "relationship_network_module_arrange_position", value = FALSE)
            disable("relationship_network_module_arrange_position")
          }
        } else if (query_type() == "metabolite") {
          if (length(c(enriched_functional_module()@merged_pathway_hmdb,
                       enriched_functional_module()@merged_pathway_metkegg)) == 0) {
            
            current_selection <- input$barplot_level
            valid_choices <- c("functional_module", "pathway")
            selected_value <- if (!is.null(current_selection) && current_selection %in% valid_choices) {
              current_selection
            } else {
              "functional_module"
            }
            
            updateSelectInput(session, "barplot_level",
                              choices = c("FM" = "functional_module",
                                          "Pathway" = "pathway"),
                              selected = selected_value)
            
            updateSelectInput(session, "module_similarity_network_level",
                              choices = c("FM" = "functional_module"),
                              selected = "functional_module")
            updateSelectInput(session, "module_information_level",
                              choices = c("FM" = "functional_module"),
                              selected = "functional_module")
            updateSelectInput(session, "relationship_network_level", choices = c("FM" = "functional_module"), selected = "functional_module")
            updateCheckboxInput(session, "relationship_network_include_modules", value = FALSE)
            disable("relationship_network_include_modules")
            updateCheckboxInput(session, "relationship_network_module_text", value = FALSE)
            disable("relationship_network_module_text")
            updateCheckboxInput(session, "relationship_network_module_arrange_position", value = FALSE)
            disable("relationship_network_module_arrange_position")
          }
        }
        
        if (length(enriched_functional_module()@llm_module_interpretation) == 0 | input$barplot_level == "module") {
          updateCheckboxInput(session, "barplot_llm_text", value = FALSE)
          disable("barplot_llm_text")
        } else {
          enable("barplot_llm_text")
        }
        
        if (length(enriched_functional_module()@llm_module_interpretation) == 0 | input$module_similarity_network_level == "module") {
          updateCheckboxInput(session, "module_similarity_network_llm_text", value = FALSE)
          disable("module_similarity_network_llm_text")
        } else {
          enable("module_similarity_network_llm_text")
        }
        
        if (length(enriched_functional_module()@llm_module_interpretation) == 0 | input$module_information_level == "module") {
          updateCheckboxInput(session, "module_information_llm_text", value = FALSE)
          disable("module_information_llm_text")
        } else {
          enable("module_information_llm_text")
        }
        
        if (length(enriched_functional_module()@llm_module_interpretation) == 0 | input$relationship_network_level == "module") {
          updateCheckboxInput(session, "relationship_network_llm_text", value = FALSE)
          disable("relationship_network_llm_text")
        } else {
          enable("relationship_network_llm_text")
        }
        
        if (length(enriched_functional_module()@llm_module_interpretation) == 0) {
          updateCheckboxInput(session, "relationship_heatmap_llm_text", value = FALSE)
          disable("relationship_heatmap_llm_text")
        } else {
          enable("relationship_heatmap_llm_text")
        }
        
        if (input$module_information_level == "functional_module") {
          updateSelectInput(session, "module_information_database", choices = NULL)
        }
      })

      ## Barplot ----
      # Observe generate barplot button click
      barplot <-
        reactiveVal()
      barplot_code <-
        reactiveVal()

      observeEvent(input$generate_barplot, {
        message("generating barplot")
        if (is.null(enriched_functional_module())) {
          # No enriched functional module available
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched functional module data available. Please complete the previous steps or upload the data",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched_functional_module available",
            text = "Complete the previous steps to get an enriched fucntional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          
          barplot_alert_id <- shinyalert::shinyalert(
            title = "Generating pathway barplot",
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
            if (query_type() == "gene") {
              plot <-
                mapa::plot_pathway_bar(
                  object = enriched_functional_module(),
                  top_n = input$barplot_top_n,
                  x_axis_name = input$x_axis_name,
                  y_label_width = input$barplot_y_lable_width,
                  p.adjust.cutoff = input$barplot_p_adjust_cutoff,
                  count.cutoff = input$barplot_count_cutoff,
                  level = input$barplot_level,
                  llm_text = input$barplot_llm_text,
                  database = input$gene_barplot_database,
                  line_type = input$line_type,
                  database_color = c(
                    GO = input$barplot_go_color,
                    KEGG = input$barplot_kegg_color,
                    Reactome = input$barplot_reactome_color
                  )
                  # translation = input$barplot_translation
                )
            } else {
              plot <-
                mapa::plot_pathway_bar(
                  object = enriched_functional_module(),
                  top_n = input$barplot_top_n,
                  x_axis_name = input$x_axis_name,
                  y_label_width = input$barplot_y_lable_width,
                  p.adjust.cutoff = input$barplot_p_adjust_cutoff,
                  count.cutoff = input$barplot_count_cutoff,
                  level = input$barplot_level,
                  llm_text = input$barplot_llm_text,
                  database = input$met_barplot_database,
                  line_type = input$line_type,
                  database_color = c(
                    HMDB = input$barplot_hmdb_color,
                    KEGG = input$barplot_metkegg_color
                  )
                  # translation = input$barplot_translation
                )
            }
          },
          error = function(e) {
            shinyalert::closeAlert(id = barplot_alert_id)
            # shiny::showModal(modalDialog(
            #   title = "Error",
            #   paste("Details:", e$message),
            #   easyClose = TRUE,
            #   footer = modalButton("Close")
            # ))
            shinyalert::shinyalert(
              text = paste("Details:", e$message),
              html = TRUE,
              type = "error",
              confirmButtonCol = "#dd4b39"
            )
          }
          )
          
          shinyalert::closeAlert(id = barplot_alert_id)

          barplot(plot)

          ###save code
          if (query_type() == "gene") {
            data_color <-
              paste0("c(", paste(paste(
                c("GO", "KEGG", "Reactome"),
                c(
                  paste0('"', input$barplot_go_color, '"'),
                  paste0('"', input$barplot_kegg_color, '"'),
                  paste0('"', input$barplot_reactome_color, '"')
                ),
                sep = " = "
              ),
              collapse = ", "), ")")
            barplot_database <-
              paste0("c(", paste(unlist(lapply(paste(input$gene_barplot_database), function(x)
                paste0('"', x, '"'))),
                collapse = ", "), ")")
          } else {
            data_color <-
              paste0("c(", paste(paste(
                c("HMDB", "KEGG"),
                c(
                  paste0('"', input$barplot_hmdb_color, '"'),
                  paste0('"', input$barplot_metkegg_color, '"')
                ),
                sep = " = "
              ),
              collapse = ", "), ")")
            barplot_database <-
              paste0("c(", paste(unlist(lapply(paste(input$met_barplot_database), function(x)
                paste0('"', x, '"'))),
                collapse = ", "), ")")
          }

          barplot_code <-
            sprintf(
              "
plot_pathway_bar(
  object = enriched_functional_module,
  top_n = %s,
  x_axis_name = %s,
  y_lable_width = %s,
  p.adjust.cutoff = %s,
  count.cutoff = %s,
  level = %s,
  llm_text = %s,
  database = %s,
  line_type = %s,
  database_color = %s)
              ",
              input$barplot_top_n,
              input$x_axis_name,
              input$barplot_y_lable_width,
              input$barplot_p_adjust_cutoff,
              input$barplot_count_cutoff,
              paste0('"', input$barplot_level, '"'),
              input$barplot_llm_text,
              barplot_database,
              paste0('"', input$line_type, '"'),
              data_color
            )

          barplot_code(barplot_code)
        }
      })


      output$barplot <-
        renderPlot({
          req(barplot())
          barplot()
        },
        res = 96)

      # output$barplot <-
      #   renderPlot({
      #     req(barplot())
      #     barplot()
      #   },
      #   width = function() {
      #     input$barplot_width_show
      #   },
      #   height = function() {
      #     input$barplot_height_show
      #   })

      output$download_barplot <-
        downloadHandler(
          filename = function() {
            paste0("pathway_barplot.", input$barplot_type)
          },
          content = function(file) {
            ggplot2::ggsave(
              file,
              plot = barplot(),
              width = input$barplot_width,
              height = input$barplot_height
            )
          }
        )

      observe({
        if (is.null(barplot()) ||
            length(barplot()) == 0) {
          shinyjs::disable("download_barplot")
        } else {
          shinyjs::enable("download_barplot")
        }
      })

      ####show code
      observeEvent(input$show_barplot_code, {
        if (is.null(barplot_code()) ||
            length(barplot_code()) == 0) {
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
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else{
          code_content <-
            barplot_code()
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



      ## Module similarity network ----
      # Observe generate module_similarity_network button click
      module_similarity_network <- reactiveVal()
      module_similarity_network_code <- reactiveVal()
      
      module_similarity_network_without_legend <- reactiveVal(NULL)
      show_module_color_legend <- reactiveVal(TRUE)
      
      observe({
        req(input$generate_module_similarity_network)
        req(input$module_similarity_network_degree_cutoff)
        
        if (is.null(enriched_functional_module())) {
          # No enriched functional module available
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched functional module data available. Please complete the previous steps or upload the data.",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched_functional_module available",
            text = "Complete the previous steps to get an enriched fucntional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          # shinyjs::show("loading")
          
          sim_network_alert_id <- shinyalert::shinyalert(
            title = "Generating module similarity network",
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
              if (sum(enriched_functional_module()@merged_module$functional_module_result$module_content_number > input$module_similarity_network_degree_cutoff) > 34) {
                show_module_color_legend(FALSE)
              } else {
                show_module_color_legend(TRUE)
              }
              
              plot <-
                mapa::plot_similarity_network(
                  object = enriched_functional_module(),
                  level = input$module_similarity_network_level,
                  database = input$module_similarity_network_database,
                  degree_cutoff = input$module_similarity_network_degree_cutoff,
                  text = input$module_similarity_network_text,
                  llm_text = input$module_similarity_network_llm_text,
                  text_all = input$module_similarity_network_text_all
                  # translation = input$module_similarity_network_translation
                ) + 
                ggplot2::theme(aspect.ratio = 1)
              
              if (!show_module_color_legend()) {
                plot_without_module_legend <- 
                  plot +
                  ggplot2::guides(fill = "none")
                
                module_similarity_network_without_legend(plot_without_module_legend)
              }
              module_similarity_network(plot)
            },
            error = function(e) {
              shinyalert::closeAlert(id = sim_network_alert_id)
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
          
          shinyalert::closeAlert(id = sim_network_alert_id)
          
          if (!show_module_color_legend()) {
            # showNotification(
            #   "Note: With more than 34 modules, the legend is hidden in the display to improve readability. The legend will be included when you download the figure.",
            #   type = "message",
            #   duration = NULL
            # )
            shinyalert::shinyalert(
              text = "With more than 34 modules, the legend is hidden in the display to improve readability. The legend will be included when you download the figure.",
              type = "info",
              html = TRUE,
              confirmButtonCol = "#dd4b39"
            )
          }

          ###save code
          module_similarity_network_code <-
            sprintf(
              '
plot_similarity_network(
  object = enriched_functional_module,
  level = %s,
  database = %s,
  degree_cutoff = %s,
  text = %s,
  llm_text = %s,
  text_all = %s) + 
  ggplot2::theme(aspect.ratio = 1)
              ',
              paste0('"', input$module_similarity_network_level, '"'),
              paste0('"', input$module_similarity_network_database, '"'),
              input$module_similarity_network_degree_cutoff,
              input$module_similarity_network_text,
              input$module_similarity_network_llm_text,
              input$module_similarity_network_text_all
            )

          module_similarity_network_code(module_similarity_network_code)

        }
      })

      output$module_similarity_network <-
        renderPlot({
          req(module_similarity_network())
          
          if (show_module_color_legend()) {
            module_similarity_network()
          } else {
            module_similarity_network_without_legend()
          }
        },
        # width = function() {
        #   req(input$module_similarity_network_width)
        #   input$module_similarity_network_width * 100
        # },
        # height = function() {
        #   req(input$module_similarity_network_height) 
        #   input$module_similarity_network_height * 100
        # },
        res = 96)


      ######code for module_similarity_network
      ####show code
      observeEvent(input$show_module_similarity_network_code, {
        if (is.null(module_similarity_network_code()) ||
            length(module_similarity_network_code()) == 0) {
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
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else{
          code_content <-
            module_similarity_network_code()
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


      output$download_module_similarity_network <-
        downloadHandler(
          filename = function() {
            paste0(
              "module_similarity_network_",
              ifelse(
                input$module_similarity_network_level == "module",
                input$module_similarity_network_database,
                "functional_module"
              ),
              ".",
              input$module_similarity_network_type
            )
          },
          content = function(file) {
            Cairo::CairoPDF(file = file, 
                            width = input$module_similarity_network_width, 
                            height = input$module_similarity_network_height)
            print(module_similarity_network())
            dev.off()
            # ggplot2::ggsave(
            #   file,
            #   plot = module_similarity_network(),
            #   width = input$module_similarity_network_width,
            #   height = input$module_similarity_network_height
            # )
          }
        )

      observe({
        if (is.null(module_similarity_network()) ||
            length(module_similarity_network()) == 0) {
          shinyjs::disable("download_module_similarity_network")
        } else {
          shinyjs::enable("download_module_similarity_network")
        }
      })

      ## Module information plot ----
      # Update the module ID
      module_information_module_id <-
        reactiveVal()

      observe({
        if (!is.null(enriched_functional_module()) &
            length(enriched_functional_module()) != 0) {
          ####level is functional module
          if (input$module_information_level == "functional_module") {
            if (length(enriched_functional_module()@merged_module) > 0) {
              module_information_module_id <-
                unique(
                  enriched_functional_module()@merged_module$functional_module_result$module
                )
              module_information_module_id(module_information_module_id)
            }
          }

          ####level is module
          if (input$module_information_level == "module") {
            ####database is go
            if (input$module_information_database == "go") {
              if (length(enriched_functional_module()@merged_pathway_go) > 0) {
                module_information_module_id <-
                  unique(
                    enriched_functional_module()@merged_pathway_go$module_result$module
                  )
                module_information_module_id(module_information_module_id)
              }
            }

            ####database is kegg
            if (input$module_information_database == "kegg") {
              if (length(enriched_functional_module()@merged_pathway_kegg) > 0) {
                module_information_module_id <-
                  unique(
                    enriched_functional_module()@merged_pathway_kegg$module_result$module
                  )
                module_information_module_id(module_information_module_id)
              }
            }

            ####database is reactome
            if (input$module_information_database == "reactome") {
              if (length(enriched_functional_module()@merged_pathway_reactome) > 0) {
                module_information_module_id <-
                  unique(
                    enriched_functional_module()@merged_pathway_reactome$module_result$module
                  )
                module_information_module_id(module_information_module_id)
              }
            }

            # metabolite module information ========
            ####database is go
            if (input$module_information_database == "go") {
              if (length(enriched_functional_module()@merged_pathway_go) > 0) {
                module_information_module_id <-
                  unique(
                    enriched_functional_module()@merged_pathway_go$module_result$module
                  )
                module_information_module_id(module_information_module_id)
              }
            }

            ####database is kegg
            if (input$module_information_database == "kegg") {
              if (length(enriched_functional_module()@merged_pathway_kegg) > 0) {
                module_information_module_id <-
                  unique(
                    enriched_functional_module()@merged_pathway_kegg$module_result$module
                  )
                module_information_module_id(module_information_module_id)
              }
            }

          }

          updateSelectInput(
            session,
            "module_information_module_id",
            choices = stringr::str_sort(module_information_module_id(), numeric = TRUE),
            selected = stringr::str_sort(module_information_module_id(), numeric = TRUE)[1]
          )
        }
      })

      # Observe generate module information button click
      module_information <-
        reactiveVal()
      module_information1 <-
        reactiveVal()
      module_information2 <-
        reactiveVal()
      module_information3 <-
        reactiveVal()
      module_information_code <-
        reactiveVal()

      ####if the module_information_module_id() is null, then show warning
      observeEvent(input$generate_module_information, {
        if (is.null(enriched_functional_module())) {
          # No enriched functional module available
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched functional module data available. Please complete the previous steps or upload the data.",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched functional module data available",
            text = "Complete previous steps to get enriched functional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          if (is.null(module_information_module_id())) {
            # No enriched functional module available
            # shiny::showModal(
            #   modalDialog(
            #     title = "Warning",
            #     "Select a module ID first",
            #     easyClose = TRUE,
            #     footer = modalButton("Close")
            #   )
            # )
            shinyalert::shinyalert(
              title = "Select a module ID first",
              type = "warning",
              confirmButtonCol = "#dd4b39"
            )
          } else{
            
            plot_module_info_alert_id <- shinyalert::shinyalert(
              title = "Generating module information plots",
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
              plot <-
                mapa::plot_module_info(
                  object = enriched_functional_module(),
                  level = input$module_information_level,
                  llm_text = input$module_information_llm_text,
                  database = input$module_information_database,
                  module_id = input$module_information_module_id
                  # translation = input$module_information_translation
                ),
              error = function(e) {
                shinyalert::closeAlert(id = plot_module_info_alert_id)
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
            
            shinyalert::closeAlert(id = plot_module_info_alert_id)
            
            if (is(plot, "ggplot")) {
              plot_all <-
                plot + plot + plot +
                patchwork::plot_layout(ncol = 1)

              module_information(plot_all)
              module_information1(plot)
              module_information2(plot)
              module_information3(plot)
            } else{
              plot_all <-
                plot[[1]] + plot[[2]] + plot[[3]] +
                patchwork::plot_layout(ncol = 1)

              module_information(plot_all)
              module_information1(plot[[1]])
              module_information2(plot[[2]])
              module_information3(plot[[3]])
            }


            ###save code
            module_information_code <-
              sprintf(
                '
plot_module_info(
  object = enriched_functional_module,
  level = %s,
  llm_text = %s,
  database = %s,
  module_id = %s)
                ',
                paste0('"', input$module_information_level, '"'),
                input$module_information_llm_text,
                paste0('"', input$module_information_database, '"'),
                paste0('"', input$module_information_module_id, '"')
              )

            module_information_code(module_information_code)

          }
        }
      })

      output$module_information <-
        renderPlot({
          req(module_information())
          module_information()
        }, res = 96)

      output$module_information1 <-
        renderPlot({
          req(module_information1())
          module_information1()
        }, res = 96)

      output$module_information2 <-
        renderPlot({
          req(module_information2())
          module_information2()
        }, res = 96)

      output$module_information3 <-
        renderPlot({
          req(module_information3())
          module_information3()
        }, res = 96)

      ######code for module_information
      ####show code
      observeEvent(input$show_module_information_code, {
        if (is.null(module_information_code()) ||
            length(module_information_code()) == 0) {
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
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else{
          code_content <-
            module_information_code()
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


      output$download_module_information <-
        downloadHandler(
          filename = function() {
            paste0(
              "module_information_",
              input$module_information_module_id,
              ".",
              input$module_information_type
            )
          },
          content = function(file) {
            Cairo::CairoPDF(file = file, 
                            width = input$module_information_width, 
                            height = input$module_information_height)
            print(module_information())
            dev.off()
            # ggplot2::ggsave(
            #   file,
            #   plot = module_information(),
            #   width = input$module_information_width,
            #   height = input$module_information_height
            # )
          }
        )

      observe({
        if (is.null(module_information1()) ||
            length(module_information1()) == 0) {
          shinyjs::disable("download_module_information")
        } else {
          shinyjs::enable("download_module_information")
        }
      })


      ## Relationship network plot ----
      # Update the module ID
      observeEvent(list(enriched_functional_module(), input$relationship_network_level),{
        
        req(enriched_functional_module())
        
        if (!("merged_module" %in% names(enriched_functional_module()@process_info))) {
          shinyalert::shinyalert(
            text = "Please do <strong>Module Identification</strong> before Module Annotation.",
            type = "error",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
          return()
        }
        
        if (!is.null(enriched_functional_module()) &
            length(enriched_functional_module()) != 0) {
          ####level is functional module
          if (input$relationship_network_level == "functional_module") {
            if (length(enriched_functional_module()@merged_module) > 0) {
              relationship_network_module_id <-
                unique(
                  enriched_functional_module()@merged_module$functional_module_result$module
                )
            }
          }

          ####level is module
          if (input$relationship_network_level == "module") {
            if (length(enriched_functional_module()@merged_pathway_go) > 0) {
              relationship_network_module_id_go <-
                unique(enriched_functional_module()@merged_pathway_go$module_result$module)
            } else{
              relationship_network_module_id_go <- NULL
            }

            if (length(enriched_functional_module()@merged_pathway_kegg) > 0) {
              relationship_network_module_id_kegg <-
                unique(
                  enriched_functional_module()@merged_pathway_kegg$module_result$module
                )
            } else{
              relationship_network_module_id_kegg <- NULL
            }

            ####database is reactome
            if (length(enriched_functional_module()@merged_pathway_reactome) > 0) {
              relationship_network_module_id_reactome <-
                unique(
                  enriched_functional_module()@merged_pathway_reactome$module_result$module
                )
            } else{
              relationship_network_module_id_reactome <- NULL
            }

            if (length(enriched_functional_module()@merged_pathway_hmdb) > 0) {
              relationship_network_module_id_hmdb <-
                unique(enriched_functional_module()@merged_pathway_hmdb$module_result$module)
            } else{
              relationship_network_module_id_hmdb <- NULL
            }

            if (length(enriched_functional_module()@merged_pathway_metkegg) > 0) {
              relationship_network_module_id_metkegg <-
                unique(
                  enriched_functional_module()@merged_pathway_metkegg$module_result$module
                )
            } else{
              relationship_network_module_id_metkegg <- NULL
            }

            relationship_network_module_id <-
              c(
                relationship_network_module_id_go,
                relationship_network_module_id_kegg,
                relationship_network_module_id_reactome,
                relationship_network_module_id_hmdb,
                relationship_network_module_id_metkegg
              )
          }

          updateSelectInput(
            session,
            "relationship_network_module_id",
            choices = stringr::str_sort(relationship_network_module_id, numeric = TRUE),
            selected = NULL
          )
        }
      })

      # Observe generate relationship network button click
      relationship_network <-
        reactiveVal()
      relationship_network_code <-
        reactiveVal()

      ####get the filtered enriched_functional_module
      object <-
        reactiveVal()

      observeEvent(input$generate_relationship_network, {
        if (is.null(enriched_functional_module())) {
          # No enriched functional module available
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched functional module data available. Please complete the previous steps or upload the data.",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched functional module data available",
            text = "Complete the previous steps to get an enriched functional module result or upload a result",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          ####if filtered by functional module and modules
          object <-
            enriched_functional_module()
          if (!is.null(input$relationship_network_module_id)) {
            tryCatch({
              object <-
                mapa::filter_functional_module(
                  object,
                  level = input$relationship_network_level,
                  remain_id = input$relationship_network_module_id
                )
            },
            error = function(e) {
              # shiny::showModal(modalDialog(
              #   c,
              #   paste("Details:", e$message),
              #   easyClose = TRUE,
              #   footer = modalButton("Close")
              # ))
              shinyalert::shinyalert(
                text = paste("Details:", e$message),
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            })
          }

          object(object)

          plot_relationship_alert_id <- shinyalert::shinyalert(
            title = "Generating relationship plot",
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
            plot <-
              mapa::plot_relationship_network(
                object = object(),
                include_functional_modules = input$relationship_network_include_functional_modules,
                include_modules = input$relationship_network_include_modules,
                include_pathways = input$relationship_network_include_pathways,
                include_molecules = input$relationship_network_include_molecules,
                functional_module_text = input$relationship_network_functional_module_text,
                llm_text = input$relationship_network_llm_text,
                module_text = input$relationship_network_module_text,
                pathway_text = input$relationship_network_pathway_text,
                molecule_text = input$relationship_network_molecule_text,
                circular_plot = input$relationship_network_circular_plot,
                functional_module_color = input$relationship_network_functional_module_color,
                module_color = input$relationship_network_module_color,
                pathway_color = input$relationship_network_pathway_color,
                molecule_color = input$relationship_network_molecule_color,
                functional_module_arrange_position = input$relationship_network_functional_module_arrange_position,
                module_arrange_position = input$relationship_network_module_arrange_position,
                pathway_arrange_position = input$relationship_network_pathway_arrange_position,
                molecule_arrange_position = input$relationship_network_molecule_arrange_position,
                functional_module_position_limits = c(
                  input$relationship_network_functional_module_position_limits[1],
                  input$relationship_network_functional_module_position_limits[2]
                ),
                module_position_limits = c(
                  input$relationship_network_module_position_limits[1],
                  input$relationship_network_module_position_limits[2]
                ),
                pathway_position_limits = c(
                  input$relationship_network_pathway_position_limits[1],
                  input$relationship_network_pathway_position_limits[2]
                ),
                molecule_position_limits = c(
                  input$relationship_network_molecule_position_limits[1],
                  input$relationship_network_molecule_position_limits[2]
                )
                # translation = input$relationship_network_translation
              ),
            error = function(e) {
              shinyalert::closeAlert(id = plot_relationship_alert_id)
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
          
          shinyalert::closeAlert(id = plot_relationship_alert_id)

          relationship_network(plot)

          ###save code
          relationship_network_module_id <-
            paste0("c(",
                   paste0(
                     paste0('"',
                            input$relationship_network_module_id,
                            '"'),
                     collapse = ", "
                   ),
                   ")")

          relationship_network_code1 <-
            sprintf(
              '
object <-
  filter_functional_module(
    object,
    level = %s,
    remain_id = %s
  )
              ',
              paste0('"', input$relationship_network_level, '"'),
              relationship_network_module_id
            )

          functional_module_position_limits <-
            paste0(
              "c(",
              paste0(
                input$relationship_network_functional_module_position_limits,
                collapse = ", "
              ),
              ")"
            )

          module_position_limits <-
            paste0(
              "c(",
              paste0(
                input$relationship_network_module_position_limits,
                collapse = ", "
              ),
              ")"
            )

          pathway_position_limits <-
            paste0(
              "c(",
              paste0(
                input$relationship_network_pathway_position_limits,
                collapse = ", "
              ),
              ")"
            )

          molecule_position_limits <-
            paste0(
              "c(",
              paste0(
                input$relationship_network_molecule_position_limits,
                collapse = ", "
              ),
              ")"
            )

          relationship_network_code2 <-
            sprintf(
              '
plot_relationship_network(
  object = object,
  include_functional_modules = %s,
  include_modules = %s,
  include_pathways = %s,
  include_molecules = %s,
  functional_module_text = %s,
  llm_text = %s,
  module_text = %s,
  pathway_text = %s,
  molecule_text = %s,
  circular_plot = %s,
  functional_module_color = %s,
  module_color = %s,
  pathway_color = %s,
  molecule_color = %s,
  functional_module_arrange_position = %s,
  module_arrange_position = %s,
  pathway_arrange_position = %s,
  molecule_arrange_position = %s,
  functional_module_position_limits = %s,
  module_position_limits = %s,
  pathway_position_limits = %s,
  molecule_position_limits = %s)
              ',
              input$relationship_network_include_functional_modules,
              input$relationship_network_include_modules,
              input$relationship_network_include_pathways,
              input$relationship_network_include_molecules,
              input$relationship_network_functional_module_text,
              input$relationship_network_llm_text,
              input$relationship_network_module_text,
              input$relationship_network_pathway_text,
              input$relationship_network_molecule_text,
              input$relationship_network_circular_plot,
              paste0(
                '"',
                input$relationship_network_functional_module_color,
                '"'
              ),
              paste0('"', input$relationship_network_module_color, '"'),
              paste0('"', input$relationship_network_pathway_color, '"'),
              paste0('"', input$relationship_network_molecule_color, '"'),
              input$relationship_network_functional_module_arrange_position,
              input$relationship_network_module_arrange_position,
              input$relationship_network_pathway_arrange_position,
              input$relationship_network_molecule_arrange_position,
              functional_module_position_limits,
              module_position_limits,
              pathway_position_limits,
              molecule_position_limits
            )

          relationship_network_code <-
            paste0(relationship_network_code1,
                   relationship_network_code2,
                   sep = "\n")

          relationship_network_code(relationship_network_code)

        }
      })

      output$relationship_network <-
        renderPlot({
          req(relationship_network())
          relationship_network()
        },
        res = 96)

      ######code for relationship_network
      ####show code
      observeEvent(input$show_relationship_network_code, {
        if (is.null(relationship_network_code()) ||
            length(relationship_network_code()) == 0) {
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
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else{
          code_content <-
            relationship_network_code()
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


      output$download_relationship_network <-
        downloadHandler(
          filename = function() {
            paste0(
              "relationship_network_",
              input$relationship_network_level,
              ".",
              input$relationship_network_type
            )
          },
          content = function(file) {
            Cairo::CairoPDF(file = file, 
                            width = input$relationship_network_width, 
                            height = input$relationship_network_height)
            print(relationship_network())
            dev.off()
            # ggplot2::ggsave(
            #   file,
            #   plot = relationship_network(),
            #   width = input$relationship_network_width,
            #   height = input$relationship_network_height
            # )
          }
        )

      observe({
        if (is.null(relationship_network()) ||
            length(relationship_network()) == 0) {
          shinyjs::disable("download_relationship_network")
        } else {
          shinyjs::enable("download_relationship_network")
        }
      })
      
      ## Module expression heatmap plot ====
      # Reactive values for expression data
      expression_data_uploaded <- reactiveVal(NULL)
      expression_data_valid <- reactiveVal(FALSE)
      
      # File upload and validation for expression data
      observeEvent(input$upload_expression_data, {
        if (!is.null(input$upload_expression_data$datapath)) {
          file_path <- input$upload_expression_data$datapath
          file_ext <- tools::file_ext(input$upload_expression_data$name)
          
          # Show loading alert
          loading_alert_id <- shinyalert::shinyalert(
            title = "Loading expression data",
            text = tags$div(
              style = "text-align: center;",
              "Please wait while validating your expression file...",
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
            closeOnClickOutside = FALSE
          )
          
          tryCatch({
            # Load data based on file extension
            if (file_ext == "csv") {
              data <- read.csv(file_path, stringsAsFactors = FALSE)
            } else if (file_ext == "xlsx") {
              if (!requireNamespace("readxl", quietly = TRUE)) {
                stop("readxl package is required for Excel files")
              }
              data <- readxl::read_excel(file_path)
              data <- as.data.frame(data)
            } else if (file_ext == "rda") {
              tempEnv <- new.env()
              load(file_path, envir = tempEnv)
              obj_names <- ls(tempEnv)
              
              if (length(obj_names) != 1) {
                stop("The .rda file should contain exactly one object")
              }
              
              data <- get(obj_names[1], envir = tempEnv)
              
              if (!is.data.frame(data)) {
                stop("The object in .rda file must be a data frame")
              }
            } else {
              stop("Unsupported file format. Please upload .csv, .xlsx, or .rda files")
            }
            
            # Close loading alert
            shinyalert::closeAlert(id = loading_alert_id)
            
            # Validate data format
            validation_result <- validate_expression_data(data)
            
            if (validation_result$valid) {
              expression_data_uploaded(data)
              expression_data_valid(TRUE)
              
              # Show success alert
              shinyalert::shinyalert(
                title = "Expression data loaded successfully",
                type = "success",
                html = TRUE,
                confirmButtonCol = "#dd4b39"
              )
              
            } else {
              expression_data_uploaded(NULL)
              expression_data_valid(FALSE)
              
              # Show error alert with validation details
              shinyalert::shinyalert(
                title = "Invalid expression data format",
                text = paste0(
                  "<div style='text-align: left;'>",
                  "<strong>File:</strong> ", input$upload_expression_data$name, "<br>",
                  "<strong>Error:</strong> ", validation_result$message, "<br><br>",
                  "<strong>Required format:</strong><br>",
                  "• Must be a data frame<br>",
                  "• Must contain an 'id' column (ENSEMBL for genes/proteins, HMDB ID or KEGG ID for metabolites)<br>",
                  "• Must have at least one numeric column for sample/group expression values<br>",
                  "• No duplicate or missing IDs allowed",
                  "</div>"
                ),
                type = "error",
                html = TRUE,
                confirmButtonCol = "#dd4b39"
              )
            }
            
          }, error = function(e) {
            # Close loading alert if still open
            shinyalert::closeAlert(id = loading_alert_id)
            
            expression_data_uploaded(NULL)
            expression_data_valid(FALSE)
            
            # Show error alert
            shinyalert::shinyalert(
              title = "Error loading expression data",
              text = paste0(
                "<div style='text-align: left;'>",
                "<strong>File:</strong> ", input$upload_expression_data$name, "<br>",
                "<strong>Error details:</strong> ", e$message, "<br><br>",
                "<strong>Supported formats:</strong><br>",
                "• CSV files (.csv)<br>",
                "• Excel files (.xlsx)<br>",
                "• R data files (.rda)",
                "</div>"
              ),
              type = "error",
              html = TRUE,
              confirmButtonCol = "#dd4b39"
            )
          })
        } else {
          expression_data_uploaded(NULL)
          expression_data_valid(FALSE)
        }
      })
      
      # Update the module ID for relationship heatmap
      observeEvent(list(enriched_functional_module()),{
        if (!is.null(enriched_functional_module()) &
            length(enriched_functional_module()) != 0) {
          if (length(enriched_functional_module()@merged_module) > 0) {
            relationship_heatmap_module_id <-
              unique(
                enriched_functional_module()@merged_module$functional_module_result$module
              )
            
            updateSelectInput(
              session,
              "relationship_heatmap_module_id",
              choices = stringr::str_sort(relationship_heatmap_module_id, numeric = TRUE),
              selected = NULL
            )
          }
        }
      })
      
      # Update the checkbox
      observe({
        req(input$relationship_heatmap_level)
        
        if (input$relationship_heatmap_level == "molecule") {
          updateCheckboxInput(session, "relationship_heatmap_wordcloud", value = FALSE)
          disable("relationship_heatmap_wordcloud")
        } else {
          enable("relationship_heatmap_wordcloud")
        }
        
        if (input$relationship_heatmap_level == "pathway") {
          updateCheckboxInput(session, "relationship_heatmap_show_cluster_tree", value = FALSE)
          disable("relationship_heatmap_show_cluster_tree")
          
          updateCheckboxInput(session, "relationship_heatmap_cluster_rows", value = FALSE)
          disable("relationship_heatmap_cluster_rows")
        } else {
          enable("relationship_heatmap_cluster_rows")
        }
      })
      
      # Observe generate module expression heatmap button click
      relationship_heatmap <- reactiveVal()
      relationship_heatmap_code <- reactiveVal()
      
      observeEvent(input$generate_relationship_heatmap, {
        if (is.null(enriched_functional_module())) {
          shinyalert::shinyalert(
            title = "No enriched functional module available",
            text = "Complete the previous steps to get an enriched functional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else if (!expression_data_valid()) {
          shinyalert::shinyalert(
            title = "Expression data required",
            text = paste0(
              "<div style='text-align: left;'>",
              "Please upload a valid expression data file before generating the heatmap.<br><br>",
              "<strong>Required format:</strong><br>",
              "• Data frame with 'id' column<br>",
              "• At least one numeric column for expression values<br>",
              "• Supported formats: .csv, .xlsx, .rda",
              "</div>"
            ),
            type = "warning",
            html = TRUE,
            confirmButtonCol = "#dd4b39"
          )
        } else {
          plot_heatmap_alert_id <- shinyalert::shinyalert(
            title = "Generating module-expression heatmap plot",
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
              plot <-
                mapa::plot_relationship_heatmap(
                  object = enriched_functional_module(),
                  level = input$relationship_heatmap_level,
                  expression_data = expression_data_uploaded(),
                  module_content_number_cutoff = NULL,
                  module_ids = input$relationship_heatmap_module_id,
                  scale_expression_data = input$relationship_heatmap_scale_expression_data,
                  cluster_rows = input$relationship_heatmap_cluster_rows,
                  show_cluster_tree = input$relationship_heatmap_show_cluster_tree,
                  wordcloud = input$relationship_heatmap_wordcloud,
                  llm_text = input$relationship_heatmap_llm_text,
                  functional_module_color = input$relationship_heatmap_functional_module_color,
                  pathway_color = input$relationship_heatmap_pathway_color,
                  molecule_color = input$relationship_heatmap_molecule_color,
                  functional_module_position_limits = c(
                    input$relationship_heatmap_functional_module_position_limits[1],
                    input$relationship_heatmap_functional_module_position_limits[2]
                  ),
                  pathway_position_limits = c(
                    input$relationship_heatmap_pathway_position_limits[1],
                    input$relationship_heatmap_pathway_position_limits[2]
                  ),
                  molecule_position_limits = c(
                    input$relationship_heatmap_molecule_position_limits[1],
                    input$relationship_heatmap_molecule_position_limits[2]
                  ),
                  heatmap_height_ratios = c(
                    input$relationship_heatmap_heatmap_height_ratios_1,
                    input$relationship_heatmap_heatmap_height_ratios_2
                  ),
                  network_height_ratios = c(
                    input$relationship_heatmap_network_height_ratios_1,
                    input$relationship_heatmap_network_height_ratios_2
                  )
                )
              
              relationship_heatmap(plot)
            },
            error = function(e) {
              shinyalert::closeAlert(id = plot_heatmap_alert_id)
              shinyalert::shinyalert(
                text = paste("Details:", e$message),
                html = TRUE,
                type = "error",
                confirmButtonCol = "#dd4b39"
              )
            }
          )
          
          shinyalert::closeAlert(id = plot_heatmap_alert_id)
          
          ###save code
          relationship_heatmap_module_ids <-
            if(!is.null(input$relationship_heatmap_module_id)) {
              paste0("c(",
                     paste0(
                       paste0('"',
                              input$relationship_heatmap_module_id,
                              '"'),
                       collapse = ", "
                     ),
                     ")")
            } else {
              "NULL"
            }
          
          relationship_heatmap_code_text <-
            sprintf(
              '
plot_relationship_heatmap(
  object = enriched_functional_module,
  level = %s,
  expression_data = expression_data,
  module_content_number_cutoff = %s,
  module_ids = %s,
  scale_expression_data = %s,
  cluster_rows = %s,
  show_cluster_tree = %s,
  wordcloud = %s,
  llm_text = %s,
  functional_module_color = %s,
  pathway_color = %s,
  molecule_color = %s,
  functional_module_position_limits = %s,
  pathway_position_limits = %s,
  molecule_position_limits = %s,
  heatmap_height_ratios = %s,
  network_height_ratios = %s)
        ',
              paste0('"', input$relationship_heatmap_level, '"'),
              if(is.null(input$relationship_heatmap_module_id)) input$relationship_heatmap_content_cutoff else "NULL",
              relationship_heatmap_module_ids,
              input$relationship_heatmap_scale_expression_data,
              input$relationship_heatmap_cluster_rows,
              input$relationship_heatmap_show_cluster_tree,
              input$relationship_heatmap_wordcloud,
              input$relationship_heatmap_llm_text,
              paste0('"', input$relationship_heatmap_functional_module_color, '"'),
              paste0('"', input$relationship_heatmap_pathway_color, '"'),
              paste0('"', input$relationship_heatmap_molecule_color, '"'),
              paste0("c(", paste0(input$relationship_heatmap_functional_module_position_limits, collapse = ", "), ")"),
              paste0("c(", paste0(input$relationship_heatmap_pathway_position_limits, collapse = ", "), ")"),
              paste0("c(", paste0(input$relationship_heatmap_molecule_position_limits, collapse = ", "), ")"),
              paste0("c(", paste0(c(input$relationship_heatmap_heatmap_height_ratios_1, input$relationship_heatmap_heatmap_height_ratios_2), collapse = ", "), ")"),
              paste0("c(", paste0(c(input$relationship_heatmap_network_height_ratios_1, input$relationship_heatmap_network_height_ratios_2), collapse = ", "), ")")
            )
          
          relationship_heatmap_code(relationship_heatmap_code_text)
        }
      })
      
      output$relationship_heatmap <-
        renderPlot({
          req(relationship_heatmap())
          relationship_heatmap()
        },
        res = 96)
      
      ####show code
      observeEvent(input$show_relationship_heatmap_code, {
        if (is.null(relationship_heatmap_code()) ||
            length(relationship_heatmap_code()) == 0) {
          shinyalert::shinyalert(
            title = "No code available",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else{
          code_content <-
            relationship_heatmap_code()
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
      
      output$download_relationship_heatmap <-
        downloadHandler(
          filename = function() {
            paste0(
              "module_expression_",
              input$relationship_heatmap_level,
              ".",
              input$relationship_heatmap_type
            )
          },
          content = function(file) {
            Cairo::CairoPDF(file = file, 
                            width = input$relationship_heatmap_width, 
                            height = input$relationship_heatmap_height)
            print(relationship_heatmap())
            dev.off()
          }
        )
      
      observe({
        if (is.null(relationship_heatmap()) ||
            length(relationship_heatmap()) == 0) {
          shinyjs::disable("download_relationship_heatmap")
        } else {
          shinyjs::enable("download_relationship_heatmap")
        }
      })
      
      ## Go to results tab ====
      ####if there is not enriched_functional_module, show a warning message
      observeEvent(input$go2results_1, {
        # Check if enriched_functional_module is available
        if (is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0) {
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched_functional_module available",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched_functional_module available",
            text = "Complete the previous steps to get an enriched fucntional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          tab_switch("results")
        }
      })

      observeEvent(input$go2results_2, {
        # Check if enriched_functional_module is available
        if (is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0) {
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched_functional_module available",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched_functional_module available",
            text = "Complete the previous steps to get an enriched fucntional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          tab_switch("results")
        }
      })

      observeEvent(input$go2results_3, {
        # Check if enriched_functional_module is available
        if (is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0) {
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched_functional_module available",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched_functional_module available",
            text = "Complete the previous steps to get an enriched fucntional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          tab_switch("results")
        }
      })

      observeEvent(input$go2results_4, {
        # Check if enriched_functional_module is available
        if (is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0) {
          # shiny::showModal(
          #   modalDialog(
          #     title = "Warning",
          #     "No enriched_functional_module available",
          #     easyClose = TRUE,
          #     footer = modalButton("Close")
          #   )
          # )
          shinyalert::shinyalert(
            title = "No enriched_functional_module available",
            text = "Complete the previous steps to get an enriched fucntional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          tab_switch("results")
        }
      })
      
      observeEvent(input$go2results_5, {
        if (is.null(enriched_functional_module()) ||
            length(enriched_functional_module()) == 0) {
          shinyalert::shinyalert(
            title = "No enriched_functional_module available",
            text = "Complete the previous steps to get an enriched fucntional module result or upload a result",
            type = "warning",
            confirmButtonCol = "#dd4b39"
          )
        } else {
          tab_switch("results")
        }
      })
    }
  )
}
