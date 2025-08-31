intro_cleaned_content <- grep("<(/?(html|head|body))>", 
                              readLines(app_sys("app/www/introduction.html")),
                              invert = TRUE, value = TRUE)

load("inst/app/www/met_org_kegg_choices.rda")

# tutorial_cleaned_content <- grep("<(/?(html|head|body))>", 
#                                  readLines(app_sys("app/www/tutorials.html")),
#                                  invert = TRUE, value = TRUE)

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      skin = "red",
      
      shinydashboard::dashboardHeader(title = "MAPA"),
      
      ## sidebar of the app ====
      shinydashboard::dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem(text = "Introduction", tabName = "introduction", icon = icon("info-circle")),
          # menuItem(text = "Tutorial", tabName = "tutorial", icon = icon("book")),
          menuItem(text = "Data Upload", tabName = "upload_data", icon = icon("upload")),
          menuItem(text = "Pathway Enrichment", tabName = "enrich_pathways", icon = icon("cogs")),
          menuItem(text = "Pathway Similarity Calculation", tabName = "pathway_similarity", icon = icon("project-diagram")),
          menuItem(text = "Module Identification", tabName = "pathway_clustering", icon = icon("sitemap")),
          # menuItem(text = "Pathway Clustering", tabName = NULL, icon = icon("sitemap"),
          #          menuItem(text = HTML("Method1: SimCluster"), tabName = NULL,
          #                   menuSubItem(text = "Step1: Merge Pathways", tabName = "merge_pathways", icon = NULL),
          #                   menuSubItem(text = "Step2: Merge Modules", tabName = "merge_modules", icon = NULL)
          #          ),
          #          menuItem(text = HTML("Method2: EmbedCluster"), tabName = "embed_cluster_pathways")
          # ),
          menuItem(text = "Module Annotation", tabName = "llm_interpretation", icon = icon("brain")),
          menuItem(text = "Data Visualization", tabName = "data_visualization", icon = icon("chart-line")),
          menuItem(text = "Results & Report", tabName = "results", icon = icon("clipboard-list"))
        )
      ),
      
      ## dashboard body code ====
      shinydashboard::dashboardBody(
        shinyjs::useShinyjs(),
        
        div(
          id = "loading",
          hidden = TRUE,
          class = "loading-style",
          "",
          tags$img(src = "loading.gif",
                   height = "200px")
        ),
        
        ### tabitems ====
        tabItems(
          #### 1. Introduction tab ====
          tabItem(tabName = "introduction",
                  fluidPage(
                    titlePanel("Introduction of MAPA"),
                    fluidRow(
                      column(12,
                             htmltools::HTML(intro_cleaned_content)
                      )
                    )
                  )),
          
          # #### 2. Tutorial tab ====
          # tabItem(tabName = "tutorial",
          #         fluidPage(
          #           titlePanel("Tutorials of MAPA"),
          #           fluidRow(
          #             column(12,
          #                    htmltools::HTML(tutorial_cleaned_content)
          #             )
          #           )
          #         )),
          
          #### 3. Upload data tab ====
          upload_data_ui("upload_data_tab"),
          
          #### 4. Enrich pathways tab ====
          enrich_pathway_ui("enrich_pathway_tab"),
          
          # #### 5-6. Pathway clustering tab ===
          # #### 5a. Merge pathways tab ====
          # merge_pathways_ui("merge_pathways_tab"),
          # 
          # #### 6a. Merge modules tab ====
          # merge_modules_ui("merge_modules_tab"),
          # 
          # #### 5-6b. Embed and cluster pathways tab =====
          # embed_cluster_pathways_ui("embed_cluster_pathways_tab"),
          # Replace the old clustering tabs with the new ones
          #### 5. Pathway Similarity tab ====
          pathway_similarity_ui("pathway_similarity_tab"),
          
          #### 6. Pathway Clustering tab ====
          pathway_clustering_ui("pathway_clustering_tab"),
          
          #### 7. Translation tab ====
          
          #### 8. LLM Interpretation tab ====
          llm_interpretation_ui("llm_interpretation_tab"),
          
          #### 9. Data visualization tab ====
          data_visualization_ui("data_visualization_tab"),
          
          #### 10. Result and report tab =====
          results_ui("results_tab")
        ),
        
        ### footer ====
        tags$footer(
          div(
            class = "app-footer",
            tags$img(
              src = "www/shen_lab_logo.png",
              class = "footer-logo"
            ),
            div(
              class = "footer-content",
              HTML("The Shen Lab at Nanyang Technological University Singapore"),
              HTML("<br>"),
              tags$a(
                href = "http://www.shen-lab.org",
                target = "_blank",
                class = "footer-link",
                tags$i(class = "fa fa-house footer-icon"),
                " Shen Lab"
              ),
              tags$a(
                href = "https://www.shen-lab.org/#contact",
                target = "_blank",
                class = "footer-link",
                tags$i(class = "fa fa-envelope footer-icon"),
                " Email"
              ),
              tags$a(
                href = "https://github.com/jaspershen/mapa",
                target = "_blank",
                class = "footer-link",
                tags$i(class = "fa fa-github footer-icon"),
                " GitHub"
              )
            ),
            tags$img(
              src = "www/mapa_logo.png",
              class = "footer-logo-right"
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mapashiny"
    ),
    # Add external CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "www/app.css"),
    # Add external JS file
    tags$script(src = "www/app.js")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
