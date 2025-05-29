
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{mapashiny}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{mapashiny}` like so:

``` r
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# 
# remotes::install_github(
#   "jaspershen-lab/mapashiny",
#   dependencies = TRUE,
#   repos        = BiocManager::repositories(),
#   upgrade      = "ask" 
# )
```

## Run

You can launch the application by running:

``` r
mapashiny::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-05-29 09:54:22 +08"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> 
#> 
#> ℹ Loading mapashiny
#> ── R CMD check results ─────────────────────────────── mapashiny 0.0.0.9000 ────
#> Duration: 2m 34.8s
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard files/directories found at top level:
#>     ‘deprec-R’ ‘deprec-renv’
#> 
#> ❯ checking R code for possible problems ... [21s/21s] NOTE
#>   data_visualization_server : <anonymous>: no visible global function
#>     definition for ‘plot_pathway_bar’
#>   data_visualization_server : <anonymous> : <anonymous>: no visible
#>     global function definition for ‘ggsave’
#>   data_visualization_server : <anonymous>: no visible global function
#>     definition for ‘plot_similarity_network’
#>   data_visualization_server : <anonymous>: no visible global function
#>     definition for ‘plot_module_info’
#>   data_visualization_server : <anonymous>: no visible global function
#>     definition for ‘filter_functional_module’
#>   data_visualization_server : <anonymous>: no visible global function
#>     definition for ‘plot_relationship_network’
#>   embed_cluster_pathways_server : <anonymous>: no visible global function
#>     definition for ‘get_bioembedsim’
#>   embed_cluster_pathways_server : <anonymous>: no visible global function
#>     definition for ‘merge_pathways_bioembedsim’
#>   embed_cluster_pathways_server : <anonymous>: no visible global function
#>     definition for ‘plot_similarity_network’
#>   enrich_pathway_server : <anonymous>: no visible binding for global
#>     variable ‘describtion’
#>   enrich_pathway_server : <anonymous> : <anonymous>: no visible binding
#>     for global variable ‘describtion’
#>   id_conversion: no visible global function definition for ‘%>%’
#>   id_conversion: no visible binding for global variable ‘ENTREZID’
#>   id_conversion: no visible binding for global variable ‘.’
#>   id_conversion: no visible binding for global variable ‘HMDB.ID’
#>   id_conversion: no visible binding for global variable ‘KEGG.ID’
#>   id_conversion: no visible global function definition for ‘across’
#>   id_conversion: no visible global function definition for ‘everything’
#>   id_conversion: no visible binding for global variable ‘.data’
#>   llm_interpretation_server : <anonymous>: no visible binding for global
#>     variable ‘future’
#>   llm_interpretation_server : <anonymous>: no visible binding for global
#>     variable ‘promises’
#>   llm_interpretation_server : <anonymous>: no visible binding for global
#>     variable ‘mapa’
#>   merge_modules_server : <anonymous>: no visible global function
#>     definition for ‘merge_modules’
#>   merge_modules_server : <anonymous>: no visible global function
#>     definition for ‘plot_similarity_network’
#>   merge_pathways_server : <anonymous>: no visible global function
#>     definition for ‘merge_pathways’
#>   merge_pathways_server : <anonymous>: no visible global function
#>     definition for ‘plot_similarity_network’
#>   results_server : <anonymous> : <anonymous>: no visible global function
#>     definition for ‘zip’
#>   run_app: no visible binding for global variable ‘mapa’
#>   upload_data_server : <anonymous>: no visible global function definition
#>     for ‘data’
#>   upload_data_server : <anonymous>: no visible binding for global
#>     variable ‘example_ora_data’
#>   upload_data_server : <anonymous>: no visible binding for global
#>     variable ‘example_gsea’
#>   upload_data_server : <anonymous>: no visible binding for global
#>     variable ‘example_met_data’
#>   upload_data_server : <anonymous>: no visible global function definition
#>     for ‘read.csv’
#>   Undefined global functions or variables:
#>     %>% . .data ENTREZID HMDB.ID KEGG.ID across data describtion
#>     everything example_gsea example_met_data example_ora_data
#>     filter_functional_module future get_bioembedsim ggsave mapa
#>     merge_modules merge_pathways merge_pathways_bioembedsim
#>     plot_module_info plot_pathway_bar plot_relationship_network
#>     plot_similarity_network promises read.csv zip
#>   Consider adding
#>     importFrom("utils", "data", "read.csv", "zip")
#>   to your NAMESPACE file.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 3 notes ✖
```

``` r
covr::package_coverage()
#> mapashiny Coverage: 56.43%
#> R/run_app.R: 0.00%
#> R/utils.R: 0.00%
#> R/9_data_visualization.R: 51.20%
#> R/4_enrich_pathway.R: 52.45%
#> R/8_llm_interpretation.R: 52.73%
#> R/10_results.R: 53.85%
#> R/6_merge_modules.R: 55.59%
#> R/5_embed_cluster_pathways.R: 58.97%
#> R/3_upload_data.R: 59.85%
#> R/5_merge_pathways.R: 63.92%
#> R/app_server.R: 97.83%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
```
