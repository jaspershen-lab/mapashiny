
<!-- README.md is generated from README.Rmd. Please edit that file -->

## `mapashiny`: A user-friendly shinyapp designed for mapa. <a href="https://github.com/jaspershen-lab/mapashiny"><img src="inst/app/www/mapa_logo.png" align="right" height="139" alt="maapashiny github repo" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jaspershen-lab/mapashiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jaspershen-lab/mapashiny/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

You can install the development version of `mapashiny` like so:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

remotes::install_github(
  "jaspershen-lab/mapashiny",
  dependencies = TRUE,
  repos        = BiocManager::repositories(),
  upgrade      = "ask"
)
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'mapashiny' from a github remote, the SHA1 (5188ba6d) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Run

You can launch the application by running:

``` r
mapashiny::run_mapa_shiny()
```

## About

You are reading the doc about version : 1.1.0

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-08-31 12:33:02 +08"
```

Here are the tests results and package coverage:

``` r
# devtools::check(quiet = TRUE)
```

``` r
covr::package_coverage()
#> mapashiny Coverage: 49.40%
#> R/run_app.R: 0.00%
#> R/5_pathway_similarity.R: 42.61%
#> R/8_llm_interpretation.R: 43.85%
#> R/4_enrich_pathway.R: 49.89%
#> R/9_data_visualization.R: 50.53%
#> R/3_upload_data.R: 52.57%
#> R/10_results.R: 53.02%
#> R/6_pathway_clustering.R: 54.47%
#> R/app_server.R: 84.00%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
```
