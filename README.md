
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
#> Downloading GitHub repo jaspershen-lab/mapashiny@HEAD
#> 'getOption("repos")' replaces Bioconductor standard repositories, see
#> 'help("repositories", package = "BiocManager")' for details.
#> Replacement repositories:
#>     CRAN: https://cran.rstudio.com/
#> pkgbuild   (1.4.7        -> 1.4.8       ) [CRAN]
#> openssl    (2.3.2        -> 2.3.3       ) [CRAN]
#> curl       (6.2.2        -> 6.2.3       ) [CRAN]
#> data.table (1.17.2       -> 1.17.4      ) [CRAN]
#> mapa       (d1cc7c121... -> 96b62a3a4...) [GitHub]
#> Installing 4 packages: pkgbuild, openssl, curl, data.table
#> Installing packages into '/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpZx7WWj/temp_libpath31b42364024d'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T//RtmpU4EDjT/downloaded_packages
#> Downloading GitHub repo jaspershen-lab/mapa@HEAD
#> RcppArmad... (14.4.2-1 -> 14.4.3-1) [CRAN]
#> Installing 1 packages: RcppArmadillo
#> Installing package into '/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpZx7WWj/temp_libpath31b42364024d'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T//RtmpU4EDjT/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpU4EDjT/remotes4dc74611ff2f/jaspershen-lab-mapa-96b62a3a4e84bfa6f0dca94c90efc2c0286a7901/DESCRIPTION’ ... OK
#> * preparing ‘mapa’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘mapa_0.1.30.tar.gz’
#> Installing package into '/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpZx7WWj/temp_libpath31b42364024d'
#> (as 'lib' is unspecified)
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpU4EDjT/remotes4dc75da6172c/jaspershen-lab-mapashiny-271fb5f/DESCRIPTION’ ... OK
#> * preparing ‘mapashiny’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘mapashiny_0.1.0.tar.gz’
#> Installing package into '/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpZx7WWj/temp_libpath31b42364024d'
#> (as 'lib' is unspecified)
```

## Run

You can launch the application by running:

``` r
mapashiny::run_app()
```

## About

You are reading the doc about version : 0.1.0

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-05-29 20:12:30 +08"
```

Here are the tests results and package coverage:

``` r
# devtools::check(quiet = TRUE)
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
