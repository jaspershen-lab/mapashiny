
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
#> rprojroot    (2.0.4   -> 2.1.1  ) [CRAN]
#> xfun         (0.52    -> 0.53   ) [CRAN]
#> evaluate     (1.0.3   -> 1.0.5  ) [CRAN]
#> Rcpp         (1.0.14  -> 1.1.0  ) [CRAN]
#> commonmark   (1.9.5   -> 2.0.0  ) [CRAN]
#> later        (1.4.2   -> 1.4.4  ) [CRAN]
#> utf8         (1.2.5   -> 1.2.6  ) [CRAN]
#> pillar       (1.10.2  -> 1.11.0 ) [CRAN]
#> tibble       (3.2.1   -> 3.3.0  ) [CRAN]
#> curl         (6.2.3   -> 7.0.0  ) [CRAN]
#> GenomeInfoDb (1.44.0  -> 1.44.2 ) [CRAN]
#> KEGGREST     (1.48.0  -> 1.48.1 ) [CRAN]
#> RSQLite      (2.3.11  -> 2.4.3  ) [CRAN]
#> purrr        (1.0.4   -> 1.1.0  ) [CRAN]
#> yulab.utils  (0.2.0   -> 0.2.1  ) [CRAN]
#> cowplot      (1.1.3   -> 1.2.0  ) [CRAN]
#> data.table   (1.17.4  -> 1.17.8 ) [CRAN]
#> ggforce      (0.4.2   -> 0.5.0  ) [CRAN]
#> fgsea        (1.34.0  -> 1.34.2 ) [CRAN]
#> BiocParallel (1.42.0  -> 1.42.1 ) [CRAN]
#> patchwork    (1.3.0   -> 1.3.2  ) [CRAN]
#> ggtree       (3.16.0  -> 3.16.3 ) [CRAN]
#> scatterpie   (0.2.4   -> 0.2.5  ) [CRAN]
#> ggtangle     (0.0.6   -> 0.0.7  ) [CRAN]
#> ggnewscale   (0.5.1   -> 0.5.2  ) [CRAN]
#> ggfun        (0.1.8   -> 0.2.0  ) [CRAN]
#> aplot        (0.2.5   -> 0.2.8  ) [CRAN]
#> waldo        (0.6.1   -> 0.6.2  ) [CRAN]
#> xml2         (1.3.8   -> 1.4.0  ) [CRAN]
#> shiny        (1.10.0  -> 1.11.1 ) [CRAN]
#> parallelly   (1.44.0  -> 1.45.1 ) [CRAN]
#> BiocManager  (1.30.25 -> 1.30.26) [CRAN]
#> BiocFileC... (2.16.0  -> 2.16.2 ) [CRAN]
#> spelling     (2.3.1   -> 2.3.2  ) [CRAN]
#> future       (1.49.0  -> 1.67.0 ) [CRAN]
#> Cairo        (1.6-2   -> 1.6-5  ) [CRAN]
#> Annotatio... (3.16.0  -> 3.16.1 ) [CRAN]
#> Installing 37 packages: rprojroot, xfun, evaluate, Rcpp, commonmark, later, utf8, pillar, tibble, curl, GenomeInfoDb, KEGGREST, RSQLite, purrr, yulab.utils, cowplot, data.table, ggforce, fgsea, BiocParallel, patchwork, ggtree, scatterpie, ggtangle, ggnewscale, ggfun, aplot, waldo, xml2, shiny, parallelly, BiocManager, BiocFileCache, spelling, future, Cairo, AnnotationHub
#> 
#> The downloaded binary packages are in
#>  /var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T//RtmpBLrSoE/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>      checking for file ‘/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpBLrSoE/remotes1615423e11a66/jaspershen-lab-mapashiny-254c99a/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/6d/g00_j1mn3wddb038xh8zrn6h0000gn/T/RtmpBLrSoE/remotes1615423e11a66/jaspershen-lab-mapashiny-254c99a/DESCRIPTION’
#>   ─  preparing ‘mapashiny’:
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  excluding invalid files
#>      Subdirectory 'R' contains invalid file names:
#>      ‘_disable_autoload.R’
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘mapashiny_1.1.0.tar.gz’
#>      
#> 
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
#> [1] "2025-08-31 12:29:53 +08"
```

Here are the tests results and package coverage:

``` r
# devtools::check(quiet = TRUE)
```

``` r
covr::package_coverage()
#> mapashiny Coverage: 49.65%
#> R/run_app.R: 0.00%
#> R/5_pathway_similarity.R: 42.61%
#> R/8_llm_interpretation.R: 46.43%
#> R/4_enrich_pathway.R: 49.89%
#> R/9_data_visualization.R: 50.53%
#> R/3_upload_data.R: 52.57%
#> R/10_results.R: 53.85%
#> R/6_pathway_clustering.R: 54.47%
#> R/app_server.R: 97.67%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
```
