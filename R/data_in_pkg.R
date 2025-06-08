#' Example ORA Data
#'
#' A dataset containing example data for Over-Representation Analysis (ORA).
#' Contains gene identifiers and their corresponding Ensembl IDs for pathway
#' enrichment analysis.
#'
#' @format A data frame with 119 rows and 2 columns:
#' \describe{
#'   \item{variable_id}{Character vector of gene identifiers (gene_1, gene_2, etc.)}
#'   \item{ensembl}{Character vector of Ensembl gene IDs (ENSG format)}
#' }
#' 

"example_ora_data"

#' Example GSEA Data
#'
#' A dataset containing example data for Gene Set Enrichment Analysis (GSEA).
#' Includes gene annotations, expression fold changes, and statistical significance
#' measures for differential expression analysis.
#'

"example_gsea_data"

#' Example Metabolomics Data
#'
#' A dataset containing example metabolomics data for pathway enrichment analysis.
#' This dataset includes metabolite identifiers, KEGG compound annotations, and 
#' statistical measures from differential abundance analysis.
#'
#' @format A tibble with 106 rows and 4 variables:
#' \describe{
#'   \item{variable_id}{Character vector of metabolite identifiers in the format 
#'                     "M[mass]T[retention_time]_[ionization_mode]"}
#'   \item{keggid}{Character vector of KEGG compound identifiers (e.g., "C05466"). 
#'                 Contains NA values for unidentified metabolites}
#'   \item{fdr}{Numeric vector of false discovery rate adjusted p-values from 
#'              differential abundance testing}
#'   \item{score}{Numeric vector of abundance fold change scores or effect sizes}
#' }
#'

"example_met_data"