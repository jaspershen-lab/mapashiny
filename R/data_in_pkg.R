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
#' A dataset containing example metabolomics data for pathway analysis.
#' Includes metabolite identifiers and their corresponding HMDB (Human Metabolome
#' Database) identifiers.
#'
#' @format A data frame with 17 rows and 2 columns:
#' \describe{
#'   \item{metabolite_id}{Character vector of metabolite identifiers (metabolite_1, metabolite_2, etc.)}
#'   \item{hmdbid}{Character vector of HMDB identifiers for metabolite database mapping}
#' }

"example_met_data"