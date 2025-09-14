#' Example Dataset for Over-Representation Analysis
#'
#' Significantly downregulated proteins from the muscle of aging mice
#' (6 vs 30 months, male C57BL/6). Contains 66 proteins with |log2FC| ≥ 0.5
#' and FDR < 0.05, suitable for pathway enrichment analysis.
#'
#' @format A tibble with 66 rows and 3 columns:
#' \describe{
#'   \item{symbol}{Gene symbols}
#'   \item{log2FC (6 vs 30mo)}{Log2 fold changes (all negative)}
#'   \item{FDR (6 vs 30mo)}{False discovery rates (all < 0.05)}
#' }
#'
#' @source
#' Takasugi, M., et al. An atlas of the aging mouse proteome reveals the
#' features of age-related post-transcriptional dysregulation.
#' \emph{Nat Commun} \strong{15}, 8520 (2024).
#' \doi{10.1038/s41467-024-52845-x}
#'

"example_ora_data"

#' Example Dataset for Gene Set Enrichment Analysis
#'
#' Complete proteomics dataset from liver of aging mice (6 vs 30 months,
#' male C57BL/6). Contains 5,290 proteins with fold changes and adjusted
#' p-values, suitable for gene set enrichment analysis (GSEA).
#'
#' @format A tibble with 5,290 rows and 3 columns:
#' \describe{
#'   \item{symbol}{Gene symbols}
#'   \item{fc}{Fold changes (6 vs 30 months)}
#'   \item{p_value_adjust}{Adjusted p-values}
#' }
#'
#' @source
#' Takasugi, M., et al. An atlas of the aging mouse proteome reveals the
#' features of age-related post-transcriptional dysregulation.
#' \emph{Nat Commun} \strong{15}, 8520 (2024).
#' \doi{10.1038/s41467-024-52845-x}
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