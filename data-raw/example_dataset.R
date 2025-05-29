## code to prepare `example_dataset` dataset goes here
example_ora_data <- read.csv("data-raw/example_enrich_pathway.csv")
usethis::use_data(example_ora_data, overwrite = TRUE)

example_gsea_data <- read.csv("data-raw/example_gsea.csv")
usethis::use_data(example_gsea_data, overwrite = TRUE)

example_met_data <- read.csv("data-raw/example_enrich_pathway_metabolite.csv")
usethis::use_data(example_met_data, overwrite = TRUE)
