## code to prepare example datasets

# Single-omics: gene ORA example (mouse, symbol IDs)
example_ora_data <- as.data.frame(
  readxl::read_xlsx("data-raw/single_omics/demo_data/example_ora.xlsx")
)
usethis::use_data(example_ora_data, overwrite = TRUE)

# Single-omics: gene GSEA example (mouse, symbol IDs)
example_gsea_data <- as.data.frame(
  readxl::read_xlsx("data-raw/single_omics/demo_data/example_gsea.xlsx")
)
usethis::use_data(example_gsea_data, overwrite = TRUE)

# Single-omics: metabolite example (KEGG IDs, human)
example_met_data <- as.data.frame(
  readxl::read_xlsx("data-raw/single_omics/demo_data/example_met.xlsx")
)
usethis::use_data(example_met_data, overwrite = TRUE)

# Multi-omics: transcriptomics demo (human, symbol IDs)
demo_mo_T_data <- as.data.frame(
  readxl::read_xlsx("data-raw/multi-omics/demo_data/demo_up_T_list.xlsx")
)
usethis::use_data(demo_mo_T_data, overwrite = TRUE)

# Multi-omics: proteomics demo (human, symbol IDs)
demo_mo_P_data <- as.data.frame(
  readxl::read_xlsx("data-raw/multi-omics/demo_data/demo_up_P_list.xlsx")
)
usethis::use_data(demo_mo_P_data, overwrite = TRUE)

# Multi-omics: metabolomics demo (human/hsa, KEGG IDs)
demo_mo_M_data <- as.data.frame(
  readxl::read_xlsx("data-raw/multi-omics/demo_data/demo_M_list.xlsx")
)
usethis::use_data(demo_mo_M_data, overwrite = TRUE)
