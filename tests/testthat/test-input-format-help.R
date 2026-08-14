test_that("input format help adapts to gene and metabolite inputs", {
  gene_help <- as.character(.input_format_popover(
    query_type = "gene",
    id_type = "symbol",
    multi_omics = FALSE
  ))
  expect_match(gene_help, "Required ID column", fixed = TRUE)
  expect_match(gene_help, "symbol", fixed = TRUE)
  expect_match(gene_help, "any gene/metabolite ID type", fixed = TRUE)
  expect_match(gene_help, "case-sensitive", fixed = TRUE)
  expect_match(gene_help, "For GSEA", fixed = TRUE)
  expect_match(gene_help, "input-format-trigger", fixed = TRUE)
  expect_match(gene_help, "input-format-glyph", fixed = TRUE)
  expect_false(grepl("fa-circle-info", gene_help, fixed = TRUE))

  metabolite_help <- as.character(.input_format_popover(
    query_type = "metabolite",
    id_type = "keggid",
    multi_omics = TRUE
  ))
  expect_match(metabolite_help, "keggid", fixed = TRUE)
  expect_match(metabolite_help, "cpd_name", fixed = TRUE)
  expect_match(metabolite_help, "human-readable", fixed = TRUE)
  expect_match(metabolite_help, "network visualizations", fixed = TRUE)
  expect_match(metabolite_help, "functional-module\\s+results")
  expect_match(metabolite_help,
               "results\\.\\s*</li>\\s*<li>\\s*An optional numeric")
  expect_false(grepl("network construction", metabolite_help, fixed = TRUE))
  expect_match(metabolite_help, "diff_metric", fixed = TRUE)
})

test_that("custom stylesheet URL is cache-busted", {
  resources <- htmltools::renderTags(golem_add_external_resources())$head

  expect_match(resources, "www/custom.css?v=", fixed = TRUE)
})

test_that("upload pages contain input-format help placeholders", {
  single_ui <- as.character(mod_so_upload_ui("single"))
  multi_ui <- as.character(mod_mo_upload_ui("multi"))

  expect_match(single_ui, "single-input_format_info", fixed = TRUE)
  expect_match(multi_ui, "multi-input_format_T", fixed = TRUE)
  expect_match(multi_ui, "multi-input_format_P", fixed = TRUE)
  expect_match(multi_ui, "multi-input_format_M", fixed = TRUE)
})
