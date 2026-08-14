# ── Input-data format help ──────────────────────────────────────────────────

.input_id_example <- function(id_type) {
  switch(
    id_type,
    symbol   = "TP53",
    ensembl  = "ENSG00000141510",
    entrezid = "7157",
    uniprot  = "P04637",
    keggid   = "C00031",
    hmdbid   = "HMDB0000122",
    "example_id"
  )
}

.input_format_popover <- function(query_type,
                                  id_type,
                                  layer_label = NULL,
                                  multi_omics = FALSE) {
  is_gene <- identical(query_type, "gene")
  id_type <- id_type %||% if (is_gene) "symbol" else "keggid"
  id_example <- .input_id_example(id_type)
  title <- paste0(layer_label %||% if (is_gene) "Gene" else "Metabolite",
                  " input format")

  additional_requirement <- if (is_gene && !multi_omics) {
    tags$li(
      tags$strong("For GSEA: "),
      "include at least one additional numeric ranking column (for example ",
      tags$code("log2FC"), "). It must not contain missing or infinite values. ",
      "ORA does not require a ranking column."
    )
  } else if (is_gene) {
    tags$li(
      "Other columns, such as ", tags$code("beta"), ", ",
      tags$code("log2FC"), ", or ", tags$code("pvalue"),
      ", are optional and will be preserved. Multi-omics enrichment uses ORA, ",
      "so a ranking column is not required."
    )
  } else if (multi_omics) {
    tagList(
      tags$li(
        tags$code("cpd_name"), " is also required to provide a human-readable ",
        "name for each metabolite in network visualizations and functional-module ",
        "results."
      ),
      tags$li(
        "An optional numeric ", tags$code("diff_metric"),
        " column can be supplied for metabolite colouring."
      )
    )
  } else {
    tags$li(
      "Other annotation or statistic columns are optional and will be preserved."
    )
  }

  example_header <- if (is_gene) {
    if (multi_omics) paste(id_type, "beta", sep = ",")
    else paste(id_type, "log2FC", sep = ",")
  } else if (multi_omics) {
    paste(id_type, "cpd_name", "diff_metric", sep = ",")
  } else {
    paste(id_type, "score", sep = ",")
  }
  example_row <- if (is_gene) {
    paste(id_example, "1.25", sep = ",")
  } else if (multi_omics) {
    paste(id_example, "D-Glucose", "1.25", sep = ",")
  } else {
    paste(id_example, "1.25", sep = ",")
  }

  bslib::popover(
    trigger = tags$button(
      type = "button",
      class = "input-format-trigger",
      style = paste(
        "display:inline-flex;align-items:center;justify-content:center;",
        "width:1.1rem;height:1.1rem;padding:0;border:1px solid currentColor;",
        "border-radius:50%;background:transparent;color:var(--mapa-muted,#64748B);",
        "font-size:.7rem;line-height:1;vertical-align:middle;cursor:pointer;"
      ),
      `aria-label` = paste0("Show ", title),
      tags$span(class = "input-format-glyph", `aria-hidden` = "true", "i"),
      tags$span(class = "visually-hidden", "Input format")
    ),
    title = title,
    placement = "auto",
    options = list(customClass = paste(
      "input-format-popover",
      if (multi_omics) "input-format-popover-mo" else "input-format-popover-so"
    )),
    tags$p(
      class = "mb-2",
      "Upload a comma-separated ", tags$code(".csv"), " file or an ",
      tags$code(".xlsx"), " workbook. The first row must contain column names; ",
      "for Excel files, the first worksheet is read."
    ),
    tags$ul(
      class = "mb-2 ps-3",
      tags$li(
        "You may use any gene/metabolite ID type available in the dropdown; ",
        "no particular identifier system is mandatory."
      ),
      tags$li(
        tags$strong("Required ID column for the current selection: "),
        tags$code(id_type), "."
      ),
      tags$li(
        "The uploaded column name is case-sensitive and must exactly match ",
        "the selected value shown above."
      ),
      tags$li(
        "Use one feature per row and avoid empty ID cells or merged cells."
      ),
      additional_requirement
    ),
    tags$div(class = "small fw-semibold mb-1", "Minimal example"),
    tags$pre(
      class = "input-format-example mb-0",
      paste(example_header, example_row, sep = "\n")
    )
  )
}
