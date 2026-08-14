# ── Multi-omics input helpers ───────────────────────────────────────────────

# Count the omics layers that currently contain data/results.
.mo_available_layer_count <- function(layers) {
  sum(!vapply(layers, is.null, logical(1)))
}

# MAPA multi-omics analysis requires any two of T, P, and M.
.mo_has_minimum_layers <- function(layers, minimum = 2L) {
  .mo_available_layer_count(layers) >= minimum
}
