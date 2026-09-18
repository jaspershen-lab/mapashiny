# ── Edge database caching utilities ──────────────────────────────────────────
# STRING and Reactome files are managed by the mapa package and stored under:
#   tools::R_user_dir("mapa", which = "cache") / edge_database /
# These helpers delegate to mapa's exported functions so that mapashiny and
# the mapa CLI share a single cache location.

.edge_db_dir <- function() {
  file.path(tools::R_user_dir("mapa", which = "cache"), "edge_database")
}

.db_status_check <- function() {
  mapa::mapa_db_status_check()
}

# on_file(label): optional callback called just before each new download starts
.ensure_edge_databases <- function(on_file = NULL) {
  mapa::mapa_ensure_edge_databases(on_file = on_file)
}

# Derive NCBI taxon ID from an OrgDb package name string.
.orgdb_to_taxon_id <- function(org_str) {
  lookup <- c(
    "org.Hs.eg.db" = 9606L,
    "org.Mm.eg.db" = 10090L,
    "org.Rn.eg.db" = 10116L,
    "org.Dr.eg.db" = 7955L
  )
  result <- lookup[org_str %||% ""]
  if (length(result) == 0L || is.na(result)) return(9606L)
  unname(result)
}
