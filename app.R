# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Redirect R package cache to /root/.cache on the server
if (file.exists("/root/.cache")) {
  Sys.setenv(XDG_CACHE_HOME = "/root/.cache")
}

# Load all R files directly instead of using pkgload
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file, local = FALSE)
}

# Set production mode
options("golem.app.prod" = TRUE)

# On the server, pre-downloaded databases are used directly.
.server_data <- "/srv/shiny-server/mapa-shiny/data"
if (file.exists(file.path(.server_data, "mapa_pathway_embedding_v1.sqlite"))) {
  options(mapa.embedding_db_path =
            file.path(.server_data, "mapa_pathway_embedding_v1.sqlite"))
}
if (dir.exists(file.path(.server_data, "string_db"))) {
  options(mapa.edge_db_dir = .server_data)
}
rm(.server_data)

# Run the app directly instead of using mapashiny::run_app()
run_mapa_shiny() # add parameters here (if any)