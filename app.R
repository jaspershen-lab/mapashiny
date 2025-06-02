# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
# options( "golem.app.prod" = TRUE)
# mapashiny::run_app() # add parameters here (if any)

# Load all R files directly instead of using pkgload
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file, local = FALSE)
}

# Set production mode
options("golem.app.prod" = TRUE)

# Run the app directly instead of using mapashiny::run_app()
run_app() # add parameters here (if any)