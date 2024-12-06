#' Run the MAPA Shiny Application
#'
#' This function launches the MAPA Shiny application by calling the `shinyApp` function
#' and wrapping it with golem options.
#'
#' @param onStart A function that is executed each time the Shiny app is started. Defaults to `NULL`.
#' @param options A list of options to configure the Shiny application. Defaults to an empty list.
#' @param enableBookmarking Either `NULL`, "server", or "url" to enable bookmarking of the Shiny application. Defaults to `NULL`.
#' @param uiPattern A character string that defines the URL pattern for the user interface. Defaults to "/".
#' @param ... Additional arguments to pass to `golem_opts`.
#'
#' @return This function does not return a value; it is used for its side effects to run the Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   run_mapa_shiny()
#' }
#'
#' @export

run_mapa_shiny <-
  function(onStart = NULL,
           options = list(),
           enableBookmarking = NULL,
           uiPattern = "/",
           ...) {
    golem::with_golem_options(
      app = shiny::shinyApp(
        ui = app_ui,
        server = app_server,
        onStart = onStart,
        options = options,
        enableBookmarking = enableBookmarking,
        uiPattern = uiPattern
      ),
      golem_opts = list(...)
    )
  }