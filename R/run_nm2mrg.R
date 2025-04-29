#' Launch the Shiny App
#'
#' @export
run_nm2mrg <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required to run this app.")
  }
  appDir <- system.file("shiny", package = "nm2mrg")
  shiny::runApp(appDir, display.mode = "normal")
}
