#' Launch the HiHai Shiny App
#'
#' @export
#' @importFrom shiny runApp
launch_app <- function() {
  appDir <- system.file("shiny", package = "hihai")
  shiny::runApp(appDir)
}
