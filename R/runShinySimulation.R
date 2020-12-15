#' Reactive Gboot Simulations with Shiny
#'
#' This function will let you tweak simulated data parameters and view results
#' @return Runs a shiny app in your local session
#' @examples
#' \dontrun{
#' runShinySimulation()
#' }
#' @importFrom shiny runApp eventReactive navbarPage tabPanel
#' sidebarLayout sidebarPanel numericInput selectInput actionButton mainPanel
#' plotOutput renderPlot shinyApp
#' @export
runShinySimulation <- function() {
  appDir <- system.file("shiny-examples", "app.R", package = "Gboot")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Gboot`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
