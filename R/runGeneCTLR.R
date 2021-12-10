#' Launch Shiny App for GeneCTLR
#'
#' A function that launches the Shiny app for GeneCTLR.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' GeneCTLR::runGeneCTLR()
#' }
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials. \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp
runGeneCTLR <- function() {
  appDir <- system.file("<FolderWithApp.R>",
                        package = "GeneCTLR")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}

# [END]
