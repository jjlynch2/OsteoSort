#' This function starts the OsteoSort app with shiny
#' 
#' @keywords OsteoSort
#' @export
#' @examples
#' OsteoSort()



OsteoSort <- function()
{
	library(shiny)
	library(shinyBS)
	runApp(system.file("OsteoSort", package = "osteosort"), launch.browser = TRUE)
}