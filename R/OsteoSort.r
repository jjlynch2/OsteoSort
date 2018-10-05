#' This function starts the OsteoSort app with shiny
#' 
#' @keywords OsteoSort
#' @export
#' @examples
#' OsteoSort()



OsteoSort <- function()
{
	library(shiny)
	runApp(system.file("OsteoSort", package = "OsteoSort"), launch.browser = TRUE)
}