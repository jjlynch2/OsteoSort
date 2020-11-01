output$postmortem_template <- downloadHandler(
	filename <- function() {
		"postmortem_template.csv"
	},
	content <- function(file) {
		file.copy(system.file("extdata", 'postmortem_template.csv', package = "OsteoSort"), file)
	},
)

output$antemortem_template <- downloadHandler(
	filename <- function() {
		"antemortem_template.csv"
	},
	content <- function(file) {
		file.copy(system.file("extdata", 'antemortem_template.csv', package = "OsteoSort"), file)
	},
)

output$example_data <- downloadHandler(
	filename <- function() {
		"Example_Data.zip"
	},
	content <- function(file) {
		file.copy(system.file("extdata", 'Example_Data.zip', package = "OsteoSort"), file)
	},
)
