output$template <- downloadHandler(
	filename <- function() {
		"template.csv"
	},
	content <- function(file) {
		file.copy("./extdata/template.csv", file)
	},
)

output$example <- downloadHandler(
	filename <- function() {
		"example_data.csv"
	},
	content <- function(file) {
		file.copy("./extdata/example_data.csv", file)
	},
)
