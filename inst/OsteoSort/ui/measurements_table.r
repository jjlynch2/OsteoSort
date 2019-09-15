measurements_table <- tabPanel("Measurements",icon = icon("archive", lib="font-awesome"),
	DT::dataTableOutput('measurement_conversion_table')
)
