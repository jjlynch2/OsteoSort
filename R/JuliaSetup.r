RJS <- function(libraries = FALSE) {
	if(libraries) {
		pkg = c("Statistics", "Optim", "Rmath", "GLM")
		p_len = length(pkg) + 1
		withProgress(message = 'Loading analytical environment', detail = '', value = 0, min=0, max=p_len, {
			julia_setup()
			for(i in pkg) {
				incProgress(amount = 1, message = paste("Loading ", i, " library", sep=""))
				print(paste("Loading Julia package: ", i, sep=""))
				julia_install_package_if_needed(i)
				julia_library(i)
			}
			incProgress(amount = 1, message = "Importing source code")
			julia_source(system.file("jl", "t_test_plot.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "t_test.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "yeojohnson.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "z_test.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression_plot.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression_helpers.jl", package = "OsteoSort"))
		})
	}
		
	JV <- JuliaCall:::julia_line(c("-e", "print(VERSION)"), stdout=TRUE)
	return(JV)
}
