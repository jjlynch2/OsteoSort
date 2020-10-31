JuliaSetup <- function(add_cores = 1, remove_cores = FALSE, libraries = FALSE, source = FALSE, recall_libraries = FALSE) {
	if(!julia_exists("ref_dif_s")) {
		if (libraries) {
			withProgress(message = 'Loading analytical environment', detail = '', value = 0, min=0, max=9, {
				julia <- JuliaCall::julia_setup(install=TRUE) #Set to false after deploying to shiny-server for startup speed
				pkg = c("Pkg","Statistics", "Distributed","SharedArrays", "Optim", "Rmath", "GLM", "NearestNeighbors")
				for(i in pkg) {
					incProgress(amount = 1, message = paste("Loading ", i, " library", sep=""))
					print(paste("Loading Julia package: ", i, sep=""))
					julia_install_package_if_needed(i)
					JuliaCall::julia_library(i)
				}
				incProgress(amount = 1, message = "Importing source code")
				julia_source(system.file("jl", "cores.jl", package = "OsteoSort"))
				julia_source(system.file("jl", "t_test_plot_MC.jl", package = "OsteoSort"))
				julia_source(system.file("jl", "t_test_MC.jl", package = "OsteoSort"))
				julia_source(system.file("jl", "regression_MC.jl", package = "OsteoSort"))
				julia_source(system.file("jl", "antemortem_MC.jl", package = "OsteoSort"))
				julia_source(system.file("jl", "z_test.jl", package = "OsteoSort"))
				julia_source(system.file("jl", "regression_plot_MC.jl", package = "OsteoSort"))
				julia_source(system.file("jl", "radius_search.jl", package = "OsteoSort"))
			})
		}

		sycores <- detectCores()
		jcores <- julia_call("nprocs")

		if(add_cores > jcores && add_cores <= sycores) {
			julia_call("add_cores", add_cores - jcores)
		}
		if(add_cores < jcores) {
			julia_call("clean_cores")
			julia_call("add_cores", add_cores-1)
		}
		if(remove_cores) {
			julia_call("clean_cores")
		}

		if (source) {
			julia_source(system.file("jl", "library.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "t_test_WC.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "Box_Cox_WC.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression_WC.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "antemortem_WC.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression_helpers_WC.jl", package = "OsteoSort"))
		}

		if(recall_libraries) {
			julia_source(system.file("jl", "library.jl", package = "OsteoSort"))
		}
	}
	JV <- JuliaCall:::julia_line(c("-e", "print(VERSION)"), stdout=TRUE)
	return(JV)
}
