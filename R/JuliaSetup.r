RJS <- function(add_cores = 1, remove_cores = FALSE, libraries = FALSE) {
	if(libraries) {
		pkg = c("Pkg","Statistics", "Distributed","SharedArrays", "Optim", "Rmath", "GLM", "NearestNeighbors", "Dates", "DelimitedFiles", "LinearAlgebra", "MultivariateStats", "Printf", "StatsBase")
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
			julia_source(system.file("jl", "cores.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "t_test_plot.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "t_test.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "yeojohnson.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "antemortem.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "z_test.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression_plot.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "regression_helpers.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "radius_search.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "knn_ind_dst.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "point_to_plane.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "point_to_point.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "fragment_landmarks.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "alignment_landmarks.jl", package = "OsteoSort"))
			julia_source(system.file("jl", "icp.jl", package = "OsteoSort"))
		})
	}
	jcores <- julia_call("nprocs")
	if(add_cores > jcores) {
		sycores <- detectCores()
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

		julia_source(system.file("jl", "library.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "t_test.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "t_test_plot.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "yeojohnson.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "regression.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "antemortem.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "regression_plot.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "regression_helpers.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "z_test.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "knn_ind_dst.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "point_to_plane.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "point_to_point.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "fragment_landmarks.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "alignment_landmarks.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "icp.jl", package = "OsteoSort"))
		print(paste("Source code loaded on ", julia_call("nprocs"), " cores", sep=""))
	}
		
	JV <- JuliaCall:::julia_line(c("-e", "print(VERSION)"), stdout=TRUE)
	return(JV)
}
