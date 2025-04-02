#set working directory
cd("/home/shiny/sysimage/")

using Pkg
Pkg.add("PackageCompiler")

using PackageCompiler

#create new environment
Pkg.activate("/home/shiny/sysimage/")

#add packages
Pkg.add("Pkg")
Pkg.add("Suppressor")
Pkg.add("Statistics")
Pkg.add("Optim")
Pkg.add("Rmath")
Pkg.add("GLM")
Pkg.add("RCall")
Pkg.develop(PackageSpec(path="/home/shiny/OSJ"))

create_sysimage(sysimage_path="/srv/shiny-server/OsteoSort/OsteoSort.so", precompile_execution_file="/home/shiny/sysimage/execution_precompile.jl")