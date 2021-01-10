function clean_cores()
	rmprocs(workers())
end

#wrap to force Int64 from R
function add_cores(a)
	a = Int64(a)
	addprocs(a)
end