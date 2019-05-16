# Wrapper to avoid adding more processors than actually detected

function Set_Procs(a)
	a = Int64(a)
	if a == 0 
		rmprocs(workers())
	end
	if a > 0
		addprocs(a)
	end
end