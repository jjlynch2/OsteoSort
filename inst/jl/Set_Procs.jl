# Wrapper to avoid adding more processors than actually detected

@everywhere function Set_Procs(a, n)
	a = Int64(a)
	n = Int64(n)
	if a < 1
		return 0
	end
	p = nprocs()
	if a <= n
		if a > p
			addprocs(a-p)
		elseif a < p
			temp = procs()
			for i in 1:(p-a)
				rmprocs(temp[p])
				p -= 1
			end
		end
	end
end