function REGS_Ante(AN, PM, REF)
	n1 = size(AN,1)
	n2 = size(PM,1)
	n3 = size(REF,2)
	Results = SharedArray{Float64}(n2*n1,+6)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = REGS_Ante_worker(AN[k,:], PM, k, REF)
	end
	return Results
end
