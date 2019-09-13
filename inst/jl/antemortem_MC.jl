function REGS_Ante(AN, PM, REF)
	n1 = size(AN,1)-1
	n2 = size(PM,1)-1
	n3 = size(REF,2)
	XX = hcat(fill(1, size(REF[:,1],1)), REF[:,1])
	YY = REF[:,2]
	OLS = fit(LinearModel, XX, YY, false)
	sigma = res_std_err(OLS)
	r2 = cor(REF[:,1], REF[:,2]) ^ 2
	mean_ref = mean(REF[:,1])
	sd_ref = std(REF[:,1])
	n = size(REF[:,1],1)
	Results = SharedArray{Float64}(n2*n1,+5)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = REGS_Ante_worker(AN[k,:], PM[1:end-1,:], k, OLS, sigma, r2, mean_ref, sd_ref, n)
	end
	return Results
end
