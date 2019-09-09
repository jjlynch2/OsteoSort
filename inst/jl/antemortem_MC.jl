function REGS_Ante(AN, PM, REF)
	n1 = size(AN,1)-1
	n2 = size(PM,1)-1
	n3 = size(REF,2)
	XX = hcat(fill(1, size(REF, 2)), REF[:,2])
	YY = REF[:,1]
	OLS = fit(LinearModel, XX, YY, false)
	sigma = res_std_err(OLS)
	r2 = cor(YY, REF[:,1]) ^ 2
	mean_ref = mean(YY)
	sd_ref = std(YY)
	n = size(YY,1)
	Results = SharedArray{Float64}(n2*n1,+5)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = REGS_Ante_worker(AN[k,:], PM[1:end-1,:], k, OLS, sigma, r2, mean_ref, sd_ref, n)
	end
	return Results
end
