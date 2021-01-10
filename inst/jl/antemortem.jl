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
	Results = zeros(n2*n1,5)
	Results_Iterator = 1
	for j in 1:(size(AN,1)-1)
		for x in 1:(size(PM,1)-1)
			cX = hcat(1, AN[j,1])
			POLS = predict(OLS, cX)
			POLS = POLS[1]
			tStat = reg_t_stat(sigma, r2, POLS, PM[x,1], AN[j,1], mean_ref, sd_ref, n)
			pVal = 2 * pt(-abs(tStat), n-2) #always uses 2-tails with 2 degrees of freedom
			Results[Results_Iterator,1] = j #index of left
			Results[Results_Iterator,2] = x #index of right
			Results[Results_Iterator,3] = pVal #p-value
			Results[Results_Iterator,4] = n #reference sample size
			Results[Results_Iterator,5] = r2 #r-square
			Results_Iterator += 1
		end
	end
	return Results
end
