@everywhere function REGS_Ante_worker(v1, m2, li, OLS, sigma, r2, mean_ref, sd_ref, n)
	res = zeros(size(m2,1),5)
	for x in 1:size(m2,1)
		cX = hcat(1, m2[x,1])
		POLS = predict(OLS, cX)
		POLS = POLS[1]
		tStat = reg_t_stat(sigma, r2, POLS, m2[x,1], v1[1],  mean_ref, sd_ref, n)
		pVal = 2 * pt(-abs(tStat), n-2) #always uses 2-tails with 2 degrees of freedom
		res[x,1] = li #index of left
		res[x,2] = x #index of right
		res[x,3] = pVal #p-value
		res[x,4] = n #reference sample size
		res[x,5] = r2 #r-square
	end
	return res
end
