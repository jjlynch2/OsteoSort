
#antemortem only requires 1 cache... since its the same model eevery single time!
#doesnt require cross-checking data since 1 measurement
#everything is prefiltered in R should be cake to implement

@everywhere function REGS_Ante_worker(v1, m2, li, R)

	res = zeros(size(m2,1),1+6+1) #1 stature, 1 measurement, 6 ID data



	m_cache_id = []
	m_cache_de = []
	mean_ref_cache = []
	sd_ref_cache = []
	n_ref_cache = []
	r2_ref_cache = []
	sigma_ref_cache = []
	OLS_ref_cache = []

	#fit R[,1] and R[,2] for model


	X = hcat(fill(1, size(refd_1, 1)), refd_1)
	OLS = fit(LinearModel, X, refd_2, false)
	sigma = res_std_err(OLS)
	r2 = cor(refd_1, refd_2) ^ 2
	mean_ref = mean(refd_2)
	sd_ref = std(refd_2)
	n = size(refd_2,1)

	push!(m_cache_id, res_1_i)
	push!(m_cache_de, res_2_i)
	push!(mean_ref_cache, mean_ref)
	push!(sd_ref_cache, sd_ref)
	push!(n_ref_cache, n)
	push!(r2_ref_cache, r2)
	push!(sigma_ref_cache, sigma)
	push!(OLS_ref_cache, OLS)


	for x in 1:size(m2,1)



		refd_1 = zeros(1,1)
		refd_2 = zeros(1,1)


		mean_ref = mean_ref_cache[1]
		sd_ref = sd_ref_cache[1]
		n = n_ref_cache[1]
		r2 = r2_ref_cache[1]
		sigma = sigma_ref_cache[1]
		OLS = OLS_ref_cache[1]


		cX = hcat(1, dsum_1)
		POLS = predict(OLS, cX)
		POLS = POLS[1]
		tStat = reg_t_stat(sigma, r2, POLS, dsum_2, dsum_1, mean_ref, sd_ref, n)
		pVal = 2 * pt(-abs(tStat), n-2) #always uses 2-tails with 2 degrees of freedom
		res[x,1] = li #index of left
		res[x,2] = x #index of right
		res[x,3] = 0 #not needed but left here so array columns match in R
		res[x,4] = pVal #p-value
		res[x,5] = n #reference sample size
		res[x,6] = r2 #r-square

	end
	return res
end
