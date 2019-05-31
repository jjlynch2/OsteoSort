@everywhere function res_std_err(model)
	resd = residuals(model)
	resd .^=2 #is equal correct?
	samples = size(resd,1)
	resd = sum(resd)
	nK = samples - 2 #is this correct? i think so
	temp = resd / nK
	return sqrt(Complex(temp)) #is complex needed?
end
#########residua lstandard error function needs fixing
#########residua lstandard error function needs fixing
#########residua lstandard error function needs fixing
#########residua lstandard error function needs fixing

@everywhere function reg_t_stat(sigma, r2, predicted, comparison, comparison_p, mean, sd, n)
	return abs(predicted - comparison) / sigma * sqrt(1+(1/n)) + (comparison_p - mean) ^2 / n*sd^2
end

@everywhere function measurement_counter(v1)
	res = 0
	for i in 1:size(v1,1)
		if v1[i] != 0
			res += 1
		end
	end
	return res
end

@everywhere function REGS_worker(v1, m2, li, RL, RR)
	res = zeros(size(m2,1),size(m2,2)+7+size(v1,2)) #plus 7 and size of measurements for v1 m2?
	res_1 = measurement_counter(v1[1,1:end]) #transposes and counts
	dsum_1 = sum(v1) #should work outside of the loops since 0 
	refd_a = zeros(1,1)
	refd_b = zeros(1,1)
	for x in 1:size(m2,1)
		res_2 = measurement_counter(m2[x,1:end])
		dsum_2 =sum(m2[x,1:end])
		refd_1 = zeros(1,1)
		refd_2 = zeros(1,1)
		for i in 1:size(RL,1) #works since RL and RR have same number of rows due to R parsing therefore index of row matches specimens
			m_counter_1 = 0
			m_counter_2 = 0
			ref_dsum_1 = 0
			ref_dsum_2 = 0
			for j in 1:size(v1,2)
				if v1[j] != 0 && RL[i,j] != 0
					m_counter_1 += 1 #counts if reference data measurements match that of comparison
					ref_dsum_1 += RL[i,j]
				end
			end
			for j in 1:size(m2,2)
				if m2[x,j] != 0 && RR[i,j] != 0
					m_counter_2 += 1 #counts if reference data measurements match that of comparison
					ref_dsum_2 += RR[i,j]
				end
			end
			if m_counter_1 == res_1 && m_counter_2 == res_2 #if ref 1 and ref 2 both match the sort comparisons
				refd_1 = vcat(refd_1, log(ref_dsum_1))
				refd_2 = vcat(refd_2, log(ref_dsum_2))
			end
		end
		refd_1 = refd_1[2:end,1] #remove first row
		refd_2 = refd_2[2:end,1] #remove first row


		#fixed code
		#fixed code
		X = hcat(fill(1, size(X, 1)), X)
		hcat(X,ones(length(x),1))
		 fit(LinearModel, X, Y, false)
		#fixed code
		#fixed code
		POLS = predict(OLS, reshape([dsum_2],1,1)) #prediction level = PL 

		sigma = res_std_err(OLS)
		r2 = cor(refd_1, refd_2) ^ 2
		mean_ref = mean(refd_2)
		sd_ref = std(refd_2)
		n = size(refd_2,1)

		tStat = reg_t_stat(sigma, r2, POLS, dsum_1, dsum_2, mean_ref, sd_ref, n)
		pVal = 2 * pt(-abs(tStat), n-2) #always uses 2-tails with 2 degrees of freedom

		res[x,1] = li #index of left
		res[x,2] = x #index of right
		res[x,3] = 0 #no needed but left here so array columns match in R
		res[x,4] = pVal #final P from norm
		res[x,5] = 0 #since final is norm, return 0 for mean
		res[x,6] = 1 #since final is norm, return 1 for sd
		res[x,7] = n #reference sample size
#change all variable names to dep and ind for comp and ref to clarify it
#will make it easier to follow

	end
	return res
end