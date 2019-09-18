@everywhere function measurement_counter(v1)
	res = 0
	for i in 1:size(v1,1)
		if v1[i] != 0
			res += 1
		end
	end
	return res
end

@everywhere function measurement_counter_indices(v1)
	res = 0
	for i in 1:size(v1,1)
		res = hcat(res, i)
	end
	return res[2:end]
end

@everywhere function check_cache(res_1_i, res_2_i, m_cache_id, m_cache_de)
	for i in 1:size(m_cache_id,1)
		noc1 = true
		noc2 = true
		if size(res_1_i,1) == size(m_cache_id[i],1) && size(res_2_i,1) == size(m_cache_de[i],1)
			for j in 1:size(res_1_i,1)
				if res_1_i[j] != m_cache_id[i][j]
					noc1 = false
				end
			end
			for j in 1:size(res_2_i,1)
				if res_2_i[j] != m_cache_de[i][j]
					noc2 = false
				end
			end
			if noc1 && noc2
				return i
			end
		end
	end
	return false
end

@everywhere function REGSL_worker(v1, m2, li, RL, RR)
	res = zeros(size(m2,1),size(m2,2)+5+size(v1,1)) #plus 8 and size of measurements for v1 m2?
	res_1 = measurement_counter(v1) #transposes and counts
	dsum_1 = log(sum(v1)) #should work outside of the loops since 0 
	refd_a = zeros(1,1)
	refd_b = zeros(1,1)
	m_counter_1_o = 0
	m_counter_2_o = 0
	m_cache_id = []
	m_cache_de = []
	mean_ref_cache = []
	sd_ref_cache = []
	n_ref_cache = []
	r2_ref_cache = []
	sigma_ref_cache = []
	OLS_ref_cache = []
	cache_i = false
	res_1_i = 0
	res_2_i = 0
	for x in 1:size(m2,1)
		res_2 = measurement_counter(m2[x,1:end])
		dsum_2 = log(sum(m2[x,1:end]))
		if x != 1
			res_1_i = measurement_counter_indices(v1)
			res_2_i = measurement_counter_indices(m2[x,1:end])
			cache_i = check_cache(res_1_i, res_2_i, m_cache_id, m_cache_de)
		end
		refd_1 = zeros(1,1)
		refd_2 = zeros(1,1)
		for i in 1:size(RL,1) #works since RL and RR have same number of rows due to R parsing therefore index of row matches specimens
			if cache_i != false
				break #break early to pull from cache
			end
			m_counter_1 = 0
			m_counter_2 = 0
			ref_dsum_1 = 0
			ref_dsum_2 = 0
			for j in 1:size(v1,1)
				if v1[j] != 0 && RL[i,j] != 0
					m_counter_1 = hcat(m_counter_1, j)
					ref_dsum_1 += RL[i,j]
				end
			end
			for j in 1:size(m2,2)
				if m2[x,j] != 0 && RR[i,j] != 0
					m_counter_2 = hcat(m_counter_2, j)
					ref_dsum_2 += RR[i,j]
				end
			end
			if length(m_counter_1)-1 == res_1 && length(m_counter_2)-1 == res_2 #if ref 1 and ref 2 both match the sort comparisons
				m_counter_1_o = m_counter_1[2:end] #indices of measurements being used
				m_counter_2_o = m_counter_2[2:end] #indices of measurements being used
				refd_1 = vcat(refd_1, log(ref_dsum_1))
				refd_2 = vcat(refd_2, log(ref_dsum_2))
			end
		end
		if cache_i != false
			mean_ref = mean_ref_cache[cache_i]
			sd_ref = sd_ref_cache[cache_i]
			n = n_ref_cache[cache_i]
			r2 = r2_ref_cache[cache_i]
			sigma = sigma_ref_cache[cache_i]
			OLS = OLS_ref_cache[cache_i]
		else
			refd_1 = refd_1[2:end,1] #remove first row 0
			refd_2 = refd_2[2:end,1] #remove first row 0
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
		end
		cX = hcat(1, dsum_1)
		POLS = predict(OLS, cX)
		POLS = POLS[1]
		tStat = reg_t_stat(sigma, r2, POLS, dsum_2, dsum_1, mean_ref, sd_ref, n)
		pVal = 2 * pt(-abs(tStat), n-2) #always uses 2-tails with 2 degrees of freedom
		res[x,1] = li #index of left
		res[x,2] = x #index of right
		res[x,3] = pVal #p-value
		res[x,4] = n #reference sample size
		res[x,5] = r2 #r-square
		for j in m_counter_1_o
			res[x,j+5] = 1
		end
		for j in m_counter_2_o
			res[x,j+5+size(v1,1)] = 1
		end
	end
	return res
end
