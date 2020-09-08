@everywhere function ZTEST_worker(v1, m2, li, RL, RR)
	res = zeros(size(m2,1),size(m2,2))
	res_f = zeros(size(m2,1),size(m2,2)*2+7)
	res_cache = []
	refd_cache = []
	refz_cache = []
	refc_cache = []
	cache_index = 0
	for j in 1:size(m2,1)
		#comparison d-values and res for present measurements
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0 #measurement is being used
				res[j,g] = 1 #still recover index
			end
		end
		cache = false
		temp_index = 0
		if j != 1
			for x_cache in 1:cache_index
				cache = true
				for i_cache in 1:size(v1,1)
					if res[j,i_cache] != res_cache[x_cache][i_cache]
						cache = false
						break
					end
				end
				if cache
					temp_index = x_cache
					break
				end
			end
		end
		if cache 
			refd = refd_cache[temp_index]
			z_temp_ref = refz_cache[temp_index]
			c_temp_ref = refc_cache[temp_index]
		else
			refd = ref_dif_s(res[j,1:end], RL, RR)
			z_temp_ref = zeros(size(refd,1),size(refd,2))
			c_temp_ref = zeros(size(refd,2))
			#ref z-scores
			for i in 1:size(refd,1)
				for g in 1:size(refd,2)
					z_temp_ref[i,g] = qnorm(2*pt(-abs(abs(refd[i,g] - mean(refd[1:end,g])) / std(refd[1:end,g])), size(refd,1) - 1), true, false)
				end
			end
			#ref z-scores correlation
			c_temp_ref = cor(z_temp_ref, z_temp_ref)
			push!(refc_cache, c_temp_ref)
			push!(refz_cache, z_temp_ref)
			push!(refd_cache, refd)
			push!(res_cache, res)
			cache_index <- cache_index + 1
		end
		#comparison z-scores
		z_temp_comp = zeros(1,size(refd,2))
		for i in 1:size(refd,2)
			dcomp = zeros(1,trunc(Int, sum(res[j,1:end])))
			co = 1
			for p in 1:size(res,2)
				if res[j,p] == 1
					dcomp[co] = v1[p] - m2[j,p]
					co += 1
				end
			end
			z_temp_comp[i] = qnorm(2*pt(-abs(abs(abs(dcomp[i]) - mean(refd[1:end,i])) / std(refd[1:end,i])), size(refd,1) - 1), true, false)
		end
		#weights
		wA = 0
		wAA = zeros(1,size(refd,2))
		for i in 1:size(c_temp_ref,2)
			wAA[i] = mean(refd[1:end,i])/std(refd[1:end,i])
			for g in i+1:size(c_temp_ref,2)-1
				wA += wAA[i] * mean(refd[1:end,g])/std(refd[1:end,g]) * c_temp_ref[i,g]
			end
		end
		wZ = sum(wAA .* z_temp_comp) / sqrt(sum(wAA .^ 2) + (wA * 2)) #combined Z-score
		wZP = 1 - pnorm(wZ, false, false) #combined P-value from normal distribution
		res_f[j,1] = li #index of left
		res_f[j,2] = j #index of right
		res_f[j,3] = 0 #not needed but left here so array columns match in R
		res_f[j,4] = wZP #final P from norm
		res_f[j,5] = 0 #since final is norm, return 0 for mean
		res_f[j,6] = 1 #since final is norm, return 1 for sd
		res_f[j,7] = size(refd,1) #reference sample size
		#mean and std per measurement thats used
		co = 1
		for g in 1:size(res,2)
			if res[j,g] == 1
				res_f[j,g+7] = mean(refd[1:end,co])
				res_f[j,g+7+size(res,2)] = std(refd[1:end,co])
				co += 1
			end
		end
	end
	return res_f
end

@everywhere function ref_dif_s(res, RL, RR)
	refd = zeros(1,trunc(Int, sum(res)))
	for i in 1:size(RL,1)
		dsum = []
		m_counter = 0
		for j in 1:size(res,1)
			if RL[i,j] != 0 && RR[i,j] != 0 && res[j] == 1
				push!(dsum, abs(RL[i,j] - RR[i,j]))
				m_counter += 1 #counts if reference data measurements match that of comparison
			end
		end
		if m_counter == sum(res)
			refd = vcat(refd, transpose(dsum))
		end
	end
	return refd[2:end,1:end]
end
