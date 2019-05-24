@everywhere function ZTEST_worker(v1, m2, li, RL, RR)
	res = zeros(size(m2,1),size(m2,2)+7)
	for j in 1:size(m2,1)
		#comparison d-values and res for present measurements
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0 #measurement is being used
				res[j,g+7] = 1 #still recover index
			end
		end
		refd = ref_dif_s(res[j,8:end], RL, RR)
		z_temp_ref = zeros(size(refd,1),size(refd,2))
		c_temp_ref = zeros(size(refd,2))
		z_temp_comp = zeros(1,size(refd,2))
		t_temp_comp = zeros(1,size(refd,2))
		#reference p and t values
		for i in 1:size(refd,1)
			for g in 1:size(refd,2)
				temp = pt(-abs(abs(refd[i,g] - mean(refd[1:end,g])) / std(refd[1:end,g])), size(refd,1) - 1)
				z_temp_ref[i,g] = qnorm(temp)
			end
		end
		c_temp_ref = cor(z_temp_ref, z_temp_ref)
		#correlation of z-scores and calculate comparison t, p, z
		for i in 1:size(refd,2)
			t_temp_comp[i] = -abs(abs(abs(v1[i] - m2[j,i]) - mean(refd[1:end,i])) / std(refd[1:end,i]))
			p_temp_comp = pt(t_temp_comp[i], size(refd,1) - 1) #p.value
			z_temp_comp[i] = qnorm(p_temp_comp)
		end
		wA = 0
		for i in 1:size(t_temp_comp,2)
			for g in 1:size(t_temp_comp,2)
				if i != g
					wA += t_temp_comp[i] * t_temp_comp[g] * c_temp_ref[i,g]
				end
			end
		end
		wZ = sum(t_temp_comp .* z_temp_comp) / sqrt(sum(t_temp_comp .^ 2) + (wA * 2)) #combined Z-score
		wZP = pnorm(-abs(wZ)) #combined P-value fromn normal distribution
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = 0 #no needed but left here so array columns match in R
		res[j,4] = wZP #final P from norm
		res[j,5] = 0 #since final is norm, return 0 for mean
		res[j,6] = 1 #since final is norm, return 1 for sd
		res[j,7] = size(refd,1) #reference sample size
	end
	return res
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