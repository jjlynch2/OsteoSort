function z_ref(refd)
	z_temp_ref = zeros(size(refd,1),size(refd,2))
	for i in 1:size(refd,1)
		for g in 1:size(refd,2)
			z_temp_ref[i,g] = qnorm(2*pt(-abs(abs(refd[i,g] - mean(refd[1:end,g])) / std(refd[1:end,g])), size(refd,1) - 1), true, false)
		end
	end
	return z_temp_ref
end

function d_comp(v1,m2,res)
	dcomp = zeros(1,trunc(Int, sum(res)))
	co = 1
	for p in 1:size(res,2)
		if res[p] == 1
			dcomp[co] = abs(v1[p] - m2[p])
			co += 1
		end
	end
	return dcomp
end

function z_comp(dcomp, refd_mean, refd_std, refd_size)
	z_temp_comp = zeros(1,size(refd_mean,2))
	for i in 1:size(refd_mean,2)
		z_temp_comp[i] = qnorm(2*pt(-abs(abs(dcomp[i] - refd_mean[i]) / refd_std[i]), refd_size - 1), true, false)
	end
	return z_temp_comp
end

function wA_c(c_temp_ref, refd_mean, refd_std)
	wA = 0
	wAA = zeros(1,size(refd_mean,2))
	for i in 1:size(c_temp_ref,2)
		wAA[i] = (refd_mean[i]/refd_std[i])
		for g in i+1:size(c_temp_ref,2)-1
			wA += wAA[i] * (refd_mean[g]/refd_std[g]) * c_temp_ref[i,g]
		end
	end
	return [wA, wAA]
end

function MSD(refd, res)
	msd1 = zeros(1,trunc(Int, sum(res)))
	msd2 = zeros(1,trunc(Int, sum(res)))
	co = 1
	for g in 1:size(res,2)
		if res[g] == 1
			msd1[co] = mean(refd[1:end,co])
			msd2[co] = std(refd[1:end,co])
			co += 1
		end
	end
	return [msd1,msd2]
end

function ZTEST(m1, m2, RL, RR)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7+n3)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_cache = Array[]
	refz_cache = Array[]
	refc_cache = Array[]
	refd_mean = Array[]
	refd_std = Array[]
	refd_mean_cache = Array[]
	refd_std_cache = Array[]
	wA_cache = Float64[]
	wAA_cache = Array[]
	wA = 0
	wAA = zeros(1,n3)
	cache_index = 0
	temp_index = 0
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end
			if temp_index != 0
				refd = refd_cache[temp_index]
				z_temp_ref = refz_cache[temp_index]
				c_temp_ref = refc_cache[temp_index]
				refd_mean = refd_mean_cache[temp_index]
				refd_std = refd_std_cache[temp_index]
				wA = wA_cache[temp_index]
				wAA = wAA_cache[temp_index]
			else
				refd = ref_dif_s(res, RL, RR)
				msd_temp = MSD(refd, res)
				refd_mean = msd_temp[1]
				refd_std = msd_temp[2]
				z_temp_ref = zeros(size(refd,1),size(refd,2))
				c_temp_ref = zeros(size(refd,2))
				z_temp_ref = z_ref(refd)
				c_temp_ref = cor(z_temp_ref, z_temp_ref)
				wAA_t = wA_c(c_temp_ref, refd_mean, refd_std)
				wA = wAA_t[1]
				wAA = wAA_t[2]
				push!(refc_cache, c_temp_ref)
				push!(refz_cache, z_temp_ref)
				push!(refd_cache, refd)
				push!(res_cache, res)
				push!(refd_mean_cache, refd_mean)
				push!(refd_std_cache, refd_std)
				push!(wA_cache, wA)
				push!(wAA_cache, wAA)
				cache_index += 1
			end
			dcomp = zeros(1,trunc(Int, sum(res)))
			dcomp = d_comp(m1[x,1:end], m2[j,1:end], res)
			z_temp_comp = z_comp(dcomp, refd_mean, refd_std, size(refd,1))
			wZ = sum(wAA .* z_temp_comp) / sqrt(sum(wAA .^ 2) + (wA * 2)) #combined Z-score
			wZP = 1 - pnorm(wZ, false, false) #combined P-value from normal distribution
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = 0 #not needed but left here so array columns match in R
			Results[ri,4] = wZP #final P from norm
			Results[ri,5] = 0 #since final is norm, return 0 for mean
			Results[ri,6] = 1 #since final is norm, return 1 for sd
			Results[ri,7] = size(refd,1) #reference sample size
			for s in 1:size(res,2)
				co = 1
				if res[s] == 1
					Results[ri,7+s] = refd_mean[co]
					Results[ri,7+(size(res,2)+s)] = refd_std[co]
					co += 1
				end
			end
			ri += 1
		end
	end
	return Results
end

function ref_dif_s(res, RL, RR)
	refd = zeros(1,trunc(Int, sum(res)))
	for i in 1:size(RL,1)
		dsum = []
		m_counter = 0
		for j in 1:size(res,2)
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
