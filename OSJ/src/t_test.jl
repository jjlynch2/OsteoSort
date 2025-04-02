#############################################
#############################################
## t test
function TTEST(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_mean_cache = Float64[]
	refd_std_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0

	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_mean = refd_mean_cache[temp_index]
				refd_std = refd_std_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				refd = ref_dif(Results[ri,8:end], RL, RR)
				refd_mean = mean(refd)
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_mean_cache, refd_mean)
				push!(refd_std_cache, refd_std)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			Results[ri,4] = TL[1] * pt(-abs( (dsum - refd_mean) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = refd_mean #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

## absolute value
function TTESTA(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_mean_cache = Float64[]
	refd_std_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0

	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_mean = refd_mean_cache[temp_index]
				refd_std = refd_std_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				refd = ref_difa(Results[ri,8:end], RL, RR)
				refd_mean = mean(refd)
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_mean_cache, refd_mean)
				push!(refd_std_cache, refd_std)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			Results[ri,4] = TL[1] * pt(-abs( (abs(dsum - refd_mean) ) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = refd_mean #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

## absolute value YeoJohnsonTrans
function TTESTAB(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_mean_cache = Float64[]
	refd_std_cache = Float64[]
	refd_bc_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0

	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_mean = refd_mean_cache[temp_index]
				refd_std = refd_std_cache[temp_index]
				refd_bc = refd_bc_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				RT = ref_difa(Results[ri,8:end], RL, RR)
				refd_bc = lambda(RT)[1] #calculate lambda
				refd = transform(RT, refd_bc) #transform ref data by lambda
				refd_mean = mean(refd)
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_mean_cache, refd_mean)
				push!(refd_std_cache, refd_std)
				push!(refd_bc_cache, refd_bc)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			dsum = transform([dsum,dsum], refd_bc)[1] #transform sort data by lambda
			Results[ri,4] = TL[1] * pt(-abs( (abs(dsum - refd_mean) ) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = refd_mean #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

## absolute value YeoJohnsonTrans zero mean
function TTESTABM(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_std_cache = Float64[]
	refd_bc_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0

	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_std = refd_std_cache[temp_index]
				refd_bc = refd_bc_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				RT = ref_difa(Results[ri,8:end], RL, RR)
				refd_bc = lambda(RT)[1] #calculate lambda
				refd = transform(RT, refd_bc) #transform ref data by lambda
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_std_cache, refd_std)
				push!(refd_bc_cache, refd_bc)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			dsum = transform([dsum,dsum], refd_bc)[1] #transform sort data by lambda
			Results[ri,4] = TL[1] * pt(-abs( (abs(dsum - 0) ) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = 0 #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

## YeoJohnsonTrans
function TTESTB(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_mean_cache = Float64[]
	refd_std_cache = Float64[]
	refd_bc_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_mean = refd_mean_cache[temp_index]
				refd_std = refd_std_cache[temp_index]
				refd_bc = refd_bc_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				RT = ref_dif(Results[ri,8:end], RL, RR)
				refd_bc = lambda(RT)[1] #calculate lambda
				refd = transform(RT, refd_bc) #transform ref data by lambda
				refd_mean = mean(refd)
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_mean_cache, refd_mean)
				push!(refd_std_cache, refd_std)
				push!(refd_bc_cache, refd_bc)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			dsum = transform([dsum,dsum], refd_bc)[1] #transform sort data by lambda
			Results[ri,4] = TL[1] * pt(-abs( (dsum - refd_mean) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = refd_mean #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

##YeoJohnsonTrans zero mean
function TTESTBM(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_std_cache = Float64[]
	refd_bc_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0

	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_std = refd_std_cache[temp_index]
				refd_bc = refd_bc_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				RT = ref_dif(Results[ri,8:end], RL, RR)
				refd_bc = lambda(RT)[1] #calculate lambda
				refd = transform(RT, refd_bc) #transform ref data by lambda
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_std_cache, refd_std)
				push!(refd_bc_cache, refd_bc)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			dsum = transform([dsum,dsum], refd_bc)[1] #transform sort data by lambda
			Results[ri,4] = TL[1] * pt(-abs( (dsum - 0) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = 0 #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

##zero mean
function TTESTM(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_std_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0

	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_std = refd_std_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				refd = ref_dif(Results[ri,8:end], RL, RR)
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_std_cache, refd_std)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			Results[ri,4] = TL[1] * pt(-abs( (dsum - 0) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = 0 #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

##absolute value zero mean
function TTESTAM(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	ri = 1
	res = zeros(1,size(m2,2))
	res_cache = Array[]
	refd_std_cache = Float64[]
	refd_size_cache = []
	cache_index = 0
	temp_index = 0

	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			res = res_check(m1[x,1:end], m2[j,1:end])
			if j != 1 || x != 1
				temp_index = cache_check(res, res_cache, cache_index, size(m1[x,1:end],1))
			end

			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[ri,g+7] = 1
				end
			end

			if temp_index != 0
				refd_std = refd_std_cache[temp_index]
				refd_size = refd_size_cache[temp_index]
			else
				refd = ref_difa(Results[ri,8:end], RL, RR)
				refd_std = std(refd)
				refd_size = size(refd,1)
				push!(res_cache, res)
				push!(refd_std_cache, refd_std)
				push!(refd_size_cache, refd_size)
				cache_index += 1
			end
			Results[ri,1] = x #index of left
			Results[ri,2] = j #index of right
			Results[ri,3] = dsum
			Results[ri,4] = TL[1] * pt(-abs( (abs(dsum - 0) ) / refd_std), refd_size - 1) #p.value
			Results[ri,5] = 0 #reference mean
			Results[ri,6] = refd_std #reference standard deviation
			Results[ri,7] = refd_size #reference sample size
			ri += 1
		end
	end
	return Results
end

#############################################
#############################################
##Reference absolute value difference
function ref_difa(res, RL, RR)
	refd = []
	for i in 1:size(RL,1)
		dsum = 0
		m_counter = 0
		for j in 1:size(res,1)
			if RL[i,j] != 0 && RR[i,j] != 0 && res[j] == 1
				dsum += abs(RL[i,j] - RR[i,j])
				m_counter += 1 #counts if reference data measurements match that of comparison
			end
		end
		if m_counter == sum(res)
			push!(refd, dsum)
		end
	end
	return refd
end

##Reference value difference
function ref_dif(res, RL, RR)
	refd = []
	for i in 1:size(RL,1)
		dsum = 0
		m_counter = 0
		for j in 1:size(res,1)
			if RL[i,j] != 0 && RR[i,j] != 0 && res[j] == 1
				dsum += (RL[i,j] - RR[i,j])
				m_counter += 1 #counts if reference data measurements match that of comparison
			end
		end
		if m_counter == sum(res)
			push!(refd, dsum)
		end
	end
	return refd
end
