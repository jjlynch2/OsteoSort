#############################################
#############################################
##
@everywhere function TTEST_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += (v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		ref = ref_dif(res[j,8:end], RL, RR)
		ref_size = size(ref,1)
		ref_mean = mean(ref)
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

## absolute value
@everywhere function TTESTA_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += abs(v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		ref = ref_difa(res[j,8:end], RL, RR)
		ref_size = size(ref,1)
		ref_mean = mean(ref)
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

## absolute value boxcox
@everywhere function TTESTAB_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += abs(v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		RT = ref_difa(res[j,8:end], RL, RR)
		RT .+= 0.005 #shift to avoid 0
		dsum += 0.005 #shift to avoid 0
		bc = lambda(RT)[1] #calculate lambda
		ref = ((RT.^bc).-1)./bc #transform ref data by lambda
		dsum = (dsum^bc-1)/bc #transform sort data by lambda
		ref_size = size(ref,1)
		ref_mean = mean(ref)
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

## absolute value boxcox zero mean
@everywhere function TTESTABM_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += abs(v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		RT = ref_difa(res[j,8:end], RL, RR)
		RT .+= 0.005 #shift to avoid 0
		dsum += 0.005 #shift to avoid 0
		bc = lambda(RT)[1] #calculate lambda
		ref = ((RT.^bc).-1)./bc #transform ref data by lambda
		dsum = (dsum^bc-1)/bc #transform sort data by lambda
		ref_size = size(ref,1)
		ref_mean = 0
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##ing boxcox
@everywhere function TTESTB_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += (v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		RT = ref_dif(res[j,8:end], RL, RR)
		RT .+= 0.005 #shift to avoid 0
		dsum += 0.005 #shift to avoid 0
		bc = lambda(RT)[1] #calculate lambda
		ref = ((RT.^bc).-1)./bc #transform ref data by lambda
		dsum = (dsum^bc-1)/bc #transform sort data by lambda
		ref_size = size(ref,1)
		ref_mean = mean(ref)
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##ing boxcox
@everywhere function TTESTBM_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += (v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		RT = ref_dif(res[j,8:end], RL, RR)
		RT .+= 0.005 #shift to avoid 0
		dsum += 0.005 #shift to avoid 0
		bc = lambda(RT)[1] #calculate lambda
		ref = ((RT.^bc).-1)./bc #transform ref data by lambda
		dsum = (dsum^bc-1)/bc #transform sort data by lambda
		ref_size = size(ref,1)
		ref_mean = 0
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##ing mean
@everywhere function TTESTM_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += (v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		ref = ref_dif(res[j,8:end], RL, RR)
		ref_size = size(ref,1)
		ref_mean = 0
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs((dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##ing mean
@everywhere function TTESTAM_worker(v1, m2, li, RL, RR, TL)
	res = zeros(size(m2,1),size(m2,2)+7)
	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[g] != 0 && m2[j,g] != 0
				dsum += abs(v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum
		ref = ref_difa(res[j,8:end], RL, RR)
		ref_size = size(ref,1)
		ref_mean = 0
		ref_sd = std(ref)
		res[j,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

#############################################
#############################################
##Reference absolute value difference
@everywhere function ref_difa(res, RL, RR)
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
@everywhere function ref_dif(res, RL, RR)
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
