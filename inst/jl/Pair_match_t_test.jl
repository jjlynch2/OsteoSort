#############################################
#############################################
##Pair-match absolute value mean
@everywhere function PM_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
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

		res[j,4] = TL[1] * pt(-abs( dsum / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##pair-match absolute value mean
@everywhere function PMAM_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
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

##pair-match absolute value boxcox
@everywhere function PMAB_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)
	
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
				dsum += abs(v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum

		RT = ref_difa(res[j,8:end], RL, RR)

		#boxcox cache comparison
		cache_temp = 0
		cache_index = 0
		for a in 1:bc_iter_counter
			for b in 1:size(v1,1)
				if res[j,(b+7)] == bc_worker_cache[a,b]
					cache_temp = cache_temp + 1
				end
			end
			if cache_temp == size(v1,1)
				cache_index = a
				break
			else
				cache_index = 0
				cache_temp = 0
			end
		end
		if cache_temp != size(v1,1)
			bc = lambda(RT)
			bc_worker_cache[bc_iter_counter,1:(end-1)] = res[j,8:end]
			bc_worker_cache[bc_iter_counter,end] = bc
			bc_iter_counter = bc_iter_counter + 1

		else
			bc = bc_worker_cache[cache_index,end]
		end

		ref = zeros(size(RT,1),1)
		ref = transform(RT)

		ref_size = size(ref,1)
		ref_mean = 0
		ref_sd = std(ref)

		res[j,4] = TL[1] * pt(-abs(( dsum^bc) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##Pair-match absolute value boxcox mean
@everywhere function PMABM_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)
	
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
				dsum += abs(v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum

		RT = ref_difa(res[j,8:end], RL, RR)

		#boxcox cache comparison
		cache_temp = 0
		cache_index = 0
		for a in 1:bc_iter_counter
			for b in 1:size(v1,1)
				if res[j,(b+7)] == bc_worker_cache[a,b]
					cache_temp = cache_temp + 1
				end
			end
			if cache_temp == size(v1,1)
				cache_index = a
				break
			else
				cache_index = 0
				cache_temp = 0
			end
		end
		if cache_temp != size(v1,1)
			bc = lambda(RT)
			bc_worker_cache[bc_iter_counter,1:(end-1)] = res[j,8:end]
			bc_worker_cache[bc_iter_counter,end] = bc
			bc_iter_counter = bc_iter_counter + 1

		else
			bc = bc_worker_cache[cache_index,end]
		end

		ref = zeros(size(RT,1),1)
		ref = transform(RT)

		ref_size = size(ref,1)
		ref_mean = mean(ref)
		ref_sd = std(ref)

		res[j,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ^ bc) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##Pair-matching boxcox mean
@everywhere function PMBM_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)
	
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
				dsum += (v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum

		RT = ref_dif(res[j,8:end], RL, RR)

		#boxcox cache comparison
		cache_temp = 0
		cache_index = 0
		for a in 1:bc_iter_counter
			for b in 1:size(v1,1)
				if res[j,(b+7)] == bc_worker_cache[a,b]
					cache_temp = cache_temp + 1
				end
			end
			if cache_temp == size(v1,1)
				cache_index = a
				break
			else
				cache_index = 0
				cache_temp = 0
			end
		end
		if cache_temp != size(v1,1)
			bc = lambda(RT)
			bc_worker_cache[bc_iter_counter,1:(end-1)] = res[j,8:end]
			bc_worker_cache[bc_iter_counter,end] = bc
			bc_iter_counter = bc_iter_counter + 1

		else
			bc = bc_worker_cache[cache_index,end]
		end

		ref = zeros(size(RT,1),1)
		ref = transform(RT)

		ref_size = size(ref,1)
		ref_mean = mean(ref)
		ref_sd = std(ref)

		res[j,4] = TL[1] * pt(-abs( ((dsum - ref_mean) ^ bc) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##Pair-matching boxcox
@everywhere function PMB_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)
	
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
				dsum += (v1[g] - m2[j,g])
				res[j,g+7] = 1
			end
		end
		res[j,1] = li #index of left
		res[j,2] = j #index of right
		res[j,3] = dsum

		RT = ref_dif(res[j,8:end], RL, RR)

		#boxcox cache comparison
		cache_temp = 0
		cache_index = 0
		for a in 1:bc_iter_counter
			for b in 1:size(v1,1)
				if res[j,(b+7)] == bc_worker_cache[a,b]
					cache_temp = cache_temp + 1
				end
			end
			if cache_temp == size(v1,1)
				cache_index = a
				break
			else
				cache_index = 0
				cache_temp = 0
			end
		end
		if cache_temp != size(v1,1)
			bc = lambda(RT)
			bc_worker_cache[bc_iter_counter,1:(end-1)] = res[j,8:end]
			bc_worker_cache[bc_iter_counter,end] = bc
			bc_iter_counter = bc_iter_counter + 1

		else
			bc = bc_worker_cache[cache_index,end]
		end

		ref = zeros(size(RT,1),1)
		ref = transform(RT)

		ref_size = size(ref,1)
		ref_mean = 0
		ref_sd = std(ref)

		res[j,4] = TL[1] * pt(-abs( (dsum^bc) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##Pair-matching mean
@everywhere function PMM_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)
	
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
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

		res[j,4] = TL[1] * pt(-abs((dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
		res[j,5] = ref_mean #reference mean
		res[j,6] = ref_sd #reference standard deviation
		res[j,7] = ref_size #reference sample size
	end
	return res
end

##Pair-matching mean
@everywhere function PMA_worker(v1, m2, li, RL, RR, TL)
	#results
	res = zeros(size(m2,1),size(m2,2)+7)
	
	#boxcox cache for each worker
	bc_worker_cache = zeros((size(v1,1) * size(v1,1)), (size(v1,1)+1))
	bc_iter_counter = 1

	for j in 1:size(m2,1)
		dsum = 0
		for g in 1:size(v1,1)
			if v1[1] != 0 && m2[j,g] != 0
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

		res[j,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
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
	refd = zeros(size(RL,1),1)
	for i in 1:size(RL,1)
		for j in 1:size(res,1)
			if RL[i,j] != 0 && RR[i,j] != 0 && res[j] != 0
				refd[i,1] += abs(RL[i,j] - RR[i,j])
			end
		end
	end
	return refd
end

##Reference value difference
@everywhere function ref_dif(res, RL, RR)
	refd = zeros(size(RL,1),1)
	for i in 1:size(RL,1)
		for j in 1:size(res,1)
			if RL[i,j] != 0 && RR[i,j] != 0 && res[j] != 0
				refd[i,1] += (RL[i,j] - RR[i,j])
			end
		end
	end
	return refd
end

#############################################
#############################################
##Pair-match absolute value boxcox mean R call
@everywhere function PMABM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PMABM_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end

##Pair-match absolute vale boxcox R call
@everywhere function PMAB(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PMAB_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end

##Pair-match absolute value mean R call
@everywhere function PMAM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PMAM_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end

##Pair-match boxcox mean R call
@everywhere function PMBM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PMBM_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end

##Pair-match absolute value R call
@everywhere function PMA(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PMA_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end

##Pair-match boxcox R call
@everywhere function PMB(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PMB_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end

##Pair-match mean R call
@everywhere function PMM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PMM_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end

##Pair-match R call
@everywhere function PM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)

	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = PM_worker(SL[k,:], SR, k, RL, RR, TL)
	end

	return Results
end