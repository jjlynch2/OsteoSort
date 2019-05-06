#############################################
#############################################
#Single comparison code to return difference values
#for plotting in the R environment

#No boxcox or absolute value
@everywhere function PM_plot(SL, SR, RL, RR)
	res = zeros(1,size(SL,2))
	dsum = 0
	for g in 1:size(SL,2)
		if SL[g] != 0 && SR[g] != 0
			dsum += (SL[g] - SR[g])
			res[g] = 1
		end
	end
	res = res[1,1:end]
	ref = ref_dif(res, RL, RR)
	results = zeros(size(ref,1)+1,1)
	results[1:size(ref,1),1] = ref
	results[size(ref,1)+1,1] = dsum
	return results
end

#Absolute value only
@everywhere function PMA_plot(SL, SR, RL, RR)
	res = zeros(1,size(SL,2))
	dsum = 0
	for g in 1:size(SL,2)
		if SL[g] != 0 && SR[g] != 0
			dsum += abs(SL[g] - SR[g])
			res[g] = 1
		end
	end
	res = res[1,1:end]
	ref = ref_difa(res, RL, RR)
	results = zeros(size(ref,1)+1,1)
	results[1:size(ref,1),1] = ref
	results[size(ref,1)+1,1] = dsum
	return results
end

#Absolute value and boxcox
@everywhere function PMAB_plot(SL, SR, RL, RR)
	res = zeros(1,size(SL,2))
	dsum = 0
	for g in 1:size(SL,2)
		if SL[g] != 0 && SR[g] != 0
			dsum += abs(SL[g] - SR[g])
			res[g] = 1
		end
	end
	res = res[1,1:end]
	ref = ref_difa(res, RL, RR)
	bc = lambda(ref)
	ref = transform(ref)
	results = zeros(size(ref,1)+1,1)
	results[1:size(ref,1),1] = ref
	results[size(ref,1)+1,1] = dsum^bc
	return results
end

#Boxcox only
@everywhere function PMB_plot(SL, SR, RL, RR)
	res = zeros(1,size(SL,2))
	dsum = 0
	for g in 1:size(SL,2)
		if SL[g] != 0 && SR[g] != 0
			dsum += (SL[g] - SR[g])
			res[g] = 1
		end
	end
	res = res[1,1:end]
	ref = ref_difa(res, RL, RR)
	bc = lambda(ref)
	ref = transform(ref)
	results = zeros(size(ref,1)+1,1)
	results[1:size(ref,1),1] = ref
	results[size(ref,1)+1,1] = dsum^bc
	return results
end
#############################################
#############################################