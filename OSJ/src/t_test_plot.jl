#############################################
#############################################
#Single comparison code to return difference values
#for plotting in the R environment

#No boxcox or absolute value
function TTEST_plot(SL, SR, RL, RR)
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
function TTESTA_plot(SL, SR, RL, RR)
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

#Absolute value and YeoJohnsonTrans
function TTESTAB_plot(SL, SR, RL, RR)
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
	bc = lambda(ref)[1]
	ref = transform(ref, bc)
	results = zeros(size(ref,1)+1,1)
	results[1:size(ref,1),1] = ref
	results[size(ref,1)+1,1] = transform([dsum, dsum], bc)[1]
	return results
end

#YeoJohnsonTrans only
function TTESTB_plot(SL, SR, RL, RR)
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
	bc = lambda(ref)[1]
	ref = transform(ref, bc)
	results = zeros(size(ref,1)+1,1)
	results[1:size(ref,1),1] = ref
	results[size(ref,1)+1,1] = transform([dsum, dsum], bc)[1]
	return results
end
#############################################
#############################################
