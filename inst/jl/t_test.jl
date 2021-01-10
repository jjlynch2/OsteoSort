#############################################
#############################################
## t test
function TTEST(m1, m2, RL, RR, TL)
	n1 = size(m1,1)
	n2 = size(m2,1)
	n3 = size(m1,2)
	Results = zeros(n2*n1,n3+7)
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			ref = ref_dif(Results[Results_Iterator,8:end], RL, RR)
			ref_size = size(ref,1)
			ref_mean = mean(ref)
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
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
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			ref = ref_difa(Results[Results_Iterator,8:end], RL, RR)
			ref_size = size(ref,1)
			ref_mean = mean(ref)
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
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
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			RT = ref_difa(Results[Results_Iterator,8:end], RL, RR)
			bc = lambda(RT)[1] #calculate lambda
			ref = transform(RT, bc) #transform ref data by lambda
			dsum = transform([dsum,dsum], bc)[1] #transform sort data by lambda
			ref_size = size(ref,1)
			ref_mean = mean(ref)
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
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
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			RT = ref_difa(Results[Results_Iterator,8:end], RL, RR)
			bc = lambda(RT)[1] #calculate lambda
			ref = transform(RT, bc) #transform ref data by lambda
			dsum = transform([dsum,dsum], bc)[1] #transform sort data by lambda
			ref_size = size(ref,1)
			ref_mean = 0
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
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
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			RT = ref_dif(Results[Results_Iterator,8:end], RL, RR)
			bc = lambda(RT)[1] #calculate lambda
			ref = transform(RT, bc) #transform ref data by lambda
			dsum = transform([dsum,dsum], bc)[1] #transform sort data by lambda
			ref_size = size(ref,1)
			ref_mean = mean(ref)
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
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
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			RT = ref_dif(Results[Results_Iterator,8:end], RL, RR)
			bc = lambda(RT)[1] #calculate lambda
			ref = transform(RT, bc) #transform ref data by lambda
			dsum = transform([dsum,dsum], bc)[1] #transform sort data by lambda
			ref_size = size(ref,1)
			ref_mean = 0
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
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
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += (m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			ref = ref_dif(Results[Results_Iterator,8:end], RL, RR)
			ref_size = size(ref,1)
			ref_mean = 0
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (dsum - ref_mean) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
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
	Results_Iterator = 1
	for x in 1:size(m1,1)
		for j in 1:size(m2,1)
			dsum = 0
			for g in 1:size(m1[x,1:end],1)
				if m1[x,g] != 0 && m2[j,g] != 0
					dsum += abs(m1[x,g] - m2[j,g])
					Results[Results_Iterator,g+7] = 1
				end
			end
			Results[Results_Iterator,1] = x #index of left
			Results[Results_Iterator,2] = j #index of right
			Results[Results_Iterator,3] = dsum
			ref = ref_difa(Results[Results_Iterator,8:end], RL, RR)
			ref_size = size(ref,1)
			ref_mean = 0
			ref_sd = std(ref)
			Results[Results_Iterator,4] = TL[1] * pt(-abs( (abs(dsum - ref_mean) ) / ref_sd), ref_size - 1) #p.value
			Results[Results_Iterator,5] = ref_mean #reference mean
			Results[Results_Iterator,6] = ref_sd #reference standard deviation
			Results[Results_Iterator,7] = ref_size #reference sample size
			Results_Iterator += 1
		end
	end
	return Results
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
