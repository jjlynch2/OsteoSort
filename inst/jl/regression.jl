resd = residuals(ols)
resd .^=2
samples = size(resd,1)
resd = sum(resd)
nK = samples - samples - 1
temp = resd / nK
sqrt(temp) #residuals standard error

##R code to translate to julia
##			tt <- abs(pm1[1] - df2) / ( summary.lm((model1))$sigma * sqrt( 1+(1/length(cmodel1$scores$xscores[,1])) + ((df1 - mean(cmodel1$scores$xscores[,1]))^2) / (length(cmodel1$scores$xscores[,1]) * sd(cmodel1$scores$xscores[,1])^2) ) )
##			pp <- 2 * pt(-abs(as.numeric(tt)), df = length(cmodel1$scores$xscores[,1]) - 2)


@everywhere function measurement_counter(v1)
	res = 0
	for i in 1:size(v1,1)
		if v1[i] != 0
			res += 1
		end
	end
	return res
end

@everywhere function REGS_worker(v1, m2, li, RL, RR)
	results = zeros(size(m2,1),size(m2,2)+7+size(v1,2)) #plus 7 and size of measurements for v1 m2?
	res_1 = measurement_counter(v1[1,1:end]) #transposes and counts
	dsum_1 = log(sum(v1)) #should work outside of the loops since 0 
		refd_a = []
		refd_b = []
	for x in 1:size(m2,1)
		res_2 = measurement_counter(m2[x,1:end])
		dsum_2 = log(sum(m2[x,1:end]))


		refd_1 = []
		refd_2 = []

		for i in 1:size(RL,1) #works since RL and RR have same number of rows due to R parsing therefore index of row matches specimens
			m_counter_1 = 0
			m_counter_2 = 0
			ref_dsum_1 = 0
			ref_dsum_2 = 0
			for j in 1:size(v1,2)
				if v1[j] != 0 && RL[i,j] != 0
					m_counter_1 += 1 #counts if reference data measurements match that of comparison
					ref_dsum_1 += RL[i,j]
				end
			end
			for j in 1:size(m2,2)
				if m2[x,j] != 0 && RR[i,j] != 0
					m_counter_2 += 1 #counts if reference data measurements match that of comparison
					ref_dsum_2 += RR[i,j]
				end
			end
			if m_counter_1 == sum(res_1) && m_counter_2 == sum(res_2) #if ref 1 and ref 2 both match the sort comparisons
				push!(refd_1, log(ref_dsum_1))
				push!(refd_2, log(ref_dsum_2))
			end
		end
		push!(refd_a, refd_1)
		push!(refd_b, refd_2)
		#add regression here??
		#lm(X ~ Y, refd) #?
		#println(refd)


	end
end

function REGS(SL, SR, RL, RR)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = REGS_worker(SL[k,:], SR, k, RL, RR)
	end
	return Results
end