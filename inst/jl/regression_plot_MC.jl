function REGSL_plot(SL, SR, RL, RR)
	res_1 = measurement_counter(SL)
	res_2 = measurement_counter(SR)
	dsum_1 = log(sum(SL))
	dsum_2 = log(sum(SR))
	refd_1 = zeros(1,1)
	refd_2 = zeros(1,1)
	for i in 1:size(RL,1)
		m_counter_1 = 0
		m_counter_2 = 0
		ref_dsum_1 = 0
		ref_dsum_2 = 0
		for j in 1:size(SL,1)
			if SL[j] != 0 && RL[i,j] != 0
				m_counter_1 += 1
				ref_dsum_1 += RL[i,j]
			end
		end
		for j in 1:size(SR,1)
			if SR[j] != 0 && RR[i,j] != 0
				m_counter_2 += 1
				ref_dsum_2 += RR[i,j]
			end
		end
		if m_counter_1 == res_1 && m_counter_2 == res_2
			refd_1 = vcat(refd_1, log(ref_dsum_1))
			refd_2 = vcat(refd_2, log(ref_dsum_2))
		end
	end
	refd_1 = refd_1[2:end,] #remove 0
	refd_2 = refd_2[2:end,] #remove 0
	return [refd_1, refd_2, dsum_1, dsum_2]
end
