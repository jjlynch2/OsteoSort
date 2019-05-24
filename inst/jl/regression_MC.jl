function REGS(SL, SR, RL, RR, PL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = REGS_worker(SL[k,:], SR, k, RL, RR, PL)
	end
	return Results
end