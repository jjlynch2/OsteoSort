#############################################
#############################################
## absolute value boxcox mean R call
function TTESTABM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTESTABM_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end

## absolute vale boxcox R call
function TTESTAB(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTESTAB_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end

## absolute value mean R call
function TTESTAM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTESTAM_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end

## boxcox mean R call
function TTESTBM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTESTBM_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end

## absolute value R call
function TTESTA(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTESTA_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end

## boxcox R call
function TTESTB(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTESTB_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end

## mean R call
function TTESTM(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTESTM_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end

## R call
function TTEST(SL, SR, RL, RR, TL)
	n1 = size(SL,1)
	n2 = size(SR,1)
	n3 = size(SL,2)
	Results = SharedArray{Float64}(n2*n1,n3+7)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = TTEST_worker(SL[k,:], SR, k, RL, RR, TL)
	end
	return Results
end
