#worker for distributed function
@everywhere function OMS_worker(filelist1, filelist2, k)
	X_fix = readdlm(filelist1, skipstart = 1)
	Results = zeros(size(filelist2,1),3) 
	x = 1
	for i in 1:size(filelist2,1)
		ft = filelist2[i]
		X_mov = readdlm(ft, skipstart = 1) 
		co = Int(round(size(X_fix,1) * 0.2)) #20% of landmarks
		Re = simpleicp(X_fix, X_mov, correspondences=co)
		Results[x,1] = k
		Results[x,2] = i
		Results[x,3] = Re
		x = x + 1
	end
	return Results
end

#distributed function call
function OMS(filelist1, filelist2)
	n1 = size(filelist1,1)
	n2 = size(filelist2,1)
	Results = SharedArray{Float64}(n2*n1,3)
	@sync @distributed for k in 1:n1
		Results[((k*n2) - n2+1):(k*n2),:] = OMS_worker(filelist1[k], filelist2, k)
	end
	return Results
end

@everywhere function simpleicp(X_fix::Matrix, X_mov::Matrix; correspondences::Integer=1000, neighbors::Integer=10, min_planarity::Number=0.3, min_change::Number=3, max_iterations::Integer=100)
	size(X_fix)[2] == 5 || error(""""X_fix" must have 5 columns""")
	size(X_mov)[2] == 5 || error(""""X_mov" must have 5 columns""")
	correspondences >= 10 || error(""""correspondences" must be >= 10""")
	min_planarity >= 0 && min_planarity < 1 || error(""""min_planarity" must be >= 0 and < 1""")
	neighbors >= 2 || error(""""neighbors" must be >= 2""")
	min_change > 0 || error(""""min_change" must be > 0""")
	max_iterations > 0 || error(""""max_iterations" must be > 0""")
	X_mov[:,1] = X_mov[:,1] * -1
	L1 = extract_landmarks(X_fix)
	L2 = extract_landmarks(X_mov)
	Landmarks = compare_landmarks(L1,L2)
	if Landmarks[1,1] >= 3 #checks if there are at least 3 corresponding initial landmarks
		@info "Start point-to-point initial alignment ..."
		R = trafo(X_mov[Landmarks[:,2],1:3], X_fix[Landmarks[:,1],1:3])
		X_mov[:,1:3] = applyTrafo(X_mov[:,1:3], R)
	end
	pcfix = PointCloud(X_fix[:,1], X_fix[:,2], X_fix[:,3])
	pcmov = PointCloud(X_mov[:,1], X_mov[:,2], X_mov[:,3])
	select_n_points!(pcfix, correspondences)
	sel_orig = pcfix.sel
	estimate_normals!(pcfix, neighbors)
	H = Matrix{Float64}(I,4,4)
	residual_distances = Any[]
	@info "Start point-to-plane alignment ..."
	for i in 1:max_iterations
	    initial_distances = matching!(pcmov, pcfix)
	    reject!(pcmov, pcfix, min_planarity, initial_distances)
	    dH, residuals = estimate_rigid_body_transformation(pcfix.x[pcfix.sel], pcfix.y[pcfix.sel], pcfix.z[pcfix.sel],pcfix.nx[pcfix.sel], pcfix.ny[pcfix.sel], pcfix.nz[pcfix.sel],pcmov.x[pcmov.sel], pcmov.y[pcmov.sel], pcmov.z[pcmov.sel])
	    push!(residual_distances, residuals)
	    transform!(pcmov, dH)
	    pcfix.sel = sel_orig
	    if i > 1
		if check_convergence_criteria(residual_distances[i], residual_distances[i-1], min_change)
		    @info "Convergence criteria fulfilled!"
		    break
		end
	    end
	end
	@info "Calculating Hausdorff distance ..."
	fix_landmarks = extract_fragment_margin(X_fix)
	mov_landmarks = extract_fragment_margin(X_mov)
	MDH = remove_fragmented_margins(hcat(pcfix.x, pcfix.y, pcfix.z), hcat(pcmov.x, pcmov.y, pcmov.z), fix_landmarks, mov_landmarks)
    return MDH
end