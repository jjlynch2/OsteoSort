##Minimum Euclidean Distances Worker Functions
#############################################
#############################################
#Three-dimensional
@everywhere function MD3D(v1, m2)
	dsum = Inf
	for j in 1:size(m2,1)
		dtemp = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]) + (v1[3] - m2[j,3]) * (v1[3] - m2[j,3]))
		if dtemp < dsum
			dsum = dtemp
		end
	end
	return dsum
end

#Two-dimensional
@everywhere function MD2D(v1, m2)
	dsum = Inf
	for j in 1:size(m2,1)
		dtemp = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]))
		if dtemp < dsum
			dsum = dtemp
		end
	end
	return dsum
end

##Euclidean Distances Worker Functions
#############################################
#############################################
#Three-dimensional
@everywhere function AD3D(v1, m2)
	dsum = zeros(1,size(m2,1))
	for j in 1:size(m2,1)
		dsum[1,j] = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]) + (v1[3] - m2[j,3]) * (v1[3] - m2[j,3]))
	end
	return dsum
end

#Two-dimensional
@everywhere function AD2D(v1, m2)
	dsum = zeros(1,size(m2,1))
	for j in 1:size(m2,1)
		dsum[1,j] = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]))
	end
	return dsum
end

##Minimum Euclidean Distances and Indices Worker Functions
#############################################
#############################################
#Three-dimensional
@everywhere function MDI3D(v1, m2)
	dsum = Inf
	dsum_ind = 0
	Dist = zeros(1,2)
	for j in 1:size(m2,1)
		dtemp = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]) + (v1[3] - m2[j,3]) * (v1[3] - m2[j,3]))
		if dtemp < dsum
			dsum = dtemp
			dsum_ind = j
		end
	end
	Dist[1,1] = dsum
	Dist[1,2] = dsum_ind
	return Dist
end

#Two-dimensional
@everywhere function MDI3D(v1, m2)
	dsum = Inf
	dsum_ind = 0
	Dist = zeros(1,2)
	for j in 1:size(m2,1)
		dtemp = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]))
		if dtemp < dsum
			dsum = dtemp
			dsum_ind = j
		end
	end
	Dist[1,1] = dsum
	Dist[1,2] = dsum_ind
	return Dist
end

##Minimum Euclidean Distances Distributed Call
#############################################
#############################################
#Three-dimensional
@everywhere function MED3D(m1, m2)
	n1 = size(m1,1)
	Dist = SharedArray{Float64}(n1)
	@sync @distributed for k in 1:n1
		Dist[k] = MD3D(m1[k,:], m2)
	end
	return Dist
end

#Two-dimensional
@everywhere function MED2D(m1, m2)
	n1 = size(m1,1)
	Dist = SharedArray{Float64}(n1)
	@sync @distributed for k in 1:n1
		Dist[k] = MD2D(m1[k,:], m2)
	end
	return Dist
end

##Minimum Euclidean Distances Distributed Call
#############################################
#############################################
#Three-dimensional
@everywhere function EDM3D(m1, m2)
	n1 = size(m1,1)
	n2 = size(m2,1)
	Dist = SharedArray{Float64}(n1,n2)
	@sync @distributed for k in 1:n1
		Dist[k,:] = AD3D(m1[k,:], m2)
	end
	return Dist
end

#Three-dimensional
@everywhere function EDM2D(m1, m2)
	n1 = size(m1,1)
	n2 = size(m2,1)
	Dist = SharedArray{Float64}(n1,n2)
	@sync @distributed for k in 1:n1
		Dist[k,:] = AD2D(m1[k,:], m2)
	end
	return Dist
end

#Minimum Euclidean Distances and Indices Distributed Call
#############################################
#############################################
#Three-dimensional
@everywhere function MEDI3D(m1, m2)
	n1 = size(m1,1)
	Dist = SharedArray{Float64}(n1,2)
	@sync @distributed for k in 1:n1
		Dist[k,:] = MDI3D(m1[k,:], m2)
	end
	return Dist
end

#Two-dimensional
@everywhere function MEDI2D(m1, m2)
	n1 = size(m1,1)
	Dist = SharedArray{Float64}(n1,2)
	@sync @distributed for k in 1:n1
		Dist[k,:] = MDI2D(m1[k,:], m2)
	end
	return Dist
end