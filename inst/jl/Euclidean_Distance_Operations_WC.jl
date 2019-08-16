##Minimum Euclidean Distances Worker Functions
#############################################
#############################################
#Three-dimensional
@everywhere function MD3D(v1, m2)
	dsum::Float64 = Inf
	for j in 1:size(m2,1)
		dtemp::Float64 = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]) + (v1[3] - m2[j,3]) * (v1[3] - m2[j,3]))
		if dtemp < dsum
			dsum = dtemp
		end
	end
	return dsum
end

#Two-dimensional
@everywhere function MD2D(v1, m2)
	dsum::Float64 = Inf
	for j in 1:size(m2,1)
		dtemp::Float64 = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]))
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
	dsum::Float64 = zeros(1,size(m2,1))
	for j in 1:size(m2,1)
		dsum[1,j] = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]) + (v1[3] - m2[j,3]) * (v1[3] - m2[j,3]))
	end
	return dsum
end

#Two-dimensional
@everywhere function AD2D(v1, m2)
	dsum::Float64 = zeros(1,size(m2,1))
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
		dtemp::Float64 = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]) + (v1[3] - m2[j,3]) * (v1[3] - m2[j,3]))
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
@everywhere function MDI2D(v1, m2)
	dsum = Inf
	dsum_ind = 0
	Dist = zeros(1,2)
	for j in 1:size(m2,1)
		dtemp::Float64 = sqrt((v1[1] - m2[j,1]) * (v1[1] - m2[j,1]) + (v1[2] - m2[j,2]) * (v1[2] - m2[j,2]))
		if dtemp < dsum
			dsum = dtemp
			dsum_ind = j
		end
	end
	Dist[1,1] = dsum
	Dist[1,2] = dsum_ind
	return Dist
end
