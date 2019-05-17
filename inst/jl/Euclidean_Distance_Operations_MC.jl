##Minimum Euclidean Distances Distributed Call
#############################################
#############################################
#Three-dimensional
function MED3D(m1, m2)
	n1::Int = size(m1,1)
	Dist = SharedArray{Float64}(n1)
	@sync @distributed for k in 1:n1
		Dist[k] = MD3D(m1[k,:], m2)
	end
	return Dist
end

#Two-dimensional
function MED2D(m1, m2)
	n1::Int = size(m1,1)
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
function EDM3D(m1, m2)
	n1::Int = size(m1,1)
	n2::Int = size(m2,1)
	Dist = SharedArray{Float64}(n1,n2)
	@sync @distributed for k in 1:n1
		Dist[k,:] = AD3D(m1[k,:], m2)
	end
	return Dist
end

#Three-dimensional
function EDM2D(m1, m2)
	n1::Int = size(m1,1)
	n2::Int = size(m2,1)
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
function MEDI3D(m1, m2)
	n1::Int = size(m1,1)
	Dist = SharedArray{Float64}(n1,2)
	@sync @distributed for k in 1:n1
		Dist[k,:] = MDI3D(m1[k,:], m2)
	end
	return Dist
end

#Two-dimensional
function MEDI2D(m1, m2)
	n1::Int = size(m1,1)
	Dist = SharedArray{Float64}(n1,2)
	@sync @distributed for k in 1:n1
		Dist[k,:] = MDI2D(m1[k,:], m2)
	end
	return Dist
end