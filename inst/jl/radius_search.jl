function radius_search(m1, r)
	m1 = convert(Matrix{Float64},m1)
	m2 = m1
	m1 = Matrix(m1')
	kdtree = KDTree(m1)
	n1::Int = size(m2,1)
	Ind = SharedArray{Float64}((n1,1))
	@sync @distributed for k in 1:n1
		Ind[k] = size(inrange(kdtree, m2[k,:], r, true),1)
	end
	return Ind
end
