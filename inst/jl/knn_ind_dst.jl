#calculate distances and indices of correspondances
@everywhere function idxdst(m1, m2)
    kdtree = KDTree(m1)
    query_points = m2
    idxNN, dstNN = knn(kdtree, query_points, 1)
    return hcat(vcat(idxNN...), vcat(dstNN...))
end