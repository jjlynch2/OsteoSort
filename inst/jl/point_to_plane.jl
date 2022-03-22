#point cloud structure
@everywhere mutable struct PointCloud
    x::Vector{Float64}
    y::Vector{Float64}
    z::Vector{Float64}
    nx::Vector{Float64}
    ny::Vector{Float64}
    nz::Vector{Float64}
    planarity::Vector{Float64}
    no_points::Int64
    sel::Vector{Int64}
    function PointCloud(x, y, z)
        no_points = length(x)
        new(x,
            y,
            z,
            fill(NaN, no_points),
            fill(NaN, no_points),
            fill(NaN, no_points),
            fill(NaN, no_points),
            no_points,
            Int64[])
    end
end

#select points for fixed point cloud
@everywhere function select_n_points!(pc::PointCloud, n)
    if pc.no_points > n
        pc.sel = round.(Int, range(1, pc.no_points, length=n))
    else
        pc.sel = collect(1:pc.no_points)
    end
end

#estimate normals
@everywhere function estimate_normals!(pc::PointCloud, neighbors)
    kdtree = KDTree([pc.x'; pc.y'; pc.z'])
    query_points = [pc.x[pc.sel]'; pc.y[pc.sel]'; pc.z[pc.sel]']
    idxNN_all_qp, = knn(kdtree, query_points, neighbors, false)
    for (i, idxNN) in enumerate(idxNN_all_qp)
        selected_points = [pc.x[idxNN]'; pc.y[idxNN]'; pc.z[idxNN]']
        C = cov(selected_points, dims=2)
        F = eigen(C)
        pc.nx[pc.sel[i]] = F.vectors[1,1]
        pc.ny[pc.sel[i]] = F.vectors[2,1]
        pc.nz[pc.sel[i]] = F.vectors[3,1]
        pc.planarity[pc.sel[i]] = (F.values[2]-F.values[1])/F.values[3];
    end

end

#transform 
@everywhere function transform!(pc, H)
    XInH = euler_coord_to_homogeneous_coord([pc.x pc.y pc.z])
    XOutH = transpose(H*XInH')
    XOut = homogeneous_coord_to_euler_coord(XOutH)
    pc.x = XOut[:,1]
    pc.y = XOut[:,2]
    pc.z = XOut[:,3]
    return pc
end

#find coordinate correspondences
@everywhere function matching!(pcmov::PointCloud, pcfix)
    kdtree = KDTree([pcmov.x'; pcmov.y'; pcmov.z'])
    query_points = [pcfix.x[pcfix.sel]';pcfix.y[pcfix.sel]';pcfix.z[pcfix.sel]']
    idxNN, = knn(kdtree, query_points, 1)
    pcmov.sel = vcat(idxNN...)
    dx = pcmov.x[pcmov.sel] - pcfix.x[pcfix.sel]
    dy = pcmov.y[pcmov.sel] - pcfix.y[pcfix.sel]
    dz = pcmov.z[pcmov.sel] - pcfix.z[pcfix.sel]
    nx = pcfix.nx[pcfix.sel]
    ny = pcfix.ny[pcfix.sel]
    nz = pcfix.nz[pcfix.sel]
    distances = [dx[i]*nx[i] + dy[i]*ny[i] + dz[i]*nz[i] for i in 1:length(pcmov.sel)]
    return distances
end

#reject based on distance to plane
@everywhere function reject!(pcmov::PointCloud, pcfix::PointCloud, min_planarity, distances)
    planarity = pcfix.planarity[pcfix.sel]
    med = median(distances)
    sigmad = mad(distances, normalize=true)
    keep_distance = [abs(d-med) <= 3*sigmad for d in distances]
    keep_planarity = [p > min_planarity for p in planarity]
    keep = keep_distance .& keep_planarity
    pcmov.sel = pcmov.sel[keep]
    pcfix.sel = pcfix.sel[keep]
    deleteat!(distances, .!keep)
    return nothing
end

#estimate transformation
@everywhere function estimate_rigid_body_transformation(x_fix, y_fix, z_fix, nx_fix, ny_fix, nz_fix, x_mov, y_mov, z_mov)
    A = hcat(-z_mov.*ny_fix + y_mov.*nz_fix,z_mov.*nx_fix - x_mov.*nz_fix,-y_mov.*nx_fix + x_mov.*ny_fix,nx_fix,ny_fix,nz_fix)
    l = nx_fix.*(x_fix-x_mov) + ny_fix.*(y_fix-y_mov) + nz_fix.*(z_fix-z_mov)
    x = A\l
    residuals = A*x-l
    R = euler_angles_to_linearized_rotation_matrix(x[1], x[2], x[3])
    t = x[4:6]
    H = create_homogeneous_transformation_matrix(R, t)
    return H, residuals
end

@everywhere function euler_angles_to_linearized_rotation_matrix(α1, α2, α3)
    dR = [  1 -α3  α2
           α3   1 -α1
          -α2  α1   1]
end

@everywhere function create_homogeneous_transformation_matrix(R, t)
    H = [R          t
         zeros(1,3) 1]
end

@everywhere function euler_coord_to_homogeneous_coord(XE)
    no_points = size(XE, 1)
    XH = [XE ones(no_points,1)]
end

@everywhere function homogeneous_coord_to_euler_coord(XH)
    XE = XH[:,1:3]./XH[:,4]
end

@everywhere function check_convergence_criteria(distances_new, distances_old, min_change)
    change(new, old) = abs((new-old)/old*100)
    change_of_mean = change(mean(distances_new), mean(distances_old))
    change_of_std = change(std(distances_new), std(distances_old))
    return change_of_mean < min_change && change_of_std < min_change ? true : false
end