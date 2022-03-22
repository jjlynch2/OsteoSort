#calculate rotation between two point clouds
@everywhere function trafo(moving_array::Array, fixed_array::Array)
	N = fixed_array' * moving_array
	sv = svd(N)
	R = sv.V .* sign(det(N)) #preserves side?
	R = sv.V * sv.U'
	return R
end

#apply transformation
@everywhere function applyTrafo(moving_array, R)
	transformed = moving_array * R
	return transformed
end
