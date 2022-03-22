#remove fracture margins and calculate modified Hausdorff distance (MDH)
@everywhere function remove_fragmented_margins(fixed_array, moving_array, fixed_indices, moving_indices)
	test1 = idxdst(fixed_array',moving_array')
	test2 = idxdst(moving_array',fixed_array')
	test1 = test1[setdiff(1:end, moving_indices),:]
	test2 = test2[setdiff(1:end, fixed_indices),:]
	for i in 1:size(fixed_indices,1)
		test1 = test1[(test1[:,1] .!= fixed_indices[i]),:]
	end
	for i in 1:size(moving_indices,1)
		test2 = test2[(test2[:,1] .!= moving_indices[i]),:]
	end
	MDH = max(mean(test1[:,2]), mean(test2[:,2])) #Modified Hausdorff Distance
	return MDH
end

#extract fragmented margin landmarks
@everywhere function extract_fragment_margin(X_array::Array)
	ia = zeros(Int, size(X_array,1),1)
	x = 1
	for i in 1:size(X_array,1)
		if X_array[i,4] == 1
			ia[x,1] = i
			x = x + 1
		end
	end
	if x > 1
		x = x - 1
	end
	return ia[1:x,:]
end