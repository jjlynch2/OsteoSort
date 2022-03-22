#extract initial alignment landmarks
@everywhere function extract_landmarks(X_array::Array)
	ia = zeros(Int, size(X_array,1),2)
	x = 1
	for i in 1:size(X_array,1)
		if X_array[i,5] > 0
			ia[x,1] = i
			ia[x,2] = Int(X_array[i,5])
			x = x + 1
		end
	end
	if x > 1
		x = x - 1
	end
	return ia[1:x,:]
end

#compare landmarks to find common initial alignment
@everywhere function compare_landmarks(L1::Array, L2::Array)
	if size(L1,1) >= size(L2,1)
		ca = zeros(Int, size(L1,1),2)
	elseif size(L1,1) < size(L2,1)
		ca = zeros(Int, size(L2,1),2)
	end
	xi = 1
	for i in 1:size(L1,1)
		for x in 1:size(L2,1)
			if L1[i,2] == L2[x,2]
				ca[xi,1] = L1[i,1]
				ca[xi,2] = L2[x,1]
				xi = xi + 1
			end
		end
	end
	if xi > 1
		xi = xi - 1
	end
	return ca[1:xi,:]
end