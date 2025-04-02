function cache_check(res, res_cache, cache_index, v1)
	for x_cache in 1:cache_index
		cache = true
		for i_cache in 1:v1
			if res[i_cache] != res_cache[x_cache][i_cache]
				cache = false
				break
			end
		end
		if cache
			return x_cache
		end
	end
	return 0
end

function res_check(v1, m2)
	res = zeros(1,size(m2,1))
	for g in 1:size(v1,1)
		if v1[g] != 0 && m2[g] != 0 #measurement is being used
			res[g] = 1 #still recover index
		end
	end
	return res
end