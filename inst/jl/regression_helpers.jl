function res_std_err(model)
	resd = residuals(model)
	resd .^=2
	samples = size(resd,1)
	resd = sum(resd)
	nK = samples - 2
	temp = resd / nK
	return sqrt(Complex(temp))
end

function reg_t_stat(sigma, r2, predicted, comparison, comparison_p, mean, sd, n)
	return abs(predicted - comparison) / (sigma * sqrt(1+(1/n) + ((comparison_p - mean) ^2) / (n*(sd^2))))
end
