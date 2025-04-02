function transform(𝐱; optim_args...)
    λ, details = lambda(𝐱; optim_args...)
    transform(𝐱, λ)
end

function transform(𝐱, λ)
    𝐱′ = similar(𝐱, Float64)
    for (i, x) in enumerate(𝐱)
        if x >= 0
            𝐱′[i] = λ ≈ 0 ? log(x + 1) : ((x + 1)^λ - 1)/λ
        else
            𝐱′[i] = λ ≈ 2 ? -log(-x + 1) : -((-x + 1)^(2 - λ) - 1) / (2 - λ)
        end
    end
    𝐱′
end

function lambda(𝐱; interval = (-2.0, 2.0), optim_args...)
    i1, i2 = interval
    res = optimize(λ -> -log_likelihood(𝐱, λ), i1, i2; optim_args...)
    (value=Optim.minimizer(res), details=res)
end

function log_likelihood(𝐱, λ)
    N = length(𝐱)
    𝐲 = transform(float.(𝐱), λ)
    σ² = var(𝐲, corrected = false)
    c = sum(sign.(𝐱) .* log.(abs.(𝐱) .+ 1))
    llf = -N / 2.0 * log(σ²) + (λ - 1) * c
    llf
end
