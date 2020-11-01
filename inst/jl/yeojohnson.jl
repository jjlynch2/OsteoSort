#This is modified from the YeoJohnsonTrans.jl package and licensed under the MIT "Expat" License:

#Copyright (c) 2018: Tom Kwong.

#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
