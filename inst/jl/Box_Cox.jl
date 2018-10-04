#This is modified from the BoxCoxTrans.jl package and licensed under the MIT "Expat" License:

#Copyright (c) 2018: Tom Kwong.

#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

@everywhere transform(𝐱) = transform(𝐱, lambda(𝐱))

@everywhere transform(𝐱, λ) = @. λ ≈ 0 ? log(𝐱) : (𝐱 ^ λ - 1) / λ

@everywhere function lambda(𝐱; interval = (-2.0, 2.0))
    i1, i2 = interval
    res = optimize(λ -> -mle(𝐱, λ), i1, i2)
    return minimizer(res)
end

@everywhere function mle(𝐱, λ)
    𝐲 = transform(float.(𝐱), λ)
    μ = mean(𝐲)
    N = length(𝐱)
    llf = (λ - 1) * sum(log.(𝐱))
    llf -= N / 2.0 * log(sum((𝐲 .- μ) .^ 2) / N)
    return llf
end