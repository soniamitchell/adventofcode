# Read in data
path = joinpath("..", "inst", "2020", "day1.txt")
dat = vec(readdlm(path));

# Find the two entries that sum to 2020
numbers = [x for x in combinations(dat, 2) if sum(x) == 2020];

# Convert 1-element Vector{Vector{Float64}} to 2-element Vector{Float64}
numbers = reshape(numbers)[1];

# What do you get if you multiply them together?
prod(Int, numbers)
