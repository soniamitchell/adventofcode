module Testday1

using adventofcode2020
using Test
using DelimitedFiles
using Combinatorics

# Read in data
path = joinpath("2020", "day1.txt")
dat = vec(readdlm(path));

# Find the two entries that sum to 2020
numbers = [x for x in combinations(dat, 2) if sum(x) == 2020];
numbers = reshape(numbers)[1];

# What do you get if you multiply them together?
answer = 514579

Test.@testset "part1" begin
    @test prod(numbers) == answer
end

end
