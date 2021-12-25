module AOC2020

using DelimitedFiles
using Combinatorics
using Formatting

function here()
     replace(pwd(), "/vignettes/julia/src" => "");
end

include("1-1.jl")
#include("1-2.jl")
include("2-1.jl")
include("2-2.jl")
include("3-1.jl")
include("3-2.jl")
include("4-1.jl")
include("4-2.jl")
#include("5-1.jl")
#include("5-2.jl")

end