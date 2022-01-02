# Read in data ------------------------------------------------------------

path = joinpath("..", "inst", "2020", "day9.txt") 
dat = readlines(path);
dat = parse.(Int64, dat);                       # convert string to numeric

preamble = 25;

# Define functions --------------------------------------------------------

function checksums(dat, preamble)

    cmb = combinations(1:preamble, 2) |> collect    # generate pairwise combination index
    add = map(x -> dat[x[1]] + dat[x[2]], cmb)      # find sum of each pairwise combination
    
    for i in eachindex(dat)
    
        if i ∈ 1:preamble                           # don't check preamble
            continue
        end
    
        tmp = i - preamble - 1                         # remove elements outwith the preamble 
        remove = findall(x -> x[1] == tmp || x[2] == tmp, cmb)
        deleteat!(cmb, remove)
        deleteat!(add, remove)

        tmp = (tmp + 1):(i - 1)                     # generate next set of indices
        ind = map(x -> [i, x], tmp)                 
        append!(cmb, ind)
    
        new = map(x -> dat[x[1]] + dat[x[2]], ind)  # generate next set of additions               
        append!(add, new)

        if dat[i] ∉ add                             # return invalid number
            return(dat[i])
        end

    end
end;

# Find the first number in the list (after the preamble) which is not the sum of 
# two of the 25 numbers before it

# What is the first number that does not have this property?

invalid = checksums(dat, preamble)
