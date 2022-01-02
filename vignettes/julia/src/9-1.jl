# Read in data ------------------------------------------------------------

path = joinpath("..", "inst", "2020", "day9.txt");
dat = readlines(path);
dat = parse.(Int64, dat);                       # convert string to numeric

preamble = 25;

# Define functions --------------------------------------------------------

function checksums(dat, preamble)

    # generate pairwise combination index
    cmb = combinations(1:preamble, 2) |> collect    
    # find sum of each pairwise combination
    add = map(x -> dat[x[1]] + dat[x[2]], cmb)      
    
    for i in eachindex(dat)
    
        # don't check preamble
        if i ∈ 1:preamble                           
            continue
        end
    
        # remove elements outwith the preamble 
        tmp = i - preamble - 1                         
        remove = findall(x -> x[1] == tmp || x[2] == tmp, cmb)
        deleteat!(cmb, remove)
        deleteat!(add, remove)

        # generate next set of indices
        tmp = (tmp + 1):(i - 1)                     
        ind = map(x -> [i, x], tmp)                 
        append!(cmb, ind)
    
        # generate next set of additions     
        new = map(x -> dat[x[1]] + dat[x[2]], ind)            
        append!(add, new)

        # return invalid number
        if dat[i] ∉ add                             
            return(dat[i])
        end

    end
end;

# Find the first number in the list (after the preamble) which is not the sum of 
# two of the 25 numbers before it

# What is the first number that does not have this property?

invalid = checksums(dat, preamble)
