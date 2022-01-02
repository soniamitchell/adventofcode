# Read in data ------------------------------------------------------------

path = joinpath("..", "inst", "2020", "day10.txt");
dat = readlines(path);
dat = parse.(Int64, dat);                       # convert string to numeric

# Define functions --------------------------------------------------------

function chain(dat)
    current = 0
    log = []
    target = length(dat)

    while length(log) != target
        # find index of next adapter
        possible = collect(current .+ (1:3))    # possible values to match against         
        index = indexin(possible, dat)          # find index of matching values       
        filter!(x -> x != nothing, index)       # remove `nothing`    
        index = index[1]                        # use first index (lowest value)


        value = dat[index]
        # store difference
        push!(log, value - current)

        # set current value
        current = copy(value)
   
        # remove value from `dat`
        deleteat!(dat, index)
    end

    push!(log, 3)
    log
end;

# Find a chain that uses all of your adapters to connect the charging outlet to your 
# device's built-in adapter and count the joltage differences between the charging 
# outlet, the adapters, and your device

res = chain(dat);

# What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

sum(res .== 1) * sum(res .== 3)
