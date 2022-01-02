path = joinpath(".", "inst", "2020", "day10.txt")

# Find a chain that uses all of your adapters to connect the charging outlet to your 
# device's built-in adapter and count the joltage differences between the charging 
# outlet, the adapters, and your device

function day10(path)
    dat = readlines(path)                           # read data
    dat = parse.(Int64, dat)                        # convert string to integer
end

function part1(dat)
    tmp = copy(dat) |> sort                         # sort values
    whole = vcat(0, tmp, tmp[end] + 3)              # prepend with 0, append with +3
    ones = count(==(1), diff(sort(whole)))          # count number of ones
    threes = count(==(3), diff(sort(whole)))        # count number of threes
    ones * threes
end

# What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
day10(path) |> part1
