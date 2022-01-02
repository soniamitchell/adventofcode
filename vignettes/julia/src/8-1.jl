# Read in data
path = joinpath(".", "inst", "2020", "day8.txt");
dat = split(read(path, String), "\n");      # split by new line
dat = filter(!isempty, dat);                # remove empty last element
dat = split.(dat, " ");                     # split by space

# Tidy up data
inst = [x[1] for x in dat]                  # extract instructions
val = [x[2] for x in dat]                   # extract values

vals = map(val) do str
    mat = match.(r"[\+|\-](\d*)", str)      # regex to extract number
    cap = mat.captures                      # extract capture group
    num = parse(Int64, cap[1])              # convert string to number
    contains.(str, "-") ? num * -1 : num    # multiply by -1 if negative
end

# Run your copy of the boot code

accumulator = 0                             # initialise accumulator
i = 1                                       # initialise place in instruction list
log = []

while i ∉ log                               # stop before an instruction is run again
    push!(log, i)                           # record i in log

    if inst[i] == "acc"
        accumulator += vals[i]              # add value to accumulator
        i += 1                              # next instruction
    elseif inst[i] == "jmp"
        i += vals[i]                        # jump to a new instruction
    elseif inst[i] == "nop"
        i += 1                              # next instruction
    end
end

# Immediately before any instruction is executed a second time, what value is in the 
# accumulator?
accumulator