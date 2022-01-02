# Read in data ------------------------------------------------------------

path = joinpath(".", "inst", "2020", "day8.txt");
dat = read(path, String);

# Define functions --------------------------------------------------------

function tidyday8(dat)
    dat = split(dat, "\n")                      # split by new line
    dat = filter(!isempty, dat)                 # remove empty last element
    dat = split.(dat, " ")                      # split by space

    # Tidy up data
    inst = [x[1] for x in dat]                  # extract instructions
    val = [x[2] for x in dat]                   # extract values

    vals = map(val) do str
    mat = match.(r"[\+|\-](\d*)", str)          # regex to extract number
    cap = mat.captures                          # extract capture group
    num = parse(Int64, cap[1])                  # convert string to number
    contains.(str, "-") ? num * -1 : num        # multiply by -1 if negative
    end

    inst, vals
end;

"""
    boot(inst, vals)

Boot code

# Arguments
- `inst`: boot up instructions
- `vals`: values associated with each instruction
"""
function boot(inst, vals)
    accumulator = 0;                            # initialise accumulator
    i = 1;                                      # initialise place in instruction list
    log = [];

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
    return(accumulator)
end;

# Run boot code ----------------------------------------------------------

inst, vals = tidyday8(dat);

# Immediately before any instruction is executed a second time, what value is in the 
# accumulator?
boot(inst, vals)
