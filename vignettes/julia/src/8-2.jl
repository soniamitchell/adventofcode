# Run boot code ----------------------------------------------------------

function boot(inst, vals)

    stop_condition = length(inst) + 1

    for i in 1:length(inst)

        new_inst = copy(inst)
    
        if inst[i] == "acc"
            continue
        elseif inst[i] == "jmp"
            new_inst[i] = "nop"
        elseif inst[i] == "nop"
            new_inst[i] = "jmp"
        end
        
        accumulator = 0                         # initialise accumulator
        j = 1                                   # initialise place in instruction list
        log = [] 
        
        while j ∉ log                           # stop before an instruction is run again
            if j == stop_condition              # found correct termination point!
                return(accumulator)  
            end
            push!(log, j)                       # record i in log
    
            if new_inst[j] == "acc"
                accumulator += vals[j]          # add value to accumulator
                j += 1                          # next instruction
            elseif new_inst[j] == "jmp"
                j += vals[j]                    # jump to a new instruction
            elseif new_inst[j] == "nop"
                j += 1                          # next instruction
            end
        end
        println(i)

    end
end

boot(inst, vals)