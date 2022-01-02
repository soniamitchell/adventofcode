# Find a contiguous set of at least two numbers in your list which sum to the 
# invalid number from step 1

function findweakness(dat, invalid)
    for i in eachindex(dat)

        total = dat[i]
        j = i
    
        while(total < invalid)
            j += 1
            total += dat[j]
        end
    
        if total == invalid
            vals = dat[i:j]                         # Find encryption weakness
            weakness = minimum(vals) + maximum(vals)
            return(weakness)
        end
    
    end
end

findweakness(dat, invalid)