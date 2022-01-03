dat = split.(groups, "\n") 
characters = unique([(dat...)...])

counts = []

for i in dat
    tmp = 0
    for j in characters
        println(typeof(i))
        println(typeof(j))

        # count the number of times `i` is in `j`
        n_char = sum([(i...)...] .== j)   
        
        if n_char == length(i) 
            
            tmp = tmp + 1
        end
    end
    push!(counts, tmp)
end

counts

