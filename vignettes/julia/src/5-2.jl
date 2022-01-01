# What is the ID of your seat?

seats = sort(seat_ids)

myseat = function(seats)
    for i in 1:maximum(seats)
        if i ∉ seats && i-1 ∈ seats && i+1 ∈ seats
            return(i)
        end
    end
end

myseat(seats)
