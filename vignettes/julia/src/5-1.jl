# Read in data
path = joinpath("..", "inst", "2020", "day5.txt");
dat = readlines(path);

function findseat(dat)
   
    output = []

    for i in eachindex(dat)
        
        row_min, row_max = 0, 127
        col_min, col_max = 0, 7

        # Get row
        row_directions = dat[i][1:7]
        for j in row_directions
            if j == only("F")
                row_max = floor(Int, row_max - (row_max - row_min) / 2)
            elseif j == only("B")
                row_min = ceil(Int, row_min + (row_max - row_min) / 2)
            end
        end

        # Get column
        seat = dat[i][8:10]
        for j in seat
            if j == only("L")
                col_max = floor(Int, col_max - (col_max - col_min) / 2)
            elseif j == only("R")
                col_min = ceil(Int, col_min + (col_max - col_min) / 2)
            end
        end

    # Calculate seat ID
    seat_id = (row_min * 8) + col_max
    push!(output, seat_id)
    end

    output
end;

# Try test data
test = readlines(joinpath("..", "inst", "2020", "day5-test.txt"));
@assert all(findseat(test) .== [357, 567, 119, 820])

# What is the highest seat ID on a boarding pass?
seat_ids = findseat(dat);
maximum(seat_ids)
