# Read in data
path = joinpath("$(here())", "inst", "2020", "day5.txt");
dat = readlines(path);

row_min, row_max = 0, 127

function seat(dat)
    for i in eachindex(dat)
        # Get row
        row_directions = dat[i][1:7]
        for j in row_directions
#            if j == only("F")
 #               row_max = floor(row_max - (row_max - row_min) / 2)
#                row_min = row_min

#            elseif j == only("B")
#                row_max = row_max
#                row_min = round(row_min + (row_max - row_min) / 2)
#            end
#            return row_min
        end

        # Get seat
#        seat = dat[i][8:10]
#        for j in seat
 #       end
    end
end

seat(dat)
