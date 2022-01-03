# Read in data ------------------------------------------------------------

path = joinpath(".", "inst", "2020", "day11.txt")
dat = readlines(path) 
dat = map(x -> split(x, ""), dat)
grid = mapreduce(permutedims, vcat, dat) 

# Define functions --------------------------------------------------------

function neighbours(grid, r, c)
    coords = [[r-1, c-1], [r-1, c], [r-1, c+1], 
              [r, c-1], [r, c+1], 
              [r+1, c-1], [r+1, c], [r+1, c+1]]
    valid = map(x -> checkbounds(Bool, grid, x[1], x[2]), coords)
    coords[valid] 
end

function simulate(grid)

    #   if i == 1
           replace!(grid, "L" => "#")
   #    end
   
    # Determine which grids are going to flip
    flip = []
    rows, cols = size(grid)
    for r in 1:rows, c in 1:cols
        if grid[r, c] == "."
            continue
        end
               
        coords = neighbours(grid, r, c)         # get valid neighbours
        contents = map(x -> grid[x[1], x[2]], coords)      # get neighbours' contents
   
        if sum("#" .== contents) >= 4
            push!(flip, [r, c])               
        end
    end


    flip
end

# Run simulation ----------------------------------------------------------




tmp = simulate(grid)

