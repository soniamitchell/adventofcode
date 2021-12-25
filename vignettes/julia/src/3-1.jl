# Read in data
path = joinpath("$(here())", "inst", "2020", "day3.txt");
dat = readlines(path);

function trees(x, y, map)
    xpos = ypos = 1
    results = 0
    xlim = length(map[1])
    ylim = length(map)
    while ypos != ylim
        xpos += x
        xpos = (xpos > xlim) ? xpos - xlim : xpos
        ypos += y
        results += map[ypos][xpos] == only("#") ? 1 : 0
    end
    results
end;

# Starting at the top-left corner of your map and following a slope of right 3 and down 1, 
# how many trees would you encounter?
trees(3, 1, dat)
