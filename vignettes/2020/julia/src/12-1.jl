# Read in data ------------------------------------------------------------

path = joinpath(".", "inst", "2020", "day12-test.txt")
dat = readlines(path) 


m = match.(r"(\D)\d*", dat)
[x -> transpose(x.captures), m]
