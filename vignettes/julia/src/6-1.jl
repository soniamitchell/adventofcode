# Read in data
path = joinpath(".", "inst", "2020", "day6-test.txt");
raw = read(path, String)

groups = split(raw, "\n\n")          # split into groups
dat = replace.(groups, "\n" => "")   # remove line breaks

# For each group, count the number of questions to which anyone answered "yes"
length.(unique.(dat))

# What is the sum of those counts?
sum(length.(unique.(dat)))
