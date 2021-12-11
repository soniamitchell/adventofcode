# Read in data
path = joinpath(here, "inst", "2020", "day2.txt");
dat = readlines(path);

counter = 0
for i in dat
    m = match(r"(\d+)-(\d+)\s+(.):\s+(.+)", i)  # grep
    minimum = parse(Int64, m.captures[1])       # convert to Int64
    maximum = parse(Int64, m.captures[2])
    letter = m.captures[3]
    password = m.captures[4]
    n = count(letter, password)
    counter += (n >= minimum && n <= maximum) ? 1 : 0
end

# How many passwords are valid according to their policies?
counter
