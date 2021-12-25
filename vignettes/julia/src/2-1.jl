# Read in data
path = joinpath("..", "inst", "2020", "day2.txt");
dat = readlines(path);

function sled_password_check(dat)
    results = 0
    for i in dat
        m = match(r"(\d+)-(\d+)\s+(.):\s+(.+)", i)  # grep
        minimum = parse(Int64, m.captures[1])       # convert to Int64
        maximum = parse(Int64, m.captures[2])
        letter = m.captures[3]
        password = m.captures[4]
        n = count(letter, password)
        results += (n >= minimum && n <= maximum) ? 1 : 0
    end
    results
end;

# How many passwords are valid according to their policies?
sled_password_check(dat)
