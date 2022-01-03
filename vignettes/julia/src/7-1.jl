# Read in data
path = joinpath(".", "inst", "2020", "day7-test.txt");
raw = read(path, String);
dat = split(raw, "\n");
dat = filter(!isempty, dat);


meta = map(dat) do str
    tmp = match(r"(\D*\s\D*)\sbags\scontain\s(.*)", str)
    tmp.captures
end


# How many bag colors can eventually contain at least one shiny gold bag? 
