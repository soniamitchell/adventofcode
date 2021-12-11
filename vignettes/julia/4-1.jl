# Read in data
path = joinpath(here, "inst", "2020", "day4.txt");
dat = readlines(path);

function parsepassports(dat) 
    record = []
    d = Dict{String, String}()
    for i in eachindex(dat)
        if isempty(dat[i])
            push!(record, d)
            d = Dict{String, String}()
        else
            for j in split(dat[i])
                key, value = split(j, ":")
                d[key] = value
            end
            i == length(dat) ? push!(record, d) : nothing
        end
    end
    record
end;

function countpassports(passports)
    counter = 0
    fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    for i in eachindex(passports)
        invalid = sum([!haskey(passports[i], key) for key in fields]) > 0
        counter += invalid ? 0 : 1
    end
    counter
end;

# How many passports are valid?
passports = parsepassports(dat);
countpassports(passports)
