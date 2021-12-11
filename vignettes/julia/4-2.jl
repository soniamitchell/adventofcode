function checkfields(passports)
    fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    colours = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    counter = 0
    for i in passports
        invalid = sum([!haskey(i, key) for key in fields]) > 0

        if (!invalid)
            byr = 1920 <= parse(Int, i["byr"]) <= 2002
            iyr = 2010 <= parse(Int, i["iyr"]) <= 2020
            eyr = 2020 <= parse(Int, i["eyr"]) <= 2030
    
            if endswith(i["hgt"], "cm")
                tmp = parse(Int, replace(i["hgt"], "cm" => ""))
                hgt = 150 <= tmp <= 193
            elseif endswith(i["hgt"], "in")
                tmp = parse(Int, replace(i["hgt"], "in" => ""))
                hgt = 59 <= tmp <= 76
            else 
                hgt = false
            end
    
            hcl = match(r"^#[0-9a-f]{6}", i["hcl"]) != nothing
            ecl = issubset([i["ecl"]], colours)
            pid = match(r"^[0-9]{9}$", i["pid"]) != nothing
            
            # Is passport valid?
            test = byr & iyr & eyr & hgt & hcl & ecl & pid
            counter += test ? 1 : 0
        end
    end
    counter
end;

# How many passports are valid?
checkfields(passports)
