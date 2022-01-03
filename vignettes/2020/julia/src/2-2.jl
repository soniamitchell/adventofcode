function toboggan_password_check(dat)
    
    results = 0

    for i in dat
        m = match(r"(\d+)-(\d+)\s+(.):\s+(.+)", i)  # grep
        pos1 = parse(Int64, m.captures[1])          # convert to Int64
        pos2 = parse(Int64, m.captures[2])
        letter = only(m.captures[3])                # convert to ASCII/Unicode
        password = m.captures[4]

        # Exactly one of these positions must contain the given letter
        test = (password[pos1] == letter) + (password[pos2] == letter) == 1
        results += test ? 1 : 0
    end

    results
    
end;

# How many passwords are valid according to the new interpretation of the policies?
toboggan_password_check(dat)
