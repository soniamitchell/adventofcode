# Read in data
dat <- scan(here("inst", "2021", "day1.txt"))

# Count the number of times the depth increases from the previous measurement
sum(diff(dat) > 0)
