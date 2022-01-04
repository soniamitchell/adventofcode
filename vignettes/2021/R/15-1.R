# Read in data
test <- here("inst", "2021", "day15-test.txt")
path <- here("inst", "2021", "day15.txt")

test_dat <- read_day15(test)
dat <- read_day15(path)

n <- ncol(dat) + 1
start <- c(1, 1)
end <- c(10, 10)

# What is the lowest total risk of any path from the top left to the bottom right?
path_finder(start, end, dat)
