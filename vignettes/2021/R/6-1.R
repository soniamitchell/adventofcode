# Read in data
dat <- here("inst", "2021", "day6.txt") |>
  scan(what = "character", sep = ",") |>
  as.numeric()

# Initialise variables
days <- 80
fish <- dat

# Simulate lanternfish
for (i in seq_len(days)) {
  # Find zeroes
  zeroes <- which(fish == 0)
  # Subtract 1 from all fish
  fish <- fish - 1
  # Reset zeroes to six
  fish[zeroes] <- 6
  # Add eights for each zero
  fish <- c(fish, rep(8, length(zeroes)))
}

# How many lanternfish would there be after 80 days?
length(fish)
