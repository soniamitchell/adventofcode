# Read in data
dat <- scan(here("inst", "2021", "day3.txt"), what = "character")

# Number of bits in each binary number
digits <- nchar(dat[1])

# Calculate gamma and epsilon rates
gamma_binary <- rep(NA, digits)
epsilon_binary <- rep(NA, digits)

for (i in seq_len(digits)) {
  # Extract the i[th] character of each string
  character <- vapply(dat, function(x) substr(x, i, i), character(1))
  # Find most common bit
  zeroes <- sum(character == "0")
  ones <- sum(character == "1")
  gamma_binary[i] <- dplyr::if_else(zeroes > ones, 0, 1)
  epsilon_binary[i] <- dplyr::if_else(zeroes < ones, 0, 1)
}

gamma_rate <- strtoi(paste(gamma_binary, collapse = ""), base = 2)
epsilon_rate <- strtoi(paste(epsilon_binary, collapse = ""), base = 2)

# Calculate power consumption
gamma_rate * epsilon_rate
