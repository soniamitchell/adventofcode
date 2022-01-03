# Define functions --------------------------------------------------------

rating <- function(dat, type) {
  # For each bit in a binary string
  for (i in seq_len(digits)) {
    # Extract the i[th] character of each string
    character <- vapply(dat, function(x) substr(x, i, i),
                        character(1))
    # Find zeroes and ones
    zeroes <- character == "0"
    ones <- character == "1"
    # Find most common bit
    if (sum(zeroes) > sum(ones)) {
      bit_criteria <- which(zeroes)
    } else if (sum(zeroes) < sum(ones)) {
      bit_criteria <- which(ones)
    } else {
      bit_criteria <- which(ones)
    }
    # Keep binary strings with most common bit in the ith position
    if (type == "oxygen") {
      dat <- dat[bit_criteria]
    } else if (type == "CO2") {
      dat <- dat[-bit_criteria]
    }
    # If there's only one binary string left, stop
    if (length(dat) == 1) break
  }
  # Return decimal
  strtoi(paste(dat, collapse = ""), base = 2)
}

# Run simulation ----------------------------------------------------------

# Calculate oxygen generator and CO2 scrubber ratings
oxygen_generator_rating <- rating(dat, "oxygen")
CO2_scrubber_rating <- rating(dat, "CO2")

# Calculate life support rating
oxygen_generator_rating * CO2_scrubber_rating
