# Read in data
dat <- here("inst", "2021", "day8.txt") |>
  scan(what = "character", sep = "\n")

# Tidy up data
results <- c()
for (i in seq_along(dat)) {
  tmp <- strsplit(dat[i], "\\| ")[[1]][2]
  digits <- strsplit(tmp, " ")[[1]]
  results[i] <- sum(nchar(digits) %in% c(2, 4, 3, 7))
}

# How many times do digits 1, 4, 7, or 8 (length 2, 4, 3, and 7) appear?
sum(results)
