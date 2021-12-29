# Read in data ------------------------------------------------------------

# Read in polymer template
path <- here("inst", "2021", "day14.txt")
template <- path |>
  scan(what = "character", n = 1)

# Read in pair insertion rules
rules <- path |>
  scan(what = "character", sep = "\n", skip = 2) |>
  data.frame() |>
  setNames("x") |>
  tidyr::separate(x, c("pair", "insert"), " -> ")

# Run simulation ----------------------------------------------------------

current_string <- strsplit(template, "")[[1]]

# Apply 10 steps of pair insertion to the polymer template and find the most
# and least common elements in the result
for (i in seq_len(10)) {
  next_string <- ""
  # cat("\r", i)
  for (j in seq_len(length(current_string) - 1)) {
    index <- which(rules$pair == paste(current_string[j:(j + 1)],
                                       collapse = ""))
    if (length(index) != 0) {
      next_string <- paste0(next_string, current_string[j], rules$insert[index])
    } else {
      next_string <- paste0(next_string, current_string[j])
    }
  }
  next_string <- paste0(next_string, tail(current_string, 1))
  current_string <- strsplit(next_string, "")[[1]]
}

# What do you get if you take the quantity of the most common element and
# subtract the quantity of the least common element?
freq <- table(current_string)
max(freq) - min(freq)
