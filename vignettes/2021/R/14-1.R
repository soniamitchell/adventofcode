# Define functions --------------------------------------------------------

get_template <- function(path) {
  strsplit(readLines(path, n = 1), "") |>
    unlist()
}

convert_template <- function(template) {
  data.frame(first = head(template, -1),
             last = tail(template, -1))
}

get_rules <- function(path) {
  tmp <- read.table(path, skip = 1)
  data.frame(first  = substr(tmp[, 1], 1, 1),
             last = substr(tmp[, 1], 2, 2),
             insert = tmp[, 3])
}

polymerization <- function(template, rules, n) {
  for (i in seq_len(n)) {
    tmp <- convert_template(template) |>
      dplyr::left_join(rules, by = c("first", "last"))
    template <- tmp |> dplyr::select(insert, last) |>
      t() |>
      as.vector() |>
      append(tmp$first[1], 0)
  }

  freq <- table(template)
  max(freq) - min(freq)
}

# Read in data ------------------------------------------------------------

test <- here("inst", "2021", "day14-test.txt")
path <- here("inst", "2021", "day14.txt")

# Read in polymer template
test_template <- get_template(test)
template <- get_template(path)

# Read in pair insertion rules
test_rules <- get_rules(test)
rules <- get_rules(path)

# Run simulation ----------------------------------------------------------

assertthat::assert_that(polymerization(test_template, test_rules, 10) == 1588)

# What do you get if you take the quantity of the most common element and
# subtract the quantity of the least common element?
polymerization(template, rules, 10)
