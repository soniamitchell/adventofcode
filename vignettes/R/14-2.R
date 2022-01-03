# Apply 40 steps of pair insertion to the polymer template and find the most
# and least common elements in the result. What do you get if you take the
# quantity of the most common element and subtract the quantity of the least
# common element?
current_string <- strsplit(template, "")[[1]]
iterations <- 10

generate_pairs <- function(x) {
  tmp <- do.call(cbind, data.table::shift(x, 0:1, type = "lead")) |>
    head(-1)
  lapply(seq_len(nrow(tmp)), function(i) tmp[i, ])
}

new_rules <- tidyr::separate(rules, pair, c("first", "last"), "(?<=[A-Z])")

for (i in seq_len(iterations)) {
  cat("\r", i)
  # Generate list of vectors of overlapping pairs
  pairs <- generate_pairs(current_string) |>
    do.call(what = rbind) |>
    data.frame()

  for (j in seq_len(length(pairs))) {
    this_pair <- pairs[[j]]

    # If there's an insertion, insert it back into `pairs`
    insertion <- dplyr::filter(new_rules, first == this_pair[1] &
                                 last == this_pair[2])
    if (nrow(insertion) > 0)
      pairs[[j]] <- append(this_pair, insertion$insert, 1)
  }

  # Flatten list
  current_string <- lapply(pairs, function(x) head(x, -1)) |> unlist()
}

freq <- table(current_string)
max(freq) - min(freq)
