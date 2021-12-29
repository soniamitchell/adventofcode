current_string <- strsplit(template, "")[[1]]

for (i in 1:10) {
  # cat("\r", i)
  pairs <- vapply(seq_len(length(current_string) - 1), function(x)
    paste(current_string[x:(x + 1)], collapse = ""), character(1)) |>
    data.frame() |>
    setNames("pair")

  next_string <- dplyr::left_join(pairs, rules,
                                  by = "pair") |>
    tidyr::separate(pair, c("first", "last"), "(?<=[A-Z])") |>
    tidyr::unite(string, c(first, insert), sep = "", na.rm = TRUE) |>
    dplyr::select(string) |>
    unlist() |>
    paste(collapse = "") |>
    paste0(tail(current_string, 1, collapse = ""))

  current_string <- strsplit(next_string, "")[[1]]
}

freq <- table(current_string)
max(freq) - min(freq)
