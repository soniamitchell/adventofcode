#' @rdname day14
#' @export
#'
polymerization2 <- function(template, rules, n) {
  first <- template[1]
  template <- convert_template(template) |>
    dplyr::mutate(count = 1)

  for (i in seq_len(n)) {
    tmp <- dplyr::left_join(template, rules, by = c("first", "last"))

    set1 <- tmp |> dplyr::select(-last) |> dplyr::rename(last = insert)
    set2 <- tmp |> dplyr::select(-first) |> dplyr::rename(first = insert)
    pairs <- rbind(set1, set2)

    counts <- pairs |>
      dplyr::group_by(first, last) |>
      dplyr::summarise(count = sum(count), .groups = "drop")

    template <- counts
  }

  # Final count
  total <- template |>
    dplyr::select(-first) |>
    dplyr::group_by(last) |>
    dplyr::summarise(count = sum(count)) |>
    dplyr::mutate(count = dplyr::case_when(last == first ~ count + 1,
                                           TRUE ~ count))

  max(total$count) - min(total$count)
}
