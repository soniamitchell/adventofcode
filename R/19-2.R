#' @rdname day11
#' @param scanners scanners
#' @export
#'
dist_scanners <- function(scanners) {
  index <- seq_len(nrow(scanners)) - 1
  combinations <- t(combn(index, 2))

  res <- lapply(seq_len(nrow(combinations)), function(x) {
    first <- combinations[x, 1]
    second <- combinations[x, 2]
    a <- scanners |>
      dplyr::filter(scanner == first) |>
      dplyr::select(-scanner) |> unlist()
    b <- scanners |>
      dplyr::filter(scanner == second) |>
      dplyr::select(-scanner) |> unlist()
    manhattan(a, b)
  }) |> unlist()
  max(res)
}

manhattan <- function(a, b) {
  abs(a - b) |>
    sum()
}
