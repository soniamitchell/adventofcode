#' Day 14: Extended Polymerization
#' @source <https://adventofcode.com/2021/day/14>
#' @name day14
#'
NULL

#' @rdname day14
#' @param path file path
#' @export
#'
get_template <- function(path) {
  strsplit(readLines(path, n = 1), "") |>
    unlist()
}

#' @rdname day14
#' @export
#'
get_rules <- function(path) {
  tmp <- read.table(path, skip = 1)
  data.frame(first  = substr(tmp[, 1], 1, 1),
             last = substr(tmp[, 1], 2, 2),
             insert = tmp[, 3])
}

#' @rdname day14
#' @param template template
#' @param rules rules
#' @param n n
#' @export
#'
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

convert_template <- function(template) {
  data.frame(first = head(template, -1),
             last = tail(template, -1))
}
