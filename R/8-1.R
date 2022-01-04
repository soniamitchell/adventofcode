#' Day 8: Seven Segment Search
#' @source <https://adventofcode.com/2021/day/8>
#' @name day8
#'
NULL

#' @rdname day8
#' @param path file path
#' @export
#'
read_day8 <- function(path) {
  path |>
    scan(what = "character", sep = "\n")
}

#' @rdname day8
#' @param dat dat
#' @export
#'
count_digits <- function(dat) {
  results <- c()
  for (i in seq_along(dat)) {
    tmp <- strsplit(dat[i], "\\| ")[[1]][2]
    digits <- strsplit(tmp, " ")[[1]]
    results[i] <- sum(nchar(digits) %in% c(2, 4, 3, 7))
  }
  sum(results)
}
