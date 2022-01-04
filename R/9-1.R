#' Day 9: Smoke Basin
#' @source <https://adventofcode.com/2021/day/9>
#' @name day9
#'
NULL

#' @rdname day9
#' @param path file path
#' @export
#'
read_day9 <- function(path) {
  path |>
    readLines() |>
    strsplit("") |>
    do.call(what = "rbind") |>
    apply(1, as.numeric)
}

#' @rdname day9
#' @param dat dat
#' @export
#'
low_points <- function(dat) {
  # Find all of the low points on the heightmap and calculate risk level
  lowest_points <- data.frame(x = numeric(), y = numeric(), risk = numeric())
  for (i in seq_len(nrow(dat))) {
    for (j in seq_len(ncol(dat))) {
      lowest_point <- compare_heights(dat, i, j)
      lowest_points <- rbind.data.frame(lowest_points, lowest_point)
    }
  }
  lowest_points
}

#' Find `height` of adjacent points and if it's lower than all
#' `adjacent_points`, return risk level
compare_heights <- function(dat, row, column) {
  height <- dat[row, column]
  # Extract adjacent heights
  adjacent_points <- c()
  if (row != 1)
    adjacent_points <- c(adjacent_points, dat[row - 1, column]) # above
  if (column != ncol(dat))
    adjacent_points <- c(adjacent_points, dat[row, column + 1]) # right
  if (row != nrow(dat))
    adjacent_points <- c(adjacent_points, dat[row + 1, column]) # below
  if (column != 1)
    adjacent_points <- c(adjacent_points, dat[row, column - 1]) # left
  # Compare height to adjacent heights
  if (all(height < adjacent_points))
    return(data.frame(x = row, y = column, risk = height + 1))
}
