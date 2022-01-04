#' Day 5: Hydrothermal Venture
#' @source <https://adventofcode.com/2021/day/5>
#' @name day5
#'
NULL

#' @rdname day5
#' @param path file path
#' @export
#'
read_day5 <- function(path) {
  path |>
    read.table() |>
    dplyr::select(-V2) |>
    tidyr::separate(V1, c("x1", "y1")) |>
    tidyr::separate(V3, c("x2", "y2")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
}

#' @rdname day5
#' @param coordinates coordinates
#' @export
#'
track_vents <- function(coordinates) {
  # Initialise object
  xlim <- max(c(coordinates$x1, coordinates$x2))
  ylim <- max(c(coordinates$y1, coordinates$y2))
  grid <- matrix(0, nrow = ylim, ncol = xlim)

  # Track horizontal and vertical lines
  for (i in seq_len(nrow(coordinates))) {
    this_line <- coordinates[i, ]

    if (this_line$x1 == this_line$x2) {
      # Horizontal line
      this_x <- this_line$x1
      y_values <- this_line$y1:this_line$y2
      # Add one to each point on the line
      for (y in y_values) grid[this_x, y] <- grid[this_x, y] + 1

    } else if (this_line$y1 == this_line$y2) {
      # Vertical line
      this_y <- this_line$y1
      x_values <- this_line$x1:this_line$x2
      # Add one to each point on the line
      for (x in x_values) grid[x, this_y] <- grid[x, this_y] + 1
    }
  }
  sum(grid >= 2)
}
