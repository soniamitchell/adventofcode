#' @rdname day5
#' @export
#'
track_vents2 <- function(coordinates) {
  # Initialise object
  xlim <- max(c(coordinates$x1, coordinates$x2))
  ylim <- max(c(coordinates$y1, coordinates$y2))
  grid <- matrix(0, nrow = ylim, ncol = xlim)

  # Track horizontal, vertical lines, and diagonal lines
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

    } else if(abs(this_line$x1 - this_line$x2) ==
              abs(this_line$y1 - this_line$y2)) {
      # Diagonal line
      x_values <- this_line$x1:this_line$x2
      y_values <- this_line$y1:this_line$y2
      # Add one to each point on the line
      for (i in seq_along(x_values))
        grid[x_values[i], y_values[i]] <- grid[x_values[i], y_values[i]] + 1
    }
  }
  sum(grid >= 2)
}
