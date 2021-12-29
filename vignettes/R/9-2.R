# Define functions --------------------------------------------------------

check_height <- function(dat, row, column) {
  #' Check height is less than 9

  if (dat[row, column] < 9)
    return(data.frame(row = row, column = column, height = dat[row, column]))
}

check_adjacent <- function(dat, row, column) {
  #' Return neighbouring points that have a height less than 9

  # Find height of adjacent points
  adjacent_points <- data.frame(row = numeric(), column = numeric(),
                                height = numeric())

  if (row != 1)            # Above
    adjacent_points <- rbind(adjacent_points, check_height(dat, row - 1, column))
  if (column != ncol(dat)) # Right
    adjacent_points <- rbind(adjacent_points, check_height(dat, row, column + 1))
  if (row != nrow(dat))    # Below
    adjacent_points <- rbind(adjacent_points, check_height(dat, row + 1, column))
  if (column != 1)         # Left
    adjacent_points <- rbind(adjacent_points, check_height(dat, row, column - 1))

  # Return neibouring points that should be checked
  adjacent_points |>
    dplyr::filter(height != 9)
}

survey <- function(dat, row, column) {
  #' For a particular `lowest_point`, survey the basin and return it's size

  # Height of lowest point being surveyed
  height <- dat[row, column]
  # Initialise basin dataframe
  basin <- data.frame(row = row, column = column, risk = height + 1)
  # Find adjacent points with depth less than 9
  neighbours <- check_adjacent(dat, row, column)
  # Add them to the basin
  tmp <- neighbours |>
    dplyr::mutate(risk = height + 1) |>
    dplyr::select(-height)
  basin <- rbind(basin, tmp)

  # Check neighbouring points
  continue <- nrow(neighbours) > 0
  while(continue) {
    more_neighbours <- check_adjacent(dat,
                                      neighbours$row[1],
                                      neighbours$column[1])
    # Remove this point from `neighbours`
    neighbours <- neighbours[-1, ]
    # Remove new neighbours that are already in the basin
    more_neighbours <- setdiff(dplyr::select(more_neighbours, -height),
                               dplyr::select(basin, -risk)) |>
      dplyr::left_join(more_neighbours, by = c("row", "column"))

    # If any new neighbours are remaining
    if (nrow(more_neighbours) > 0) {
      # Add them to the basin
      tmp <- more_neighbours |>
        dplyr::mutate(risk = height + 1) |>
        dplyr::select(-height)
      basin <- rbind(basin, tmp)
      # And add them to `neighbours` (for checking)
      neighbours <- rbind(neighbours, more_neighbours)
    }

    continue <- nrow(neighbours) > 0
  }
  # Return size of basin
  nrow(basin)
}

# Run simulation ----------------------------------------------------------

# Find all of the low points on the heightmap and calculate basin size
results <- c()
for (i in seq_len(nrow(lowest_points))) {
  # cat("\r", i, "/", nrow(lowest_points))
  size <- survey(dat, lowest_points$x[i], lowest_points$y[i])
  results <- c(results, size)
}

# Multiply together the sizes of the three largest basins
prod(tail(sort(results), 3))
