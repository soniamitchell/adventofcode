# Read in data ------------------------------------------------------------

dat <- here("inst", "2021", "day9.txt") |>
  readLines() |>
  strsplit("") |>
  do.call(what = "rbind") |>
  apply(1, as.numeric)

# Define functions --------------------------------------------------------

compare_heights <- function(dat, row, column) {
  #' Find `height` of adjacent points and if it's lower than all
  #' `adjacent_points`, return risk level

  height <- dat[row, column]
  # Extract adjacent heights
  adjacent_points <- c()
  if (i != 1)
    adjacent_points <- c(adjacent_points, dat[row - 1, column]) # above
  if (j != ncol(dat))
    adjacent_points <- c(adjacent_points, dat[row, column + 1]) # right
  if (i != nrow(dat))
    adjacent_points <- c(adjacent_points, dat[row + 1, column]) # below
  if (j != 1)
    adjacent_points <- c(adjacent_points, dat[row, column - 1]) # left
  # Compare height to adjacent heights
  if (all(height < adjacent_points))
    return(data.frame(x = row, y = column, risk = height + 1))
}

# Run simulation ----------------------------------------------------------

# Find all of the low points on the heightmap and calculate risk level
lowest_points <- data.frame(x = numeric(), y = numeric(), risk = numeric())
for (i in seq_len(nrow(dat))) {
  for (j in seq_len(ncol(dat))) {
    lowest_point <- compare_heights(dat, i, j)
    lowest_points <- rbind.data.frame(lowest_points, lowest_point)
  }
}

# What is the sum of the risk levels of all low points on your heightmap?
sum(lowest_points$risk)
