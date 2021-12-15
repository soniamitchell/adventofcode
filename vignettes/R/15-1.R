# Read in data ------------------------------------------------------------

# dat <- scan(file.path(here(), "inst", "2021", "day15.txt"),
#             what = "character") %>%
#   strsplit("") %>%
#   do.call(rbind, .) %>%
#   apply(1, as.numeric)

dat <- scan(text = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581", what = "character") %>%
  strsplit("") %>%
  do.call(rbind, .) %>%
  apply(1, as.numeric)

# Tidy up data ------------------------------------------------------------

grid <- expand.grid(row = seq_len(nrow(dat)), col = seq_len(ncol(dat))) %>%
  dplyr::arrange(row, col) %>%
  dplyr::mutate(value = dat[cbind(row, col)])

# Functions ---------------------------------------------------------------

path_finder <- function(start, end, reference, total = 0) {
  # Get adjacent grid squares
  above <- dplyr::filter(grid, row == (start[1] - 1), col == start[2])
  below <- dplyr::filter(grid, row == (start[1] + 1), col == start[2])
  left <- dplyr::filter(grid, row == start[1], col == (start[2] - 1))
  right <- dplyr::filter(grid, row == start[1], col == (start[2] + 1))

  next_steps <- list(above, below, left, right)

  for(x in next_steps) {
    # Calculate new total
    total <- total + x$value
    # Has the `end` been found? - if the grid coordinate doesn't exist then
    # FALSE, otherwise check if it matches `end`
    ended <- dplyr::if_else(nrow(x) == 0, FALSE, all(c(x$row, x$col) == end))

    # If an end has been found then return `total`, otherwise keep going
    if (ended) {
      return(total)

    } else {
      path_finder(c(x$row, x$col), end, reference, total)
    }
  }
}

# Run ---------------------------------------------------------------------

# What is the lowest total risk of any path from the top left to the bottom right?
start <- c(1, 1)
end <- c(100, 100)

tmp <- path_finder(start, end, grid)
