# Read in data ------------------------------------------------------------

dat <- here("inst", "2021", "day13.txt") |>
  read.table(sep = "-")

# Tidy up data
dots <- dat |>
  dplyr::filter(!grepl("^fold", V1)) |>
  tidyr::separate(V1, c("y", "x"), convert = TRUE) |>
  dplyr::mutate(x = x + 1,
                y = y + 1) |>
  dplyr::select(x, y)

xlim <- max(dots$x)
ylim <- max(dots$y)

instructions <- dplyr::filter(dat, grepl("^fold", V1)) |>
  dplyr::mutate(V1 = gsub("^.*([a-z]=[0-9]*)$", "\\1", V1)) |>
  tidyr::separate(V1, c("axis", "value")) |>
  dplyr::mutate(value = as.numeric(value) + 1)

# Generate grid of dots
paper <- matrix(FALSE, nrow = xlim, ncol = ylim)
for (i in seq_len(nrow(dots))) paper[dots$x[i], dots$y[i]] <- TRUE

# Define functions --------------------------------------------------------

fold_left <- function(grid, value) {
  #' Make a vertical fold

  left <- grid[, 1:(value - 1)]
  right <- grid[, (value + 1):ncol(grid)]
  pad <- matrix(0, nrow = nrow(grid), ncol = abs(ncol(left) - ncol(right)))
  if (ncol(right) < ncol(left)) {
    right <- cbind(right, pad)
  } else if (ncol(right) > ncol(left)) {
    left <- cbind(pad, left)
  }
  left | right[, ncol(right):1]
}

# Run simulation ----------------------------------------------------------

# Make first fold
results <- fold_left(paper, instructions[1,]$value)

# How many dots are visible after completing just the first fold instruction on
# your transparent paper?
sum(results > 0)
