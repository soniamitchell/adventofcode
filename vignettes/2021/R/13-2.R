# Define functions --------------------------------------------------------

fold_up <- function(grid, value) {
  #' Make a horizontal fold

  upper <- grid[1:(value - 1), ]
  lower <- grid[(value + 1):nrow(grid), ]
  pad <- matrix(FALSE, nrow = abs(nrow(upper) - nrow(lower)), ncol = ncol(upper))
  if (nrow(upper) < nrow(lower)) {
    upper <- rbind(pad, upper)
  } else if (nrow(upper) > nrow(lower)) {
    lower <- rbind(lower, pad)
  }
  upper | lower[nrow(lower):1, ]
}

# Run simulation ----------------------------------------------------------

fold_this <- paper

# Finish folding the transparent paper according to the instructions. The
# manual says the code is always eight capital letters.
for (i in seq_len(nrow(instructions))) {
  if (instructions$axis[i] == "x") {
    fold_this <- fold_left(fold_this, instructions$value[i])

  } else if(instructions$axis[i] == "y") {
    fold_this <- fold_up(fold_this, instructions$value[i])
  }
}

# What code do you use to activate the infrared thermal imaging camera system?
coords <- which(fold_this, arr.ind = TRUE) |>
  as.data.frame() |>
  dplyr::mutate(value = 1,
                row = max(row) - row)

ggplot2::ggplot(coords, ggplot2::aes(x = col, y = row, fill = value)) +
  ggplot2::geom_tile(fill = "goldenrod2") +
  ggplot2::coord_fixed() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none")
