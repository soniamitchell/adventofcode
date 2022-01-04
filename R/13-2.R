#' @rdname day13
#' @param paper paper
#' @export
#'
simulate_origami <- function(paper, instructions) {
  # Finish folding the transparent paper according to the instructions. The
  # manual says the code is always eight capital letters.
  for (i in seq_len(nrow(instructions))) {
    if (instructions$axis[i] == "x") {
      paper <- fold_left(paper, instructions$value[i])

    } else if(instructions$axis[i] == "y") {
      paper <- fold_up(paper, instructions$value[i])
    }
  }
  coords <- which(paper, arr.ind = TRUE) |>
    as.data.frame() |>
    dplyr::mutate(value = 1,
                  row = max(row) - row)

  ggplot2::ggplot(coords, ggplot2::aes(x = col, y = row, fill = value)) +
    ggplot2::geom_tile(fill = "goldenrod2") +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}

#' @description Make a horizontal fold
#' @rdname day13
#' @export
#'
fold_up <- function(grid, value) {
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
