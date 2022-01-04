#' Day 13: Transparent Origami
#' @source <https://adventofcode.com/2021/day/13>
#' @name day13
#'
NULL

#' @rdname day13
#' @param path file path
#' @export
#'
read_day13 <- function(path) {
  read.table(path, sep = "-")
}

#' @rdname day13
#' @param dat dat
#' @export
#'
origami_instructions <- function(dat) {
  dplyr::filter(dat, grepl("^fold", V1)) |>
    dplyr::mutate(V1 = gsub("^.*([a-z]=[0-9]*)$", "\\1", V1)) |>
    tidyr::separate(V1, c("axis", "value")) |>
    dplyr::mutate(value = as.numeric(value) + 1)
}

#' @rdname day13
#' @export
#'
origami_paper <- function(dat) {
  dots <- dat |>
    dplyr::filter(!grepl("^fold", V1)) |>
    tidyr::separate(V1, c("y", "x"), convert = TRUE) |>
    dplyr::mutate(x = x + 1,
                  y = y + 1) |>
    dplyr::select(x, y)

  xlim <- max(dots$x)
  ylim <- max(dots$y)

  # Generate grid of dots
  paper <- matrix(FALSE, nrow = xlim, ncol = ylim)
  for (i in seq_len(nrow(dots))) paper[dots$x[i], dots$y[i]] <- TRUE
}

#' @rdname day13
#' @param grid grid
#' @param value value
#' @export
#'
fold_left <- function(grid, value) {
  left <- grid[, 1:(value - 1)]
  right <- grid[, (value + 1):ncol(grid)]
  pad <- matrix(0, nrow = nrow(grid), ncol = abs(ncol(left) - ncol(right)))
  if (ncol(right) < ncol(left)) {
    right <- cbind(right, pad)
  } else if (ncol(right) > ncol(left)) {
    left <- cbind(pad, left)
  }
  result <- left | right[, ncol(right):1]
  sum(result > 0)
}
