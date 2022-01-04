#' Day 25: Sea Cucumber
#' @source <https://adventofcode.com/2021/day/25>
#' @name day25
#'
NULL

#' @description Read in cucumber positions
#' @rdname day25
#' @param path file path
#' @export
#'
read_day25 <- function(path) {
  path |>
    readLines() |>
    strsplit("") |>
    do.call(what = rbind)
}

#' @rdname day25
#' @param dat dat
#' @export
#'
get_cucumbers <- function(dat) {
  east_cucumbers <- which(dat == ">", arr.ind = TRUE) |>
    data.frame() |>
    dplyr::mutate(type = "east")
  south_cucumbers <- which(dat == "v", arr.ind = TRUE) |>
    data.frame() |>
    dplyr::mutate(type = "south")
  rbind(east_cucumbers, south_cucumbers) |>
    dplyr::mutate_if(is.numeric, as.double)
}

#' @rdname day25
#' @param dat dat
#' @param cucumbers cucumbers
#' @export
#'
simulate_cucumbers <- function(dat, cucumbers) {
  # Initialise variables
  track_cucumbers <- sea_cucumbers$new(dat, cucumbers)
  continue <- TRUE
  i <- 0

  # Find somewhere safe to land your submarine

  while (continue) {
    i <- i + 1
    # cat("\r", i)
    track_cucumbers$move_east()
    track_cucumbers$move_south()
    continue <- track_cucumbers$continue_east | track_cucumbers$continue_south
  }
  i
}

sea_cucumbers <- R6::R6Class("cucumbers", list(
  dat = NULL,
  cucumbers = NULL,
  continue_east = NULL,
  continue_south = NULL,
  plot = NULL,

  initialize = function(dat, cucumbers) {
    self$dat <- dat
    self$cucumbers <- dplyr::mutate(cucumbers, id = dplyr::row_number())
    self$continue_east <- TRUE
    self$continue_south <- TRUE
    invisible(self)
  },

  print = function(...) {
    cat("\n")
    for (i in seq_len(nrow(self$dat))) {
      cat(self$dat[i, ], "\n")
    }
    invisible(self)
  },

  move_east = function() {
    dat <- self$dat
    cucumbers <- self$cucumbers
    boundary <- ncol(dat) + 1

    # Determine which cucumbers move
    tmp <- cucumbers |>
      dplyr::mutate(prev = col,
                    col = dplyr::case_when(type == "east" ~ col + 1,
                                           TRUE ~ col)) |>
      dplyr::mutate(col = dplyr::case_when(col == boundary ~ 1,
                                           TRUE ~ col))

    # These do
    moved <- tmp |>
      dplyr::filter(type == "east") |>
      dplyr::anti_join(cucumbers, by = c("row", "col"))

    # These don't
    stationary <- dplyr::anti_join(cucumbers,
                                   dplyr::select(moved, -prev), by = c("id"))

    # Move cucumbers east
    dat[as.matrix(dplyr::select(moved, row, col))] <- ">"

    # Place a dot in their previous position
    prev <- dplyr::select(moved, row, prev)
    dat[as.matrix(prev)] <- "."

    # Update the cucumber list
    self$cucumbers <- moved |>
      dplyr::select(-prev) |>
      rbind(stationary)

    # Update objects
    self$dat <- dat
    self$continue_east <- nrow(moved) > 0

    invisible(self)
  },

  move_south = function(display = FALSE) {
    dat <- self$dat
    cucumbers <- self$cucumbers
    boundary <- nrow(dat) + 1

    # Determine which cucumbers move
    tmp <- cucumbers |>
      dplyr::mutate(prev = row,
                    row = dplyr::case_when(type == "south" ~ row + 1,
                                           TRUE ~ row)) |>
      dplyr::mutate(row = dplyr::case_when(row == boundary ~ 1,
                                           TRUE ~ row))

    # These do
    moved <- tmp |>
      dplyr::filter(type == "south") |>
      dplyr::anti_join(cucumbers, by = c("row", "col"))

    # These don't
    stationary <- dplyr::anti_join(cucumbers,
                                   dplyr::select(moved, -prev), by = c("id"))

    # Move cucumbers south
    dat[as.matrix(dplyr::select(moved, row, col))] <- "v"

    # Place a dot in their previous position
    prev <- dplyr::select(moved, prev, col)
    dat[as.matrix(prev)] <- "."

    # Update the cucumber list
    self$cucumbers <- moved |>
      dplyr::select(-prev) |>
      rbind(stationary)

    # Update objects
    self$dat <- dat
    self$continue_south <-  nrow(moved) > 0

    # Print to console
    if (display) print(self)

    invisible(self)
  },

  gg_cucumbers = function() {
    rows <- seq_len(nrow(self$dat))
    cols <- seq_len(ncol(self$dat))
    fill <- c("#489FB5", "#EDE7E3", "#FFA62B")

    self$cucumbers |>
      dplyr::select(-id) |>
      tidyr::complete(row = rows, col = cols,
                      fill = list(type = "empty")) |>
      dplyr::mutate(text = dplyr::case_when(type == "south" ~ "v",
                                            type == "east" ~ ">",
                                            TRUE ~ "")) |>
      ggplot2::ggplot(ggplot2::aes(x = col, y = row, fill = type)) +
      ggplot2::theme_void() +  ggplot2::coord_fixed() +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_fill_manual(values = fill) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = text)) +
      ggplot2::theme(legend.position = "none")
  }
))
