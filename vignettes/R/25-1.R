# Read in data ------------------------------------------------------------

# Read in polymer template
dat <- readLines(here("inst", "2021", "day25.txt")) |>
  strsplit("") |>
  do.call(what = rbind)

east_cucumbers <- which(dat == ">", arr.ind = TRUE) |>
  data.frame() |>
  dplyr::mutate(type = "east")
south_cucumbers <- which(dat == "v", arr.ind = TRUE) |>
  data.frame() |>
  dplyr::mutate(type = "south")
cucumbers <- rbind(east_cucumbers, south_cucumbers) |>
  dplyr::mutate_if(is.numeric, as.double)

# Define functions --------------------------------------------------------

sea_cucumbers <- R6::R6Class("cucumbers", list(
  dat = NULL,
  cucumbers = NULL,
  continue_east = NULL,
  continue_south = NULL,

  initialize = function(dat, cucumbers) {
    self$dat <- dat
    self$cucumbers <- dplyr::mutate(cucumbers, id = dplyr::row_number())
    self$continue_east <- TRUE
    self$continue_south <- TRUE
    invisible(self)
  },

  print = function(...) {
    cat("\n")
    for (i in seq_len(nrow(self$dat))){
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
    updated_cucumbers <- moved |>
      dplyr::select(-prev) |>
      rbind(stationary)

    # Update object
    self$dat <- dat
    self$cucumbers <- updated_cucumbers
    self$continue_east <- nrow(moved) > 0

    invisible(self)
  },

  move_south = function() {
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
    updated_cucumbers <- moved |>
      dplyr::select(-prev) |>
      rbind(stationary)

    # Update object
    self$dat <- dat
    self$cucumbers <- updated_cucumbers
    self$continue_south <-  nrow(moved) > 0

    # print(self)
    invisible(self)
  }
))

# Run simulation ----------------------------------------------------------

# Initialise variables
track_cucumbers <- sea_cucumbers$new(dat, cucumbers)
continue <- TRUE
i <- 0

# Find somewhere safe to land your submarine

while (continue) {
  i <- i + 1

  cat("\r", i)
  east <- track_cucumbers$move_east()
  south <- track_cucumbers$move_south()
  cucumbers <- south$cucumbers
  continue <- track_cucumbers$continue_east | track_cucumbers$continue_south
}

# What is the first step on which no sea cucumbers move?
i
