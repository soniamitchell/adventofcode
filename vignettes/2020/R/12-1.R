# Read in data ------------------------------------------------------------

day12input <- function(path) {
  readLines(path) |>
    data.frame() |>
    setNames("dat") |>
    tidyr::separate("dat", into = c("inst", "value"),
                    sep = "(?<=[A-Za-z])(?=[0-9])") |>
    dplyr::mutate(value = as.numeric(value))
}

test <- here("inst", "2020", "day12-test.txt") |>
  day12input()

dat <- here("inst", "2020", "day12.txt") |>
  day12input()

# Define functions --------------------------------------------------------

evasive_actions <- R6::R6Class("ferry", list(
  ship = NULL,
  rot = NULL,

  initialize = function() {
    self$ship <- c(0, 0)
    self$rot <- 90
    invisible(self)
  },

  print = function(...) {
    cat("ship:", self$ship, "/n")
    invisible(self)
  },

  move = function(inst, value) {
    switch(inst,
           "N" = {self$ship[2] <- self$ship[2] + value},
           "S" = {self$ship[2] <- self$ship[2] - value},
           "E" = {self$ship[1] <- self$ship[1] + value},
           "W" = {self$ship[1] <- self$ship[1] - value},
           "L" = {tmp <- self$rot - value
           self$rot <- ifelse(tmp < 0, 360 + tmp, tmp)},
           "R" = {tmp <- self$rot + value
           self$rot <- ifelse(tmp >= 360, tmp - 360, tmp)},
           "F" = self$forward(value))
    invisible(self)
  },

  forward = function(value) {
    if (self$rot == 0) {
      self$move("N", value)
    } else if (self$rot == 90) {
      self$move("E", value)
    } else if (self$rot == 180) {
      self$move("S", value)
    } else if (self$rot == 270) {
      self$move("W", value)
    }
    invisible(self)
  }
))

evade <- function(dat) {
  ferry <- evasive_actions$new()

  # Perform evasive action
  for (i in seq_len(nrow(dat))) {
    ferry$move(dat$inst[i], dat$value[i])
  }

  # Calculate Manhattan distance
  rbind(c(0, 0), ferry$ship) |>
    dist(method = "manhattan") |>
    c()
}

# Take evasive actions ----------------------------------------------------

assertthat::assert_that(evade(test) == 25)

# What is the Manhattan distance between that location and the ship's
# starting position?

evade(dat)
