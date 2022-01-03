# Define functions --------------------------------------------------------

follow_waypoint <- R6::R6Class("waypoint", list(
  ship = NULL,
  waypoint = NULL,

  initialize = function() {
    self$ship <- c(0, 0)
    self$waypoint <- c(10, 1)
    invisible(self)
  },

  print = function(...) {
    cat("ship:", self$ship)
    cat("\nwaypoint:", self$waypoint, "\n")
    invisible(self)
  },

  move = function(inst, value) {
    switch(inst,
           "N" = {self$waypoint[2] <- self$waypoint[2] + value},
           "S" = {self$waypoint[2] <- self$waypoint[2] - value},
           "E" = {self$waypoint[1] <- self$waypoint[1] + value},
           "W" = {self$waypoint[1] <- self$waypoint[1] - value},
           "L" = self$rotate(-value),
           "R" = self$rotate(value),
           "F" = self$forward(value))
    invisible(self)
  },

  rotate = function(value) {
    rad <- value * pi / 180
    x <- self$waypoint[1]
    y <- self$waypoint[2]
    newX <- x * cos(rad) + y * sin(rad)
    newY <- y * cos(rad) - x * sin(rad)
    self$waypoint <- c(newX, newY)
    invisible(self)
  },

  forward = function(value) {
    self$ship[1] <- self$ship[1] + (self$waypoint[1] * value)
    self$ship[2] <- self$ship[2] + (self$waypoint[2] * value)
    invisible(self)
  }
))

evade2 <- function(dat) {
  ferry <- follow_waypoint$new()

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

assertthat::assert_that(evade2(test) == 286)

# What is the Manhattan distance between that location and the ship's
# starting position?

evade2(dat)
