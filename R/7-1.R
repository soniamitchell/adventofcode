#' Day 7: The Treachery of Whales
#' @source <https://adventofcode.com/2021/day/7>
#' @name day7
#'
NULL

#' @rdname day7
#' @param path file path
#' @export
#'
read_day7 <- function(path) {
  path |>
    scan(what = "character", sep = ",") |>
    as.numeric()
}

#' @rdname day7
#' @param dat dat
#' @export
#'
track_crabs <- function(dat) {
  # Initialise objects
  xlim <- max(dat)
  results <- data.frame(x = 1:xlim, total_fuel_cost = NA)

  # Determine the horizontal position that the crabs can align to using the least
  # fuel possible
  for (position in seq_len(xlim)) {
    fuel_cost <- 0
    for (crab in dat) {
      fuel_cost <- fuel_cost + abs(crab - position)
    }
    results$total_fuel_cost[position] <- sum(fuel_cost)
  }
  lowest <- which.min(results$total_fuel_cost)
  results$x[lowest]

  results$total_fuel_cost[lowest]
}
