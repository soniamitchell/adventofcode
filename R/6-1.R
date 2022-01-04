#' Day 6: Lanternfish
#' @source <https://adventofcode.com/2021/day/6>
#' @name day6
#'
NULL

#' @rdname day6
#' @param path file path
#' @export
#'
read_day6 <- function(path) {
  path |>
    scan(what = "character", sep = ",") |>
    as.numeric()
}

#' @rdname day6
#' @param dat dat
#' @export
#'
simulate_lanternfish <- function(dat) {
  # Initialise variables
  days <- 80
  fish <- dat

  # Simulate lanternfish
  for (i in seq_len(days)) {
    # Find zeroes
    zeroes <- which(fish == 0)
    # Subtract 1 from all fish
    fish <- fish - 1
    # Reset zeroes to six
    fish[zeroes] <- 6
    # Add eights for each zero
    fish <- c(fish, rep(8, length(zeroes)))
  }
  length(fish)
}
