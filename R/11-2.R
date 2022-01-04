#' @rdname day11
#' @export
#'
simulate_octopus2 <- function(octopus) {
  count <- 1
  # What is the first step during which all octopuses flash?
  while (sum(octopus) != 0) {
    # cat("\r", count)
    data <- timestep(octopus)
    octopus <- data$octopus
    count <- count + 1
  }
  count
}
