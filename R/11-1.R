#' Day 11: Dumbo Octopus
#' @source <https://adventofcode.com/2021/day/11>
#' @name day11
#'
NULL

#' @rdname day11
#' @param path file path
#' @export
#'
read_day11 <- function(path) {
  path |>
    scan(what = "character") |>
    strsplit("") |>
    do.call(what = rbind) |>
    apply(1, as.numeric)
}

#' @rdname day11
#' @param octopus octopus
#' @export
#'
simulate_octopus <- function(octopus) {
  count <- 0
  for (i in 1:100) {
    # cat("\r", i, "of 100")
    data <- timestep(octopus)
    octopus <- data$octopus
    count <- count + data$latest_flashes
  }
  count
}

timestep <- function(octopus) {
  # Increases the energy level of each octopus by 1
  octopus <- octopus + 1
  done <- matrix(nrow = 0, ncol = 2)

  # Octopi with energy levels > 9 flash and their energy levels reset to 0,
  # the energy level of all adjacent octopuses increases by 1. If this causes
  # an octopus to have an energy level > 9, it also flashes
  while (any(octopus > 9)) {
    flash <- which(octopus > 9, arr.ind = T)    # if energy is > 9
    done <- rbind(done, flash)                  # flash
    octopus[flash] <- 0                         # and reset energy level

    # For each flashing octopus, find neighbouring octopi, and increase their
    # energy levels by 1
    for (j in seq_len(nrow(flash))) {
      x <- unname(flash[j, "row"])
      y <- unname(flash[j, "col"])
      neighbours <- find_neighbours(x, y, flash, done, octopus)
      octopus[neighbours] <- octopus[neighbours] + 1
    }
  }

  # Latest flashes
  list(octopus = octopus, latest_flashes = nrow(done))
}

find_neighbours <- function(x, y, flashing_now, flashed_previously, octopus) {
  # Index neighbours
  tmp <- rbind(data.frame(row = x - 1, col = (y - 1):(y + 1)), # above
               data.frame(row = x, col = c(y - 1, y + 1)),     # sides
               data.frame(row = x + 1, col = (y - 1):(y + 1))) # below

  # Remove invalid coordinates
  xlim <- ncol(octopus) + 1
  ylim <- nrow(octopus) + 1
  tmp <- dplyr::filter(tmp, row != 0, col != 0, row != xlim, col != ylim)

  # Add to `neighbours`, remove duplicates, and remove those that are flashing
  # now or have flashed previously
  tmp |>
    unique() |>
    dplyr::anti_join(data.frame(flashing_now), by = c("row", "col")) |>
    dplyr::anti_join(data.frame(flashed_previously), by = c("row", "col")) |>
    as.matrix()
}
