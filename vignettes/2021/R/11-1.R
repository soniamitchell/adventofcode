# Define functions --------------------------------------------------------

day11 <- function(path) {
  path |>
    scan(what = "character") |>
    strsplit("") |>
    do.call(what = rbind) |>
    apply(1, as.numeric)
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

timestep <- function(octopus) {
  # Increases the energy level of each octopus by 1
  octopus <- octopus + 1
  done <- matrix(nrow = 0, ncol = 2)

  # Any octopus with an energy level greater than 9 flashes and has its energy
  # level reset to 0, the energy level of all adjacent octopuses (including
  # octopuses that are diagonally adjacent) increases by 1. If this causes an
  # octopus to have an energy level greater than 9, it also flashes
  while (any(octopus > 9)) {
    # Check if energy level is greater than 9
    flash <- which(octopus > 9, arr.ind = T)
    # Flash
    done <- rbind(done, flash)
    # Reset energy
    octopus[flash] <- 0

    # For each flashing octopus
    for (j in seq_len(nrow(flash))) {
      x <- unname(flash[j, "row"])
      y <- unname(flash[j, "col"])
      # Find adjacent octopi
      neighbours <- find_neighbours(x, y, flash, done, octopus)
      # Increase energy level by 1
      octopus[neighbours] <- octopus[neighbours] + 1
    }
  }
  # Latest flashes
  list(octopus = octopus, latest_flashes = nrow(done))
}

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

# Read in data ------------------------------------------------------------

test <- here("inst", "2021", "day11-test.txt")
path <- here("inst", "2021", "day11.txt")

test_dat <- day11(test)
dat <- day11(path)

# Run simulation ----------------------------------------------------------

assertthat::assert_that(simulate_octopus(test_dat) == 1656)

# Given the starting energy levels of the dumbo octopuses in your cavern,
# simulate 100 steps. How many total flashes are there after 100 steps?

simulate_octopus(dat)
