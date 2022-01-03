# Read in data
dat <- here("inst", "2021", "day7.txt") |>
  scan(what = "character", sep = ",") |>
  as.numeric()

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
  results$total_fuel_cost[i] <- sum(fuel_cost)
}

lowest <- which.min(results$total_fuel_cost)
results$x[lowest]

# How much fuel must they spend to align to that position?
results$total_fuel_cost[lowest]
