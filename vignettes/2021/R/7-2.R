# Initialise objects
results <- data.frame(x = 1:xlim, total_fuel_cost = NA)

# Determine the horizontal position that the crabs can align to using the least
# fuel possible
for (position in seq_len(xlim)) {
  fuel_cost <- 0
  for (crab in dat) {
    distance <- abs(crab - position)
    fuel_cost <- fuel_cost + sum(0:distance)
  }
  results$total_fuel_cost[position] <- sum(fuel_cost)
}

lowest <- which.min(results$total_fuel_cost)
results$x[lowest]

# How much fuel must they spend to align to that position?
results$total_fuel_cost[lowest]
