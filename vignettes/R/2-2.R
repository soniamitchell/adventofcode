# Initialise variables
aim <- 0
horizontal_position <- 0
depth <- 0

# Track aim
for (i in seq_len(nrow(dat))) {
  if (dat$direction[i] == "down") {
    aim <- aim  + dat$value[i]
  } else if (dat$direction[i] == "up") {
    aim <- aim  - dat$value[i]
  } else {
    horizontal_position <- horizontal_position + dat$value[i]
    depth <- depth + (aim * dat$value[i])
  }
}

horizontal_position * depth
