# Read in data
dat <- here("inst", "2021", "day12.txt") |>
  read.table(sep = "-")

# Tidy up data
dat <- rbind(dat, rename(dat, V1 = V2, V2 = V1)) |>
  dplyr::rename(from = V1,
                to = V2) |>
  dplyr::filter(from != "end",
                to != "start")

# Initialise routes
completed_routes <- list()
unfinished_routes <- filter(dat, from == "start")
unfinished_routes <- lapply(seq_len(nrow(unfinished_routes)),
                            function(x) unname(unlist(unfinished_routes[x,])))

# Plot routes through cave system
while (length(unfinished_routes) > 0) {
  this_route <- unfinished_routes[[1]]
  this_cave <- tail(this_route, 1)

  # Record next step(s) of the route
  next_steps <- dplyr::filter(dat, from == this_cave, to != "start")$to

  # If the exit has been found, move the route to `completed_routes`
  if ("end" %in% next_steps) {
    completed_routes <- c(completed_routes, list(c(this_route, "end")))
    next_steps <- setdiff(next_steps, "end")
  }

  # If the cave is small and has already been visited, ignore it
  small <- next_steps[vapply(next_steps, function(x)
    grepl("^[[:lower:]]+$", x), logical(1))]
  big <- setdiff(next_steps, small)
  next_steps <- c(setdiff(small, this_route), big)

  # Record next step(s) of the route
  add_these <- lapply(next_steps, function(x) c(this_route, x))

  # Record steps and tidy up
  unfinished_routes <- c(unfinished_routes, add_these)
  unfinished_routes <- unfinished_routes[-1]
}

# How many paths through this cave system are there that visit small caves at
# most once?
length(completed_routes)
