# Initialise routes
unfinished <- dplyr::filter(dat, from == "start")
complete <- data.frame()

last_step <- "to"

# Plot routes through cave system
while (nrow(unfinished) != 0) {
  # Record next step(s) of the route
  unfinished <- dplyr::left_join(unfinished, dat,
                                 by = setNames("from", last_step))

  # Extract column name of the last step
  last_step <- tail(names(unfinished), 1)

  # For each unfinished route, count the number of small caves that were
  # visited more than once
  test <- apply(unfinished, 1, function(x) {
    duplicates <- x[duplicated(unlist(x))]
    islower <- grepl("^[[:lower:]]+$", duplicates)
    sum(islower)})
  # Remove routes where more than one small cave was visited more than once
  index <- which(test > 1)
  if (length(index) > 0)
    unfinished <- unfinished[-index, ]

  # Add completed routes to `complete`
  complete <- dplyr::bind_rows(complete,
                               dplyr::filter(unfinished,
                                             get(last_step) == "end"))

  # Remove completed routes from `unfinished`
  unfinished <- dplyr::filter(unfinished, get(last_step) != "end")
}

# If we visit a single small cave twice, how many paths through this cave
# system are there?
nrow(complete)
