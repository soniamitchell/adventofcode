# Read in data ------------------------------------------------------------

# dat <- scan(here("inst", "data", "2021", "day15.txt"),
#             what = "character") |>
#   strsplit("") |>
#   do.call(rbind, .) |>
#   apply(1, as.numeric)

dat <- scan(text = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581", what = "character") |>
  strsplit("") |>
  do.call(what = cbind) |>
  apply(1, as.numeric)

# Functions ---------------------------------------------------------------

n <- ncol(dat) + 1

path_finder <- function(start, end, dat) {

  current <- list(start)
  total <- 0
  continue <- TRUE

  while(continue) {

    running_total <- c()
    coordinates <- list()

    for (i in seq_len(length(current))) {
      this_one <- current[[i]]

      # Get adjacent grid squares
      below <- replace(this_one, 1, this_one[1] + 1)
      right <- replace(this_one, 2, this_one[2] + 1)

      next_steps <- list(below, right)

      for (j in next_steps) {
        if (n %in% j) next              # If coordinate is out of bounds, next
        tmp <- dat[rbind(j)] + total    # Find new total
        running_total <- c(running_total, tmp)
        coordinates <- c(coordinates, list(j))
      }
    }

    if (length(running_total) != 0) {
      index <- running_total %in% min(running_total, na.rm = TRUE)
      total <- unique(running_total[index])
      current <- coordinates[index]
      print(current)
      print(total)
    }

    if (length(current) == 1 && all(unlist(current) == end))
      continue <- FALSE
  }



}

# Run ---------------------------------------------------------------------

# What is the lowest total risk of any path from the top left to the bottom right?
start <- c(1, 1)
end <- c(10, 10)

path_finder(start, end, dat)




