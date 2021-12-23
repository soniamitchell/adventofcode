# Read in data ------------------------------------------------------------

# dat <- here("inst", "2021", "day23.txt") |>
#   readLines()

dat <- scan(text = "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########", what = "character")

# Define functions --------------------------------------------------------

tidy_dat23 <- function(dat) {
  len <- lapply(dat, nchar) |>
    unique() |>
    unlist()
  pad <- (max(len) - min(len)) / 2
  lapply(dat, \(x) {
    tmp <- strsplit(x, "")[[1]]
    if(length(tmp) == min(len)) {
      tmp <- c(rep("#", pad), tmp, rep("#", pad))
    }
    tmp
  }) |>
    do.call(what = rbind)
}

# Run simulation ----------------------------------------------------------

dat <- tidy_dat23(dat)

# What is the least energy required to organize the amphipods?
