# Read in data ------------------------------------------------------------

dat <- here("inst", "2021", "day22.txt") |>
  readLines()

# Define functions --------------------------------------------------------

tidy_day22 <- function(dat, limit) {
  out <- dat |> data.frame() |>
    setNames("x") |>
    tidyr::separate(x, c("on", "x"), " ") |>
    tidyr::separate(x, c("x", "y", "z"), ",") |>
    dplyr::mutate(on = dplyr::if_else(on == "on", TRUE, FALSE)) |>
    dplyr::mutate_if(is.character, \(z)
                     gsub("^.*=(-?\\d*\\.\\.-?\\d*)$", "\\1", z)) |>
    tidyr::separate(x, c("x1", "x2"), "\\.\\.") |>
    tidyr::separate(y, c("y1", "y2"), "\\.\\.") |>
    tidyr::separate(z, c("z1", "z2"), "\\.\\.") |>
    dplyr::mutate_if(is.character, as.numeric)

  if(missing(limit)) {
    limit <- max(max(out), abs(min(out)))
  }
  dplyr::mutate_if(out, is.numeric, \(z) z + limit + 1)
}

get_indices <- function(df, reactor) {
  reactor[df$x1:df$x2, df$y1:df$y2, df$z1:df$z2] <- df$on
  reactor
}

# Run simulation ----------------------------------------------------------

dat <- tidy_day22(dat, 50)

# Execute the reboot steps

reactor <- array(FALSE, dim = c(101, 101, 101))

for (i in seq_len(nrow(dat))) {
  # Are any coordintes outside the range?
  test <- dat[i, ] |>
    unlist() |>
    tail(-1) |>
    {\(x) dplyr::between(x, 1, max(dim(reactor)))}() |>
    all()

  if (test) {
    reactor <- get_indices(dat[i, ], reactor)
  } else {
    next
  }
}

# Considering only cubes in the region x=-50..50,y=-50..50,z=-50..50, how many
# cubes are on?

sum(reactor)
