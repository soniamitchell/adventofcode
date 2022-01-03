# Define functions --------------------------------------------------------

tidy_day22 <- function(dat, limit) {
  regex <- paste0("(\\D*)\\sx=(-?\\d*)..(-?\\d*),y=(-?\\d*)..(-?\\d*),",
                  "z=(-?\\d*)..(-?\\d*)")
  out <- dat |> data.frame() |>
    setNames("x") |>
    tidyr::extract(x, c("on", "x1", "x2", "y1", "y2", "z1", "z2"),
                   regex) |>
    dplyr::mutate(on = dplyr::if_else(on == "on", TRUE, FALSE)) |>
    dplyr::mutate_if(is.character, as.numeric)

  if(missing(limit)) {
    limit <- max(max(out), abs(min(out)))
  }
  # Reset minimum to 1
  dplyr::mutate_if(out, is.numeric, \(z) z + limit + 1)
}

get_indices <- function(df, reactor) {
  reactor[df$x1:df$x2, df$y1:df$y2, df$z1:df$z2] <- df$on
  reactor
}

reboot <- function(dat) {
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
  sum(reactor)
}

# Read in data ------------------------------------------------------------

test <- here("inst", "2021", "day22-test.txt")
path <- here("inst", "2021", "day22.txt")

test_dat <- readLines(test) |> tidy_day22(50)
dat <- readLines(path) |> tidy_day22(50)

# Execute the reboot steps ------------------------------------------------

assertthat::assert_that(reboot(test_dat) == 590784)

# Considering only cubes in the region x=-50..50,y=-50..50,z=-50..50, how many
# cubes are on?
reboot(dat)
