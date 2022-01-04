#' Day 17: Trick Shot
#' @source <https://adventofcode.com/2021/day/17>
#' @name day17
#'
NULL

#' @rdname day17
#' @param path file path
#' @export
#'
read_day17 <- function(path) {
  dat <- path |>
    readLines()

  # Extract target coordinates
  xrange <- gsub("^.*x=(.*)\\.\\.(.*),.*$", "\\1 \\2", dat) |>
    strsplit(" ") |>
    unlist() |>
    as.numeric()
  yrange <- gsub("^.*y=(.*)\\.\\.(.*)$", "\\1 \\2", dat) |>
    strsplit(" ") |>
    unlist() |>
    as.numeric()

  list(xrange = xrange, yrange = yrange)
}

#' @rdname day17
#' @param xrange xrange
#' @param yrange yrange
#' @export
#'
simulate_launch <- function(xrange, yrange) {
  start <- c(0, 0)
  velocity <- c(0, 1)
  n <- 1

  results <- data.frame()

  for (i in 1:120) {
    trial <- data.frame(trial = n,
                        step = 0,
                        x = start[1],
                        y = start[2],
                        vx = velocity[1],
                        vy = velocity[2])

    results <- rbind(results, launch(trial, n, xrange, yrange))

    n <- n + 1

    if (tail(results, 1)$x < xrange[1]) {
      velocity[1] <- velocity[1] + 1
    } else if (tail(results, 1)$x > xrange[2]) {
      velocity[1] <- velocity[1] - 1
    } else {
      velocity[2] <- velocity[2] + 1
    }
  }

  this_trial <- results |>
    dplyr::filter(hit == T) |>
    dplyr::filter(y == max(y)) |>
    dplyr::pull(trial) |>
    unique()

  # Initial y velocity
  results |>
    dplyr::filter(trial == this_trial,
                  step == 0) |>
    dplyr::pull(vy)

  # What is the highest y position it reaches on this trajectory?
  results |>
    dplyr::filter(trial == this_trial) |>
    dplyr::filter(y == max(y)) |>
    dplyr::pull(y) |>
    unique()
}

#' @description Move one step
#' @rdname day17
#' @param status last step
#'
move <- function(status) {
  x <- status$x + status$vx
  y <- status$y + status$vy

  vx <- status$vx
  vy <- status$vy
  vx <- dplyr::if_else(vx > 0, vx - 1,
                       dplyr::if_else(vx < 0, vx + 1, vx))
  vy <- vy - 1

  data.frame(x = x, y = y, vx = vx, vy = vy)
}

#' @description Launch probe until target is hit or overshot
#' @rdname day17
#'
launch <- function(df, trial, xrange, yrange) {
  continue <- TRUE
  step <- 1
  while (continue) {
    update <- move(tail(df, 1))
    df <- rbind(df, mutate(trial = trial, step = step, update))
    foundtarget <- dplyr::if_else(
      update$x >= xrange[1] & update$x <= xrange[2] &
        update$y >= yrange[1] & update$y <= yrange[2],
      TRUE, FALSE)
    overshot <- dplyr::if_else(update$x > xrange[2] | update$y < yrange[2],
                               TRUE, FALSE)
    continue <- dplyr::if_else(foundtarget | overshot, FALSE, TRUE)
    step <- step + 1
  }
  mutate(df, hit = foundtarget)
}
