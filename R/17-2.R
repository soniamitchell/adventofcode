#' @rdname day17
#' @export
#'
find_possibilities <- function(xrange, yrange) {
  # Minimum x_velocity (when x_velocity = i, distance in x axis = cumsum(1:i))
  xv_min <- optimize(function(x) abs(max(cumsum(1:x)) - xrange[1]), 1:10) |>
    purrr::pluck("minimum") |> round()

  # Which velocities might hit the target?
  poss_xv <- possible_x(range = xrange,
                        min_v = xv_min,
                        max_v = 500)

  poss_yv <- possible_y(range = yrange,
                        min_v = -2500,
                        max_v = 500)

  expand.grid(poss_xv, poss_yv) |>
    dplyr::rename(xv = Var1, yv = Var2)
}

#' @rdname day17
#' @param velocities velocities
#' @export
#'
simulate_launch2 <- function(velocities, xrange, yrange) {
  lapply(seq_len(nrow(velocities)), function(i) {
    # Extract velocities
    xv <- velocities$xv[i]
    yv <- velocities$yv[i]

    # Calculate trajectory
    z <- max(abs(yrange))

    if (yv > 0) {
      y <- c(0, cumsum(seq(yv, 1, -1)))
      y <- c(y, rev(y), -cumsum((yv + 1):(yv * z)))
    } else if (yv < 0) {
      y <- c(0, cumsum(yv:(yv * z)))
    } else {
      y <- c(0, 0)
      subtract <- 1
      while (min(y) > yrange[1]) {
        y <- c(y, (tail(y, 1) - subtract))
        subtract <- subtract + 1
      }
    }

    x <- c(0, cumsum(seq(xv, 1, -1)))

    if (length(x) > length(y)) {
      y <- c(y, rep(tail(y, 1), length(x) - length(y)))
    } else {
      x <- c(x, rep(tail(x, 1), length(y) - length(x)))
    }

    # Paste coordinates together
    trajectory <- cbind.data.frame(x = x, y = y)

    # Check whether points have hit the target
    hit <- trajectory |>
      dplyr::filter(between(x, xrange[1], xrange[2]),
                    between(y, yrange[1], yrange[2]))

    if (nrow(hit) != 0) {
      out <- data.frame(xv = xv,
                        yv = yv,
                        y_max = max(trajectory$y))
    } else {
      out <- NULL
    }
    out
  }) |>
    do.call(what = rbind)
}

#' @description Test which velocities might hit the target
#' @rdname day17
#' @param range range of target
#' @param min_v minimum velocity to try
#' @param max_v maximum velocity to try
possible_x <- function(range, min_v, max_v) {
  output <- c()
  for (v in min_v:max_v) {
    # Calculate position of points
    points <- cumsum(seq(v, 1, -1))

    # Check if any points hit the target zone
    if (any(between(points, min(range), max(range))))
      output <- c(output, v)
  }
  output
}

#' @description Test which velocities might hit the target
#' @rdname day17
possible_y <- function(range, min_v, max_v) {
  output <- c()
  for (v in min_v:max_v) {
    z <- max(abs(range))

    # Calculate position of points below the x axis
    if (v > 0) {
      points <- -cumsum((v + 1):(v * z))
    } else if (v < 0) {
      points <- cumsum(seq(v, (v * z), -1))
    } else {
      points <- 0
      subtract <- 1
      while (min(points) > range[1]) {
        points <- c(points, (tail(points, 1) - subtract))
        subtract <- subtract + 1
      }
    }

    # Check if any points hit the target zone
    if (any(between(points, min(range), max(range))))
      output <- c(output, v)
  }
  output
}
