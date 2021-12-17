# Read in data ------------------------------------------------------------
dat <- readLines(file.path(here(), "inst", "2021", "day17.txt"))

# Extract target coordinates
xrange <- gsub("^.*x=(.*)\\.\\.(.*),.*$", "\\1 \\2", dat) %>%
  strsplit(" ") %>% .[[1]] %>% as.numeric()
yrange <- gsub("^.*y=(.*)\\.\\.(.*)$", "\\1 \\2", dat) %>%
  strsplit(" ") %>% .[[1]] %>% as.numeric()

# Define functions --------------------------------------------------------

# Move one step
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

launch <- function(df, trial, xrange, yrange) {
  continue <- TRUE
  step <- 1
  while (continue) {
    update <- tail(df, 1) %>% move()
    df <- rbind(df, mutate(trial = n, step = step, update))
    foundtarget <- dplyr::if_else(update$x >= xrange[1] & update$x <= xrange[2] &
                                    update$y >= yrange[1] & update$y <= yrange[2],
                                  TRUE, FALSE)
    overshot <- dplyr::if_else(update$x > xrange[2] | update$y < yrange[2],
                               TRUE, FALSE)
    continue <- dplyr::if_else(foundtarget | overshot, FALSE, TRUE)
    step <- step + 1
  }
  df %>% mutate(hit = foundtarget)
}

# Run simulation ----------------------------------------------------------

start <- c(0, 0)
velocity <- c(1,1)
n <- 1

results <- data.frame()

for (i in 1:120) {
  trial <- data.frame(trial = n,
                      step = 0,
                      x = start[1],
                      y = start[2],
                      vx = velocity[1],
                      vy = velocity[2])

  results <- launch(trial, n, xrange, yrange) %>%
    rbind(results, .)

  n <- n + 1

  if (tail(results, 1)$x < xrange[1]) {
    velocity[1] <- velocity[1] + 1
  } else if (tail(results, 1)$x > xrange[2]) {
    velocity[1] <- velocity[1] - 1
  } else {
    velocity[2] <- velocity[2] + 1
  }
}

results %>%
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, group = trial, colour = trial)) +
  ggplot2::theme_bw() + ggplot2::geom_point() + ggplot2::geom_line() +
  ggplot2::scale_x_continuous(limits = range(c(xrange, results$x))) +
  ggplot2::scale_y_continuous(limits = range(c(yrange, results$y))) +
  ggplot2::geom_rect(xmin = xrange[1], xmax = xrange[2],
                     ymin = yrange[1], ymax = yrange[2],
                     fill = "transparent", color = "red", size = 1.5) +
  ggplot2::coord_fixed()

# Find the initial velocity that causes the probe to reach the highest y
# position and still eventually be within the target area after any step
this_trial <- results %>%
  dplyr::filter(hit == T) %>%
  dplyr::filter(y == max(y)) %>%
  dplyr::pull(trial) %>%
  unique()

# Initial y velocity
results %>%
  dplyr::filter(trial == this_trial,
                step == 0) %>%
  dplyr::pull(vy)

# What is the highest y position it reaches on this trajectory?
results %>%
  dplyr::filter(trial == this_trial) %>%
  dplyr::filter(y == max(y)) %>%
  dplyr::pull(y) %>%
  unique()
