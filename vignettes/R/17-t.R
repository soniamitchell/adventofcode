# Plot test run -----------------------------------------------------------

dat = "target area: x=20..30, y=-10..-5"

xrange <- gsub("^.*x=(.*)\\.\\.(.*),.*$", "\\1 \\2", dat) %>%
  strsplit(" ") %>% .[[1]] %>% as.numeric()
yrange <- gsub("^.*y=(.*)\\.\\.(.*)$", "\\1 \\2", dat) %>%
  strsplit(" ") %>% .[[1]] %>% as.numeric()

start <- c(0, 0)
velocity <- c(6, 9)
n <- 1

results <- data.frame()

trial <- data.frame(trial = n,
                    step = 0,
                    x = start[1],
                    y = start[2],
                    vx = velocity[1],
                    vy = velocity[2])

results <- launch(trial, n, xrange, yrange) %>%
  rbind(results, .)

results %>%
  dplyr::filter(hit == TRUE) %>%
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, group = n, colour = n)) +
  ggplot2::theme_bw() + ggplot2::geom_point() + ggplot2::geom_line() +
  ggplot2::scale_x_continuous(limits = range(c(xrange, results$x))) +
  ggplot2::scale_y_continuous(limits = range(c(yrange, results$y))) +
  ggplot2::geom_rect(xmin = xrange[1], xmax = xrange[2],
                     ymin = yrange[1], ymax = yrange[2],
                     fill = "transparent", color = "red", size = 1.5)
