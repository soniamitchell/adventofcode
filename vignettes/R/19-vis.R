scans[[1]] %>%
  data.frame() |>
  setNames(c("x", "y", "z")) |>
  dplyr::mutate(what = "beacon",
                scanner = NA) |>
  rbind(scanners |>
          mutate(what = "scanner")) |>
  plotly::plot_ly(x = ~x, y = ~y, z = ~z, color = ~what,
                  colors = c("#F4D06F", "#392F5A"),
                  type = "scatter3d", mode = "markers",
                  size = 1, opacity = 0.7)
