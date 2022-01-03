# Read in data
dat <- here("inst", "2021", "day2.txt") |>
  read.table() |>
  setNames(c("direction", "value"))

# Calculate the horizontal position and depth and multiply them together
summarise_dat <- dat |>
  dplyr::group_by(direction) |>
  dplyr::summarise(total = sum(value))

horizontal_position <- summarise_dat$total[summarise_dat$direction == "forward"]
depth <- summarise_dat$total[summarise_dat$direction == "down"] -
  summarise_dat$total[summarise_dat$direction == "up"]

horizontal_position * depth
