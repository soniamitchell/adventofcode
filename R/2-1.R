#' Day 2: Dive!
#' @source <https://adventofcode.com/2021/day/2>
#' @name day2
#'
NULL

#' @rdname day2
#' @param path file path
#' @export
#'
read_day2 <- function(path) {
  path |>
    read.table() |>
    setNames(c("direction", "value"))
}

#' @rdname day2
#' @param dat dat
#' @export
dive <- function(dat) {
  summarise_dat <- dat |>
    dplyr::group_by(direction) |>
    dplyr::summarise(total = sum(value))

  horizontal_position <- summarise_dat$total[summarise_dat$direction == "forward"]
  depth <- summarise_dat$total[summarise_dat$direction == "down"] -
    summarise_dat$total[summarise_dat$direction == "up"]

  horizontal_position * depth
}
