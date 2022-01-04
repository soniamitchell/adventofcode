#' Day 20: Trench Map
#' @source <https://adventofcode.com/2021/day/20>
#' @name day20
#'
NULL

#' @rdname day20
#' @param path file path
#' @export
#'
get_algorithm <- function(path) {
  path |>
    readLines(n = 1) |>
    strsplit("") |>
    unlist()
}

#' @rdname day20
#' @export
#'
get_input <- function(path) {
  path |>
    scan(what = "character", skip = 2) |>
    vapply(function(x) as.data.frame(strsplit(x, "")[[1]]),
           data.frame(1)) |>
    do.call(what = rbind) |>
    unname()
}

#' @rdname day20
#' @param img img
#' @export
#'
view_image <- function(img) {
  expand.grid(seq_len(nrow(img)), seq_len(nrow(img))) |>
    setNames(c("row", "col")) |>
    dplyr::left_join(which(img == "#", arr.ind = TRUE) |>
                       cbind.data.frame(what = "hash"), by = c("row", "col")) |>
    dplyr::mutate(what = dplyr::case_when(is.na(what) ~ "dot",
                                          TRUE ~ what)) |>
    ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::geom_tile(ggplot2::aes(x = row, y = col, fill = what)) +
    ggplot2::scale_fill_manual(values = setNames(c("black", "white"),
                                                 c("hash", "dot"))) +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "none")
}

#' @rdname day20
#' @param input input
#' @param n n
#' @param algorithm algorithm
#' @export
#'
enhance_image <- function(input, n, algorithm) {
  img <- input
  odd <- TRUE
  first <- head(algorithm, 1)
  second <- tail(algorithm, 1)

  # When the first element of the algorithm doesn't match the last one, change
  # the composition of the image border with each iteration
  correction <- first != second

  for (i in seq_len(n)) {
    if (correction)
      pad <- ifelse(odd, second, first)
    img <- img |>
      scan_input(pad) |>
      output_pixel(algorithm) |>
      generate_image()

    odd <- !odd
  }
  img
}

scan_input <- function(input, pad) {
  with_buffer <- rbind(pad, pad, input, pad, pad)
  with_buffer <- cbind(pad, pad, with_buffer, pad, pad) |> unname()

  index <- 2:(ncol(with_buffer) - 1)
  input_coords <- lapply(index, function(x) cbind(x = x, y = index)) |>
    do.call(what = rbind.data.frame)

  lapply(seq_len(nrow(input_coords)), function(x) {
    index <- with_buffer |>
      get_pixels(unlist(input_coords[x, ])) |>
      pix2bin()
    data.frame(index = index,
               x = input_coords$x[x] - 1,
               y = input_coords$y[x] - 1)
  }) |>
    do.call(what = rbind)
}

output_pixel <- function(values, algorithm) {
  values |>
    dplyr::rowwise() |>
    dplyr::mutate(pixel = dplyr::nth(algorithm, index + 1)) |>
    dplyr::select(-index) |>
    data.frame()
}

generate_image <- function(values) {
  matrix(values$pixel, ncol = max(values$y), nrow = max(values$x),
         byrow = TRUE)
}

get_pixels <- function(input, coord) {
  rows <- (coord[1] - 1):(coord[1] + 1)
  cols <- (coord[2] - 1):(coord[2] + 1)
  input[rows, cols]
}

# Convert matrix of input pixels into a decimal number
pix2bin <- function(pixels) {
  tmp <- pixels |>
    t() |>
    matrix(nrow = 1)
  as.numeric(tmp == "#") |>
    paste(collapse = "") |>
    strtoi(base = 2)
}
