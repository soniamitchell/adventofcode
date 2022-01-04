#' Day 15: Chiton
#' @source <https://adventofcode.com/2021/day/15>
#' @name day15
#'
NULL

#' @rdname day15
#' @param path file path
#' @export
#'
read_day15 <- function(path) {
  path |>
    scan(what = "character") |>
    strsplit("") |>
    do.call(what = "rbind") |>
    apply(1, as.numeric)
}

#' @rdname day15
#' @param start start
#' @param end end
#' @param dat dat
#' @export
#'
path_finder <- function(start, end, dat) {

  current <- list(start)
  total <- 0
  continue <- TRUE

  while(continue) {

    running_total <- c()
    coordinates <- list()

    for (i in seq_len(length(current))) {
      this_one <- current[[i]]

      # Get adjacent grid squares
      below <- replace(this_one, 1, this_one[1] + 1)
      right <- replace(this_one, 2, this_one[2] + 1)

      next_steps <- list(below, right)

      for (j in next_steps) {
        if (n %in% j) next              # If coordinate is out of bounds, next
        tmp <- dat[rbind(j)] + total    # Find new total
        running_total <- c(running_total, tmp)
        coordinates <- c(coordinates, list(j))
      }

    }

    if (length(running_total) != 0) {
      index <- running_total %in% min(running_total, na.rm = TRUE)
      total <- unique(running_total[index])
      current <- coordinates[index]
      print(current)
      print(total)
    }

    if (length(current) == 1 && all(unlist(current) == end))
      continue <- FALSE
  }

}
