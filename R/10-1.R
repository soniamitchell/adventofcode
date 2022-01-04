#' Day 10: Syntax Scoring
#' @source <https://adventofcode.com/2021/day/10>
#' @name day10
#'
NULL

#' @rdname day10
#' @param path file path
#' @export
#'
read_day10 <- function(path) {
  path |>
    readLines()
}

#' @rdname day10
#' @param dat dat
#' @export
#'
syntax_score <- function(dat) {
  total <- 0
  remove_lines <- c()

  for (i in seq_along(dat)){
    expecting <- c()
    points <- NA
    check_this <- strsplit(dat[i], "")[[1]]

    for (j in seq_along(check_this)) {
      if (check_this[j] == "("){
        expecting <- c(expecting, ")")

      } else if (check_this[j] == "[") {
        expecting <- c(expecting, "]")

      } else if (check_this[j] == "{") {
        expecting <- c(expecting, "}")

      } else if (check_this[j] == "<") {
        expecting <- c(expecting, ">")

      } else {
        # If `check_this` matches the last expected value, remove it
        if (check_this[j] == tail(expecting, 1)) {
          expecting <- expecting[-length(expecting)]

        } else if (is.na(points)) {
          # Otherwise add the score to `total`
          points <- corrupted_score(check_this[j])
          total <- total + points
          remove_lines <- c(remove_lines, i)
          break
        }
      }
    }
  }
  list(total = total,
       remove_lines = remove_lines)
}

corrupted_score <- function(character) {
  score_table <- data.frame(char = c(")", "]", "}", ">"),
                            val = c(3, 57, 1197, 25137))
  score_table$val[which(score_table$char == character)]
}
