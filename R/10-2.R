#' @rdname day10
#' @export
#'
syntax_score2 <- function(dat, remove_lines) {
  # Remove corrupted lines
  incomplete_lines <- dat[-remove_lines]

  results <- c()
  for (i in seq_along(incomplete_lines)){
    expecting <- c()
    check_this <- strsplit(incomplete_lines[i], "")[[1]]
    points <- 0

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
        }
      }
    }
    expecting <- rev(expecting)

    # Calculate score
    for (k in seq_along(expecting)) {
      points <- (points * 5) + incomplete_score(expecting[k])
    }
    results <- c(results, points)
  }
  index <- ceiling(length(results) / 2)
  value <- sort(results)[index]
  format(value, scientific = FALSE)
}

incomplete_score <- function(character) {
  score_table <- data.frame(char = c(")", "]", "}", ">"),
                            val = c(1, 2, 3, 4))
  score_table$val[which(score_table$char == character)]
}
