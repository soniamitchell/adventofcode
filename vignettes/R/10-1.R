# Read in data ------------------------------------------------------------

dat <- here("inst", "2021", "day10.txt") |>
  readLines()

corrupted_score <- function(character) {
  score_table <- data.frame(char = c(")", "]", "}", ">"),
                            val = c(3, 57, 1197, 25137))
  score_table$val[which(score_table$char == character)]
}

# Run simulation ----------------------------------------------------------

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

# Find the first illegal character in each corrupted line of the navigation
# subsystem. What is the total syntax error score for those errors?
total
