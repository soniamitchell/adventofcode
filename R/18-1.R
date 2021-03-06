#' Day 18: Snailfish
#' @source <https://adventofcode.com/2021/day/18>
#' @name day18
#'
NULL

#' @rdname day18
#' @param path file path
#' @export
#'
read_day18 <- function(path) {
  path |>
    readLines()
}

#' @rdname day18
#' @param dat dat
#' @export
#'
snailfish_maths <- function(dat) {
  first <- dat[1]
  for (i in tail(seq_along(dat), -1)) {
    result <- add_snailfish(first, dat[i])
    first <- result
  }
  check_magnitude(result)
}

# Function to check magnitude of answer
check_magnitude <- function(answer) {
  while(length(answer) != 1) {
    count <- 1
    depth <- 1
    for (i in tail(seq_along(answer), -1)) {
      if (answer[i] == "[") {
        count <- count + 1

      } else if (answer[i] == "]") {
        count <- count - 1
      }
      depth[i] <- count
    }

    deepest <- which(depth == max(depth))

    index <- R.utils::seqToIntervals(deepest) |>
      data.frame() |>
      dplyr::pull(from) |>
      rev() + 1

    for (j in index) {
      first <- 3 * as.numeric(answer[j])
      second <- 2 * as.numeric(answer[j + 2])

      answer <- answer[-c(j:(j + 3))]
      answer[j - 1] <- first + second
    }
  }

  return(as.numeric(answer))
}

# Add snailfish numbers
add_snailfish <- function(first, second) {

  if (length(first) > 1)
    first <- paste(first, collapse = "")

  paste0("[", first, ",", second, "]") |>
    strsplit("") |>
    unlist() |>
    reduce_snailfish()
}

# Function to reduce a snailfish number
reduce_snailfish <- function(dat) {
  try_this <- dat
  continue <- TRUE
  it_split <- FALSE

  while (continue) {
    result <- explode(try_this)
    it_exploded <- !all(is.na(result))

    if (it_exploded) {
      try_this <- result
      it_split <- FALSE
    } else {
      result <- split(try_this)
      it_split <- !all(is.na(result))

      if (!it_exploded & !it_split) {
        return(try_this)
      } else {
        try_this <- result
      }
    }
  }
}

# Function to check for and carry out explosion
explode <- function(dat) {
  count <- 0
  i <- 1
  left <- NA
  right <- NA

  # Check for explosion
  while(i != length(dat)) {
    if (dat[i] == "[") {
      count <- count + 1

    } else if (grepl("[0-9]", dat[i])) {

      if (dat[i + 1] == "," & grepl("[0-9]", dat[i + 2]) & count > 4) {
        right <- head(grep("[0-9]", dat[(i + 3):length(dat)]), 1) + i + 2
        if (length(right) == 0) right <- NA
        left <- tail(grep("[0-9]", dat[1:(i - 1)]), 1)
        if (length(left) == 0) left <- NA
        break
      }

    } else if (dat[i] == "]") {
      count <- count - 1
    }
    i <- i + 1
  }

  # Explode
  if (is.na(left) & is.na(right)) {
    return(NA)

  } else if (is.na(left)) {
    tmp <- dat
    tmp[right] <- as.numeric(dat[right]) + as.numeric(dat[i + 2])
    tmp <- tmp[-c(i:(i + 3))]
    tmp[i - 1] <- 0
    return(tmp)

  } else if (is.na(right)) {
    tmp <- dat[-c(i:(i + 3))]
    tmp[i - 3] <- as.numeric(dat[left]) + as.numeric(dat[i])
    tmp[i - 1] <- 0
    return(tmp)

  } else {
    tmp <- dat
    tmp[left] <- as.numeric(dat[left]) + as.numeric(dat[i])
    tmp[right] <- as.numeric(dat[right]) + as.numeric(dat[i + 2])
    tmp <- tmp[-c(i:(i + 3))]
    tmp[i - 1] <- 0
    return(tmp)
  }
}

# Function to check for and carry out split
split <- function(dat) {
  # Check for split
  index <- NA
  for (i in seq_along(dat)) {
    if (grepl("[0-9]", dat[i]) && as.numeric(dat[i]) >= 10) {
      index <- i
      break
    }
  }

  # Split
  if (is.na(index)) {
    return(NA)

  } else {
    left <- dat[1:(index - 1)]
    right <- dat[(index + 1):length(dat)]
    tmp <- as.numeric(dat[index])
    tmp <- c("[", floor(tmp / 2), ",", ceiling(tmp / 2), "]")
    return(c(left, tmp, right))
  }
}
